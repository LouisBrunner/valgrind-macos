/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Read DWARF3/4 ".debug_info" sections (DIE trees).            ---*/
/*---                                                 readdwarf3.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2017 OpenWorks LLP
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)

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

   POTENTIAL BUG?  Spotted 6 Sept 08.  Why doesn't
   unitary_range_list() bias the resulting range list in the same way
   that its more general cousin, get_range_list(), does?  I don't
   know.

   TODO, 2008 Feb 17:

   get rid of cu_svma_known and document the assumed-zero svma hack.

   ML_(sizeOfType): differentiate between zero sized types and types
   for which the size is unknown.  Is this important?  I don't know.

   DW_TAG_array_types: deal with explicit sizes (currently we compute
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

   Deal with DW_TAG_array_types which have element size != stride

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

   Currently, duplicate removal and all other queries for the type
   entities array is done using cuOffset-based pointing, which
   involves a binary search (VG_(lookupXA)) for each access.  This is
   wildly inefficient, although simple.  It would be better to
   translate all the cuOffset-based references (iow, all the "R" and
   "Rs" fields in the TyEnts in 'tyents') to direct index numbers in
   'tyents' right at the start of dedup_types(), and use direct
   indexing (VG_(indexXA)) wherever possible after that.

   cmp__XArrays_of_AddrRange is also a performance bottleneck.  Move
   VG_(indexXA) into pub_tool_xarray.h so it can be inlined at all use
   points, and possibly also make an _UNCHECKED version which skips
   the range checks in performance-critical situations such as this.

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
#include "pub_core_debuginfo.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcsetjmp.h"   // setjmp facilities
#include "pub_core_hashtable.h"
#include "pub_core_options.h"
#include "pub_core_tooliface.h"    /* VG_(needs) */
#include "pub_core_xarray.h"
#include "pub_core_wordfm.h"
#include "priv_misc.h"             /* dinfo_zalloc/free */
#include "priv_image.h"
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
   if (UNLIKELY(td3)) { VG_(printf)(format, ## args); }
#define TD3 (UNLIKELY(td3))

#define D3_INVALID_CUOFF  ((UWord)(-1UL))
#define D3_FAKEVOID_CUOFF ((UWord)(-2UL))

typedef
   struct {
      DiSlice sli;      // to which this cursor applies
      DiOffT  sli_next; // offset in underlying DiImage; must be >= sli.ioff
      void (*barf)( const HChar* ) __attribute__((noreturn));
      const HChar* barfstr;
   }
   Cursor;

static inline Bool is_sane_Cursor ( const Cursor* c ) {
   if (!c)                return False;
   if (!c->barf)          return False;
   if (!c->barfstr)       return False;
   if (!ML_(sli_is_valid)(c->sli))    return False;
   if (c->sli.ioff == DiOffT_INVALID) return False;
   if (c->sli_next < c->sli.ioff)     return False;
   return True;
}

// Initialise a cursor from a DiSlice (ELF section, really) so as to
// start reading at offset |sli_initial_offset| from the start of the
// slice.
static void init_Cursor ( /*OUT*/Cursor* c,
                          DiSlice sli,
                          ULong   sli_initial_offset,
                          __attribute__((noreturn)) void (*barf)(const HChar*),
                          const HChar* barfstr )
{
   vg_assert(c);
   VG_(bzero_inline)(c, sizeof(*c));
   c->sli              = sli;
   c->sli_next         = c->sli.ioff + sli_initial_offset;
   c->barf             = barf;
   c->barfstr          = barfstr;
   vg_assert(is_sane_Cursor(c));
}

static Bool is_at_end_Cursor ( const Cursor* c ) {
   vg_assert(is_sane_Cursor(c));
   return c->sli_next >= c->sli.ioff + c->sli.szB;
}

static inline ULong get_position_of_Cursor ( const Cursor* c ) {
   vg_assert(is_sane_Cursor(c));
   return c->sli_next - c->sli.ioff;
}
static inline void set_position_of_Cursor ( Cursor* c, ULong pos ) {
   c->sli_next = c->sli.ioff + pos;
   vg_assert(is_sane_Cursor(c));
}
static inline void advance_position_of_Cursor ( Cursor* c, ULong delta ) {
   c->sli_next += delta;
   vg_assert(is_sane_Cursor(c));
}

static /*signed*/Long get_remaining_length_Cursor ( const Cursor* c ) {
   vg_assert(is_sane_Cursor(c));
   return c->sli.ioff + c->sli.szB - c->sli_next;
}

//static void* get_address_of_Cursor ( Cursor* c ) {
//   vg_assert(is_sane_Cursor(c));
//   return &c->region_start_img[ c->region_next ];
//}

static DiCursor get_DiCursor_from_Cursor ( const Cursor* c ) {
   return mk_DiCursor(c->sli.img, c->sli_next);
}

/* FIXME: document assumptions on endianness for
   get_UShort/UInt/ULong. */
static inline UChar get_UChar ( Cursor* c ) {
   UChar r;
   vg_assert(is_sane_Cursor(c));
   if (c->sli_next + sizeof(UChar) > c->sli.ioff + c->sli.szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   r = ML_(img_get_UChar)(c->sli.img, c->sli_next);
   c->sli_next += sizeof(UChar);
   return r;
}
static UShort get_UShort ( Cursor* c ) {
   UShort r;
   vg_assert(is_sane_Cursor(c));
   if (c->sli_next + sizeof(UShort) > c->sli.ioff + c->sli.szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   r = ML_(img_get_UShort)(c->sli.img, c->sli_next);
   c->sli_next += sizeof(UShort);
   return r;
}
static UInt get_UInt ( Cursor* c ) {
   UInt r;
   vg_assert(is_sane_Cursor(c));
   if (c->sli_next + sizeof(UInt) > c->sli.ioff + c->sli.szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   r = ML_(img_get_UInt)(c->sli.img, c->sli_next);
   c->sli_next += sizeof(UInt);
   return r;
}
static ULong get_ULong ( Cursor* c ) {
   ULong r;
   vg_assert(is_sane_Cursor(c));
   if (c->sli_next + sizeof(ULong) > c->sli.ioff + c->sli.szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   r = ML_(img_get_ULong)(c->sli.img, c->sli_next);
   c->sli_next += sizeof(ULong);
   return r;
}
static ULong get_ULEB128 ( Cursor* c ) {
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
static UInt get_UInt3 ( Cursor* c ) {
   UChar c1, c2, c3;
   vg_assert(is_sane_Cursor(c));
   if (c->sli_next + 3 > c->sli.ioff + c->sli.szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   c1 = ML_(img_get_UChar)(c->sli.img, c->sli_next);
   c2 = ML_(img_get_UChar)(c->sli.img, c->sli_next+1);
   c3 = ML_(img_get_UChar)(c->sli.img, c->sli_next+2);
   c->sli_next += 3;
#if defined(VG_BIGENDIAN)
   return c1 << 16 | c2 << 8 | c3;
#else
   return c1 | c2 << 8 | c3 << 16;
#endif
}


/* Assume 'c' points to the start of a string.  Return a DiCursor of
   whatever it points at, and advance it past the terminating zero.
   This makes it safe for the caller to then copy the string with
   ML_(addStr), since (w.r.t. image overruns) the process of advancing
   past the terminating zero will already have "vetted" the string. */
static DiCursor get_AsciiZ ( Cursor* c ) {
   UChar uc;
   DiCursor res = get_DiCursor_from_Cursor(c);
   do { uc = get_UChar(c); } while (uc != 0);
   return res;
}

static ULong peek_ULEB128 ( Cursor* c ) {
   DiOffT here = c->sli_next;
   ULong  r    = get_ULEB128( c );
   c->sli_next = here;
   return r;
}
static UChar peek_UChar ( Cursor* c ) {
   DiOffT here = c->sli_next;
   UChar  r    = get_UChar( c );
   c->sli_next = here;
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
                                  const HChar* barfMsg )
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

typedef
   struct _name_form {
      ULong at_name;  // Dwarf Attribute name
      ULong at_form;  // Dwarf Attribute form
      Long  at_val;   // Dwarf Attribute value (for implicit_const)
      UInt  skip_szB; // Nr of bytes skippable from here ...
      UInt  next_nf;  // ... to reach this attr/form index in the g_abbv.nf
   } name_form;
/* skip_szB and next_nf are used to optimise the skipping of uninteresting DIEs.
   Each name_form maintains how many (fixed) nr of bytes can be skipped from
   the beginning of this form till the next attr/form to look at.
   The next form to look can be:
       an 'interesting' attr/form to read while skipping a DIE
          (currently, this is only DW_AT_sibling)
   or
       a variable length form which must be read to be skipped.
   For a variable length form, the skip_szB will be equal to VARSZ_FORM.

   Note: this technique could also be used to speed up the parsing
   of DIEs : for each parser kind, we could have the nr of bytes
   to skip to directly reach the interesting form(s) for the parser. */

typedef
   struct _g_abbv {
      struct _g_abbv *next; // read/write by hash table.
      UWord  abbv_code;     // key, read by hash table
      ULong  atag;
      ULong  has_children;
      name_form nf[0];
      /* Variable-length array of name/form pairs, terminated
         by a 0/0 pair.
         The skip_szB/next_nf allows to skip efficiently a DIE
         described by this g_abbv; */
    } g_abbv;

/* Holds information about the .debug_abbrev section for this CU.  The current
  Cursor into the abbrev section, the known abbrev codes are but into an hash
  table.  The (starting) offset into the abbrev_offset can be used to check
  whether the abbv can be shared between CUs.  The done boolean is set when all
  known codes have been read.  Initialize a new abbv_state with init_ht_abbvs.
  To read any new abbrev codes not yet in the hash table call find_ht_abbvs
  (get_abbv will first query the ht_abbvs, then if not done, call
  find_ht_abbvs).  */
typedef
   struct _abbv_state {
      Cursor c; /* Current cursor into .debug_abbrev.  */
      VgHashTable *ht_abbvs; /* Hash table mapping codes to abbrevs.  */
      ULong debug_abbrev_offset; /* Starting offset into .debug_abbrev.  */
      Bool done; /* Whether there (might) still be new abbrev codes not yet
                    in the cache.  */
   } abbv_state;

/* Holds information that is constant through the parsing of a
   Compilation Unit.  This is basically plumbed through to
   everywhere. */
typedef
   struct {
      /* Call here if anything goes wrong */
      void (*barf)( const HChar* ) __attribute__((noreturn));
      /* Is this 64-bit DWARF ? */
      Bool   is_dw64;
      /* Which DWARF version ?  (2, 3, 4 or 5) */
      UShort version;
      /* Length of this Compilation Unit, as stated in the
         .unit_length :: InitialLength field of the CU Header.
         However, this size (as specified by the D3 spec) does not
         include the size of the .unit_length field itself, which is
         either 4 or 12 bytes (32-bit or 64-bit Dwarf3).  That value
         can be obtained through the expression ".is_dw64 ? 12 : 4". */
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
      //UChar* debug_abbv;
      /* Upper bound on size thereof (an overestimate, in general) */
      //UWord  debug_abbv_maxszB;
      /* A bounded area of the image, to be used as the
         debug_abbreviations table tobe used for this Unit. */
      DiSlice debug_abbv;

      /* Image information for various sections. */
      DiSlice escn_debug_str;
      DiSlice escn_debug_ranges;
      DiSlice escn_debug_rnglists;
      DiSlice escn_debug_loclists;
      DiSlice escn_debug_loc;
      DiSlice escn_debug_line;
      DiSlice escn_debug_info;
      DiSlice escn_debug_types;
      DiSlice escn_debug_info_alt;
      DiSlice escn_debug_str_alt;
      DiSlice escn_debug_line_str;
      DiSlice escn_debug_addr;
      DiSlice escn_debug_str_offsets;
      /* How much to add to .debug_types resp. alternate .debug_info offsets
         in cook_die*.  */
      UWord  types_cuOff_bias;
      UWord  alt_cuOff_bias;
      /* DW_AT_addr_base */
      Addr   cu_addr_base;
      Bool   cu_has_addr_base;
      /* DW_AT_str_offsets_base */
      Addr   cu_str_offsets_base;
      Bool   cu_has_str_offsets_base;
      /* DW_AT_rnglists_base */
      Addr   cu_rnglists_base;
      Bool   cu_has_rnglists_base;
      /* DW_AT_loclists_base */
      Addr   cu_loclists_base;
      Bool   cu_has_loclists_base;
      /* --- Needed so we can add stuff to the string table. --- */
      struct _DebugInfo* di;
      /* --- State of the hash table of g_abbv (i.e. parsed abbreviations)
             technically makes this struct not const.  --- */
      abbv_state abbv;

      /* True if this came from .debug_types; otherwise it came from
         .debug_info.  */
      Bool is_type_unit;
      /* For a unit coming from .debug_types, these hold the TU's type
         signature and the uncooked DIE offset of the TU's signatured
         type.  For a unit coming from .debug_info, these are unused.  */
      ULong type_signature;
      ULong type_offset;

      /* Signatured type hash; computed once and then shared by all
         CUs.  */
      VgHashTable *signature_types;

      /* True if this came from alternate .debug_info; otherwise
         it came from normal .debug_info or .debug_types.  */
      Bool is_alt_info;
   }
   CUConst;


/* Return the cooked value of DIE depending on whether CC represents a
   .debug_types unit.  To cook a DIE, we pretend that the .debug_info,
   .debug_types and optional alternate .debug_info sections form
   a contiguous whole, so that DIEs coming from .debug_types are numbered
   starting at the end of .debug_info and DIEs coming from alternate
   .debug_info are numbered starting at the end of .debug_types.  */
static UWord cook_die( const CUConst* cc, UWord die )
{
   if (cc->is_type_unit)
      die += cc->types_cuOff_bias;
   else if (cc->is_alt_info)
      die += cc->alt_cuOff_bias;
   return die;
}

/* Like cook_die, but understand that DIEs coming from a
   DW_FORM_ref_sig8 reference are already cooked.  Also, handle
   DW_FORM_GNU_ref_alt from within primary .debug_info or .debug_types
   as reference to alternate .debug_info.  */
static UWord cook_die_using_form( const CUConst *cc, UWord die, DW_FORM form)
{
   if (form == DW_FORM_ref_sig8)
      return die;
   if (form == DW_FORM_GNU_ref_alt)
      return die + cc->alt_cuOff_bias;
   return cook_die( cc, die );
}

/* Return the uncooked offset of DIE and set *TYPE_FLAG to true if the DIE
   came from the .debug_types section and *ALT_FLAG to true if the DIE
   came from alternate .debug_info section.  */
static UWord uncook_die( const CUConst *cc, UWord die, /*OUT*/Bool *type_flag,
                         Bool *alt_flag )
{
   *alt_flag = False;
   *type_flag = False;
   /* The use of escn_debug_{info,types}.szB seems safe to me even if
      escn_debug_{info,types} are DiSlice_INVALID (meaning the
      sections were not found), because DiSlice_INVALID.szB is always
      zero.  That said, it seems unlikely we'd ever get here if
      .debug_info or .debug_types were missing. */
   if (die >= cc->escn_debug_info.szB) {
      if (die >= cc->escn_debug_info.szB + cc->escn_debug_types.szB) {
         *alt_flag = True;
         die -= cc->escn_debug_info.szB + cc->escn_debug_types.szB;
      } else {
         *type_flag = True;
         die -= cc->escn_debug_info.szB;
      }
   }
   return die;
}

/* Return an entry from .debug_addr with the given index.
   Call one of the variants below that do error-checking. */
static ULong get_debug_addr_entry_common( ULong index, const CUConst* cc )
{
   vg_assert(cc->cu_has_addr_base);
   /* We make the same word-size assumption as DW_FORM_addr. */
   UWord addr_pos = cc->cu_addr_base + index * sizeof(UWord);
   Cursor cur;
   init_Cursor( &cur, cc->escn_debug_addr, addr_pos, cc->barf,
                "get_debug_addr_entry_common: index points outside .debug_addr" );
   return (ULong)(UWord)get_UWord(&cur);
}

static ULong get_debug_addr_entry_form( ULong index, const CUConst* cc,
                                        DW_FORM form )
{
   if(!cc->cu_has_addr_base) {
      VG_(printf)(
         "get_debug_addr_entry_form: %u (%s) without DW_AT_addr_base\n",
         form, ML_(pp_DW_FORM)(form));
      cc->barf("get_debug_addr_entry_form: DW_AT_addr_base not set");
   }
   return get_debug_addr_entry_common( index, cc );
}

static ULong get_debug_addr_entry_lle( ULong index, const CUConst* cc,
                                       DW_LLE entry )
{
   if(!cc->cu_has_addr_base) {
      VG_(printf)(
         "get_debug_addr_entry_lle: %u (%s) without DW_AT_addr_base\n",
         entry, ML_(pp_DW_LLE)(entry));
      cc->barf("get_debug_addr_entry_lle: DW_AT_addr_base not set");
   }
   return get_debug_addr_entry_common( index, cc );
}

static ULong get_debug_addr_entry_rle( ULong index, const CUConst* cc,
                                       DW_RLE entry )
{
   if(!cc->cu_has_addr_base) {
      VG_(printf)(
         "get_debug_addr_entry_rle: %u (%s) without DW_AT_addr_base\n",
         entry, ML_(pp_DW_RLE)(entry));
      cc->barf("get_debug_addr_entry_rle: DW_AT_addr_base not set");
   }
   return get_debug_addr_entry_common( index, cc );
}

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
/* 2008-sept-12: moved ML_(pp_GX) from here to d3basics.c, where
   it more logically belongs. */


/* Apply a text bias to a GX. */
static void bias_GX ( /*MOD*/GExpr* gx, const DebugInfo* di )
{
   UShort nbytes;
   UChar* p = &gx->payload[0];
   UChar* pA;
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
      pA = (UChar*)p;
      ML_(write_Addr)(pA, ML_(read_Addr)(pA) + di->text_debug_bias);
      p += sizeof(Addr);
      /* t-bias aMax */
      pA = (UChar*)p;
      ML_(write_Addr)(pA, ML_(read_Addr)(pA) + di->text_debug_bias);
      p += sizeof(Addr);
      /* nbytes, and actual expression */
      nbytes = ML_(read_UShort)(p); p += sizeof(UShort);
      p += nbytes;
   }
}

__attribute__((noinline))
static GExpr* make_singleton_GX ( DiCursor block, ULong nbytes )
{
   SizeT  bytesReqd;
   GExpr* gx;
   UChar *p, *pstart;

   vg_assert(sizeof(UWord) == sizeof(Addr));
   vg_assert(nbytes <= 0xFFFF); /* else we overflow the nbytes field */
   bytesReqd
      =   sizeof(UChar)  /*biasMe*/    + sizeof(UChar) /*!isEnd*/
        + sizeof(UWord)  /*aMin*/      + sizeof(UWord) /*aMax*/
        + sizeof(UShort) /*nbytes*/    + (SizeT)nbytes
        + sizeof(UChar); /*isEnd*/

   gx = ML_(dinfo_zalloc)( "di.readdwarf3.msGX.1", 
                           sizeof(GExpr) + bytesReqd );

   p = pstart = &gx->payload[0];

   p = ML_(write_UChar)(p, 0);        /*biasMe*/
   p = ML_(write_UChar)(p, 0);        /*!isEnd*/
   p = ML_(write_Addr)(p, 0);         /*aMin*/
   p = ML_(write_Addr)(p, ~0);        /*aMax*/
   p = ML_(write_UShort)(p, nbytes);  /*nbytes*/
   ML_(cur_read_get)(p, block, nbytes); p += nbytes;
   p = ML_(write_UChar)(p, 1);        /*isEnd*/

   vg_assert( (SizeT)(p - pstart) == bytesReqd);
   vg_assert( &gx->payload[bytesReqd] 
              == ((UChar*)gx) + sizeof(GExpr) + bytesReqd );

   return gx;
}

__attribute__((noinline))
static GExpr* make_general_GX ( const CUConst* cc,
                                Bool     td3,
                                ULong    offset,
                                Addr     svma_of_referencing_CU )
{
   Bool      done;
   Addr      base;
   Cursor    loc;
   XArray*   xa; /* XArray of UChar */
   GExpr*    gx;
   Word      nbytes;
   Bool      addBase = cc->version < 5;

   vg_assert(sizeof(UWord) == sizeof(Addr));
   if (cc->version < 5 && (!ML_(sli_is_valid)(cc->escn_debug_loc)
                           || cc->escn_debug_loc.szB == 0))
      cc->barf("make_general_GX: .debug_loc is empty/missing");
   if (cc->version >= 5 && (!ML_(sli_is_valid)(cc->escn_debug_loclists)
                           || cc->escn_debug_loclists.szB == 0))
      cc->barf("make_general_GX: .debug_loclists is empty/missing");

   if (cc->version < 5)
      init_Cursor( &loc, cc->escn_debug_loc, 0, cc->barf,
                   "Overrun whilst reading .debug_loc section(2)" );
   else
      init_Cursor( &loc, cc->escn_debug_loclists, 0, cc->barf,
                   "Overrun whilst reading .debug_loclists section(2)" );
   set_position_of_Cursor( &loc, offset );

   TRACE_D3("make_general_GX (offset = %llu, ioff = %llu) {\n",
            offset, get_DiCursor_from_Cursor(&loc).ioff );

   /* Who frees this xa?  It is freed before this fn exits. */
   xa = VG_(newXA)( ML_(dinfo_zalloc), "di.readdwarf3.mgGX.1", 
                    ML_(dinfo_free),
                    sizeof(UChar) );

   { UChar c = 1; /*biasMe*/ VG_(addBytesToXA)( xa, &c, sizeof(c) ); }

   base = 0;
   done = False;
   while (!done) {
      Bool  acquire;
      UWord len;
      UWord w1;
      UWord w2;
      if (cc->version < 5) {
         /* Read a (host-)word pair.  This is something of a hack since
            the word size to read is really dictated by the ELF file;
            however, we assume we're reading a file with the same
            word-sizeness as the host.  Reasonably enough. */
         w1 = get_UWord( &loc );
         w2 = get_UWord( &loc );

         TRACE_D3("   %08lx %08lx\n", w1, w2);
         if (w1 == 0 && w2 == 0) {
            done = True;
            break; /* end of list */
         }

         if (w1 == -1UL) {
            /* new value for 'base' */
            base = w2;
            continue;
         }
         /* else a location expression follows */
         len = (UWord)get_UShort( &loc );
      } else {
         w1 = 0;
         w2 = 0;
         len = 0;
         DW_LLE r = get_UChar( &loc );
         switch (r) {
         case DW_LLE_end_of_list:
            done = True;
            break;
         case DW_LLE_base_address:
            base = get_UWord( &loc );
            break;
         case DW_LLE_start_length:
            w1 = get_UWord( &loc );
            w2 = w1 + get_ULEB128( &loc );
            len = get_ULEB128( &loc );
            break;
         case DW_LLE_offset_pair:
            w1 = base + get_ULEB128( &loc );
            w2 = base + get_ULEB128( &loc );
            len = get_ULEB128( &loc );
            break;
         case DW_LLE_start_end:
            w1 = get_UWord ( &loc );
            w2 = get_UWord ( &loc );
            len = get_ULEB128( &loc );
            break;
         case DW_LLE_GNU_view_pair:
            get_ULEB128( &loc );
            get_ULEB128( &loc );
            break;
         case DW_LLE_base_addressx:
            base = get_debug_addr_entry_lle( get_ULEB128( &loc ), cc,
                                             DW_LLE_base_addressx );
            break;
         case DW_LLE_startx_endx:
            w1 = get_debug_addr_entry_lle( get_ULEB128( &loc ), cc,
                                           DW_LLE_startx_endx );
            w2 = get_debug_addr_entry_lle( get_ULEB128( &loc ), cc,
                                           DW_LLE_startx_endx );
            len = get_ULEB128( &loc );
            break;
         case DW_LLE_startx_length:
            w1 = get_debug_addr_entry_lle( get_ULEB128( &loc ), cc,
                                           DW_LLE_startx_length );
            w2 = w1 + get_ULEB128( &loc );
            len = get_ULEB128( &loc );
            break;
         case DW_LLE_default_location:
         default:
            cc->barf( "Unhandled or unknown loclists entry" );
            done = True;
         }
      }

      /* else enumerate [w1+base, w2+base) */
      /* w2 is 1 past end of range, as per D3 defn for "DW_AT_high_pc"
         (sec 2.17.2) */
      if (w1 > w2) {
         TRACE_D3("negative range is for .debug_loc expr at "
                  "file offset %llu\n", 
                  offset);
         cc->barf( "negative range in .debug_loc section" );
      }

      /* ignore zero length ranges */
      acquire = w1 < w2;

      if (acquire) {
         UWord  w;
         UShort s;
         UChar  c;
         c = 0; /* !isEnd*/
         VG_(addBytesToXA)( xa, &c, sizeof(c) );
         w = w1    + (addBase ? base : 0) + svma_of_referencing_CU;
         VG_(addBytesToXA)( xa, &w, sizeof(w) );
         w = w2 -1 + (addBase ? base : 0) + svma_of_referencing_CU;
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

   gx = ML_(dinfo_zalloc)( "di.readdwarf3.mgGX.2", sizeof(GExpr) + nbytes );
   VG_(memcpy)( &gx->payload[0], (UChar*)VG_(indexXA)(xa,0), nbytes );
   vg_assert( &gx->payload[nbytes] 
              == ((UChar*)gx) + sizeof(GExpr) + nbytes );

   VG_(deleteXA)( xa );

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


/* Generate an arbitrary structural total ordering on
   XArray* of AddrRange. */
static Word cmp__XArrays_of_AddrRange ( const XArray* rngs1,
                                        const XArray* rngs2 )
{
   Word n1, n2, i;
   vg_assert(rngs1 && rngs2);
   n1 = VG_(sizeXA)( rngs1 );  
   n2 = VG_(sizeXA)( rngs2 );
   if (n1 < n2) return -1;
   if (n1 > n2) return 1;
   for (i = 0; i < n1; i++) {
      AddrRange* rng1 = (AddrRange*)VG_(indexXA)( rngs1, i );
      AddrRange* rng2 = (AddrRange*)VG_(indexXA)( rngs2, i );
      if (rng1->aMin < rng2->aMin) return -1;
      if (rng1->aMin > rng2->aMin) return 1;
      if (rng1->aMax < rng2->aMax) return -1;
      if (rng1->aMax > rng2->aMax) return 1;
   }
   return 0;
}


__attribute__((noinline))
static XArray* /* of AddrRange */ empty_range_list ( void )
{
   XArray* xa; /* XArray of AddrRange */
   /* Who frees this xa?  varstack_preen() does. */
   xa = VG_(newXA)( ML_(dinfo_zalloc), "di.readdwarf3.erl.1",
                    ML_(dinfo_free),
                    sizeof(AddrRange) );
   return xa;
}


__attribute__((noinline))
static XArray* unitary_range_list ( Addr aMin, Addr aMax )
{
   XArray*   xa;
   AddrRange pair;
   vg_assert(aMin <= aMax);
   /* Who frees this xa?  varstack_preen() does. */
   xa = VG_(newXA)( ML_(dinfo_zalloc),  "di.readdwarf3.url.1",
                    ML_(dinfo_free),
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
get_range_list ( const CUConst* cc,
                 Bool     td3,
                 UWord    debug_ranges_offset,
                 Addr     svma_of_referencing_CU )
{
   Addr      base;
   Cursor    ranges;
   XArray*   xa; /* XArray of AddrRange */
   AddrRange pair;

   if (cc->version < 5 && (!ML_(sli_is_valid)(cc->escn_debug_ranges)
                           || cc->escn_debug_ranges.szB == 0))
      cc->barf("get_range_list: .debug_ranges is empty/missing");
   if (cc->version >= 5 && (!ML_(sli_is_valid)(cc->escn_debug_rnglists)
                            || cc->escn_debug_rnglists.szB == 0))
      cc->barf("get_range_list: .debug_rnglists is empty/missing");

   if (cc->version < 5)
      init_Cursor( &ranges, cc->escn_debug_ranges, 0, cc->barf,
                   "Overrun whilst reading .debug_ranges section(2)" );
   else
      init_Cursor( &ranges, cc->escn_debug_rnglists, 0, cc->barf,
                   "Overrun whilst reading .debug_rnglists section(2)" );

   set_position_of_Cursor( &ranges, debug_ranges_offset );

   /* Who frees this xa?  varstack_preen() does. */
   xa = VG_(newXA)( ML_(dinfo_zalloc), "di.readdwarf3.grl.1", ML_(dinfo_free),
                    sizeof(AddrRange) );
   base = 0;
   if (cc->version < 5) {
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
   } else {
      Bool done = False;
      while (!done) {
         UWord w1 = 0;
         UWord w2 = 0;
         DW_RLE r = get_UChar( &ranges );
         switch (r) {
         case DW_RLE_end_of_list:
            done = True;
            break;
         case DW_RLE_base_address:
            base = get_UWord( &ranges );
            break;
         case DW_RLE_start_length:
            w1 = get_UWord( &ranges );
            w2 = w1 + get_ULEB128( &ranges );
            break;
         case DW_RLE_offset_pair:
            w1 = base + get_ULEB128( &ranges );
            w2 = base + get_ULEB128( &ranges );
            break;
         case DW_RLE_start_end:
            w1 = get_UWord ( &ranges );
            w2 = get_UWord ( &ranges );
            break;
         case DW_RLE_base_addressx:
            base = get_debug_addr_entry_rle( get_ULEB128( &ranges ), cc,
                                             DW_RLE_base_addressx );
            break;
         case DW_RLE_startx_endx:
            w1 = get_debug_addr_entry_rle( get_ULEB128( &ranges ), cc,
                                           DW_RLE_startx_endx );
            w2 = get_debug_addr_entry_rle( get_ULEB128( &ranges ), cc,
                                           DW_RLE_startx_endx );
            break;
         case DW_RLE_startx_length:
            w1 = get_debug_addr_entry_rle( get_ULEB128( &ranges ), cc,
                                           DW_RLE_startx_length );
            w2 = w1 + get_ULEB128( &ranges );
            break;
         default:
            cc->barf( "Unhandled or unknown range list entry" );
            done = True;
         }
         if (w1 > w2)
            cc->barf( "negative range in .debug_rnglists section" );
         if (w1 < w2) {
            pair.aMin = w1     + svma_of_referencing_CU;
            pair.aMax = w2 - 1 + svma_of_referencing_CU;
            vg_assert(pair.aMin <= pair.aMax);
            VG_(addToXA)( xa, &pair );
         }
      }
   }
   return xa;
}

#define VARSZ_FORM 0xffffffff
static UInt get_Form_szB (const CUConst* cc, DW_FORM form );

/* Initialises the hash table of abbreviations.  This only sets up the abbv
   Cursor and hash table, but does not try to read any abbrevs yes. The actual
   reading of abbrevs will be done by get_abbv by calling find_ht_abbvs on
   demand if a requested abbrev code isn't in the hash table yet. When using the
   inline parser a lot of abbrevs will not be needed so reading everything
   upfront will often waste time and memory.  */
static void init_ht_abbvs (CUConst* cc, ULong debug_abbrev_offset,
                           Bool td3)
{
   Cursor *c = &cc->abbv.c;
   init_Cursor( c, cc->debug_abbv, 0, cc->barf,
               "Overrun whilst parsing .debug_abbrev section(2)" );
   cc->abbv.ht_abbvs = VG_(HT_construct) ("di.readdwarf3.ht_abbvs");
   cc->abbv.debug_abbrev_offset = debug_abbrev_offset;
   cc->abbv.done = False;
}

static g_abbv *find_ht_abbvs (CUConst* cc, ULong abbv_code,
                              Bool td3)
{
   Cursor *c;
   g_abbv *ta; // temporary abbreviation, reallocated if needed.
   UInt ta_nf_maxE; // max nr of pairs in ta.nf[], doubled when reallocated.
   UInt ta_nf_n;    // nr of pairs in ta->nf that are initialised.
   g_abbv *ht_ta; // abbv to insert in hash table.
   Int i;

   #define SZ_G_ABBV(_nf_szE) (sizeof(g_abbv) + _nf_szE * sizeof(name_form))

   ta_nf_maxE = 10; // starting with enough for 9 pairs+terminating pair.
   ta = ML_(dinfo_zalloc) ("di.readdwarf3.ht_ta_nf", SZ_G_ABBV(ta_nf_maxE));

   c = &cc->abbv.c;
   while (True) {
      ht_ta = NULL;
      ta->abbv_code = get_ULEB128( c );
      if (ta->abbv_code == 0) {
         cc->abbv.done = True;
         break; /* end of the table */
      }

      ta->atag = get_ULEB128( c );
      ta->has_children = get_UChar( c );
      ta_nf_n = 0;
      while (True) {
         if (ta_nf_n >= ta_nf_maxE) {
            g_abbv *old_ta = ta;
            ta = ML_(dinfo_zalloc) ("di.readdwarf3.ht_ta_nf",
                                    SZ_G_ABBV(2 * ta_nf_maxE));
            ta_nf_maxE = 2 * ta_nf_maxE;
            VG_(memcpy) (ta, old_ta, SZ_G_ABBV(ta_nf_n));
            ML_(dinfo_free) (old_ta);
         }
         ta->nf[ta_nf_n].at_name = get_ULEB128( c );
         ta->nf[ta_nf_n].at_form = get_ULEB128( c );
         if (ta->nf[ta_nf_n].at_form == DW_FORM_implicit_const)
            ta->nf[ta_nf_n].at_val = get_SLEB128( c );
         if (ta->nf[ta_nf_n].at_name == 0 && ta->nf[ta_nf_n].at_form == 0) {
            ta_nf_n++;
            break; 
         }
        ta_nf_n++;
      }

      // Initialises the skip_szB/next_nf elements : an element at position
      // i must contain the sum of its own size + the sizes of all elements
      // following i till either the next variable size element, the next
      // sibling element or the end of the DIE.
      ta->nf[ta_nf_n - 1].skip_szB = 0;
      ta->nf[ta_nf_n - 1].next_nf = 0;
      for (i = ta_nf_n - 2; i >= 0; i--) {
         const UInt form_szB = get_Form_szB (cc, (DW_FORM)ta->nf[i].at_form);
          
         if (ta->nf[i+1].at_name == DW_AT_sibling
             || ta->nf[i+1].skip_szB == VARSZ_FORM) {
            ta->nf[i].skip_szB = form_szB;
            ta->nf[i].next_nf  = i+1;
         } else if (form_szB == VARSZ_FORM) {
            ta->nf[i].skip_szB = form_szB;
            ta->nf[i].next_nf  = i+1;
         } else {
            ta->nf[i].skip_szB = ta->nf[i+1].skip_szB + form_szB;
            ta->nf[i].next_nf  = ta->nf[i+1].next_nf;
         }
      }

      ht_ta = ML_(dinfo_zalloc) ("di.readdwarf3.ht_ta", SZ_G_ABBV(ta_nf_n));
      VG_(memcpy) (ht_ta, ta, SZ_G_ABBV(ta_nf_n));
      VG_(HT_add_node) ( cc->abbv.ht_abbvs, ht_ta );
      if (TD3) {
         TRACE_D3("  Adding abbv_code %lu TAG  %s [%s] nf %u ",
                  ht_ta->abbv_code, ML_(pp_DW_TAG)(ht_ta->atag),
                  ML_(pp_DW_children)(ht_ta->has_children),
                  ta_nf_n);
         TRACE_D3("  ");
         for (i = 0; i < ta_nf_n; i++)
            TRACE_D3("[%u,%u] ", ta->nf[i].skip_szB, ta->nf[i].next_nf);
         TRACE_D3("\n");
      }
      if (ht_ta->abbv_code == abbv_code)
         break;
   }

   ML_(dinfo_free) (ta);
   #undef SZ_G_ABBV

   return ht_ta;
}

static g_abbv* get_abbv (CUConst* cc, ULong abbv_code,
                         Bool td3)
{
   g_abbv *abbv;

   abbv = VG_(HT_lookup) (cc->abbv.ht_abbvs, abbv_code);
   if (!abbv && !cc->abbv.done)
      abbv = find_ht_abbvs (cc, abbv_code, td3);
   if (!abbv)
      cc->barf ("abbv_code not found in ht_abbvs table");

   return abbv;
}

/* Parse the Compilation Unit header indicated at 'c' and 
   initialise 'cc' accordingly. */
static __attribute__((noinline))
void parse_CU_Header ( /*OUT*/CUConst* cc,
                       Bool td3,
                       Cursor* c, 
                       DiSlice escn_debug_abbv,
                       abbv_state last_abbv,
		       Bool type_unit,
                       Bool alt_info )
{
   UChar  address_size, unit_type;
   ULong  debug_abbrev_offset;

   VG_(memset)(cc, 0, sizeof(*cc));
   vg_assert(c && c->barf);
   cc->barf = c->barf;

   /* initial_length field */
   cc->unit_length 
      = get_Initial_Length( &cc->is_dw64, c, 
           "parse_CU_Header: invalid initial-length field" );

   TRACE_D3("   Length:        %llu\n", cc->unit_length );

   /* version */
   cc->version = get_UShort( c );
   if (cc->version != 2 && cc->version != 3 && cc->version != 4
       && cc->version != 5)
      cc->barf( "parse_CU_Header: "
                "is neither DWARF2 nor DWARF3 nor DWARF4 nor DWARF5" );
   TRACE_D3("   Version:       %d\n", (Int)cc->version );

   /* unit type */
   if (cc->version >= 5) {
      unit_type = get_UChar( c );
      address_size = get_UChar( c );
   } else {
      unit_type = type_unit ? DW_UT_type : DW_UT_compile;
      address_size = 0; /* Will be read later. */
   }

   /* debug_abbrev_offset */
   debug_abbrev_offset = get_Dwarfish_UWord( c, cc->is_dw64 );
   if (debug_abbrev_offset >= escn_debug_abbv.szB)
      cc->barf( "parse_CU_Header: invalid debug_abbrev_offset" );
   TRACE_D3("   Abbrev Offset: %llu\n", debug_abbrev_offset );

   /* address size.  If this isn't equal to the host word size, just
      give up.  This makes it safe to assume elsewhere that
      DW_FORM_addr and DW_FORM_ref_addr can be treated as a host
      word. */
   if (cc->version < 5)
      address_size = get_UChar( c );

   if (address_size != sizeof(void*))
      cc->barf( "parse_CU_Header: invalid address_size" );
   TRACE_D3("   Pointer Size:  %d\n", (Int)address_size );

   cc->is_type_unit = type_unit;
   cc->is_alt_info = alt_info;

   if (type_unit || (cc->version >= 5 && (unit_type == DW_UT_type
                                          || unit_type == DW_UT_split_type))) {
      cc->type_signature = get_ULong( c );
      cc->type_offset = get_Dwarfish_UWord( c, cc->is_dw64 );
   }

   if (cc->version >= 5 && (unit_type == DW_UT_skeleton
                            || unit_type == DW_UT_split_compile)) {
      /* dwo_id = */ get_ULong( c );
   }

   /* Set up cc->debug_abbv to point to the relevant table for this
      CU.  Set its .szB so that at least we can't read off the end of
      the debug_abbrev section -- potentially (and quite likely) too
      big, if this isn't the last table in the section, but at least
      it's safe.

      This amounts to taking debug_abbv_escn and moving the start
      position along by debug_abbrev_offset bytes, hence forming a
      smaller DiSlice which has the same end point.  Since we checked
      just above that debug_abbrev_offset is less than the size of
      debug_abbv_escn, this should leave us with a nonempty slice. */
   vg_assert(debug_abbrev_offset < escn_debug_abbv.szB);
   cc->debug_abbv      = escn_debug_abbv;
   cc->debug_abbv.ioff += debug_abbrev_offset;
   cc->debug_abbv.szB  -= debug_abbrev_offset;

   if (last_abbv.ht_abbvs != NULL
       && debug_abbrev_offset == last_abbv.debug_abbrev_offset) {
      cc->abbv = last_abbv;
   } else {
      if (last_abbv.ht_abbvs != NULL)
         VG_(HT_destruct) (last_abbv.ht_abbvs, ML_(dinfo_free));
      init_ht_abbvs(cc, debug_abbrev_offset, td3);
   }
}

/* This represents a single signatured type.  It maps a type signature
   (a ULong) to a cooked DIE offset.  Objects of this type are stored
   in the type signature hash table.  */
typedef
   struct D3SignatureType {
      struct D3SignatureType *next;
      UWord data;
      ULong type_signature;
      UWord die;
   }
   D3SignatureType;

/* Record a signatured type in the hash table.  */
static void record_signatured_type ( VgHashTable *tab,
                                     ULong type_signature,
                                     UWord die )
{
   D3SignatureType *dstype = ML_(dinfo_zalloc) ( "di.readdwarf3.sigtype",
                                                 sizeof(D3SignatureType) );
   dstype->data = (UWord) type_signature;
   dstype->type_signature = type_signature;
   dstype->die = die;
   VG_(HT_add_node) ( tab, dstype );
}

/* Given a type signature hash table and a type signature, return the
   cooked DIE offset of the type.  If the type cannot be found, call
   BARF.  */
static UWord lookup_signatured_type ( const VgHashTable *tab,
                                      ULong type_signature,
                                      void (*barf)( const HChar* ) __attribute__((noreturn)) )
{
   D3SignatureType *dstype = VG_(HT_lookup) ( tab, (UWord) type_signature );
   /* This may be unwarranted chumminess with the hash table
      implementation.  */
   while ( dstype != NULL && dstype->type_signature != type_signature)
      dstype = dstype->next;
   if (dstype == NULL) {
      barf("lookup_signatured_type: could not find signatured type");
      /*NOTREACHED*/
      vg_assert(0);
   }
   return dstype->die;
}


/* Represents Form data.  If szB is 1/2/4/8 then the result is in the
   lowest 1/2/4/8 bytes of u.val.  If szB is zero or negative then the
   result is an image section beginning at u.cur and with size -szB.
   No other szB values are allowed. */
typedef
   struct {
      Long szB; // 1, 2, 4, 8 or non-positive values only.
      union { ULong val; DiCursor cur; } u;
   }
   FormContents;

// Read data for get_Form_contents() from .debug_addr for the 'index' entry.
static void get_Form_contents_addr( /*OUT*/FormContents* cts, DW_FORM form,
                                    ULong index, const CUConst* cc, Bool td3 )
{
   cts->u.val = get_debug_addr_entry_form( index, cc, form );
   cts->szB   = sizeof(UWord);
   TRACE_D3("0x%lx", (UWord)cts->u.val);
}

// Read data for get_Form_contents() from .debug_str for the given offset.
static void get_Form_contents_str( /*OUT*/FormContents* cts, DW_FORM form,
                                    UWord offset, const CUConst* cc, Bool td3 )
{
   if (!ML_(sli_is_valid)(cc->escn_debug_str)
       || offset >= cc->escn_debug_str.szB) {
      VG_(printf)(
         "get_Form_contents_str: %u (%s) points outside .debug_str\n",
         form, ML_(pp_DW_FORM)(form));
      cc->barf("get_Form_contents_str: index points outside .debug_str");
   }
   /* FIXME: check the entire string lies inside debug_str,
      not just the first byte of it. */
   DiCursor str
      = ML_(cur_plus)( ML_(cur_from_sli)(cc->escn_debug_str), offset );
   if (TD3) {
      HChar* tmp = ML_(cur_read_strdup)(str, "di.getFC.1");
      TRACE_D3("(indirect string, offset: 0x%lx): %s", offset, tmp);
      ML_(dinfo_free)(tmp);
   }
   cts->u.cur = str;
   cts->szB   = - (Long)(1 + (ULong)ML_(cur_strlen)(str));
}

static inline UInt sizeof_Dwarfish_UWord (Bool is_dw64)
{
   if (is_dw64)
      return sizeof(ULong);
   else
      return sizeof(UInt);
}

// Read data for get_Form_contents() from .debug_str_offsets for the 'index' entry.
static void get_Form_contents_str_offsets( /*OUT*/FormContents* cts, DW_FORM form,
                                    ULong index, const CUConst* cc, Bool td3 )
{
   if(!cc->cu_has_str_offsets_base) {
      VG_(printf)(
         "get_Form_contents_str_offsets: %u (%s) without DW_AT_str_offsets_base\n",
         form, ML_(pp_DW_FORM)(form));
      cc->barf("get_Form_contents_str_offsets: DW_AT_str_offsets_base not set");
   }
   UWord str_offsets_pos = cc->cu_str_offsets_base
                           + index * sizeof_Dwarfish_UWord (cc->is_dw64);
   Cursor cur;
   init_Cursor( &cur, cc->escn_debug_str_offsets, str_offsets_pos, cc->barf,
                "get_Form_contents_str_offsets: index "
                "points outside .debug_str_offsets" );
   if (TD3) {
      HChar* tmp = ML_(cur_read_strdup)(get_DiCursor_from_Cursor(&cur), "di.getFC.1");
      TRACE_D3("(indirect string offset, offset: 0x%lx): %s", str_offsets_pos, tmp);
      ML_(dinfo_free)(tmp);
   }
   get_Form_contents_str( cts, form, get_Dwarfish_UWord(&cur, cc->is_dw64), cc, td3 );
}

/* From 'c', get the Form data into 'cts'.  Either it gets a 1/2/4/8
   byte scalar value, or (a reference to) zero or more bytes starting
   at a DiCursor.*/
static
void get_Form_contents ( /*OUT*/FormContents* cts,
                         const CUConst* cc, Cursor* c,
                         Bool td3, const name_form *abbv )
{
   DW_FORM form = abbv->at_form;
   VG_(bzero_inline)(cts, sizeof(*cts));
   // !!! keep switch in sync with get_Form_szB. The nr of characters read below
   // must be computed similarly in get_Form_szB.
   // The consistency is verified in trace_DIE.
   switch (form) {
      case DW_FORM_data1:
         cts->u.val = (ULong)(UChar)get_UChar(c);
         cts->szB   = 1;
         TRACE_D3("%u", (UInt)cts->u.val);
         break;
      case DW_FORM_data2:
         cts->u.val = (ULong)(UShort)get_UShort(c);
         cts->szB   = 2;
         TRACE_D3("%u", (UInt)cts->u.val);
         break;
      case DW_FORM_data4:
         cts->u.val = (ULong)(UInt)get_UInt(c);
         cts->szB   = 4;
         TRACE_D3("%u", (UInt)cts->u.val);
         break;
      case DW_FORM_data8:
         cts->u.val = get_ULong(c);
         cts->szB   = 8;
         TRACE_D3("%llu", cts->u.val);
         break;
     case DW_FORM_data16: {
         /* This is more like a block than an integral value.  */
         ULong    u64b;
         DiCursor data16 = get_DiCursor_from_Cursor(c);
         TRACE_D3("data16: ");
         for (u64b = 16; u64b > 0; u64b--) {
            UChar u8 = get_UChar(c);
            TRACE_D3("%x ", (UInt)u8);
         }
         cts->u.cur = data16;
         cts->szB   = - (Long)16;
         break;
         }
      case DW_FORM_sec_offset:
         cts->u.val = (ULong)get_Dwarfish_UWord( c, cc->is_dw64 );
         cts->szB   = cc->is_dw64 ? 8 : 4;
         TRACE_D3("%llu", cts->u.val);
         break;
      case DW_FORM_rnglistx: {
         if(!cc->cu_has_rnglists_base) {
            cc->barf("get_Form_contents: DW_FORM_rnglistsx"
                     " without DW_AT_rnglists_base");
         }
         /* Convert index to offset pointing to the offsets list. */
         ULong index = get_ULEB128(c);
         ULong offset_to_offset = cc->cu_rnglists_base + index * sizeof_Dwarfish_UWord( cc->is_dw64 );
         /* And read the offset value from there. */
         Cursor cur;
         init_Cursor( &cur, cc->escn_debug_rnglists, offset_to_offset, cc->barf,
                      "get_Form_contents: index points outside .debug_rnglists" );
         cts->u.val = cc->cu_rnglists_base + get_Dwarfish_UWord(&cur, cc->is_dw64);
         cts->szB   = 8;
         TRACE_D3("%llu", cts->u.val);
         break;
      }
      case DW_FORM_loclistx: {
         if(!cc->cu_has_loclists_base) {
            cc->barf("get_Form_contents: DW_FORM_loclistsx"
                     " without DW_AT_loclists_base");
         }
         /* Convert index to offset pointing to the offsets list. */
         ULong index = get_ULEB128(c);
         ULong offset_to_offset = cc->cu_loclists_base + index * sizeof_Dwarfish_UWord( cc->is_dw64 );
         /* And read the offset value from there. */
         Cursor cur;
         init_Cursor( &cur, cc->escn_debug_loclists, offset_to_offset, cc->barf,
                      "get_Form_contents: index points outside .debug_loclists" );
         cts->u.val = cc->cu_loclists_base + get_Dwarfish_UWord(&cur, cc->is_dw64);
         cts->szB   = 8;
         TRACE_D3("%llu", cts->u.val);
         break;
      }
      case DW_FORM_sdata:
         cts->u.val = (ULong)(Long)get_SLEB128(c);
         cts->szB   = 8;
         TRACE_D3("%llu", cts->u.val);
         break;
      case DW_FORM_udata:
         cts->u.val = (ULong)(Long)get_ULEB128(c);
         cts->szB   = 8;
         TRACE_D3("%llu", cts->u.val);
         break;
      case DW_FORM_addr:
         /* note, this is a hack.  DW_FORM_addr is defined as getting
            a word the size of the target machine as defined by the
            address_size field in the CU Header.  However,
            parse_CU_Header() rejects all inputs except those for
            which address_size == sizeof(Word), hence we can just
            treat it as a (host) Word.  */
         cts->u.val = (ULong)(UWord)get_UWord(c);
         cts->szB   = sizeof(UWord);
         TRACE_D3("0x%lx", (UWord)cts->u.val);
         break;

      case DW_FORM_ref_addr:
         /* We make the same word-size assumption as DW_FORM_addr. */
         /* What does this really mean?  From D3 Sec 7.5.4,
            description of "reference", it would appear to reference
            some other DIE, by specifying the offset from the
            beginning of a .debug_info section.  The D3 spec mentions
            that this might be in some other shared object and
            executable.  But I don't see how the name of the other
            object/exe is specified.

            At least for the DW_FORM_ref_addrs created by icc11, the
            references seem to be within the same object/executable.
            So for the moment we merely range-check, to see that they
            actually do specify a plausible offset within this
            object's .debug_info, and return the value unchanged.

            In DWARF 2, DW_FORM_ref_addr is address-sized, but in
            DWARF 3 and later, it is offset-sized.
         */
         if (cc->version == 2) {
            cts->u.val = (ULong)(UWord)get_UWord(c);
            cts->szB   = sizeof(UWord);
         } else {
            cts->u.val = get_Dwarfish_UWord(c, cc->is_dw64);
            cts->szB   = cc->is_dw64 ? sizeof(ULong) : sizeof(UInt);
         }
         TRACE_D3("0x%lx", (UWord)cts->u.val);
         if (0) VG_(printf)("DW_FORM_ref_addr 0x%lx\n", (UWord)cts->u.val);
         if (/* the following is surely impossible, but ... */
             !ML_(sli_is_valid)(cc->escn_debug_info)
             || cts->u.val >= (ULong)cc->escn_debug_info.szB) {
            /* Hmm.  Offset is nonsensical for this object's .debug_info
               section.  Be safe and reject it. */
            cc->barf("get_Form_contents: DW_FORM_ref_addr points "
                     "outside .debug_info");
         }
         break;

      case DW_FORM_strp: {
         /* this is an offset into .debug_str */
         UWord uw = (UWord)get_Dwarfish_UWord( c, cc->is_dw64 );
         get_Form_contents_str( cts, form, uw, cc, td3 );
         break;
      }
      case DW_FORM_line_strp: {
         /* this is an offset into .debug_line_str */
         UWord uw = (UWord)get_Dwarfish_UWord( c, cc->is_dw64 );
         if (!ML_(sli_is_valid)(cc->escn_debug_line_str)
             || uw >= cc->escn_debug_line_str.szB)
            cc->barf("get_Form_contents: DW_FORM_line_strp "
                     "points outside .debug_line_str");
         /* FIXME: check the entire string lies inside debug_line_str,
            not just the first byte of it. */
         DiCursor line_str
            = ML_(cur_plus)( ML_(cur_from_sli)(cc->escn_debug_line_str), uw );
         if (TD3) {
            HChar* tmp = ML_(cur_read_strdup)(line_str, "di.getFC.1.5");
            TRACE_D3("(indirect line string, offset: 0x%lx): %s", uw, tmp);
            ML_(dinfo_free)(tmp);
         }
         cts->u.cur = line_str;
         cts->szB   = - (Long)(1 + (ULong)ML_(cur_strlen)(line_str));
         break;
      }
      case DW_FORM_string: {
         DiCursor str = get_AsciiZ(c);
         if (TD3) {
            HChar* tmp = ML_(cur_read_strdup)(str, "di.getFC.2");
            TRACE_D3("%s", tmp);
            ML_(dinfo_free)(tmp);
         }
         cts->u.cur = str;
         /* strlen is safe because get_AsciiZ already 'vetted' the
            entire string */
         cts->szB   = - (Long)(1 + (ULong)ML_(cur_strlen)(str));
         break;
      }
      case DW_FORM_ref1: {
         UChar u8   = get_UChar(c);
         UWord res  = cc->cu_start_offset + (UWord)u8;
         cts->u.val = (ULong)res;
         cts->szB   = sizeof(UWord);
         TRACE_D3("<%lx>", res);
         break;
      }
      case DW_FORM_ref2: {
         UShort u16 = get_UShort(c);
         UWord  res = cc->cu_start_offset + (UWord)u16;
         cts->u.val = (ULong)res;
         cts->szB   = sizeof(UWord);
         TRACE_D3("<%lx>", res);
         break;
      }
      case DW_FORM_ref4: {
         UInt  u32  = get_UInt(c);
         UWord res  = cc->cu_start_offset + (UWord)u32;
         cts->u.val = (ULong)res;
         cts->szB   = sizeof(UWord);
         TRACE_D3("<%lx>", res);
         break;
      }
      case DW_FORM_ref8: {
         ULong u64  = get_ULong(c);
         UWord res  = cc->cu_start_offset + (UWord)u64;
         cts->u.val = (ULong)res;
         cts->szB   = sizeof(UWord);
         TRACE_D3("<%lx>", res);
         break;
      }
      case DW_FORM_ref_udata: {
         ULong u64  = get_ULEB128(c);
         UWord res  = cc->cu_start_offset + (UWord)u64;
         cts->u.val = (ULong)res;
         cts->szB   = sizeof(UWord);
         TRACE_D3("<%lx>", res);
         break;
      }
      case DW_FORM_flag: {
         UChar u8 = get_UChar(c);
         TRACE_D3("%u", (UInt)u8);
         cts->u.val = (ULong)u8;
         cts->szB   = 1;
         break;
      }
      case DW_FORM_flag_present:
         TRACE_D3("1");
         cts->u.val = 1;
         cts->szB   = 1;
         break;
      case DW_FORM_implicit_const:
         cts->u.val = (ULong)abbv->at_val;
         cts->szB   = 8;
         TRACE_D3("%llu", cts->u.val);
         break;
      case DW_FORM_block1: {
         ULong    u64b;
         ULong    u64   = (ULong)get_UChar(c);
         DiCursor block = get_DiCursor_from_Cursor(c);
         TRACE_D3("%llu byte block: ", u64);
         for (u64b = u64; u64b > 0; u64b--) {
            UChar u8 = get_UChar(c);
            TRACE_D3("%x ", (UInt)u8);
         }
         cts->u.cur = block;
         cts->szB   = - (Long)u64;
         break;
      }
      case DW_FORM_block2: {
         ULong    u64b;
         ULong    u64   = (ULong)get_UShort(c);
         DiCursor block = get_DiCursor_from_Cursor(c);
         TRACE_D3("%llu byte block: ", u64);
         for (u64b = u64; u64b > 0; u64b--) {
            UChar u8 = get_UChar(c);
            TRACE_D3("%x ", (UInt)u8);
         }
         cts->u.cur = block;
         cts->szB   = - (Long)u64;
         break;
      }
      case DW_FORM_block4: {
         ULong    u64b;
         ULong    u64   = (ULong)get_UInt(c);
         DiCursor block = get_DiCursor_from_Cursor(c);
         TRACE_D3("%llu byte block: ", u64);
         for (u64b = u64; u64b > 0; u64b--) {
            UChar u8 = get_UChar(c);
            TRACE_D3("%x ", (UInt)u8);
         }
         cts->u.cur = block;
         cts->szB   = - (Long)u64;
         break;
      }
      case DW_FORM_exprloc:
      case DW_FORM_block: {
         ULong    u64b;
         ULong    u64   = (ULong)get_ULEB128(c);
         DiCursor block = get_DiCursor_from_Cursor(c);
         TRACE_D3("%llu byte block: ", u64);
         for (u64b = u64; u64b > 0; u64b--) {
            UChar u8 = get_UChar(c);
            TRACE_D3("%x ", (UInt)u8);
         }
         cts->u.cur = block;
         cts->szB   = - (Long)u64;
         break;
      }
      case DW_FORM_ref_sig8: {
         ULong  u64b;
         ULong  signature = get_ULong (c);
         ULong  work = signature;
         TRACE_D3("8 byte signature: ");
         for (u64b = 8; u64b > 0; u64b--) {
            UChar u8 = work & 0xff;
            TRACE_D3("%x ", (UInt)u8);
            work >>= 8;
         }

         /* cc->signature_types is only built/initialised when
            VG_(clo_read_var_info) is set. In this case,
            the DW_FORM_ref_sig8 can be looked up.
            But we can also arrive here when only reading inline info
            and VG_(clo_trace_symtab) is set. In such a case,
            we cannot lookup the DW_FORM_ref_sig8, we rather assign
            a dummy value. This is a kludge, but otherwise,
            the 'dwarf inline info reader' tracing would have to
            do type processing/reading. It is better to avoid
            adding significant 'real' processing only due to tracing. */
         if (VG_(clo_read_var_info)) {
            /* Due to the way that the hash table is constructed, the
               resulting DIE offset here is already "cooked".  See
               cook_die_using_form.  */
            cts->u.val = lookup_signatured_type (cc->signature_types, signature,
                                                 c->barf);
         } else {
            vg_assert (td3);
            vg_assert (VG_(clo_read_inline_info));
            TRACE_D3("<not dereferencing signature type>");
            cts->u.val = 0; /* Assign a dummy/rubbish value */
         }
         cts->szB   = sizeof(UWord);
         break;
      }
      case DW_FORM_indirect: {
         /* Urgh, this is ugly and somewhat unclear how it works
            with DW_FORM_implicit_const. HACK.  */
         name_form nfi = *abbv;
         nfi.at_form = (DW_FORM)get_ULEB128(c);
         get_Form_contents (cts, cc, c, td3, &nfi);
         return;
      }

      case DW_FORM_GNU_ref_alt:
         cts->u.val = get_Dwarfish_UWord(c, cc->is_dw64);
         cts->szB   = cc->is_dw64 ? sizeof(ULong) : sizeof(UInt);
         TRACE_D3("0x%lx", (UWord)cts->u.val);
         if (0) VG_(printf)("DW_FORM_GNU_ref_alt 0x%lx\n", (UWord)cts->u.val);
         if (/* the following is surely impossible, but ... */
             !ML_(sli_is_valid)(cc->escn_debug_info_alt))
            cc->barf("get_Form_contents: DW_FORM_GNU_ref_addr used, "
                     "but no alternate .debug_info");
         else if (cts->u.val >= (ULong)cc->escn_debug_info_alt.szB) {
            /* Hmm.  Offset is nonsensical for this object's .debug_info
               section.  Be safe and reject it. */
            cc->barf("get_Form_contents: DW_FORM_GNU_ref_addr points "
                     "outside alternate .debug_info");
         }
         break;

      case DW_FORM_GNU_strp_alt: {
         /* this is an offset into alternate .debug_str */
         SizeT uw = (UWord)get_Dwarfish_UWord( c, cc->is_dw64 );
         if (!ML_(sli_is_valid)(cc->escn_debug_str_alt))
            cc->barf("get_Form_contents: DW_FORM_GNU_strp_alt used, "
                     "but no alternate .debug_str");
         else if (uw >= cc->escn_debug_str_alt.szB)
            cc->barf("get_Form_contents: DW_FORM_GNU_strp_alt "
                     "points outside alternate .debug_str");
         /* FIXME: check the entire string lies inside debug_str,
            not just the first byte of it. */
         DiCursor str
            = ML_(cur_plus)( ML_(cur_from_sli)(cc->escn_debug_str_alt), uw);
         if (TD3) {
            HChar* tmp = ML_(cur_read_strdup)(str, "di.getFC.3");
            TRACE_D3("(indirect alt string, offset: 0x%lx): %s", uw, tmp);
            ML_(dinfo_free)(tmp);
         }
         cts->u.cur = str;
         cts->szB   = - (Long)(1 + (ULong)ML_(cur_strlen)(str));
         break;
      }

      case DW_FORM_addrx: {
         /* this is an offset into .debug_addr */
         ULong index = (ULong)(Long)get_ULEB128(c);
         get_Form_contents_addr(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_addrx1: {
         /* this is an offset into .debug_addr */
         ULong index = (ULong)get_UChar(c);
         get_Form_contents_addr(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_addrx2: {
         /* this is an offset into .debug_addr */
         ULong index = (ULong)get_UShort(c);
         get_Form_contents_addr(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_addrx3: {
         /* this is an offset into .debug_addr */
         ULong index = (ULong)get_UInt3(c);
         get_Form_contents_addr(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_addrx4: {
         /* this is an offset into .debug_addr */
         ULong index = (ULong)get_UInt(c);
         get_Form_contents_addr(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_strx: {
         /* this is an offset into .debug_str_offsets */
         ULong index = (ULong)(Long)get_ULEB128(c);
         get_Form_contents_str_offsets(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_strx1: {
         /* this is an offset into .debug_str_offsets */
         ULong index = get_UChar(c);
         get_Form_contents_str_offsets(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_strx2: {
         /* this is an offset into .debug_str_offsets */
         ULong index = (ULong)get_UShort(c);
         get_Form_contents_str_offsets(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_strx3: {
         /* this is an offset into .debug_str_offsets */
         ULong index = (ULong)get_UInt3(c);
         get_Form_contents_str_offsets(cts, form, index, cc, td3);
         break;
      }
      case DW_FORM_strx4: {
         /* this is an offset into .debug_str_offsets */
         ULong index = (ULong)get_UInt(c);
         get_Form_contents_str_offsets(cts, form, index, cc, td3);
         break;
      }

      default:
         VG_(printf)(
            "get_Form_contents: unhandled %u (%s) at <%llx>\n",
            form, ML_(pp_DW_FORM)(form), get_position_of_Cursor(c));
         c->barf("get_Form_contents: unhandled DW_FORM");
   }
}

#define VARSZ_FORM 0xffffffff
/* If the form is a fixed length form, return the nr of bytes for this form.
   If the form is a variable length form, return VARSZ_FORM. */
static
UInt get_Form_szB (const CUConst* cc, DW_FORM form )
{
   // !!! keep switch in sync with get_Form_contents : the nr of bytes
   // read from a cursor by get_Form_contents must be returned by
   // the below switch.
   // The consistency is verified in trace_DIE.
   switch (form) {
      case DW_FORM_data1: return 1;
      case DW_FORM_data2: return 2;
      case DW_FORM_data4: return 4;
      case DW_FORM_data8: return 8;
      case DW_FORM_data16: return 16;
      case DW_FORM_sec_offset:
         if (cc->is_dw64)
            return 8;
         else
            return 4;
      case DW_FORM_rnglistx:
      case DW_FORM_loclistx:
         return VARSZ_FORM;
      case DW_FORM_sdata:
         return VARSZ_FORM;
      case DW_FORM_udata:
         return VARSZ_FORM;
      case DW_FORM_addr: // See hack in get_Form_contents
         return sizeof(UWord);
      case DW_FORM_ref_addr: // See hack in get_Form_contents
         if (cc->version == 2)
            return sizeof(UWord);
         else 
            return sizeof_Dwarfish_UWord (cc->is_dw64);
      case DW_FORM_strp:
      case DW_FORM_line_strp:
         return sizeof_Dwarfish_UWord (cc->is_dw64);
      case DW_FORM_string: 
         return VARSZ_FORM;
      case DW_FORM_ref1:
         return 1;
      case DW_FORM_ref2:
         return 2;
      case DW_FORM_ref4:
         return 4;
      case DW_FORM_ref8:
         return 8;
      case DW_FORM_ref_udata:
         return VARSZ_FORM;
      case DW_FORM_flag: 
         return 1;
      case DW_FORM_flag_present:
         return 0; // !!! special case, no data.
      case DW_FORM_block1:
         return VARSZ_FORM;
      case DW_FORM_block2:
         return VARSZ_FORM;
      case DW_FORM_block4:
         return VARSZ_FORM;
      case DW_FORM_exprloc:
      case DW_FORM_block:
         return VARSZ_FORM;
      case DW_FORM_ref_sig8:
         return 8;
      case DW_FORM_indirect:
         return VARSZ_FORM;
      case DW_FORM_GNU_ref_alt:
         return sizeof_Dwarfish_UWord(cc->is_dw64);
      case DW_FORM_GNU_strp_alt:
         return sizeof_Dwarfish_UWord(cc->is_dw64);
      case DW_FORM_implicit_const:
	 return 0; /* Value inside abbrev. */
      case DW_FORM_addrx:
         return VARSZ_FORM;
      case DW_FORM_strx:
         return VARSZ_FORM;
      case DW_FORM_addrx1:
      case DW_FORM_strx1:
         return 1;
      case DW_FORM_addrx2:
      case DW_FORM_strx2:
         return 2;
      case DW_FORM_addrx3:
      case DW_FORM_strx3:
         return 3;
      case DW_FORM_addrx4:
      case DW_FORM_strx4:
         return 4;
      default:
         VG_(printf)(
            "get_Form_szB: unhandled %u (%s)\n",
            form, ML_(pp_DW_FORM)(form));
         cc->barf("get_Form_contents: unhandled DW_FORM");
   }
}

/* Skip a DIE as described by abbv.
   If the DIE has a sibling, *sibling is set to the skipped DIE sibling value. */
static
void skip_DIE (UWord  *sibling,
               Cursor* c_die,
               const g_abbv *abbv,
               const CUConst* cc)
{
   UInt nf_i;
   FormContents cts;
   nf_i = 0;
   while (True) {
      if (abbv->nf[nf_i].at_name == DW_AT_sibling) {
         get_Form_contents( &cts, cc, c_die, False /*td3*/,
                            &abbv->nf[nf_i] );
         if ( cts.szB > 0 ) 
            *sibling = cts.u.val;
         nf_i++;
      } else if (abbv->nf[nf_i].skip_szB == VARSZ_FORM) {
         DW_FORM form = abbv->nf[nf_i].at_form;
         if(form == DW_FORM_addrx || form == DW_FORM_strx
            || form == DW_FORM_rnglistx || form == DW_FORM_loclistx) {
            /* Skip without interpreting them, they may depend on e.g.
               DW_AT_addr_base that has not been read yet. */
            (void) get_ULEB128(c_die);
         } else
            get_Form_contents( &cts, cc, c_die, False /*td3*/,
                               &abbv->nf[nf_i] );
         nf_i++;
      } else {
         advance_position_of_Cursor (c_die, (ULong)abbv->nf[nf_i].skip_szB);
         nf_i = abbv->nf[nf_i].next_nf;
      }
      if (nf_i == 0)
         break;
   }
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Parsing of variable-related DIEs                     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

typedef
   struct _TempVar {
      const HChar*  name; /* in DebugInfo's .strpool */
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
      XArray* rngMany; /* of AddrRange.  NON-UNIQUE PTR in AR_DINFO. */
      /* Do not free .rngMany, since many TempVars will have the same
         value.  Instead the associated storage is to be freed by
         deleting 'rangetree', which stores a single copy of each
         range. */
      /* --- */
      Int     level;
      UWord   typeR; /* a cuOff */
      GExpr*  gexpr; /* for this variable */
      GExpr*  fbGX;  /* to find the frame base of the enclosing fn, if
                        any */
      UInt    fndn_ix; /* declaring file/dirname index in fndnpool, or 0 */
      Int     fLine; /* declaring file line number, or zero */
      /* offset in .debug_info, so that abstract instances can be
         found to satisfy references from concrete instances. */
      UWord   dioff;
      UWord   absOri; /* so the absOri fields refer to dioff fields
                         in some other, related TempVar. */
   }
   TempVar;

typedef
   struct {
      /* Contains the range stack: a stack of address ranges, one
         stack entry for each nested scope.  

         Some scope entries are created by function definitions
         (DW_AT_subprogram), and for those, we also note the GExpr
         derived from its DW_AT_frame_base attribute, if any.
         Consequently it should be possible to find, for any
         variable's DIE, the GExpr for the containing function's
         DW_AT_frame_base by scanning back through the stack to find
         the nearest entry associated with a function.  This somewhat
         elaborate scheme is provided so as to make it possible to
         obtain the correct DW_AT_frame_base expression even in the
         presence of nested functions (or to be more precise, in the
         presence of nested DW_AT_subprogram DIEs). 
      */
      Int     sp; /* [sp] is innermost active entry; sp==-1 for empty
                     stack */
      Int     stack_size;
      XArray **ranges; /* XArray of AddrRange */
      Int     *level;  /* D3 DIE levels */
      Bool    *isFunc; /* from DW_AT_subprogram? */
      GExpr  **fbGX;   /* if isFunc, contains the FB expr, else NULL */
   }
   D3VarParser;

/* Completely initialise a variable parser object */
static void
var_parser_init ( D3VarParser *parser )
{
   parser->sp = -1;
   parser->stack_size = 0;
   parser->ranges = NULL;
   parser->level  = NULL;
   parser->isFunc = NULL;
   parser->fbGX = NULL;
}

/* Release any memory hanging off a variable parser object */
static void
var_parser_release ( D3VarParser *parser )
{
   ML_(dinfo_free)( parser->ranges );
   ML_(dinfo_free)( parser->level );
   ML_(dinfo_free)( parser->isFunc );
   ML_(dinfo_free)( parser->fbGX );
}

static void varstack_show ( const D3VarParser* parser, const HChar* str )
{
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
            VG_(printf)("[%#lx,%#lx] ", range->aMin, range->aMax);
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
   vg_assert(parser->sp < parser->stack_size);
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
      parser->sp--;
      changed = True;
   }
   if (changed && td3)
      varstack_show( parser, "after preen" );
}

static void varstack_push ( const CUConst* cc,
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
   vg_assert(parser->sp < parser->stack_size);
   if (parser->sp == parser->stack_size - 1) {
      parser->stack_size += 48;
      parser->ranges =
         ML_(dinfo_realloc)("di.readdwarf3.varpush.1", parser->ranges,
                            parser->stack_size * sizeof parser->ranges[0]);
      parser->level =
         ML_(dinfo_realloc)("di.readdwarf3.varpush.2", parser->level,
                            parser->stack_size * sizeof parser->level[0]);
      parser->isFunc =
         ML_(dinfo_realloc)("di.readdwarf3.varpush.3", parser->isFunc,
                            parser->stack_size * sizeof parser->isFunc[0]);
      parser->fbGX =
         ML_(dinfo_realloc)("di.readdwarf3.varpush.4", parser->fbGX,
                            parser->stack_size * sizeof parser->fbGX[0]);
   }
   if (parser->sp >= 0)
      vg_assert(parser->level[parser->sp] < level);
   parser->sp++;
   vg_assert(ranges != NULL);
   if (!isFunc) vg_assert(fbGX == NULL);
   parser->ranges[parser->sp] = ranges;
   parser->level[parser->sp]  = level;
   parser->isFunc[parser->sp] = isFunc;
   parser->fbGX[parser->sp]   = fbGX;
   if (TD3)
      varstack_show( parser, "after push" );
}


/* cts is derived from a DW_AT_location and so refers either to a
   location expression or to a location list.  Figure out which, and
   in both cases bundle the expression or location list into a
   so-called GExpr (guarded expression). */
__attribute__((noinline))
static GExpr* get_GX ( const CUConst* cc, Bool td3, const FormContents* cts )
{
   GExpr* gexpr = NULL;
   if (cts->szB < 0) {
      /* represents a non-empty in-line location expression, and
         cts->u.cur points at the image bytes */
      gexpr = make_singleton_GX( cts->u.cur, (ULong)(- cts->szB) );
   }
   else 
   if (cts->szB > 0) {
      /* represents a location list.  cts->u.val is the offset of it
         in .debug_loc. */
      if (!cc->cu_svma_known)
         cc->barf("get_GX: location list, but CU svma is unknown");
      gexpr = make_general_GX( cc, td3, cts->u.val, cc->cu_svma );
   }
   else {
      vg_assert(0); /* else caller is bogus */
   }
   return gexpr;
}

static
HChar * get_line_str (struct _DebugInfo* di, Bool is_dw64,
                      Cursor *data, const UInt form,
                      DiSlice debugstr_img, DiSlice debuglinestr_img)
{
   HChar *str = NULL;
   switch (form) {
   case DW_FORM_string: {
      DiCursor distr = get_AsciiZ(data);
      str = ML_(cur_step_strdup)(&distr, "di.gls.string");
      break;
   }
   case DW_FORM_strp: {
      UWord uw = (UWord)get_Dwarfish_UWord( data, is_dw64 );
      DiCursor distr
         = ML_(cur_plus)( ML_(cur_from_sli)(debugstr_img), uw );
      str = ML_(cur_read_strdup)(distr, "di.gls.strp");
      break;
   }
   case DW_FORM_line_strp: {
      UWord uw = (UWord)get_Dwarfish_UWord( data, is_dw64 );
      DiCursor distr
         = ML_(cur_plus)( ML_(cur_from_sli)(debuglinestr_img), uw );
      str = ML_(cur_read_strdup)(distr, "di.gls.line_strp");
      break;
   }
   default:
      ML_(symerr)(di, True,
                  "Unknown path string FORM in .debug_line");
      break;
   }
   return str;
}

static
Int get_line_ndx (struct _DebugInfo* di,
                  Cursor *data, const UInt form)
{
   Int res = 0;
   switch (form) {
   case DW_FORM_data1:
      res = get_UChar(data);
      break;
   case DW_FORM_data2:
      res = get_UShort(data);
      break;
   case DW_FORM_udata:
      res = get_ULEB128(data);
      break;
   default:
      ML_(symerr)(di, True,
                  "Unknown directory_index value FORM in .debug_line");
      break;
   }
   return res;
}

static
void skip_line_form (struct _DebugInfo* di, Bool is_dw64,
                         Cursor *d, const UInt form)
{
   switch (form) {
   case DW_FORM_block: {
      ULong len = get_ULEB128(d);
      advance_position_of_Cursor (d, len);
      break;
   }
   case DW_FORM_block1: {
      UChar len = get_UChar(d);
      advance_position_of_Cursor (d, len);
      break;
   }
   case DW_FORM_block2: {
      UShort len = get_UShort(d);
      advance_position_of_Cursor (d, len);
      break;
   }
   case DW_FORM_block4: {
      UInt len = get_UInt(d);
      advance_position_of_Cursor (d, len);
      break;
   }
   case DW_FORM_flag:
   case DW_FORM_data1:
      advance_position_of_Cursor (d, 1);
      break;
   case DW_FORM_data2:
      advance_position_of_Cursor (d, 2);
      break;
   case DW_FORM_data4:
      advance_position_of_Cursor (d, 4);
      break;
   case DW_FORM_data8:
      advance_position_of_Cursor (d, 8);
      break;
   case DW_FORM_data16:
      advance_position_of_Cursor (d, 16);
      break;
   case DW_FORM_string:
      (void)get_AsciiZ (d);
      break;
   case DW_FORM_strp:
   case DW_FORM_line_strp:
   case DW_FORM_sec_offset:
      advance_position_of_Cursor (d, is_dw64 ? 8 : 4);
      break;
   case DW_FORM_udata:
      (void)get_ULEB128(d);
      break;
   case DW_FORM_sdata:
      (void)get_SLEB128(d);
      break;
   default:
      ML_(symerr)(di, True, "Unknown FORM in .debug_line");
      break;
   }
}

/* Returns an xarray* of directory names (indexed by the dwarf dirname
   integer).
   If 'compdir' is NULL, entry [0] will be set to "."
   otherwise entry [0] is set to compdir.
   Entry [0] basically means "the current directory of the compilation",
   whatever that means, according to the DWARF3 spec.
   FIXME??? readdwarf3.c/readdwarf.c have a lot of duplicated code */
static
XArray* read_dirname_xa (DebugInfo* di, UShort version, const HChar *compdir,
                         Cursor *c, const CUConst *cc,
                         Bool td3 )
{
   XArray*        dirname_xa;   /* xarray of HChar* dirname */
   const HChar*   dirname;
   UInt           compdir_len;

   dirname_xa = VG_(newXA) (ML_(dinfo_zalloc), "di.rdxa.1", ML_(dinfo_free),
                            sizeof(HChar*) );

   if (compdir == NULL) {
      dirname = ".";
      compdir_len = 1;
   } else {
      dirname = compdir;
      compdir_len = VG_(strlen)(compdir);
   }

   /* For version 5, the compdir is the first (zero) entry. */
   if (version < 5)
      VG_(addToXA) (dirname_xa, &dirname);

   if (version < 5) {
      TRACE_D3("The Directory Table%s\n",
               peek_UChar(c) == 0 ? " is empty." : ":" );

      while (peek_UChar(c) != 0) {

         DiCursor cur = get_AsciiZ(c);
         HChar* data_str = ML_(cur_read_strdup)( cur, "dirname_xa.1" );
         TRACE_D3("  %s\n", data_str);

         /* If data_str[0] is '/', then 'data' is an absolute path and we
            don't mess with it.  Otherwise, construct the
            path 'compdir' ++ "/" ++ 'data'. */

         if (data_str[0] != '/'
             /* not an absolute path */
             && compdir
             /* actually got something sensible for compdir */
             && compdir_len)
         {
            SizeT  len = compdir_len + 1 + VG_(strlen)(data_str);
            HChar *buf = ML_(dinfo_zalloc)("dirname_xa.2", len + 1);

            VG_(strcpy)(buf, compdir);
            VG_(strcat)(buf, "/");
            VG_(strcat)(buf, data_str);

            dirname = ML_(addStr)(di, buf, len);
            VG_(addToXA) (dirname_xa, &dirname);
            if (0) VG_(printf)("rel path  %s\n", buf);
            ML_(dinfo_free)(buf);
         } else {
            /* just use 'data'. */
            dirname = ML_(addStr)(di,data_str,-1);
            VG_(addToXA) (dirname_xa, &dirname);
            if (0) VG_(printf)("abs path  %s\n", data_str);
         }

         ML_(dinfo_free)(data_str);
      }
   } else {
      UChar forms[256];
      UChar p_ndx = 0;
      UInt directories_count;
      UChar directory_entry_format_count;
      UInt n;
      DiSlice debugstr_img = cc->escn_debug_str;
      DiSlice debuglinestr_img = cc->escn_debug_line_str;

      directory_entry_format_count = get_UChar(c);
      for (n = 0; n < directory_entry_format_count; n++) {
         UInt lnct = get_ULEB128(c);
         UInt form = get_ULEB128(c);
         if (lnct == DW_LNCT_path)
            p_ndx = n;
         forms[n] = form;
      }
      directories_count = get_ULEB128(c);
      TRACE_D3("The Directory Table%s\n",
               directories_count == 0 ? " is empty." : ":" );

      for (n = 0; n < directories_count; n++) {
         UInt f;
         for (f = 0; f < directory_entry_format_count; f++) {
            UInt form = forms[f];
            if (f == p_ndx) {
               HChar *data_str = get_line_str (di, cc->is_dw64, c, form,
                                               debugstr_img,
                                               debuglinestr_img);
               TRACE_D3("  %s\n", data_str);

               /* If data_str[0] is '/', then 'data' is an absolute path and we
                  don't mess with it.  Otherwise, construct the
                  path 'compdir' ++ "/" ++ 'data'. */

               if (data_str[0] != '/'
                   /* not an absolute path */
                   && compdir
                   /* actually got something sensible for compdir */
                   && compdir_len)
               {
                  SizeT  len = compdir_len + 1 + VG_(strlen)(data_str);
                  HChar *buf = ML_(dinfo_zalloc)("dirname_xa.2", len + 1);

                  VG_(strcpy)(buf, compdir);
                  VG_(strcat)(buf, "/");
                  VG_(strcat)(buf, data_str);

                  dirname = ML_(addStr)(di, buf, len);
                  VG_(addToXA) (dirname_xa, &dirname);
                  if (0) VG_(printf)("rel path  %s\n", buf);
                  ML_(dinfo_free)(buf);
               } else {
                  /* just use 'data'. */
                  dirname = ML_(addStr)(di,data_str,-1);
                  VG_(addToXA) (dirname_xa, &dirname);
                  if (0) VG_(printf)("abs path  %s\n", data_str);
               }

               ML_(dinfo_free)(data_str);
            } else {
               skip_line_form (di, cc->is_dw64, c, form);
            }
         }
      }
   }

   TRACE_D3 ("\n");

   if (version < 5 && get_UChar (c) != 0) {
      ML_(symerr)(NULL, True,
                  "could not get NUL at end of DWARF directory table");
      VG_(deleteXA)(dirname_xa);
      return NULL;
   }

   return dirname_xa;
}

static 
void read_filename_table( /*MOD*/XArray* /* of UInt* */ fndn_ix_Table,
                          const HChar* compdir,
                          const CUConst* cc, ULong debug_line_offset,
                          Bool td3 )
{
   Bool   is_dw64;
   Cursor c;
   Word   i;
   UShort version;
   UChar  opcode_base;
   const HChar* str;
   XArray* dirname_xa;   /* xarray of HChar* dirname */
   ULong  dir_xa_ix;     /* Index in dirname_xa, as read from dwarf info. */
   const HChar* dirname;
   UInt   fndn_ix;

   vg_assert(fndn_ix_Table && cc && cc->barf);
   if (!ML_(sli_is_valid)(cc->escn_debug_line)
       || cc->escn_debug_line.szB <= debug_line_offset) {
      cc->barf("read_filename_table: .debug_line is missing?");
   }

   init_Cursor( &c, cc->escn_debug_line, debug_line_offset, cc->barf, 
                "Overrun whilst reading .debug_line section(1)" );

   /* unit_length = */
   get_Initial_Length( &is_dw64, &c,
                       "read_filename_table: invalid initial-length field" );
   version = get_UShort( &c );
   if (version != 2 && version != 3 && version != 4 && version != 5)
     cc->barf("read_filename_table: Only DWARF version 2, 3, 4 and 5 "
              "line info is currently supported.");
   if (version >= 5) {
      /* addrs_size = */ get_UChar( &c );
      /* seg_size =   */ get_UChar( &c );
   }
   /*header_length              = (ULong)*/ get_Dwarfish_UWord( &c, is_dw64 );
   /*minimum_instruction_length = */ get_UChar( &c );
   if (version >= 4)
      /*maximum_operations_per_insn = */ get_UChar( &c );
   /*default_is_stmt            = */ get_UChar( &c );
   /*line_base                  = (Char)*/ get_UChar( &c );
   /*line_range                 = */ get_UChar( &c );
   opcode_base                = get_UChar( &c );
   /* skip over "standard_opcode_lengths" */
   for (i = 1; i < (Word)opcode_base; i++)
     (void)get_UChar( &c );

   dirname_xa = read_dirname_xa(cc->di, version, compdir, &c, cc, td3);

   /* Read and record the file names table */
   vg_assert( VG_(sizeXA)( fndn_ix_Table ) == 0 );
   if (version < 5) {
      /* Add a dummy index-zero entry.  DWARF3 numbers its files
         from 1, for some reason. */
      fndn_ix = ML_(addFnDn) ( cc->di, "<unknown_file>", NULL );
      VG_(addToXA)( fndn_ix_Table, &fndn_ix );
      while (peek_UChar(&c) != 0) {
         DiCursor cur = get_AsciiZ(&c);
         str = ML_(addStrFromCursor)( cc->di, cur );
         dir_xa_ix = get_ULEB128( &c );
         if (dirname_xa != NULL && dir_xa_ix < VG_(sizeXA) (dirname_xa))
            dirname = *(HChar**)VG_(indexXA) ( dirname_xa, dir_xa_ix );
         else
            dirname = NULL;
         fndn_ix = ML_(addFnDn)( cc->di, str, dirname);
         TRACE_D3("  read_filename_table: %ld fndn_ix %u %s %s\n",
                  VG_(sizeXA)(fndn_ix_Table), fndn_ix,
                  dirname, str);
         VG_(addToXA)( fndn_ix_Table, &fndn_ix );
         (void)get_ULEB128( &c ); /* skip last mod time */
         (void)get_ULEB128( &c ); /* file size */
      }
   } else {
      UChar forms[256];
      UChar p_ndx = 0, d_ndx = 0;
      UInt file_names_count;
      UChar file_names_entry_format_count;
      UInt n;
      DiSlice debugstr_img = cc->escn_debug_str;
      DiSlice debuglinestr_img = cc->escn_debug_line_str;
      file_names_entry_format_count = get_UChar( &c );
      for (n = 0; n < file_names_entry_format_count; n++) {
         UInt lnct = get_ULEB128( &c );
         UInt form = get_ULEB128( &c );
         if (lnct == DW_LNCT_path)
            p_ndx = n;
         if (lnct == DW_LNCT_directory_index)
            d_ndx = n;
         forms[n] = form;
      }
      file_names_count = get_ULEB128( &c );
      for (n = 0; n < file_names_count; n++) {
         UInt f;
         dir_xa_ix  = 0;
         str = NULL;
         for (f = 0; f < file_names_entry_format_count; f++) {
            UInt form = forms[f];
            if (f == p_ndx)
               str = get_line_str (cc->di, cc->is_dw64, &c, form,
                                   debugstr_img, debuglinestr_img);
            else if (f == d_ndx)
               dir_xa_ix = get_line_ndx (cc->di, &c, form);
            else
               skip_line_form (cc->di, cc->is_dw64, &c, form);
         }

         if (dirname_xa != NULL
             && dir_xa_ix >= 0 && dir_xa_ix < VG_(sizeXA) (dirname_xa))
            dirname = *(HChar**)VG_(indexXA) ( dirname_xa, dir_xa_ix );
         else
            dirname = NULL;
         fndn_ix = ML_(addFnDn)( cc->di, str, dirname);
         TRACE_D3("  read_filename_table: %ld fndn_ix %u %s %s\n",
                  VG_(sizeXA)(fndn_ix_Table), fndn_ix,
                  dirname, str);
         VG_(addToXA)( fndn_ix_Table, &fndn_ix );
      }
   }
   /* We're done!  The rest of it is not interesting. */
   if (dirname_xa != NULL)
      VG_(deleteXA)(dirname_xa);
}

/* setup_cu_svma to be called when a cu is found at level 0,
   to establish the cu_svma. */
static void setup_cu_svma(CUConst* cc, Bool have_lo, Addr ip_lo, Bool td3)
{
   Addr cu_svma;
   /* We have potentially more than one type of parser parsing the
      dwarf information. At least currently, each parser establishes
      the cu_svma. So, in case cu_svma_known, we check that the same
      result is obtained by the 2nd parsing of the cu.

      Alternatively, we could reset cu_svma_known after each parsing
      and then check that we only see a single DW_TAG_compile_unit DIE
      at level 0, DWARF3 only allows exactly one top level DIE per
      CU. */

   if (have_lo)
      cu_svma = ip_lo;
   else {
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
      /* .. whereas The Reality is, simply assume the SVMA is zero
         if it isn't specified. */
      cu_svma = 0;
   }

   if (cc->cu_svma_known) {
      vg_assert (cu_svma == cc->cu_svma);
   } else {
      cc->cu_svma_known = True;
      cc->cu_svma = cu_svma;
      if (0)
         TRACE_D3("setup_cu_svma: acquire CU_SVMA of %p\n", (void*) cc->cu_svma);
   }
}

/* Setup info from DW_AT_addr_base, DW_AT_str_offsets_base, DW_AT_rnglists_base
   and DW_AT_loclists_base. This needs to be done early, because other DW_AT_*
   info may depend on it. */
static void setup_cu_bases(CUConst* cc, Cursor* c_die, const g_abbv* abbv)
{
   FormContents cts;
   UInt nf_i;
   ULong saved_c_pos;
   if(cc->cu_has_addr_base && cc->cu_has_str_offsets_base
      && cc->cu_has_rnglists_base && cc->cu_has_loclists_base)
      return;
   saved_c_pos = get_position_of_Cursor(c_die);
   nf_i = 0;
   while (!cc->cu_has_addr_base || !cc->cu_has_str_offsets_base
      || !cc->cu_has_rnglists_base || !cc->cu_has_loclists_base) {
      DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
      DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
      const name_form *nf = &abbv->nf[nf_i];
      if (attr == 0 && form == 0)
         break;
      if (attr != DW_AT_addr_base && attr != DW_AT_str_offsets_base
          && attr != DW_AT_rnglists_base && attr != DW_AT_loclists_base) {
         const UInt form_szB = get_Form_szB (cc, form);
         if (form_szB == VARSZ_FORM) {
            if(form == DW_FORM_addrx || form == DW_FORM_strx
               || form == DW_FORM_rnglistx || form == DW_FORM_loclistx) {
               /* Skip without interpreting them, they depend on *_base. */
               (void) get_ULEB128(c_die);
            } else {
               /* Need to read the contents of this one to skip it. */
               get_Form_contents( &cts, cc, c_die, False /*td3*/,
                                  &abbv->nf[nf_i] );
            }
         } else {
            /* Skip without even reading it, as it may depend on *_base. */
            advance_position_of_Cursor (c_die, form_szB);
         }
         nf_i++;
         continue;
      }
      get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
      if (attr == DW_AT_addr_base && cts.szB > 0) {
         Addr addr_base = cts.u.val;
         if (cc->cu_has_addr_base)
            vg_assert (addr_base == cc->cu_addr_base);
         else {
            cc->cu_has_addr_base = True;
            cc->cu_addr_base = addr_base;
         }
      }
      if (attr == DW_AT_str_offsets_base && cts.szB > 0) {
         Addr str_offsets_base = cts.u.val;
         if (cc->cu_has_str_offsets_base)
            vg_assert (str_offsets_base == cc->cu_str_offsets_base);
         else {
            cc->cu_has_str_offsets_base = True;
            cc->cu_str_offsets_base = str_offsets_base;
         }
       }
      if (attr == DW_AT_rnglists_base && cts.szB > 0) {
         Addr rnglists_base = cts.u.val;
         if (cc->cu_has_rnglists_base)
            vg_assert (rnglists_base == cc->cu_rnglists_base);
         else {
            cc->cu_has_rnglists_base = True;
            cc->cu_rnglists_base = rnglists_base;
         }
       }
      if (attr == DW_AT_loclists_base && cts.szB > 0) {
         Addr loclists_base = cts.u.val;
         if (cc->cu_has_loclists_base)
            vg_assert (loclists_base == cc->cu_loclists_base);
         else {
            cc->cu_has_loclists_base = True;
            cc->cu_loclists_base = loclists_base;
         }
       }
       nf_i++;
   }
   set_position_of_Cursor(c_die, saved_c_pos);
}

static void trace_DIE(
   DW_TAG dtag,
   UWord posn,
   Int level,
   UWord saved_die_c_offset,
   const g_abbv *abbv,
   const CUConst* cc)
{
   Cursor c;
   FormContents cts;
   UWord sibling = 0;
   UInt nf_i;
   Bool  debug_types_flag;
   Bool  alt_flag;
   Cursor check_skip;
   UWord check_sibling = 0;

   posn = uncook_die( cc, posn, &debug_types_flag, &alt_flag );
   init_Cursor (&c, 
                debug_types_flag ? cc->escn_debug_types :
                alt_flag ? cc->escn_debug_info_alt : cc->escn_debug_info,
                saved_die_c_offset, cc->barf, 
                "Overrun trace_DIE");
   check_skip = c;
   VG_(printf)(" <%d><%lx>: Abbrev Number: %llu (%s)%s%s\n",
               level, posn, (ULong) abbv->abbv_code, ML_(pp_DW_TAG)( dtag ),
               debug_types_flag ? " (in .debug_types)" : "",
               alt_flag ? " (in alternate .debug_info)" : "");
   nf_i = 0;
   while (True) {
      DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
      DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
      const name_form *nf = &abbv->nf[nf_i];
      nf_i++;
      if (attr == 0 && form == 0) break;
      VG_(printf)("     %-18s: ", ML_(pp_DW_AT)(attr));
      /* Get the form contents, so as to print them */
      get_Form_contents( &cts, cc, &c, True, nf );
      if (attr == DW_AT_sibling && cts.szB > 0) {
         sibling = cts.u.val;
      }
      VG_(printf)("\t\n");
   }

   /* Verify that skipping a DIE gives the same displacement as
      tracing (i.e. reading) a DIE. If there is an inconsistency in
      the nr of bytes read by get_Form_contents and get_Form_szB, this
      should be detected by the below. Using --trace-symtab=yes
      --read-var-info=yes will ensure all DIEs are systematically
      verified. */
   skip_DIE (&check_sibling, &check_skip, abbv, cc);
   vg_assert (check_sibling == sibling);
   vg_assert (get_position_of_Cursor (&check_skip) 
              == get_position_of_Cursor (&c));
}

__attribute__((noreturn))
static void dump_bad_die_and_barf(
   const HChar *whichparser,
   DW_TAG dtag,
   UWord posn,
   Int level,
   Cursor* c_die,
   UWord saved_die_c_offset,
   const g_abbv *abbv,
   const CUConst* cc)
{
   trace_DIE (dtag, posn, level, saved_die_c_offset, abbv, cc);
   VG_(printf)("%s:\n", whichparser);
   cc->barf("confused by the above DIE");
}

__attribute__((noinline))
static void bad_DIE_confusion(int linenr)
{
   VG_(printf)("\nparse DIE(readdwarf3.c:%d): confused by:\n", linenr);
}
#define goto_bad_DIE do {bad_DIE_confusion(__LINE__); goto bad_DIE;} while (0)

/* Reset the fndn_ix_Table.  When we come across the top level DIE for a CU we
   will copy all the file names out of the .debug_line img area and use this
   table to look up the copies when we later see filename numbers in
   DW_TAG_variables etc. The table can be be reused between parsers (var and
   inline) and between CUs. So we keep a copy of the last one parsed. Call
   reset_fndn_ix_table before reading a new one from a new offset.  */
static
void reset_fndn_ix_table (XArray** fndn_ix_Table, ULong *debug_line_offset,
                          ULong new_offset)
{
   vg_assert (new_offset == -1
              || *debug_line_offset != new_offset);
   Int size = *fndn_ix_Table == NULL ? 0 : VG_(sizeXA) (*fndn_ix_Table);
   if (size > 0) {
      VG_(deleteXA) (*fndn_ix_Table);
      *fndn_ix_Table = NULL;
   }
   if (*fndn_ix_Table == NULL)
      *fndn_ix_Table = VG_(newXA)( ML_(dinfo_zalloc),
                                   "di.readdwarf3.reset_ix_table",
                                   ML_(dinfo_free),
                                   sizeof(UInt) );
   *debug_line_offset = new_offset;
}

__attribute__((noinline))
static void parse_var_DIE (
   /*MOD*/WordFM* /* of (XArray* of AddrRange, void) */ rangestree,
   /*MOD*/XArray* /* of TempVar* */ tempvars,
   /*MOD*/XArray* /* of GExpr* */ gexprs,
   /*MOD*/D3VarParser* parser,
   XArray** fndn_ix_Table,
   ULong *debug_line_offset,
   DW_TAG dtag,
   UWord posn,
   Int level,
   Cursor* c_die,
   const g_abbv *abbv,
   CUConst* cc,
   Bool td3
)
{
   FormContents cts;
   UInt nf_i;

   UWord saved_die_c_offset  = get_position_of_Cursor( c_die );

   varstack_preen( parser, td3, level-1 );

   if (dtag == DW_TAG_compile_unit
       || dtag == DW_TAG_type_unit
       || dtag == DW_TAG_partial_unit
       || dtag == DW_TAG_skeleton_unit) {
      Bool have_lo    = False;
      Bool have_hi1   = False;
      Bool hiIsRelative = False;
      Bool have_range = False;
      Addr ip_lo    = 0;
      Addr ip_hi1   = 0;
      Addr rangeoff = 0;
      const HChar *compdir = NULL;

      if (level == 0)
         setup_cu_bases(cc, c_die, abbv);
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_low_pc && cts.szB > 0) {
            ip_lo   = cts.u.val;
            have_lo = True;
         }
         if (attr == DW_AT_high_pc && cts.szB > 0) {
            ip_hi1   = cts.u.val;
            have_hi1 = True;
            if (form != DW_FORM_addr)
               hiIsRelative = True;
         }
         if (attr == DW_AT_ranges && cts.szB > 0) {
            rangeoff   = cts.u.val;
            have_range = True;
         }
         if (attr == DW_AT_comp_dir) {
            if (cts.szB >= 0)
               cc->barf("parse_var_DIE compdir: expecting indirect string");
            HChar *str = ML_(cur_read_strdup)( cts.u.cur,
                                               "parse_var_DIE.compdir" );
            compdir = ML_(addStr)(cc->di, str, -1);
            ML_(dinfo_free) (str);
         }
         if (attr == DW_AT_stmt_list && cts.szB > 0) {
            if (cts.u.val != *debug_line_offset) {
               reset_fndn_ix_table( fndn_ix_Table, debug_line_offset,
                                    cts.u.val );
               read_filename_table( *fndn_ix_Table, compdir,
                                    cc, cts.u.val, td3 );
            }
         }
      }
      if (have_lo && have_hi1 && hiIsRelative)
         ip_hi1 += ip_lo;

      /* Now, does this give us an opportunity to find this
         CU's svma? */
      if (level == 0)
         setup_cu_svma(cc, have_lo, ip_lo, td3);

      /* Do we have something that looks sane? */
      if (have_lo && have_hi1 && (!have_range)) {
         if (ip_lo < ip_hi1)
            varstack_push( cc, parser, td3, 
                           unitary_range_list(ip_lo, ip_hi1 - 1),
                           level,
                           False/*isFunc*/, NULL/*fbGX*/ );
         else if (ip_lo == 0 && ip_hi1 == 0)
            /* CU has no code, presumably?
               Such situations have been encountered for code
               compiled with -ffunction-sections -fdata-sections
               and linked with --gc-sections. Completely
               eliminated CU gives such 0 lo/hi pc. Similarly
               to a CU which has no lo/hi/range pc, we push
               an empty range list. */
            varstack_push( cc, parser, td3,
                           empty_range_list(),
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
      if (have_lo && (!have_hi1) && have_range && ip_lo == 0) {
         /* broken DIE created by gcc-4.3.X ?  Ignore the
            apparently-redundant DW_AT_low_pc and use the DW_AT_ranges
            instead. */
         varstack_push( cc, parser, td3, 
                        get_range_list( cc, td3,
                                        rangeoff, cc->cu_svma ),
                        level,
                        False/*isFunc*/, NULL/*fbGX*/ );
      } else {
         if (0) VG_(printf)("I got hlo %d hhi1 %d hrange %d\n",
                            (Int)have_lo, (Int)have_hi1, (Int)have_range);
         goto_bad_DIE;
      }
   }

   if (dtag == DW_TAG_lexical_block || dtag == DW_TAG_subprogram) {
      Bool   have_lo    = False;
      Bool   have_hi1   = False;
      Bool   have_range = False;
      Bool   hiIsRelative = False;
      Addr   ip_lo      = 0;
      Addr   ip_hi1     = 0;
      Addr   rangeoff   = 0;
      Bool   isFunc     = dtag == DW_TAG_subprogram;
      GExpr* fbGX       = NULL;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_low_pc && cts.szB > 0) {
            ip_lo   = cts.u.val;
            have_lo = True;
         }
         if (attr == DW_AT_high_pc && cts.szB > 0) {
            ip_hi1   = cts.u.val;
            have_hi1 = True;
            if (form != DW_FORM_addr)
               hiIsRelative = True;
         }
         if (attr == DW_AT_ranges && cts.szB > 0) {
            rangeoff   = cts.u.val;
            have_range = True;
         }
         if (isFunc
             && attr == DW_AT_frame_base
             && cts.szB != 0 /* either scalar or nonempty block */) {
            fbGX = get_GX( cc, False/*td3*/, &cts );
            vg_assert(fbGX);
            VG_(addToXA)(gexprs, &fbGX);
         }
      }
      if (have_lo && have_hi1 && hiIsRelative)
         ip_hi1 += ip_lo;
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
         goto_bad_DIE;
   }

   if (dtag == DW_TAG_variable || dtag == DW_TAG_formal_parameter) {
      const  HChar* name = NULL;
      UWord  typeR       = D3_INVALID_CUOFF;
      Bool   global      = False;
      GExpr* gexpr       = NULL;
      Int    n_attrs     = 0;
      UWord  abs_ori     = (UWord)D3_INVALID_CUOFF;
      Int    lineNo      = 0;
      UInt   fndn_ix     = 0;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         n_attrs++;
         if (attr == DW_AT_name && cts.szB < 0) {
            name = ML_(addStrFromCursor)( cc->di, cts.u.cur );
         }
         if (attr == DW_AT_location
             && cts.szB != 0 /* either scalar or nonempty block */) {
            gexpr = get_GX( cc, False/*td3*/, &cts );
            vg_assert(gexpr);
            VG_(addToXA)(gexprs, &gexpr);
         }
         if (attr == DW_AT_type && cts.szB > 0) {
            typeR = cook_die_using_form( cc, cts.u.val, form );
         }
         if (attr == DW_AT_external && cts.szB > 0 && cts.u.val > 0) {
            global = True;
         }
         if (attr == DW_AT_abstract_origin && cts.szB > 0) {
            abs_ori = (UWord)cts.u.val;
         }
         if (attr == DW_AT_declaration && cts.szB > 0 && cts.u.val > 0) {
            /*declaration = True;*/
         }
         if (attr == DW_AT_decl_line && cts.szB > 0) {
            lineNo = (Int)cts.u.val;
         }
         if (attr == DW_AT_decl_file && cts.szB > 0) {
            Int ftabIx = (Int)cts.u.val;
            if (ftabIx >= 1
                && ftabIx < VG_(sizeXA)( *fndn_ix_Table )) {
               fndn_ix = *(UInt*)VG_(indexXA)( *fndn_ix_Table, ftabIx );
            }
            if (0) VG_(printf)("XXX filename fndn_ix = %u %s\n", fndn_ix,
                               ML_(fndn_ix2filename) (cc->di, fndn_ix));
         }
      }
      if (!global && dtag == DW_TAG_variable && level == 1) {
         /* Case of a static variable. It is better to declare
            it global as the variable is not really related to
            a PC range, as its address can be used by program
            counters outside of the ranges where it is visible . */
         global = True;
      }

      /* We'll collect it under if one of the following three
         conditions holds:
         (1) has location and type    -> completed
         (2) has type only            -> is an abstract instance
         (3) has location and abs_ori -> is a concrete instance
         Name, fndn_ix and line number are all optional frills.
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
         const XArray*  /* of AddrRange */ xa;
         TempVar* tv;
         /* Stack can't be empty; we put a dummy entry on it for the
            entire address range before starting with the DIEs for
            this CU. */
         vg_assert(parser->sp >= 0);

         /* If this is a local variable (non-global), try to find
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
         if (!global) {
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
                     "warning: parse_var_DIE: non-global variable "
                     "outside DW_TAG_subprogram\n");
               }
               /* goto_bad_DIE; */
               /* This seems to happen a lot.  Just ignore it -- if,
                  when we come to evaluation of the location (guarded)
                  expression, it requires a frame base value, and
                  there's no expression for that, then evaluation as a
                  whole will fail.  Harmless - a bit of a waste of
                  cycles but nothing more. */
            }
         }

         /* re "global ? 0 : parser->sp" (twice), if the var is
            marked 'global' then we must put it at the global scope,
            as only the global scope (level 0) covers the entire PC
            address space.  It is asserted elsewhere that level 0 
            always covers the entire address space. */
         xa = parser->ranges[global ? 0 : parser->sp];
         nRanges = VG_(sizeXA)(xa);
         vg_assert(nRanges >= 0);

         tv = ML_(dinfo_zalloc)( "di.readdwarf3.pvD.1", sizeof(TempVar) );
         tv->name   = name;
         tv->level  = global ? 0 : parser->sp;
         tv->typeR  = typeR;
         tv->gexpr  = gexpr;
         tv->fbGX   = fbGX;
         tv->fndn_ix= fndn_ix;
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
            /* See if we already have a range list which is
               structurally identical.  If so, use that; if not, clone
               this one, and add it to our collection. */
            UWord keyW, valW;
            if (VG_(lookupFM)( rangestree, &keyW, &valW, (UWord)xa )) {
               XArray* old = (XArray*)keyW;
               vg_assert(valW == 0);
               vg_assert(old != xa);
               tv->rngMany = old;
            } else {
               XArray* cloned = VG_(cloneXA)( "di.readdwarf3.pvD.2", xa );
               tv->rngMany = cloned;
               VG_(addToFM)( rangestree, (UWord)cloned, 0 );
            }
         }

         VG_(addToXA)( tempvars, &tv );

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
   dump_bad_die_and_barf("parse_var_DIE", dtag, posn, level,
                         c_die, saved_die_c_offset,
                         abbv,
                         cc);
   /*NOTREACHED*/
}

typedef
   struct {
      UWord sibling; // sibling of the last read DIE (if it has a sibling).
   }
   D3InlParser;

/* Return the function name corresponding to absori.

   absori is a 'cooked' reference to a DIE, i.e. absori can be either
   in cc->escn_debug_info or in cc->escn_debug_info_alt.
   get_inlFnName will uncook absori. 

   The returned value is a (permanent) string in DebugInfo's .strchunks.

   LIMITATION: absori must point in the CU of cc. If absori points
   in another CU, returns "UnknownInlinedFun".

   Here are the problems to retrieve the fun name if absori is in
   another CU:  the DIE reading code cannot properly extract data from
   another CU, as the abbv code retrieved in the other CU cannot be
   translated in an abbreviation. Reading data from the alternate debug
   info also gives problems as the string reference is also in the alternate
   file, but when reading the alt DIE, the string form is a 'local' string,
   but cannot be read in the current CU, but must be read in the alt CU.
   See bug 338803 comment#3 and attachment for a failed attempt to handle
   these problems (failed because with the patch, only one alt abbrev hash
   table is kept, while we must handle all abbreviations in all CUs
   referenced by an absori (being a reference to an alt CU, or a previous
   or following CU). */
static const HChar* get_inlFnName (Int absori, CUConst* cc, Bool td3)
{
   Cursor c;
   const g_abbv *abbv;
   ULong  atag, abbv_code;
   UInt   has_children;
   UWord  posn;
   Bool type_flag, alt_flag;
   const HChar *ret = NULL;
   FormContents cts;
   UInt nf_i;

   /* Some inlined subroutine call dwarf entries do not have the abstract
      origin attribute, resulting in absori being 0 (see callers of
      get_inlFnName). This is observed at least with gcc 6.3.0 when compiling
      valgrind with lto. So, in case we have a 0 absori, do not report an
      error, instead, rather return an unknown inlined function. */
   if (absori == 0) {
      static Bool absori0_reported = False;
      if (!absori0_reported && VG_(clo_verbosity) > 1) {
         VG_(message)(Vg_DebugMsg,
                      "Warning: inlined fn name without absori\n"
                      "is shown as UnknownInlinedFun\n");
         absori0_reported = True;
      }
      TRACE_D3(" <get_inlFnName>: absori is not set");
      return ML_(addStr)(cc->di, "UnknownInlinedFun", -1);
   }

   posn = uncook_die( cc, absori, &type_flag, &alt_flag);
   if (type_flag)
      cc->barf("get_inlFnName: uncooked absori in type debug info");

   /* LIMITATION: check we are in the same CU.
      If not, return unknown inlined function name. */
   /* if crossing between alt debug info<>normal info
          or posn not in the cu range,
      then it is in another CU. */
   if (alt_flag != cc->is_alt_info
       || posn < cc->cu_start_offset
       || posn >= cc->cu_start_offset + cc->unit_length) {
      static Bool reported = False;
      if (!reported && VG_(clo_verbosity) > 1) {
         VG_(message)(Vg_DebugMsg,
                      "Warning: cross-CU LIMITATION: some inlined fn names\n"
                      "might be shown as UnknownInlinedFun\n");
         reported = True;
      }
      TRACE_D3(" <get_inlFnName><%lx>: cross-CU LIMITATION", posn);
      return ML_(addStr)(cc->di, "UnknownInlinedFun", -1);
   }

   init_Cursor (&c, cc->escn_debug_info, posn, cc->barf, 
                "Overrun get_inlFnName absori");

   abbv_code = get_ULEB128( &c );
   abbv      = get_abbv ( cc, abbv_code, td3);
   atag      = abbv->atag;
   TRACE_D3(" <get_inlFnName><%lx>: Abbrev Number: %llu (%s)\n",
            posn, abbv_code, ML_(pp_DW_TAG)( atag ) );

   if (atag == 0)
      cc->barf("get_inlFnName: invalid zero tag on DIE");

   has_children = abbv->has_children;
   if (has_children != DW_children_no && has_children != DW_children_yes)
      cc->barf("get_inlFnName: invalid has_children value");

   if (atag != DW_TAG_subprogram)
      cc->barf("get_inlFnName: absori not a subprogram");

   nf_i = 0;
   while (True) {
      DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
      DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
      const name_form *nf = &abbv->nf[nf_i];
      nf_i++;
      if (attr == 0 && form == 0) break;
      get_Form_contents( &cts, cc, &c, False/*td3*/, nf );
      if (attr == DW_AT_name) {
         HChar *fnname;
         if (cts.szB >= 0)
            cc->barf("get_inlFnName: expecting indirect string");
         fnname = ML_(cur_read_strdup)( cts.u.cur,
                                        "get_inlFnName.1" );
         ret = ML_(addStr)(cc->di, fnname, -1);
         ML_(dinfo_free) (fnname);
         break; /* Name found, get out of the loop, as this has priority over
                 DW_AT_specification. */
      }
      if (attr == DW_AT_specification) {
         UWord cdie;

         if (cts.szB == 0)
            cc->barf("get_inlFnName: AT specification missing");

         /* The recursive call to get_inlFnName will uncook its arg.
            So, we need to cook it here, so as to reference the
            correct section (e.g. the alt info). */
         cdie = cook_die_using_form(cc, (UWord)cts.u.val, form);

         /* hoping that there is no loop */
         ret = get_inlFnName (cdie, cc, td3);
         /* Unclear if having both DW_AT_specification and DW_AT_name is
            possible but in any case, we do not break here. 
            If we find later on a DW_AT_name, it will override the name found
            in the DW_AT_specification.*/
      }
   }

   if (ret)
      return ret;
   else {
      TRACE_D3("AbsOriFnNameNotFound");
      return ML_(addStr)(cc->di, "AbsOriFnNameNotFound", -1);
   }
}

/* Returns True if the (possibly) childrens of the current DIE are interesting
   to parse. Returns False otherwise.
   If the current DIE has a sibling, the non interesting children can
   maybe be skipped (if the DIE has a DW_AT_sibling).  */
__attribute__((noinline))
static Bool parse_inl_DIE (
   /*MOD*/D3InlParser* parser,
   XArray** fndn_ix_Table,
   ULong *debug_line_offset,
   DW_TAG dtag,
   UWord posn,
   Int level,
   Cursor* c_die,
   const g_abbv *abbv,
   CUConst* cc,
   Bool td3
)
{
   FormContents cts;
   UInt nf_i;

   UWord saved_die_c_offset  = get_position_of_Cursor( c_die );

   /* Get info about DW_TAG_compile_unit and DW_TAG_partial_unit which in theory
      could also contain inlined fn calls, if they cover an address range.  */
   Bool unit_has_addrs = False;
   if (dtag == DW_TAG_compile_unit || dtag == DW_TAG_partial_unit
       || dtag == DW_TAG_skeleton_unit) {
      Bool have_lo    = False;
      Addr ip_lo    = 0;
      const HChar *compdir = NULL;
      Bool has_stmt_list = False;
      ULong cu_line_offset = 0;

      if (level == 0)
         setup_cu_bases(cc, c_die, abbv);
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_low_pc && cts.szB > 0) {
            ip_lo   = cts.u.val;
            have_lo = True;
            unit_has_addrs = True;
         }
         if (attr == DW_AT_ranges && cts.szB > 0)
            unit_has_addrs = True;
         if (attr == DW_AT_comp_dir) {
            if (cts.szB >= 0)
               cc->barf("parse_inl_DIE compdir: expecting indirect string");
            HChar *str = ML_(cur_read_strdup)( cts.u.cur,
                                               "parse_inl_DIE.compdir" );
            compdir = ML_(addStr)(cc->di, str, -1);
            ML_(dinfo_free) (str);
         }
         if (attr == DW_AT_stmt_list && cts.szB > 0) {
	    has_stmt_list = True;
            cu_line_offset = cts.u.val;
         }
         if (attr == DW_AT_sibling && cts.szB > 0) {
            parser->sibling = cts.u.val;
         }
      }
      if (level == 0) {
         setup_cu_svma (cc, have_lo, ip_lo, td3);
         if (has_stmt_list && unit_has_addrs
            && *debug_line_offset != cu_line_offset) {
            reset_fndn_ix_table ( fndn_ix_Table, debug_line_offset,
                                  cu_line_offset );
            read_filename_table( *fndn_ix_Table, compdir,
                                 cc, cu_line_offset, td3 );
         }
      }
   }

   if (dtag == DW_TAG_inlined_subroutine) {
      Bool   have_lo    = False;
      Bool   have_hi1   = False;
      Bool   have_range = False;
      Bool   hiIsRelative = False;
      Addr   ip_lo      = 0;
      Addr   ip_hi1     = 0;
      Addr   rangeoff   = 0;
      UInt   caller_fndn_ix = 0;
      Int caller_lineno = 0;
      Int inlinedfn_abstract_origin = 0;
      // 0 will be interpreted as no abstract origin by get_inlFnName

      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_call_file && cts.szB > 0) {
            Int ftabIx = (Int)cts.u.val;
            if (ftabIx >= 1
                && ftabIx < VG_(sizeXA)( *fndn_ix_Table )) {
               caller_fndn_ix = *(UInt*)
                          VG_(indexXA)( *fndn_ix_Table, ftabIx );
            }
            if (0) VG_(printf)("XXX caller_fndn_ix = %u %s\n", caller_fndn_ix,
                               ML_(fndn_ix2filename) (cc->di, caller_fndn_ix));
         }  
         if (attr == DW_AT_call_line && cts.szB > 0) {
            caller_lineno = cts.u.val;
         }  

         if (attr == DW_AT_abstract_origin  && cts.szB > 0) {
            inlinedfn_abstract_origin
               = cook_die_using_form (cc, (UWord)cts.u.val, form);
         }

         if (attr == DW_AT_low_pc && cts.szB > 0) {
            ip_lo   = cts.u.val;
            have_lo = True;
         }
         if (attr == DW_AT_high_pc && cts.szB > 0) {
            ip_hi1   = cts.u.val;
            have_hi1 = True;
            if (form != DW_FORM_addr)
               hiIsRelative = True;
         }
         if (attr == DW_AT_ranges && cts.szB > 0) {
            rangeoff   = cts.u.val;
            have_range = True;
         }
         if (attr == DW_AT_sibling && cts.szB > 0) {
            parser->sibling = cts.u.val;
         }
      }
      if (have_lo && have_hi1 && hiIsRelative)
         ip_hi1 += ip_lo;
      /* Do we have something that looks sane? */
      if (dtag == DW_TAG_inlined_subroutine
          && (!have_lo) && (!have_hi1) && (!have_range)) {
         /* Seems strange. How can an inlined subroutine have
            no code ? */
         goto_bad_DIE;
      } else
      if (have_lo && have_hi1 && (!have_range)) {
         /* This inlined call is just a single address range. */
         if (ip_lo < ip_hi1) {
            /* Apply text debug biasing */
            ip_lo += cc->di->text_debug_bias;
            ip_hi1 += cc->di->text_debug_bias;
            ML_(addInlInfo) (cc->di,
                             ip_lo, ip_hi1, 
                             get_inlFnName (inlinedfn_abstract_origin, cc, td3),
                             caller_fndn_ix,
                             caller_lineno, level);
         }
      } else if (have_range) {
         /* This inlined call is several address ranges. */
         XArray *ranges;
         Word j;
         const HChar *inlfnname =
            get_inlFnName (inlinedfn_abstract_origin, cc, td3);

         /* Ranges are biased for the inline info using the same logic
            as what is used for biasing ranges for the var info, for which
            ranges are read using cc->cu_svma (see parse_var_DIE).
            Then text_debug_bias is added when a (non global) var
            is recorded (see just before the call to ML_(addVar)) */
         ranges = get_range_list( cc, td3,
                                  rangeoff, cc->cu_svma );
         for (j = 0; j < VG_(sizeXA)( ranges ); j++) {
            AddrRange* range = (AddrRange*) VG_(indexXA)( ranges, j );
            ML_(addInlInfo) (cc->di,
                             range->aMin   + cc->di->text_debug_bias,
                             range->aMax+1 + cc->di->text_debug_bias,
                             // aMax+1 as range has its last bound included
                             // while ML_(addInlInfo) expects last bound not
                             // included.
                             inlfnname,
                             caller_fndn_ix,
                             caller_lineno, level);
         }
         VG_(deleteXA)( ranges );
      } else
         goto_bad_DIE;
   }

   // Only recursively parse the (possible) children for the DIE which
   // might maybe contain a DW_TAG_inlined_subroutine:
   Bool ret = (unit_has_addrs
               || dtag == DW_TAG_lexical_block || dtag == DW_TAG_subprogram
               || dtag == DW_TAG_inlined_subroutine || dtag == DW_TAG_namespace);
   return ret;

  bad_DIE:
   dump_bad_die_and_barf("parse_inl_DIE", dtag, posn, level,
                         c_die, saved_die_c_offset,
                         abbv,
                         cc);
   /*NOTREACHED*/
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Parsing of type-related DIEs                         ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

typedef
   struct {
      /* What source language?  'A'=Ada83/95,
                                'C'=C/C++, 
                                'F'=Fortran,
                                '?'=other
         Established once per compilation unit. */
      UChar language;
      /* A stack of types which are currently under construction */
      Int   sp; /* [sp] is innermost active entry; sp==-1 for empty
                   stack */
      Int   stack_size;
      /* Note that the TyEnts in qparentE are temporary copies of the
         ones accumulating in the main tyent array.  So it is not safe
         to free up anything on them when popping them off the stack
         (iow, it isn't safe to use TyEnt__make_EMPTY on them).  Just
         memset them to zero when done. */
      TyEnt *qparentE; /* parent TyEnts */
      Int   *qlevel;
   }
   D3TypeParser;

/* Completely initialise a type parser object */
static void
type_parser_init ( D3TypeParser *parser )
{
   parser->sp = -1;
   parser->language = '?';
   parser->stack_size = 0;
   parser->qparentE = NULL;
   parser->qlevel   = NULL;
}

/* Release any memory hanging off a type parser object */
static void
type_parser_release ( D3TypeParser *parser )
{
   ML_(dinfo_free)( parser->qparentE );
   ML_(dinfo_free)( parser->qlevel );
}

static void typestack_show ( const D3TypeParser* parser, const HChar* str )
{
   Word i;
   VG_(printf)("  typestack (%s) {\n", str);
   for (i = 0; i <= parser->sp; i++) {
      VG_(printf)("    [%ld] (level %d): ", i, parser->qlevel[i]);
      ML_(pp_TyEnt)( &parser->qparentE[i] );
      VG_(printf)("\n");
   }
   VG_(printf)("  }\n");
}

/* Remove from the stack, all entries with .level > 'level' */
static 
void typestack_preen ( D3TypeParser* parser, Bool td3, Int level )
{
   Bool changed = False;
   vg_assert(parser->sp < parser->stack_size);
   while (True) {
      vg_assert(parser->sp >= -1);
      if (parser->sp == -1) break;
      if (parser->qlevel[parser->sp] <= level) break;
      if (0) 
         TRACE_D3("BBBBAAAA typestack_pop [newsp=%d]\n", parser->sp-1);
      vg_assert(ML_(TyEnt__is_type)(&parser->qparentE[parser->sp]));
      parser->sp--;
      changed = True;
   }
   if (changed && td3)
      typestack_show( parser, "after preen" );
}

static Bool typestack_is_empty ( const D3TypeParser* parser )
{
   vg_assert(parser->sp >= -1 && parser->sp < parser->stack_size);
   return parser->sp == -1;
}

static void typestack_push ( const CUConst* cc,
                             D3TypeParser* parser,
                             Bool td3,
                             const TyEnt* parentE, Int level )
{
   if (0)
   TRACE_D3("BBBBAAAA typestack_push[newsp=%d]: %d  %05lx\n",
            parser->sp+1, level, parentE->cuOff);

   /* First we need to zap everything >= 'level', as we are about to
      replace any previous entry at 'level', so .. */
   typestack_preen(parser, /*td3*/False, level-1);

   vg_assert(parser->sp >= -1);
   vg_assert(parser->sp < parser->stack_size);
   if (parser->sp == parser->stack_size - 1) {
      parser->stack_size += 16;
      parser->qparentE =
         ML_(dinfo_realloc)("di.readdwarf3.typush.1", parser->qparentE,
                            parser->stack_size * sizeof parser->qparentE[0]);
      parser->qlevel =
         ML_(dinfo_realloc)("di.readdwarf3.typush.2", parser->qlevel,
                            parser->stack_size * sizeof parser->qlevel[0]);
   }
   if (parser->sp >= 0)
      vg_assert(parser->qlevel[parser->sp] < level);
   parser->sp++;
   vg_assert(parentE);
   vg_assert(ML_(TyEnt__is_type)(parentE));
   vg_assert(parentE->cuOff != D3_INVALID_CUOFF);
   parser->qparentE[parser->sp] = *parentE;
   parser->qlevel[parser->sp]  = level;
   if (TD3)
      typestack_show( parser, "after push" );
}

/* True if the subrange type being parsed gives the bounds of an array. */
static Bool subrange_type_denotes_array_bounds ( const D3TypeParser* parser,
                                                 DW_TAG dtag ) {
   vg_assert(dtag == DW_TAG_subrange_type);
   /* If we don't know the language, assume false.  */
   if (parser->language == '?')
      return False;
   /* For most languages, a subrange_type dtag always gives the 
      bounds of an array.
      For Ada, there are additional conditions as a subrange_type
      is also used for other purposes. */
   if (parser->language != 'A')
      /* not Ada, so it definitely denotes an array bound. */
      return True;
   else
      /* Extra constraints for Ada: it only denotes an array bound if .. */
      return (! typestack_is_empty(parser)
              && parser->qparentE[parser->sp].tag == Te_TyArray);
}

/* True if the form is one of the forms supported to give an array bound.
   For some arrays (scope local arrays with variable size), 
   a DW_FORM_ref4 was used, and was wrongly used as the bound value.
   So, refuse the forms that are known to give a problem. */
static Bool form_expected_for_bound ( DW_FORM form ) {
   if (form == DW_FORM_ref1 
       || form == DW_FORM_ref2
       || form == DW_FORM_ref4
       || form == DW_FORM_ref8)
      return False;

   return True;
}

/* Parse a type-related DIE.  'parser' holds the current parser state.
   'admin' is where the completed types are dumped.  'dtag' is the tag
   for this DIE.  'c_die' points to the start of the data fields (FORM
   stuff) for the DIE.  abbv is the parsed abbreviation which describe
   the DIE.

   We may find the DIE uninteresting, in which case we should ignore
   it.

   What happens: the DIE is examined.  If uninteresting, it is ignored.
   Otherwise, the DIE gives rise to two things:

   (1) the offset of this DIE in the CU -- the cuOffset, a UWord
   (2) a TyAdmin structure, which holds the type, or related stuff

   (2) is added at the end of 'tyadmins', at some index, say 'i'.

   A pair (cuOffset, i) is added to 'tydict'.

   Hence 'tyadmins' holds the actual type entities, and 'tydict' holds
   a mapping from cuOffset to the index of the corresponding entry in
   'tyadmin'.

   When resolving a cuOffset to a TyAdmin, first look up the cuOffset
   in the tydict (by binary search).  This gives an index into
   tyadmins, and the required entity lives in tyadmins at that index.
*/
__attribute__((noinline))
static void parse_type_DIE ( /*MOD*/XArray* /* of TyEnt */ tyents,
                             /*MOD*/D3TypeParser* parser,
                             DW_TAG dtag,
                             UWord posn,
                             Int level,
                             Cursor* c_die,
                             const g_abbv *abbv,
                             CUConst* cc,
                             Bool td3 )
{
   FormContents cts;
   UInt nf_i;
   TyEnt typeE;
   TyEnt atomE;
   TyEnt fieldE;
   TyEnt boundE;

   UWord saved_die_c_offset  = get_position_of_Cursor( c_die );

   VG_(memset)( &typeE,  0xAA, sizeof(typeE) );
   VG_(memset)( &atomE,  0xAA, sizeof(atomE) );
   VG_(memset)( &fieldE, 0xAA, sizeof(fieldE) );
   VG_(memset)( &boundE, 0xAA, sizeof(boundE) );

   /* If we've returned to a level at or above any previously noted
      parent, un-note it, so we don't believe we're still collecting
      its children. */
   typestack_preen( parser, td3, level-1 );

   if (dtag == DW_TAG_compile_unit
       || dtag == DW_TAG_type_unit
       || dtag == DW_TAG_partial_unit
       || dtag == DW_TAG_skeleton_unit) {
      if (level == 0)
         setup_cu_bases(cc, c_die, abbv);
      /* See if we can find DW_AT_language, since it is important for
         establishing array bounds (see DW_TAG_subrange_type below in
         this fn) */
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr != DW_AT_language)
            continue;
         if (cts.szB <= 0)
           goto_bad_DIE;
         switch (cts.u.val) {
            case DW_LANG_C89: case DW_LANG_C:
            case DW_LANG_C_plus_plus: case DW_LANG_ObjC:
            case DW_LANG_ObjC_plus_plus: case DW_LANG_UPC:
            case DW_LANG_Upc: case DW_LANG_C99: case DW_LANG_C11:
            case DW_LANG_C_plus_plus_11: case DW_LANG_C_plus_plus_14:
               parser->language = 'C'; break;
            case DW_LANG_Fortran77: case DW_LANG_Fortran90:
            case DW_LANG_Fortran95: case DW_LANG_Fortran03:
            case DW_LANG_Fortran08:
               parser->language = 'F'; break;
            case DW_LANG_Ada83: case DW_LANG_Ada95: 
               parser->language = 'A'; break;
            case DW_LANG_Cobol74:
            case DW_LANG_Cobol85: case DW_LANG_Pascal83:
            case DW_LANG_Modula2: case DW_LANG_Java:
            case DW_LANG_PLI:
            case DW_LANG_D: case DW_LANG_Python: case DW_LANG_Go:
            case DW_LANG_Mips_Assembler:
               parser->language = '?'; break;
            default:
               goto_bad_DIE;
         }
      }
   }

   if (dtag == DW_TAG_base_type) {
      /* We can pick up a new base type any time. */
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff = D3_INVALID_CUOFF;
      typeE.tag   = Te_TyBase;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_name && cts.szB < 0) {
            typeE.Te.TyBase.name
               = ML_(cur_read_strdup)( cts.u.cur,
                                       "di.readdwarf3.ptD.base_type.1" );
         }
         if (attr == DW_AT_byte_size && cts.szB > 0) {
            typeE.Te.TyBase.szB = cts.u.val;
         }
         if (attr == DW_AT_encoding && cts.szB > 0) {
            switch (cts.u.val) {
               case DW_ATE_unsigned: case DW_ATE_unsigned_char:
               case DW_ATE_UTF: /* since DWARF4, e.g. char16_t from C++ */
               case DW_ATE_boolean:/* FIXME - is this correct? */
               case DW_ATE_unsigned_fixed:
                  typeE.Te.TyBase.enc = 'U'; break;
               case DW_ATE_signed: case DW_ATE_signed_char:
               case DW_ATE_signed_fixed:
                  typeE.Te.TyBase.enc = 'S'; break;
               case DW_ATE_float:
                  typeE.Te.TyBase.enc = 'F'; break;
               case DW_ATE_complex_float:
                  typeE.Te.TyBase.enc = 'C'; break;
               default:
                  goto_bad_DIE;
            }
         }
      }

      /* Invent a name if it doesn't have one.  gcc-4.3
         -ftree-vectorize is observed to emit nameless base types. */
      if (!typeE.Te.TyBase.name)
         typeE.Te.TyBase.name 
            = ML_(dinfo_strdup)( "di.readdwarf3.ptD.base_type.2",
                                 "<anon_base_type>" );

      /* Do we have something that looks sane? */
      if (/* must have a name */
          typeE.Te.TyBase.name == NULL
          /* and a plausible size.  Yes, really 32: "complex long
             double" apparently has size=32 */
          || typeE.Te.TyBase.szB < 0 || typeE.Te.TyBase.szB > 32
          /* and a plausible encoding */
          || (typeE.Te.TyBase.enc != 'U'
              && typeE.Te.TyBase.enc != 'S' 
              && typeE.Te.TyBase.enc != 'F'
              && typeE.Te.TyBase.enc != 'C'))
         goto_bad_DIE;
      /* Last minute hack: if we see this
         <1><515>: DW_TAG_base_type
             DW_AT_byte_size   : 0
             DW_AT_encoding    : 5
             DW_AT_name        : void
         convert it into a real Void type. */
      if (typeE.Te.TyBase.szB == 0
          && 0 == VG_(strcmp)("void", typeE.Te.TyBase.name)) {
         ML_(TyEnt__make_EMPTY)(&typeE);
         typeE.tag = Te_TyVoid;
         typeE.Te.TyVoid.isFake = False; /* it's a real one! */
      }

      goto acquire_Type;
   }

   /*
    * An example of DW_TAG_rvalue_reference_type:
    *
    * $ readelf --debug-dump /usr/lib/debug/usr/lib/libstdc++.so.6.0.16.debug
    *  <1><1014>: Abbrev Number: 55 (DW_TAG_rvalue_reference_type)
    *     <1015>   DW_AT_byte_size   : 4
    *     <1016>   DW_AT_type        : <0xe52>
    */
   if (dtag == DW_TAG_pointer_type || dtag == DW_TAG_reference_type
       || dtag == DW_TAG_ptr_to_member_type
       || dtag == DW_TAG_rvalue_reference_type) {
      /* This seems legit for _pointer_type and _reference_type.  I
         don't know if rolling _ptr_to_member_type in here really is
         legit, but it's better than not handling it at all. */
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff = D3_INVALID_CUOFF;
      switch (dtag) {
      case DW_TAG_pointer_type:
         typeE.tag = Te_TyPtr;
         break;
      case DW_TAG_reference_type:
         typeE.tag = Te_TyRef;
         break;
      case DW_TAG_ptr_to_member_type:
         typeE.tag = Te_TyPtrMbr;
         break;
      case DW_TAG_rvalue_reference_type:
         typeE.tag = Te_TyRvalRef;
         break;
      default:
         vg_assert(False);
      }
      /* target type defaults to void */
      typeE.Te.TyPorR.typeR = D3_FAKEVOID_CUOFF;
      /* These four type kinds don't *have* to specify their size, in
         which case we assume it's a machine word.  But if they do
         specify it, it must be a machine word :-)  This probably
         assumes that the word size of the Dwarf3 we're reading is the
         same size as that on the machine.  gcc appears to give a size
         whereas icc9 doesn't. */
      typeE.Te.TyPorR.szB = sizeof(UWord);
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_byte_size && cts.szB > 0) {
            typeE.Te.TyPorR.szB = cts.u.val;
         }
         if (attr == DW_AT_type && cts.szB > 0) {
            typeE.Te.TyPorR.typeR
               = cook_die_using_form( cc, (UWord)cts.u.val, form );
         }
      }
      /* Do we have something that looks sane? */
      if (typeE.Te.TyPorR.szB != sizeof(UWord))
         goto_bad_DIE;
      else
         goto acquire_Type;
   }

   if (dtag == DW_TAG_enumeration_type) {
      /* Create a new Type to hold the results. */
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff = posn;
      typeE.tag   = Te_TyEnum;
      Bool is_decl = False;
      typeE.Te.TyEnum.atomRs
         = VG_(newXA)( ML_(dinfo_zalloc), "di.readdwarf3.ptD.enum_type.1", 
                       ML_(dinfo_free),
                       sizeof(UWord) );
      nf_i=0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_name && cts.szB < 0) {
            typeE.Te.TyEnum.name
               = ML_(cur_read_strdup)( cts.u.cur,
                                       "di.readdwarf3.pTD.enum_type.2" );
         }
         if (attr == DW_AT_byte_size && cts.szB > 0) {
            typeE.Te.TyEnum.szB = cts.u.val;
         }
         if (attr == DW_AT_declaration) {
            is_decl = True;
         }
      }

      if (!typeE.Te.TyEnum.name)
         typeE.Te.TyEnum.name 
            = ML_(dinfo_strdup)( "di.readdwarf3.pTD.enum_type.3",
                                 "<anon_enum_type>" );

      /* Do we have something that looks sane? */
      if (typeE.Te.TyEnum.szB == 0 
          /* we must know the size */
          /* but not for Ada, which uses such dummy
             enumerations as helper for gdb ada mode.
             Also GCC allows incomplete enums as GNU extension.
             http://gcc.gnu.org/onlinedocs/gcc/Incomplete-Enums.html
             These are marked as DW_AT_declaration and won't have
             a size. They can only be used in declaration or as
             pointer types.  You can't allocate variables or storage
             using such an enum type. (Also GCC seems to have a bug
             that will put such an enumeration_type into a .debug_types
             unit which should only contain complete types.) */
          && (parser->language != 'A' && !is_decl)) {
         goto_bad_DIE;
      }

      /* On't stack! */
      typestack_push( cc, parser, td3, &typeE, level );
      goto acquire_Type;
   }

   /* gcc (GCC) 4.4.0 20081017 (experimental) occasionally produces
      DW_TAG_enumerator with only a DW_AT_name but no
      DW_AT_const_value.  This is in violation of the Dwarf3 standard,
      and appears to be a new "feature" of gcc - versions 4.3.x and
      earlier do not appear to do this.  So accept DW_TAG_enumerator
      which only have a name but no value.  An example:

      <1><180>: Abbrev Number: 6 (DW_TAG_enumeration_type)
         <181>   DW_AT_name        : (indirect string, offset: 0xda70):
                                     QtMsgType
         <185>   DW_AT_byte_size   : 4
         <186>   DW_AT_decl_file   : 14
         <187>   DW_AT_decl_line   : 1480
         <189>   DW_AT_sibling     : <0x1a7>
      <2><18d>: Abbrev Number: 7 (DW_TAG_enumerator)
         <18e>   DW_AT_name        : (indirect string, offset: 0x9e18):
                                     QtDebugMsg
      <2><192>: Abbrev Number: 7 (DW_TAG_enumerator)
         <193>   DW_AT_name        : (indirect string, offset: 0x1505f):
                                     QtWarningMsg
      <2><197>: Abbrev Number: 7 (DW_TAG_enumerator)
         <198>   DW_AT_name        : (indirect string, offset: 0x16f4a):
                                     QtCriticalMsg
      <2><19c>: Abbrev Number: 7 (DW_TAG_enumerator)
         <19d>   DW_AT_name        : (indirect string, offset: 0x156dd):
                                     QtFatalMsg
      <2><1a1>: Abbrev Number: 7 (DW_TAG_enumerator)
         <1a2>   DW_AT_name        : (indirect string, offset: 0x13660):
                                     QtSystemMsg
   */
   if (dtag == DW_TAG_enumerator) {
      VG_(memset)( &atomE, 0, sizeof(atomE) );
      atomE.cuOff = posn;
      atomE.tag   = Te_Atom;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_name && cts.szB < 0) {
            atomE.Te.Atom.name 
              = ML_(cur_read_strdup)( cts.u.cur,
                                      "di.readdwarf3.pTD.enumerator.1" );
         }
         if (attr == DW_AT_const_value && cts.szB > 0) {
            atomE.Te.Atom.value      = cts.u.val;
            atomE.Te.Atom.valueKnown = True;
         }
      }
      /* Do we have something that looks sane? */
      if (atomE.Te.Atom.name == NULL)
         goto_bad_DIE;
      /* Do we have a plausible parent? */
      if (typestack_is_empty(parser)) goto_bad_DIE;
      vg_assert(ML_(TyEnt__is_type)(&parser->qparentE[parser->sp]));
      vg_assert(parser->qparentE[parser->sp].cuOff != D3_INVALID_CUOFF);
      if (level != parser->qlevel[parser->sp]+1) goto_bad_DIE;
      if (parser->qparentE[parser->sp].tag != Te_TyEnum) goto_bad_DIE;
      /* Record this child in the parent */
      vg_assert(parser->qparentE[parser->sp].Te.TyEnum.atomRs);
      VG_(addToXA)( parser->qparentE[parser->sp].Te.TyEnum.atomRs,
                    &atomE );
      /* And record the child itself */
      goto acquire_Atom;
   }

   /* Treat DW_TAG_class_type as if it was a DW_TAG_structure_type.  I
      don't know if this is correct, but it at least makes this reader
      usable for gcc-4.3 produced Dwarf3. */
   if (dtag == DW_TAG_structure_type || dtag == DW_TAG_class_type
       || dtag == DW_TAG_union_type) {
      Bool have_szB = False;
      Bool is_decl  = False;
      Bool is_spec  = False;
      /* Create a new Type to hold the results. */
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff = posn;
      typeE.tag   = Te_TyStOrUn;
      typeE.Te.TyStOrUn.name = NULL;
      typeE.Te.TyStOrUn.typeR = D3_INVALID_CUOFF;
      typeE.Te.TyStOrUn.fieldRs
         = VG_(newXA)( ML_(dinfo_zalloc), "di.readdwarf3.pTD.struct_type.1", 
                       ML_(dinfo_free),
                       sizeof(UWord) );
      typeE.Te.TyStOrUn.complete = True;
      typeE.Te.TyStOrUn.isStruct = dtag == DW_TAG_structure_type 
                                   || dtag == DW_TAG_class_type;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_name && cts.szB < 0) {
            typeE.Te.TyStOrUn.name
               = ML_(cur_read_strdup)( cts.u.cur,
                                       "di.readdwarf3.ptD.struct_type.2" );
         }
         if (attr == DW_AT_byte_size && cts.szB >= 0) {
            typeE.Te.TyStOrUn.szB = cts.u.val;
            have_szB = True;
         }
         if (attr == DW_AT_declaration && cts.szB > 0 && cts.u.val > 0) {
            is_decl = True;
         }
         if (attr == DW_AT_specification && cts.szB > 0 && cts.u.val > 0) {
            is_spec = True;
         }
         if (attr == DW_AT_signature && form == DW_FORM_ref_sig8
             && cts.szB > 0) {
            have_szB = True;
            typeE.Te.TyStOrUn.szB = 8;
            typeE.Te.TyStOrUn.typeR
               = cook_die_using_form( cc, (UWord)cts.u.val, form );
         }
      }
      /* Do we have something that looks sane? */
      if (is_decl && (!is_spec)) {
         /* It's a DW_AT_declaration.  We require the name but
            nothing else. */
         /* JRS 2012-06-28: following discussion w/ tromey, if the
            type doesn't have name, just make one up, and accept it.
            It might be referred to by other DIEs, so ignoring it
            doesn't seem like a safe option. */
         if (typeE.Te.TyStOrUn.name == NULL)
            typeE.Te.TyStOrUn.name
               = ML_(dinfo_strdup)( "di.readdwarf3.ptD.struct_type.3",
                                    "<anon_struct_type>" );
         typeE.Te.TyStOrUn.complete = False;
         /* JRS 2009 Aug 10: <possible kludge>? */
         /* Push this tyent on the stack, even though it's incomplete.
            It appears that gcc-4.4 on Fedora 11 will sometimes create
            DW_TAG_member entries for it, and so we need to have a
            plausible parent present in order for that to work.  See
            #200029 comments 8 and 9. */
         typestack_push( cc, parser, td3, &typeE, level );
         /* </possible kludge> */
         goto acquire_Type;
      }
      if ((!is_decl) /* && (!is_spec) */) {
         /* this is the common, ordinary case */
         /* The name can be present, or not */
         if (!have_szB) { 
            /* We must know the size.
               But in Ada, record with discriminants might have no size.
               But in C, VLA in the middle of a struct (gcc extension)
               might have no size.
               Instead, some GNAT dwarf extensions and/or dwarf entries
               allow to calculate the struct size at runtime.
               We cannot do that (yet?) so, the temporary kludge is to use
               a small size. */
            typeE.Te.TyStOrUn.szB = 1;
         }
         /* On't stack! */
         typestack_push( cc, parser, td3, &typeE, level );
         goto acquire_Type;
      }
      else {
         /* don't know how to handle any other variants just now */
         goto_bad_DIE;
      }
   }

   if (dtag == DW_TAG_member) {
      /* Acquire member entries for both DW_TAG_structure_type and
         DW_TAG_union_type.  They differ minorly, in that struct
         members must have a DW_AT_data_member_location expression
         whereas union members must not. */
      Bool parent_is_struct;
      Bool is_artificial = False;
      VG_(memset)( &fieldE, 0, sizeof(fieldE) );
      fieldE.cuOff = posn;
      fieldE.tag   = Te_Field;
      fieldE.Te.Field.typeR = D3_INVALID_CUOFF;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_name && cts.szB < 0) {
            fieldE.Te.Field.name
               = ML_(cur_read_strdup)( cts.u.cur,
                                       "di.readdwarf3.ptD.member.1" );
         }
         if (attr == DW_AT_type && cts.szB > 0) {
            fieldE.Te.Field.typeR
               = cook_die_using_form( cc, (UWord)cts.u.val, form );
         }
         /* There are 2 different cases for DW_AT_data_member_location.
            If it is a constant class attribute, it contains byte offset
            from the beginning of the containing entity.
            Otherwise it is a location expression.  */
         if (attr == DW_AT_data_member_location && cts.szB > 0) {
            fieldE.Te.Field.nLoc = -1;
            fieldE.Te.Field.pos.offset = cts.u.val;
         }
         if (attr == DW_AT_data_member_location && cts.szB <= 0) {
            fieldE.Te.Field.nLoc = (UWord)(-cts.szB);
            fieldE.Te.Field.pos.loc
               = ML_(cur_read_memdup)( cts.u.cur, 
                                       (SizeT)fieldE.Te.Field.nLoc,
                                       "di.readdwarf3.ptD.member.2" );
         }
	 if (attr == DW_AT_artificial && cts.u.val == 1)
            is_artificial = True;
      }
      /* Skip artificial members, they might not behave as expected.  */
      if (is_artificial)
         goto no_location;
      /* Do we have a plausible parent? */
      if (typestack_is_empty(parser)) goto_bad_DIE;
      vg_assert(ML_(TyEnt__is_type)(&parser->qparentE[parser->sp]));
      vg_assert(parser->qparentE[parser->sp].cuOff != D3_INVALID_CUOFF);
      if (level != parser->qlevel[parser->sp]+1) goto_bad_DIE;
      if (parser->qparentE[parser->sp].tag != Te_TyStOrUn) goto_bad_DIE;
      /* Do we have something that looks sane?  If this a member of a
         struct, we must have a location expression; but if a member
         of a union that is irrelevant (D3 spec sec 5.6.6).  We ought
         to reject in the latter case, but some compilers have been
         observed to emit constant-zero expressions.  So just ignore
         them. */
      parent_is_struct
         = parser->qparentE[parser->sp].Te.TyStOrUn.isStruct;
      if (!fieldE.Te.Field.name)
         fieldE.Te.Field.name
            = ML_(dinfo_strdup)( "di.readdwarf3.ptD.member.3",
                                 "<anon_field>" );
      if (fieldE.Te.Field.typeR == D3_INVALID_CUOFF)
         goto_bad_DIE;
      if (fieldE.Te.Field.nLoc) {
         if (!parent_is_struct) {
            /* If this is a union type, pretend we haven't seen the data
               member location expression, as it is by definition
               redundant (it must be zero). */
            if (fieldE.Te.Field.nLoc > 0)
               ML_(dinfo_free)(fieldE.Te.Field.pos.loc);
            fieldE.Te.Field.pos.loc = NULL;
            fieldE.Te.Field.nLoc = 0;
         }
         /* Record this child in the parent */
         fieldE.Te.Field.isStruct = parent_is_struct;
         vg_assert(parser->qparentE[parser->sp].Te.TyStOrUn.fieldRs);
         VG_(addToXA)( parser->qparentE[parser->sp].Te.TyStOrUn.fieldRs,
                       &posn );
         /* And record the child itself */
         goto acquire_Field;
      } else {
         /* Member with no location - this can happen with static
            const members in C++ code which are compile time constants
            that do no exist in the class. They're not of any interest
            to us so we ignore them. */
        no_location:
         ML_(TyEnt__make_EMPTY)(&fieldE);
      }
   }

   if (dtag == DW_TAG_array_type) {
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff = posn;
      typeE.tag   = Te_TyArray;
      typeE.Te.TyArray.typeR = D3_INVALID_CUOFF;
      typeE.Te.TyArray.boundRs
         = VG_(newXA)( ML_(dinfo_zalloc), "di.readdwarf3.ptD.array_type.1",
                       ML_(dinfo_free),
                       sizeof(UWord) );
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_type && cts.szB > 0) {
            typeE.Te.TyArray.typeR
               = cook_die_using_form( cc, (UWord)cts.u.val, form );
         }
      }
      if (typeE.Te.TyArray.typeR == D3_INVALID_CUOFF)
         goto_bad_DIE;
      /* On't stack! */
      typestack_push( cc, parser, td3, &typeE, level );
      goto acquire_Type;
   }

   /* this is a subrange type defining the bounds of an array. */
   if (dtag == DW_TAG_subrange_type 
       && subrange_type_denotes_array_bounds(parser, dtag)) {
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
         case 'A': have_lower = False; break;
         default:  vg_assert(0); /* assured us by handling of
                                    DW_TAG_compile_unit in this fn */
      }

      VG_(memset)( &boundE, 0, sizeof(boundE) );
      boundE.cuOff = D3_INVALID_CUOFF;
      boundE.tag   = Te_Bound;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_lower_bound && cts.szB > 0 
             && form_expected_for_bound (form)) {
            lower      = (Long)cts.u.val;
            have_lower = True;
         }
         if (attr == DW_AT_upper_bound && cts.szB > 0 
             && form_expected_for_bound (form)) {
            upper      = (Long)cts.u.val;
            have_upper = True;
         }
         if (attr == DW_AT_count && cts.szB > 0) {
            count    = (Long)cts.u.val;
            have_count = True;
         }
      }
      /* FIXME: potentially skip the rest if no parent present, since
         it could be the case that this subrange type is free-standing
         (not being used to describe the bounds of a containing array
         type) */
      /* Do we have a plausible parent? */
      if (typestack_is_empty(parser)) goto_bad_DIE;
      vg_assert(ML_(TyEnt__is_type)(&parser->qparentE[parser->sp]));
      vg_assert(parser->qparentE[parser->sp].cuOff != D3_INVALID_CUOFF);
      if (level != parser->qlevel[parser->sp]+1) goto_bad_DIE;
      if (parser->qparentE[parser->sp].tag != Te_TyArray) goto_bad_DIE;

      /* Figure out if we have a definite range or not */
      if (have_lower && have_upper && (!have_count)) {
         boundE.Te.Bound.knownL = True;
         boundE.Te.Bound.knownU = True;
         boundE.Te.Bound.boundL = lower;
         boundE.Te.Bound.boundU = upper;
      }
      else if (have_lower && (!have_upper) && (!have_count)) {
         boundE.Te.Bound.knownL = True;
         boundE.Te.Bound.knownU = False;
         boundE.Te.Bound.boundL = lower;
         boundE.Te.Bound.boundU = 0;
      }
      else if ((!have_lower) && have_upper && (!have_count)) {
         boundE.Te.Bound.knownL = False;
         boundE.Te.Bound.knownU = True;
         boundE.Te.Bound.boundL = 0;
         boundE.Te.Bound.boundU = upper;
      }
      else if ((!have_lower) && (!have_upper) && (!have_count)) {
         boundE.Te.Bound.knownL = False;
         boundE.Te.Bound.knownU = False;
         boundE.Te.Bound.boundL = 0;
         boundE.Te.Bound.boundU = 0;
      } else if (have_lower && (!have_upper) && (have_count)) {
         boundE.Te.Bound.knownL = True;
         boundE.Te.Bound.knownU = True;
         boundE.Te.Bound.boundL = lower;
         boundE.Te.Bound.boundU = lower + count - 1;
      } else {
         /* FIXME: handle more cases */
         goto_bad_DIE;
      }

      /* Record this bound in the parent */
      boundE.cuOff = posn;
      vg_assert(parser->qparentE[parser->sp].Te.TyArray.boundRs);
      VG_(addToXA)( parser->qparentE[parser->sp].Te.TyArray.boundRs,
                    &boundE.cuOff );
      /* And record the child itself */
      goto acquire_Bound;
   }

   /* typedef or subrange_type other than array bounds. */
   if (dtag == DW_TAG_typedef 
       || (dtag == DW_TAG_subrange_type 
           && !subrange_type_denotes_array_bounds(parser, dtag))) {
      /* subrange_type other than array bound is only for Ada. */
      vg_assert (dtag == DW_TAG_typedef || (parser->language == 'A'
                                            || parser->language == '?'));
      /* We can pick up a new typedef/subrange_type any time. */
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff = D3_INVALID_CUOFF;
      typeE.tag   = Te_TyTyDef;
      typeE.Te.TyTyDef.name = NULL;
      typeE.Te.TyTyDef.typeR = D3_INVALID_CUOFF;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_name && cts.szB < 0) {
            typeE.Te.TyTyDef.name
               = ML_(cur_read_strdup)( cts.u.cur,
                                       "di.readdwarf3.ptD.typedef.1" );
         }
         if (attr == DW_AT_type && cts.szB > 0) {
            typeE.Te.TyTyDef.typeR
               = cook_die_using_form( cc, (UWord)cts.u.val, form );
         }
      }
      /* Do we have something that looks sane?
         gcc gnat Ada generates minimal typedef
         such as the below
         <6><91cc>: DW_TAG_typedef
            DW_AT_abstract_ori: <9066>
         g++ for OMP can generate artificial functions that have
         parameters that refer to pointers to unnamed typedefs.
         See https://bugs.kde.org/show_bug.cgi?id=273475
         So we cannot require a name for a DW_TAG_typedef.
      */
      goto acquire_Type;
   }

   if (dtag == DW_TAG_subroutine_type) {
      /* function type? just record that one fact and ask no
         further questions. */
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff = D3_INVALID_CUOFF;
      typeE.tag   = Te_TyFn;
      goto acquire_Type;
   }

   if (dtag == DW_TAG_volatile_type || dtag == DW_TAG_const_type
       || dtag == DW_TAG_restrict_type || dtag == DW_TAG_atomic_type) {
      Int have_ty = 0;
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff = D3_INVALID_CUOFF;
      typeE.tag   = Te_TyQual;
      typeE.Te.TyQual.qual
         = (dtag == DW_TAG_volatile_type ? 'V'
            : (dtag == DW_TAG_const_type ? 'C'
               : (dtag == DW_TAG_restrict_type ? 'R' : 'A')));
      /* target type defaults to 'void' */
      typeE.Te.TyQual.typeR = D3_FAKEVOID_CUOFF;
      nf_i = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  abbv->nf[nf_i].at_name;
         DW_FORM form = (DW_FORM)abbv->nf[nf_i].at_form;
         const name_form *nf = &abbv->nf[nf_i];
         nf_i++;
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, cc, c_die, False/*td3*/, nf );
         if (attr == DW_AT_type && cts.szB > 0) {
            typeE.Te.TyQual.typeR
               = cook_die_using_form( cc, (UWord)cts.u.val, form );
            have_ty++;
         }
      }
      /* gcc sometimes generates DW_TAG_const/volatile_type without
         DW_AT_type and GDB appears to interpret the type as 'const
         void' (resp. 'volatile void').  So just allow it .. */
      if (have_ty == 1 || have_ty == 0)
         goto acquire_Type;
      else
         goto_bad_DIE;
   }

   /*
    * Treat DW_TAG_unspecified_type as type void. An example of DW_TAG_unspecified_type:
    *
    * $ readelf --debug-dump /usr/lib/debug/usr/lib/libstdc++.so.6.0.16.debug
    *  <1><10d4>: Abbrev Number: 53 (DW_TAG_unspecified_type)
    *     <10d5>   DW_AT_name        : (indirect string, offset: 0xdb7): decltype(nullptr)
    */
   if (dtag == DW_TAG_unspecified_type) {
      VG_(memset)(&typeE, 0, sizeof(typeE));
      typeE.cuOff           = D3_INVALID_CUOFF;
      typeE.tag             = Te_TyQual;
      typeE.Te.TyQual.typeR = D3_FAKEVOID_CUOFF;
      goto acquire_Type;
   }

   /* else ignore this DIE */
   return;
   /*NOTREACHED*/

  acquire_Type:
   if (0) VG_(printf)("YYYY Acquire Type\n");
   vg_assert(ML_(TyEnt__is_type)( &typeE ));
   vg_assert(typeE.cuOff == D3_INVALID_CUOFF || typeE.cuOff == posn);
   typeE.cuOff = posn;
   VG_(addToXA)( tyents, &typeE );
   return;
   /*NOTREACHED*/

  acquire_Atom:
   if (0) VG_(printf)("YYYY Acquire Atom\n");
   vg_assert(atomE.tag == Te_Atom);
   vg_assert(atomE.cuOff == D3_INVALID_CUOFF || atomE.cuOff == posn);
   atomE.cuOff = posn;
   VG_(addToXA)( tyents, &atomE );
   return;
   /*NOTREACHED*/

  acquire_Field:
   /* For union members, Expr should be absent */
   if (0) VG_(printf)("YYYY Acquire Field\n");
   vg_assert(fieldE.tag == Te_Field);
   vg_assert(fieldE.Te.Field.nLoc <= 0 || fieldE.Te.Field.pos.loc != NULL);
   vg_assert(fieldE.Te.Field.nLoc != 0 || fieldE.Te.Field.pos.loc == NULL);
   if (fieldE.Te.Field.isStruct) {
      vg_assert(fieldE.Te.Field.nLoc != 0);
   } else {
      vg_assert(fieldE.Te.Field.nLoc == 0);
   }
   vg_assert(fieldE.cuOff == D3_INVALID_CUOFF || fieldE.cuOff == posn);
   fieldE.cuOff = posn;
   VG_(addToXA)( tyents, &fieldE );
   return;
   /*NOTREACHED*/

  acquire_Bound:
   if (0) VG_(printf)("YYYY Acquire Bound\n");
   vg_assert(boundE.tag == Te_Bound);
   vg_assert(boundE.cuOff == D3_INVALID_CUOFF || boundE.cuOff == posn);
   boundE.cuOff = posn;
   VG_(addToXA)( tyents, &boundE );
   return;
   /*NOTREACHED*/

  bad_DIE:
   dump_bad_die_and_barf("parse_type_DIE", dtag, posn, level,
                         c_die, saved_die_c_offset,
                         abbv,
                         cc);
   /*NOTREACHED*/
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Compression of type DIE information                  ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

static UWord chase_cuOff ( Bool* changed,
                           const XArray* /* of TyEnt */ ents,
                           TyEntIndexCache* ents_cache,
                           UWord cuOff )
{
   TyEnt* ent;
   ent = ML_(TyEnts__index_by_cuOff)( ents, ents_cache, cuOff );

   if (!ent) {
      if (VG_(clo_verbosity) > 1)
         VG_(printf)("chase_cuOff: no entry for 0x%05lx\n", cuOff);
      *changed = False;
      return cuOff;
   }

   vg_assert(ent->tag != Te_EMPTY);
   if (ent->tag != Te_INDIR) {
      *changed = False;
      return cuOff;
   } else {
      vg_assert(ent->Te.INDIR.indR < cuOff);
      *changed = True;
      return ent->Te.INDIR.indR;
   }
}

static
void chase_cuOffs_in_XArray ( Bool* changed,
                              const XArray* /* of TyEnt */ ents,
                              TyEntIndexCache* ents_cache,
                              /*MOD*/XArray* /* of UWord */ cuOffs )
{
   Bool b2 = False;
   Word i, n = VG_(sizeXA)( cuOffs );
   for (i = 0; i < n; i++) {
      Bool   b = False;
      UWord* p = VG_(indexXA)( cuOffs, i );
      *p = chase_cuOff( &b, ents, ents_cache, *p );
      if (b)
         b2 = True;
   }
   *changed = b2;
}

static Bool TyEnt__subst_R_fields ( const XArray* /* of TyEnt */ ents,
                                    TyEntIndexCache* ents_cache,
                                    /*MOD*/TyEnt* te )
{
   Bool b, changed = False;
   switch (te->tag) {
      case Te_EMPTY:
         break;
      case Te_INDIR:
         te->Te.INDIR.indR
            = chase_cuOff( &b, ents, ents_cache, te->Te.INDIR.indR );
         if (b) changed = True;
         break;
      case Te_UNKNOWN:
         break;
      case Te_Atom:
         break;
      case Te_Field:
         te->Te.Field.typeR
            = chase_cuOff( &b, ents, ents_cache, te->Te.Field.typeR );
         if (b) changed = True;
         break;
      case Te_Bound:
         break;
      case Te_TyBase:
         break;
      case Te_TyPtr:
      case Te_TyRef:
      case Te_TyPtrMbr:
      case Te_TyRvalRef:
         te->Te.TyPorR.typeR
            = chase_cuOff( &b, ents, ents_cache, te->Te.TyPorR.typeR );
         if (b) changed = True;
         break;
      case Te_TyTyDef:
         te->Te.TyTyDef.typeR
            = chase_cuOff( &b, ents, ents_cache, te->Te.TyTyDef.typeR );
         if (b) changed = True;
         break;
      case Te_TyStOrUn:
         chase_cuOffs_in_XArray( &b, ents, ents_cache, te->Te.TyStOrUn.fieldRs );
         if (b) changed = True;
         break;
      case Te_TyEnum:
         chase_cuOffs_in_XArray( &b, ents, ents_cache, te->Te.TyEnum.atomRs );
         if (b) changed = True;
         break;
      case Te_TyArray:
         te->Te.TyArray.typeR
            = chase_cuOff( &b, ents, ents_cache, te->Te.TyArray.typeR );
         if (b) changed = True;
         chase_cuOffs_in_XArray( &b, ents, ents_cache, te->Te.TyArray.boundRs );
         if (b) changed = True;
         break;
      case Te_TyFn:
         break;
      case Te_TyQual:
         te->Te.TyQual.typeR
            = chase_cuOff( &b, ents, ents_cache, te->Te.TyQual.typeR );
         if (b) changed = True;
         break;
      case Te_TyVoid:
         break;
      default:
         ML_(pp_TyEnt)(te);
         vg_assert(0);
   }
   return changed;
}

/* Make a pass over 'ents'.  For each tyent, inspect the target of any
   'R' or 'Rs' fields (those which refer to other tyents), and replace
   any which point to INDIR nodes with the target of the indirection
   (which should not itself be an indirection).  In summary, this
   routine shorts out all references to indirection nodes. */
static
Word dedup_types_substitution_pass ( /*MOD*/XArray* /* of TyEnt */ ents,
                                     TyEntIndexCache* ents_cache )
{
   Word i, n, nChanged = 0;
   Bool b;
   n = VG_(sizeXA)( ents );
   for (i = 0; i < n; i++) {
      TyEnt* ent = VG_(indexXA)( ents, i );
      vg_assert(ent->tag != Te_EMPTY);
      /* We have to substitute everything, even indirections, so as to
         ensure that chains of indirections don't build up. */
      b = TyEnt__subst_R_fields( ents, ents_cache, ent );
      if (b)
         nChanged++;
   }

   return nChanged;
}


/* Make a pass over 'ents', building a dictionary of TyEnts as we go.
   Look up each new tyent in the dictionary in turn.  If it is already
   in the dictionary, replace this tyent with an indirection to the
   existing one, and delete any malloc'd stuff hanging off this one.
   In summary, this routine commons up all tyents that are identical
   as defined by TyEnt__cmp_by_all_except_cuOff. */
static
Word dedup_types_commoning_pass ( /*MOD*/XArray* /* of TyEnt */ ents )
{
   Word    n, i, nDeleted;
   WordFM* dict; /* TyEnt* -> void */
   TyEnt*  ent;
   UWord   keyW, valW;

   dict = VG_(newFM)(
             ML_(dinfo_zalloc), "di.readdwarf3.dtcp.1", 
             ML_(dinfo_free),
             (Word(*)(UWord,UWord)) ML_(TyEnt__cmp_by_all_except_cuOff)
          );

   nDeleted = 0;
   n = VG_(sizeXA)( ents );
   for (i = 0; i < n; i++) {
      ent = VG_(indexXA)( ents, i );
      vg_assert(ent->tag != Te_EMPTY);
     
      /* Ignore indirections, although check that they are
         not forming a cycle. */
      if (ent->tag == Te_INDIR) {
         vg_assert(ent->Te.INDIR.indR < ent->cuOff);
         continue;
      }

      keyW = valW = 0;
      if (VG_(lookupFM)( dict, &keyW, &valW, (UWord)ent )) {
         /* it's already in the dictionary. */
         TyEnt* old = (TyEnt*)keyW;
         vg_assert(valW == 0);
         vg_assert(old != ent);
         vg_assert(old->tag != Te_INDIR);
         /* since we are traversing the array in increasing order of
            cuOff: */
         vg_assert(old->cuOff < ent->cuOff); 
         /* So anyway, dump this entry and replace it with an
            indirection to the one in the dictionary.  Note that the
            assertion above guarantees that we cannot create cycles of
            indirections, since we are always creating an indirection
            to a tyent with a cuOff lower than this one. */
         ML_(TyEnt__make_EMPTY)( ent );
         ent->tag = Te_INDIR;
         ent->Te.INDIR.indR = old->cuOff;
         nDeleted++;
      } else {
         /* not in dictionary; add it and keep going. */
         VG_(addToFM)( dict, (UWord)ent, 0 );
      }
   }

   VG_(deleteFM)( dict, NULL, NULL );

   return nDeleted;
}


static
void dedup_types ( Bool td3, 
                   /*MOD*/XArray* /* of TyEnt */ ents,
                   TyEntIndexCache* ents_cache )
{
   Word m, n, i, nDel, nSubst, nThresh;
   if (0) td3 = True;

   n = VG_(sizeXA)( ents );

   /* If a commoning pass and a substitution pass both make fewer than
      this many changes, just stop.  It's pointless to burn up CPU
      time trying to compress the last 1% or so out of the array. */
   nThresh = n / 200;

   /* First we must sort .ents by its .cuOff fields, so we
      can index into it. */
   VG_(setCmpFnXA)( ents, (XACmpFn_t) ML_(TyEnt__cmp_by_cuOff_only) );
   VG_(sortXA)( ents );

   /* Now repeatedly do commoning and substitution passes over
      the array, until there are no more changes. */
   do {
      nDel   = dedup_types_commoning_pass ( ents );
      nSubst = dedup_types_substitution_pass ( ents, ents_cache );
      vg_assert(nDel >= 0 && nSubst >= 0);
      TRACE_D3("   %ld deletions, %ld substitutions\n", nDel, nSubst);
   } while (nDel > nThresh || nSubst > nThresh);

   /* Sanity check: all INDIR nodes should point at a non-INDIR thing.
      In fact this should be true at the end of every loop iteration
      above (a commoning pass followed by a substitution pass), but
      checking it on every iteration is excessively expensive.  Note,
      this loop also computes 'm' for the stats printing below it. */
   m = 0;
   n = VG_(sizeXA)( ents );
   for (i = 0; i < n; i++) {
      TyEnt *ent, *ind;
      ent = VG_(indexXA)( ents, i );
      if (ent->tag != Te_INDIR) continue;
      m++;
      ind = ML_(TyEnts__index_by_cuOff)( ents, ents_cache,
                                         ent->Te.INDIR.indR );
      vg_assert(ind);
      vg_assert(ind->tag != Te_INDIR);
   }

   TRACE_D3("Overall: %ld before, %ld after\n", n, n-m);
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Resolution of references to type DIEs                ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Make a pass through the (temporary) variables array.  Examine the
   type of each variable, check is it found, and chase any Te_INDIRs.
   Postcondition is: each variable has a typeR field that refers to a
   valid type in tyents, or a Te_UNKNOWN, and is certainly guaranteed
   not to refer to a Te_INDIR.  (This is so that we can throw all the
   Te_INDIRs away later). */

__attribute__((noinline))
static void resolve_variable_types (
               void (*barf)( const HChar* ) __attribute__((noreturn)),
               /*R-O*/XArray* /* of TyEnt */ ents,
               /*MOD*/TyEntIndexCache* ents_cache,
               /*MOD*/XArray* /* of TempVar* */ vars
            )
{
   Word i, n;
   n = VG_(sizeXA)( vars );
   for (i = 0; i < n; i++) {
      TempVar* var = *(TempVar**)VG_(indexXA)( vars, i );
      /* This is the stated type of the variable.  But it might be
         an indirection, so be careful. */
      TyEnt* ent = ML_(TyEnts__index_by_cuOff)( ents, ents_cache,
                                                var->typeR );
      if (ent && ent->tag == Te_INDIR) {
         ent = ML_(TyEnts__index_by_cuOff)( ents, ents_cache, 
                                            ent->Te.INDIR.indR );
         vg_assert(ent);
         vg_assert(ent->tag != Te_INDIR);
      }

      /* Deal first with "normal" cases */
      if (ent && ML_(TyEnt__is_type)(ent)) {
         var->typeR = ent->cuOff;
         continue;
      }

      /* If there's no ent, it probably we did not manage to read a
         type at the cuOffset which is stated as being this variable's
         type.  Maybe a deficiency in parse_type_DIE.  Complain. */
      if (ent == NULL) {
         VG_(printf)("\n: Invalid cuOff = 0x%05lx\n", var->typeR );
         barf("resolve_variable_types: "
              "cuOff does not refer to a known type");
      }
      vg_assert(ent);
      /* If ent has any other tag, something bad happened, along the
         lines of var->typeR not referring to a type at all. */
      vg_assert(ent->tag == Te_UNKNOWN);
      /* Just accept it; the type will be useless, but at least keep
         going. */
      var->typeR = ent->cuOff;
   }
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Parsing of Compilation Units                         ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

static Int cmp_TempVar_by_dioff ( const void* v1, const void* v2 ) {
   const TempVar* t1 = *(const TempVar *const *)v1;
   const TempVar* t2 = *(const TempVar *const *)v2;
   if (t1->dioff < t2->dioff) return -1;
   if (t1->dioff > t2->dioff) return 1;
   return 0;
}

static void read_DIE ( 
   /*MOD*/WordFM* /* of (XArray* of AddrRange, void) */ rangestree,
   /*MOD*/XArray* /* of TyEnt */ tyents,
   /*MOD*/XArray* /* of TempVar* */ tempvars,
   /*MOD*/XArray* /* of GExpr* */ gexprs,
   /*MOD*/D3TypeParser* typarser,
   /*MOD*/D3VarParser* varparser,
   /*MOD*/D3InlParser* inlparser,
   XArray** fndn_ix_Table,
   ULong *debug_line_offset,
   Cursor* c, Bool td3, CUConst* cc, Int level
)
{
   const g_abbv *abbv;
   ULong  atag, abbv_code;
   UWord  posn;
   UInt   has_children;
   UWord  start_die_c_offset;
   UWord  after_die_c_offset;
   // If the DIE we will parse has a sibling and the parser(s) are
   // all indicating that parse_children is not necessary, then
   // we will skip the children by jumping to the sibling of this DIE
   // (if it has a sibling).
   UWord  sibling = 0;
   Bool   parse_children = False;

   /* --- Deal with this DIE --- */
   posn      = cook_die( cc, get_position_of_Cursor( c ) );
   abbv_code = get_ULEB128( c );
   abbv = get_abbv(cc, abbv_code, td3);
   atag      = abbv->atag;

   if (TD3) {
      TRACE_D3("\n");
      trace_DIE ((DW_TAG)atag, posn, level,
                 get_position_of_Cursor( c ), abbv, cc);
   }

   if (atag == 0)
      cc->barf("read_DIE: invalid zero tag on DIE");

   has_children = abbv->has_children;
   if (has_children != DW_children_no && has_children != DW_children_yes)
      cc->barf("read_DIE: invalid has_children value");

   /* We're set up to look at the fields of this DIE.  Hand it off to
      any parser(s) that want to see it.  Since they will in general
      advance the DIE cursor, remember the current settings so that we
      can then back up. . */
   start_die_c_offset  = get_position_of_Cursor( c );
   after_die_c_offset  = 0; // set to c position if a parser has read the DIE.

   if (VG_(clo_read_var_info)) {
      parse_type_DIE( tyents,
                      typarser,
                      (DW_TAG)atag,
                      posn,
                      level,
                      c,     /* DIE cursor */
                      abbv,  /* abbrev */
                      cc,
                      td3 );
      if (get_position_of_Cursor( c ) != start_die_c_offset) {
         after_die_c_offset = get_position_of_Cursor( c );
         set_position_of_Cursor( c, start_die_c_offset );
      }

      parse_var_DIE( rangestree,
                     tempvars,
                     gexprs,
                     varparser,
                     fndn_ix_Table,
                     debug_line_offset,
                     (DW_TAG)atag,
                     posn,
                     level,
                     c,     /* DIE cursor */
                     abbv,  /* abbrev */
                     cc,
                     td3 );
      if (get_position_of_Cursor( c ) != start_die_c_offset) {
         after_die_c_offset = get_position_of_Cursor( c );
         set_position_of_Cursor( c, start_die_c_offset );
      }

      parse_children = True;
      // type and var parsers do not have logic to skip childrens and establish
      // the value of sibling.
   }

   if (VG_(clo_read_inline_info)) {
      inlparser->sibling = 0;
      parse_children = 
         parse_inl_DIE( inlparser,
                        fndn_ix_Table,
                        debug_line_offset,
                        (DW_TAG)atag,
                        posn,
                        level,
                        c,     /* DIE cursor */
                        abbv, /* abbrev */
                        cc,
                        td3 )
         || parse_children;
      if (get_position_of_Cursor( c ) != start_die_c_offset) {
         after_die_c_offset = get_position_of_Cursor( c );
         // Last parser, no need to reset the cursor to start_die_c_offset.
      }
      if (sibling == 0)
         sibling = inlparser->sibling;
      vg_assert (inlparser->sibling == 0 || inlparser->sibling == sibling);
   }

   /* Top level CU DIE, but we don't want to read anything else, just skip
      to the end and return.  */
   if (level == 0 && !parse_children) {
      UWord cu_size_including_IniLen = (cc->unit_length
                                        + (cc->is_dw64 ? 12 : 4));
      set_position_of_Cursor( c, (cc->cu_start_offset
                                  + cu_size_including_IniLen));
      return;
   }

   if (after_die_c_offset > 0) {
      // DIE was read by a parser above, so we know where the DIE ends.
      set_position_of_Cursor( c, after_die_c_offset );
   } else {
      /* No parser has parsed this DIE. So, we need to skip the DIE,
         in order to read the next DIE.
         At the same time, establish sibling value if the DIE has one. */
      TRACE_D3("    uninteresting DIE -> skipping ...\n");
      skip_DIE (&sibling, c, abbv, cc);
   }

   /* --- Now recurse into its children, if any 
      and the parsing of the children is requested by a parser --- */
   if (has_children == DW_children_yes) {
      if (parse_children || sibling == 0) {
         if (0) TRACE_D3("BEGIN children of level %d\n", level);
         while (True) {
            atag = peek_ULEB128( c );
            if (atag == 0) break;
            if (parse_children) {
               read_DIE( rangestree, tyents, tempvars, gexprs,
                         typarser, varparser, inlparser,
                         fndn_ix_Table, debug_line_offset,
                         c, td3, cc, level+1 );
            } else {
               Int skip_level = level + 1;
               while (True) {
                  atag = peek_ULEB128( c );
                  if (atag == 0) {
                     skip_level--;
                     if (skip_level == level) break;
                     /* Eat the terminating zero and continue skipping the
                        children one level up.  */
                     atag = get_ULEB128( c );
                     vg_assert(atag == 0);
                     continue;
                  }

                  abbv_code = get_ULEB128( c );
                  abbv = get_abbv(cc, abbv_code, td3);
                  sibling = 0;
                  skip_DIE (&sibling, c, abbv, cc);
                  if (abbv->has_children) {
                     if (sibling == 0)
                        skip_level++;
                     else
                        set_position_of_Cursor( c, sibling );
                  }
               }
            }
         }
         /* Now we need to eat the terminating zero */
         atag = get_ULEB128( c );
         vg_assert(atag == 0);
         if (0) TRACE_D3("END children of level %d\n", level);
      } else {
         // We can skip the childrens, by jumping to the sibling
         TRACE_D3("    SKIPPING DIE's children,"
                  "jumping to sibling <%d><%lx>\n",
                  level, sibling);
         set_position_of_Cursor( c, sibling );
      }
   }

}

static void trace_debug_loc (const DebugInfo* di,
                             __attribute__((noreturn)) void (*barf)( const HChar* ),
                             DiSlice escn_debug_loc)
{
#if 0
   /* This doesn't work properly because it assumes all entries are
      packed end to end, with no holes.  But that doesn't always
      appear to be the case, so it loses sync.  And the D3 spec
      doesn't appear to require a no-hole situation either. */
   /* Display .debug_loc */
   Addr  dl_base;
   UWord dl_offset;
   Cursor loc; /* for showing .debug_loc */
   Bool td3 = di->trace_symtab;

   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("\n------ The contents of .debug_loc ------\n");
   TRACE_SYMTAB("    Offset   Begin    End      Expression\n");
   if (ML_(sli_is_valid)(escn_debug_loc)) {
      init_Cursor( &loc, escn_debug_loc, 0, barf, 
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
   }
#endif
}

static void trace_debug_ranges (const DebugInfo* di,
                                __attribute__((noreturn)) void (*barf)( const HChar* ),
                                DiSlice escn_debug_ranges)
{
   Cursor ranges; /* for showing .debug_ranges */
   Addr  dr_base;
   UWord dr_offset;
   Bool td3 = di->trace_symtab;

   /* Display .debug_ranges */
   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("\n------ The contents of .debug_ranges ------\n");
   TRACE_SYMTAB("    Offset   Begin    End\n");
   if (ML_(sli_is_valid)(escn_debug_ranges)) {
      init_Cursor( &ranges, escn_debug_ranges, 0, barf, 
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
   }
}

static void trace_debug_abbrev (const DebugInfo* di,
                                __attribute__((noreturn)) void (*barf)( const HChar* ),
                                DiSlice escn_debug_abbv)
{
   Cursor abbv; /* for showing .debug_abbrev */
   Bool td3 = di->trace_symtab;

   /* Display .debug_abbrev */
   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("\n------ The contents of .debug_abbrev ------\n");
   if (ML_(sli_is_valid)(escn_debug_abbv)) {
      init_Cursor( &abbv, escn_debug_abbv, 0, barf, 
                   "Overrun whilst reading .debug_abbrev section" );
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
               if (at_form == DW_FORM_implicit_const) {
                  /* Long at_val = */ get_SLEB128 ( &abbv );
               }
               if (at_name == 0 && at_form == 0) break;
               TRACE_D3("    %-18s %s\n", 
                        ML_(pp_DW_AT)(at_name), ML_(pp_DW_FORM)(at_form));
            }
         }
      }
   }
}

static
void new_dwarf3_reader_wrk ( 
   DebugInfo* di,
   __attribute__((noreturn)) void (*barf)( const HChar* ),
   DiSlice escn_debug_info,      DiSlice escn_debug_types,
   DiSlice escn_debug_abbv,      DiSlice escn_debug_line,
   DiSlice escn_debug_str,       DiSlice escn_debug_ranges,
   DiSlice escn_debug_rnglists,  DiSlice escn_debug_loclists,
   DiSlice escn_debug_loc,       DiSlice escn_debug_info_alt,
   DiSlice escn_debug_abbv_alt,  DiSlice escn_debug_line_alt,
   DiSlice escn_debug_str_alt,   DiSlice escn_debug_line_str,
   DiSlice escn_debug_addr,      DiSlice escn_debug_str_offsets
)
{
   XArray* /* of TyEnt */     tyents = NULL;
   XArray* /* of TyEnt */     tyents_to_keep = NULL;
   XArray* /* of GExpr* */    gexprs = NULL;
   XArray* /* of TempVar* */  tempvars = NULL;
   WordFM* /* of (XArray* of AddrRange, void) */ rangestree = NULL;
   TyEntIndexCache* tyents_cache = NULL;
   TyEntIndexCache* tyents_to_keep_cache = NULL;
   TempVar *varp, *varp2;
   GExpr* gexpr;
   Cursor info; /* primary cursor for parsing .debug_info */
   D3TypeParser typarser;
   D3VarParser varparser;
   D3InlParser inlparser;
   XArray* /* of UInt */ fndn_ix_Table = NULL;
   ULong debug_line_offset = (ULong) -1;
   Word  i, j, n;
   Bool td3 = di->trace_symtab;
   XArray* /* of TempVar* */ dioff_lookup_tab;
   Int pass;
   VgHashTable *signature_types = NULL;

   /* Display/trace various information, if requested. */
   if (TD3) {
      trace_debug_loc    (di, barf, escn_debug_loc);
      trace_debug_ranges (di, barf, escn_debug_ranges);
      trace_debug_abbrev (di, barf, escn_debug_abbv);
      TRACE_SYMTAB("\n");
   }

   /* Zero out all parsers. Parsers will really be initialised
      according to VG_(clo_read_*_info). */
   VG_(memset)( &inlparser, 0, sizeof(inlparser) );

   if (VG_(clo_read_var_info)) {
      /* We'll park the harvested type information in here.  Also create
         a fake "void" entry with offset D3_FAKEVOID_CUOFF, so we always
         have at least one type entry to refer to.  D3_FAKEVOID_CUOFF is
         huge and presumably will not occur in any valid DWARF3 file --
         it would need to have a .debug_info section 4GB long for that to
         happen.  These type entries end up in the DebugInfo. */
      tyents = VG_(newXA)( ML_(dinfo_zalloc), 
                           "di.readdwarf3.ndrw.1 (TyEnt temp array)",
                           ML_(dinfo_free), sizeof(TyEnt) );
      { TyEnt tyent;
        VG_(memset)(&tyent, 0, sizeof(tyent));
        tyent.tag   = Te_TyVoid;
        tyent.cuOff = D3_FAKEVOID_CUOFF;
        tyent.Te.TyVoid.isFake = True;
        VG_(addToXA)( tyents, &tyent );
      }
      { TyEnt tyent;
        VG_(memset)(&tyent, 0, sizeof(tyent));
        tyent.tag   = Te_UNKNOWN;
        tyent.cuOff = D3_INVALID_CUOFF;
        VG_(addToXA)( tyents, &tyent );
      }

      /* This is a tree used to unique-ify the range lists that are
         manufactured by parse_var_DIE.  References to the keys in the
         tree wind up in .rngMany fields in TempVars.  We'll need to
         delete this tree, and the XArrays attached to it, at the end of
         this function. */
      rangestree = VG_(newFM)( ML_(dinfo_zalloc),
                               "di.readdwarf3.ndrw.2 (rangestree)",
                               ML_(dinfo_free),
                               (Word(*)(UWord,UWord))cmp__XArrays_of_AddrRange );

      /* List of variables we're accumulating.  These don't end up in the
         DebugInfo; instead their contents are handed to ML_(addVar) and
         the list elements are then deleted. */
      tempvars = VG_(newXA)( ML_(dinfo_zalloc),
                             "di.readdwarf3.ndrw.3 (TempVar*s array)",
                             ML_(dinfo_free), 
                             sizeof(TempVar*) );

      /* List of GExprs we're accumulating.  These wind up in the
         DebugInfo. */
      gexprs = VG_(newXA)( ML_(dinfo_zalloc), "di.readdwarf3.ndrw.4",
                           ML_(dinfo_free), sizeof(GExpr*) );

      /* We need a D3TypeParser to keep track of partially constructed
         types.  It'll be discarded as soon as we've completed the CU,
         since the resulting information is tipped in to 'tyents' as it
         is generated. */
      type_parser_init(&typarser);

      var_parser_init(&varparser);

      signature_types = VG_(HT_construct) ("signature_types");
   }

   /* Do an initial pass to scan the .debug_types section, if any, and
      fill in the signatured types hash table.  This lets us handle
      mapping from a type signature to a (cooked) DIE offset directly
      in get_Form_contents.  */
   if (VG_(clo_read_var_info) && ML_(sli_is_valid)(escn_debug_types)) {
      init_Cursor( &info, escn_debug_types, 0, barf,
                   "Overrun whilst reading .debug_types section" );
      TRACE_D3("\n------ Collecting signatures from "
               ".debug_types section ------\n");

      abbv_state last_abbv;
      last_abbv.debug_abbrev_offset = (ULong) -1;
      last_abbv.ht_abbvs = NULL;
      while (True) {
         UWord   cu_start_offset, cu_offset_now;
         CUConst cc;

         cu_start_offset = get_position_of_Cursor( &info );
         TRACE_D3("\n");
         TRACE_D3("  Compilation Unit @ offset 0x%lx:\n", cu_start_offset);
         /* parse_CU_header initialises the CU's abbv hash table.  */
         parse_CU_Header( &cc, td3, &info, escn_debug_abbv,
                          last_abbv, True, False );

         /* Needed by cook_die.  */
         cc.types_cuOff_bias = escn_debug_info.szB;

         record_signatured_type( signature_types, cc.type_signature,
                                 cook_die( &cc, cc.type_offset ));

         /* Until proven otherwise we assume we don't need the icc9
            workaround in this case; see the DIE-reading loop below
            for details.  */
         cu_offset_now = (cu_start_offset + cc.unit_length
                          + (cc.is_dw64 ? 12 : 4));

         last_abbv = cc.abbv;

         if (cu_offset_now >= escn_debug_types.szB) {
            break;
         }

         set_position_of_Cursor ( &info, cu_offset_now );
      }
      if (last_abbv.ht_abbvs != NULL)
         VG_(HT_destruct) (last_abbv.ht_abbvs, ML_(dinfo_free));
   }

   /* Perform three DIE-reading passes.  The first pass reads DIEs from
      alternate .debug_info (if any), the second pass reads DIEs from
      .debug_info, and the third pass reads DIEs from .debug_types.
      Moving the body of this loop into a separate function would
      require a large number of arguments to be passed in, so it is
      kept inline instead.  */
   for (pass = 0; pass < 3; ++pass) {
      ULong section_size;

      if (pass == 0) {
         if (!ML_(sli_is_valid)(escn_debug_info_alt))
	    continue;
         /* Now loop over the Compilation Units listed in the alternate
            .debug_info section (see D3SPEC sec 7.5) paras 1 and 2.
            Each compilation unit contains a Compilation Unit Header
            followed by precisely one DW_TAG_compile_unit or
            DW_TAG_partial_unit DIE. */
         init_Cursor( &info, escn_debug_info_alt, 0, barf,
                      "Overrun whilst reading alternate .debug_info section" );
         section_size = escn_debug_info_alt.szB;

	 /* Keep track of the last line table we have seen,
            it might turn up again.  */
         reset_fndn_ix_table(&fndn_ix_Table, &debug_line_offset, (ULong) -1);

         TRACE_D3("\n------ Parsing alternate .debug_info section ------\n");
      } else if (pass == 1) {
         /* Now loop over the Compilation Units listed in the .debug_info
            section (see D3SPEC sec 7.5) paras 1 and 2.  Each compilation
            unit contains a Compilation Unit Header followed by precisely
            one DW_TAG_compile_unit or DW_TAG_partial_unit DIE. */
         init_Cursor( &info, escn_debug_info, 0, barf,
                      "Overrun whilst reading .debug_info section" );
         section_size = escn_debug_info.szB;

	 /* Keep track of the last line table we have seen,
            it might turn up again.  */
         reset_fndn_ix_table(&fndn_ix_Table, &debug_line_offset, (ULong) -1);

         TRACE_D3("\n------ Parsing .debug_info section ------\n");
      } else {
         if (!ML_(sli_is_valid)(escn_debug_types))
            continue;
         if (!VG_(clo_read_var_info))
            continue; // Types not needed when only reading inline info.
         init_Cursor( &info, escn_debug_types, 0, barf,
                      "Overrun whilst reading .debug_types section" );
         section_size = escn_debug_types.szB;

	 /* Keep track of the last line table we have seen,
            it might turn up again.  */
         reset_fndn_ix_table(&fndn_ix_Table, &debug_line_offset, (ULong) -1);

         TRACE_D3("\n------ Parsing .debug_types section ------\n");
      }

      abbv_state last_abbv;
      last_abbv.debug_abbrev_offset = (ULong) -1;
      last_abbv.ht_abbvs = NULL;
      while (True) {
         ULong   cu_start_offset, cu_offset_now;
         CUConst cc;
         /* It may be that the stated size of this CU is larger than the
            amount of stuff actually in it.  icc9 seems to generate CUs
            thusly.  We use these variables to figure out if this is
            indeed the case, and if so how many bytes we need to skip to
            get to the start of the next CU.  Not skipping those bytes
            causes us to misidentify the start of the next CU, and it all
            goes badly wrong after that (not surprisingly). */
         UWord cu_size_including_IniLen, cu_amount_used;

         /* It seems icc9 finishes the DIE info before debug_info_sz
            bytes have been used up.  So be flexible, and declare the
            sequence complete if there is not enough remaining bytes to
            hold even the smallest conceivable CU header.  (11 bytes I
            reckon). */
         /* JRS 23Jan09: I suspect this is no longer necessary now that
            the code below contains a 'while (cu_amount_used <
            cu_size_including_IniLen ...'  style loop, which skips over
            any leftover bytes at the end of a CU in the case where the
            CU's stated size is larger than its actual size (as
            determined by reading all its DIEs).  However, for prudence,
            I'll leave the following test in place.  I can't see that a
            CU header can be smaller than 11 bytes, so I don't think
            there's any harm possible through the test -- it just adds
            robustness. */
         Word avail = get_remaining_length_Cursor( &info );
         if (avail < 11) {
            if (avail > 0)
               TRACE_D3("new_dwarf3_reader_wrk: warning: "
                        "%ld unused bytes after end of DIEs\n", avail);
            break;
         }

         if (VG_(clo_read_var_info)) {
            /* Check the varparser's stack is in a sane state. */
            vg_assert(varparser.sp == -1);
            /* Check the typarser's stack is in a sane state. */
            vg_assert(typarser.sp == -1);
         }

         cu_start_offset = get_position_of_Cursor( &info );
         TRACE_D3("\n");
         TRACE_D3("  Compilation Unit @ offset 0x%llx:\n", cu_start_offset);
         /* parse_CU_header initialises the CU's hashtable of abbvs ht_abbvs */
         if (pass == 0) {
            parse_CU_Header( &cc, td3, &info, escn_debug_abbv_alt,
                             last_abbv, False, True );
         } else {
            parse_CU_Header( &cc, td3, &info, escn_debug_abbv,
                             last_abbv, pass == 2, False );
         }
         cc.escn_debug_str      = pass == 0 ? escn_debug_str_alt
                                            : escn_debug_str;
         cc.escn_debug_ranges   = escn_debug_ranges;
         cc.escn_debug_rnglists = escn_debug_rnglists;
         cc.escn_debug_loclists = escn_debug_loclists;
         cc.escn_debug_loc      = escn_debug_loc;
         cc.escn_debug_line     = pass == 0 ? escn_debug_line_alt
                                            : escn_debug_line;
         cc.escn_debug_info     = pass == 0 ? escn_debug_info_alt
                                            : escn_debug_info;
         cc.escn_debug_types    = escn_debug_types;
         cc.escn_debug_info_alt = escn_debug_info_alt;
         cc.escn_debug_str_alt  = escn_debug_str_alt;
         cc.escn_debug_line_str = escn_debug_line_str;
         cc.escn_debug_addr     = escn_debug_addr;
         cc.escn_debug_str_offsets = escn_debug_str_offsets;
         cc.types_cuOff_bias    = escn_debug_info.szB;
         cc.alt_cuOff_bias      = escn_debug_info.szB + escn_debug_types.szB;
         cc.cu_start_offset     = cu_start_offset;
         cc.cu_addr_base        = 0;
         cc.cu_has_addr_base    = False;
         cc.cu_str_offsets_base = 0;
         cc.cu_has_str_offsets_base = False;
         cc.cu_rnglists_base = 0;
         cc.cu_has_rnglists_base = False;
         cc.cu_loclists_base = 0;
         cc.cu_has_loclists_base = False;
         cc.di = di;
         /* The CU's svma can be deduced by looking at the AT_low_pc
            value in the top level TAG_compile_unit, which is the topmost
            DIE.  We'll leave it for the 'varparser' to acquire that info
            and fill it in -- since it is the only party to want to know
            it. */
         cc.cu_svma_known = False;
         cc.cu_svma       = 0;

         if (VG_(clo_read_var_info)) {
            cc.signature_types = signature_types;

            /* Create a fake outermost-level range covering the entire
               address range.  So we always have *something* to catch all
               variable declarations. */
            varstack_push( &cc, &varparser, td3, 
                           unitary_range_list(0UL, ~0UL),
                           -1, False/*isFunc*/, NULL/*fbGX*/ );

         }

         /* Now read the one-and-only top-level DIE for this CU. */
         vg_assert(!VG_(clo_read_var_info) || varparser.sp == 0);
         read_DIE( rangestree,
                   tyents, tempvars, gexprs,
                   &typarser, &varparser, &inlparser,
                   &fndn_ix_Table, &debug_line_offset,
                   &info, td3, &cc, 0 );

         cu_offset_now = get_position_of_Cursor( &info );

         if (0) VG_(printf)("Travelled: %llu  size %llu\n",
                            cu_offset_now - cc.cu_start_offset,
                            cc.unit_length + (cc.is_dw64 ? 12 : 4));

         /* How big the CU claims it is .. */
         cu_size_including_IniLen = cc.unit_length + (cc.is_dw64 ? 12 : 4);
         /* .. vs how big we have found it to be */
         cu_amount_used = cu_offset_now - cc.cu_start_offset;

         if (1) TRACE_D3("offset now %llu, d-i-size %llu\n",
                         cu_offset_now, section_size);
         if (cu_offset_now > section_size)
            barf("toplevel DIEs beyond end of CU");

         /* If the CU is bigger than it claims to be, we've got a serious
            problem. */
         if (cu_amount_used > cu_size_including_IniLen)
            barf("CU's actual size appears to be larger than it claims it is");

         /* If the CU is smaller than it claims to be, we need to skip some
            bytes.  Loop updates cu_offset_new and cu_amount_used. */
         while (cu_amount_used < cu_size_including_IniLen
                && get_remaining_length_Cursor( &info ) > 0) {
            if (0) VG_(printf)("SKIP\n");
            (void)get_UChar( &info );
            cu_offset_now = get_position_of_Cursor( &info );
            cu_amount_used = cu_offset_now - cc.cu_start_offset;
         }

         if (VG_(clo_read_var_info)) {
            /* Preen to level -2.  DIEs have level >= 0 so -2 cannot occur
               anywhere else at all.  Our fake the-entire-address-space
               range is at level -1, so preening to -2 should completely
               empty the stack out. */
            TRACE_D3("\n");
            varstack_preen( &varparser, td3, -2 );
            /* Similarly, empty the type stack out. */
            typestack_preen( &typarser, td3, -2 );
         }

         last_abbv = cc.abbv;

         if (cu_offset_now == section_size)
            break;
         /* else keep going */
      }
      if (last_abbv.ht_abbvs != NULL)
         VG_(HT_destruct) (last_abbv.ht_abbvs, ML_(dinfo_free));
   }

   if (fndn_ix_Table != NULL)
      VG_(deleteXA)(fndn_ix_Table);

   if (VG_(clo_read_var_info)) {
      /* From here on we're post-processing the stuff we got
         out of the .debug_info section. */
      if (TD3) {
         TRACE_D3("\n");
         ML_(pp_TyEnts)(tyents, "Initial type entity (TyEnt) array");
         TRACE_D3("\n");
         TRACE_D3("------ Compressing type entries ------\n");
      }

      tyents_cache = ML_(dinfo_zalloc)( "di.readdwarf3.ndrw.6",
                                        sizeof(TyEntIndexCache) );
      ML_(TyEntIndexCache__invalidate)( tyents_cache );
      dedup_types( td3, tyents, tyents_cache );
      if (TD3) {
         TRACE_D3("\n");
         ML_(pp_TyEnts)(tyents, "After type entity (TyEnt) compression");
      }

      TRACE_D3("\n");
      TRACE_D3("------ Resolving the types of variables ------\n" );
      resolve_variable_types( barf, tyents, tyents_cache, tempvars );

      /* Copy all the non-INDIR tyents into a new table.  For large
         .so's, about 90% of the tyents will by now have been resolved to
         INDIRs, and we no longer need them, and so don't need to store
         them. */
      tyents_to_keep
         = VG_(newXA)( ML_(dinfo_zalloc), 
                       "di.readdwarf3.ndrw.7 (TyEnt to-keep array)",
                       ML_(dinfo_free), sizeof(TyEnt) );
      n = VG_(sizeXA)( tyents );
      for (i = 0; i < n; i++) {
         TyEnt* ent = VG_(indexXA)( tyents, i );
         if (ent->tag != Te_INDIR)
            VG_(addToXA)( tyents_to_keep, ent );
      }

      VG_(deleteXA)( tyents );
      tyents = NULL;
      ML_(dinfo_free)( tyents_cache );
      tyents_cache = NULL;

      /* Sort tyents_to_keep so we can lookup in it.  A complete (if
         minor) waste of time, since tyents itself is sorted, but
         necessary since VG_(lookupXA) refuses to cooperate if we
         don't. */
      VG_(setCmpFnXA)( tyents_to_keep, (XACmpFn_t) ML_(TyEnt__cmp_by_cuOff_only) );
      VG_(sortXA)( tyents_to_keep );

      /* Enable cacheing on tyents_to_keep */
      tyents_to_keep_cache
         = ML_(dinfo_zalloc)( "di.readdwarf3.ndrw.8",
                              sizeof(TyEntIndexCache) );
      ML_(TyEntIndexCache__invalidate)( tyents_to_keep_cache );

      /* And record the tyents in the DebugInfo.  We do this before
         starting to hand variables to ML_(addVar), since if ML_(addVar)
         wants to do debug printing (of the types of said vars) then it
         will need the tyents.*/
      vg_assert(!di->admin_tyents);
      di->admin_tyents = tyents_to_keep;

      /* Bias all the location expressions. */
      TRACE_D3("\n");
      TRACE_D3("------ Biasing the location expressions ------\n" );

      n = VG_(sizeXA)( gexprs );
      for (i = 0; i < n; i++) {
         gexpr = *(GExpr**)VG_(indexXA)( gexprs, i );
         bias_GX( gexpr, di );
      }

      TRACE_D3("\n");
      TRACE_D3("------ Acquired the following variables: ------\n\n");

      /* Park (pointers to) all the vars in an XArray, so we can look up
         abstract origins quickly.  The array is sorted (hence, looked-up
         by) the .dioff fields.  Since the .dioffs should be in strictly
         ascending order, there is no need to sort the array after
         construction.  The ascendingness is however asserted for. */
      dioff_lookup_tab
         = VG_(newXA)( ML_(dinfo_zalloc), "di.readdwarf3.ndrw.9",
                       ML_(dinfo_free), 
                       sizeof(TempVar*) );

      n = VG_(sizeXA)( tempvars );
      Word first_primary_var = 0;
      for (first_primary_var = 0;
           escn_debug_info_alt.szB/*really?*/ && first_primary_var < n;
           first_primary_var++) {
         varp = *(TempVar**)VG_(indexXA)( tempvars, first_primary_var );
         if (varp->dioff < escn_debug_info.szB + escn_debug_types.szB)
            break;
      }
      for (i = 0; i < n; i++) {
         varp = *(TempVar**)VG_(indexXA)( tempvars, (i + first_primary_var) % n );
         if (i > first_primary_var) {
            varp2 = *(TempVar**)VG_(indexXA)( tempvars,
                                              (i + first_primary_var - 1) % n );
            /* why should this hold?  Only, I think, because we've
               constructed the array by reading .debug_info sequentially,
               and so the array .dioff fields should reflect that, and be
               strictly ascending. */
            vg_assert(varp2->dioff < varp->dioff);
         }
         VG_(addToXA)( dioff_lookup_tab, &varp );
      }
      VG_(setCmpFnXA)( dioff_lookup_tab, cmp_TempVar_by_dioff );
      VG_(sortXA)( dioff_lookup_tab ); /* POINTLESS; FIXME: rm */

      /* Now visit each var.  Collect up as much info as possible for
         each var and hand it to ML_(addVar). */
      n = VG_(sizeXA)( tempvars );
      for (j = 0; j < n; j++) {
         TyEnt* ent;
         varp = *(TempVar**)VG_(indexXA)( tempvars, j );

         /* Possibly show .. */
         if (TD3) {
            VG_(printf)("<%lx> addVar: level %d: %s :: ",
                        varp->dioff,
                        varp->level,
                        varp->name ? varp->name : "<anon_var>" );
            if (varp->typeR) {
               ML_(pp_TyEnt_C_ishly)( tyents_to_keep, varp->typeR );
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
            VG_(printf)("  declared at: %u %s:%d\n",
                        varp->fndn_ix,
                        ML_(fndn_ix2filename) (di, varp->fndn_ix),
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
            if (!found) {
               /* barf("DW_AT_abstract_origin can't be resolved"); */
               TRACE_D3("  SKIP (DW_AT_abstract_origin can't be resolved)\n\n");
               continue;
            }
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
            if (varAI->fndn_ix && !varp->fndn_ix)
               varp->fndn_ix = varAI->fndn_ix;
            if (varAI->fLine > 0 && varp->fLine == 0)
               varp->fLine = varAI->fLine;
         }

         /* Give it a name if it doesn't have one. */
         if (!varp->name)
            varp->name = ML_(addStr)( di, "<anon_var>", -1 );

         /* So now does it have enough info to be useful? */
         /* NOTE: re typeR: this is a hack.  If typeR is Te_UNKNOWN then
            the type didn't get resolved.  Really, in that case
            something's broken earlier on, and should be fixed, rather
            than just skipping the variable. */
         ent = ML_(TyEnts__index_by_cuOff)( tyents_to_keep,
                                            tyents_to_keep_cache, 
                                            varp->typeR );
         /* The next two assertions should be guaranteed by 
            our previous call to resolve_variable_types. */
         vg_assert(ent);
         vg_assert(ML_(TyEnt__is_type)(ent) || ent->tag == Te_UNKNOWN);

         if (ent->tag == Te_UNKNOWN) continue;

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
                 pcMin += di->text_debug_bias;
                 pcMax += di->text_debug_bias;
              } 

              if (i > 0 && (i%2) == 0) 
                 TRACE_D3("\n                       ");
              TRACE_D3("[%#lx,%#lx] ", pcMin, pcMax );

              ML_(addVar)(
                 di, varp->level, 
                     pcMin, pcMax,
                     varp->name,  varp->typeR,
                     varp->gexpr, varp->fbGX,
                     varp->fndn_ix, varp->fLine, td3 
              );
           }
         }

         TRACE_D3("\n\n");
         /* and move on to the next var */
      }

      /* Now free all the TempVars */
      n = VG_(sizeXA)( tempvars );
      for (i = 0; i < n; i++) {
         varp = *(TempVar**)VG_(indexXA)( tempvars, i );
         ML_(dinfo_free)(varp);
      }
      VG_(deleteXA)( tempvars );
      tempvars = NULL;

      /* and the temp lookup table */
      VG_(deleteXA)( dioff_lookup_tab );

      /* and the ranges tree.  Note that we need to also free the XArrays
         which constitute the keys, hence pass VG_(deleteXA) as a
         key-finalizer. */
      VG_(deleteFM)( rangestree, (void(*)(UWord))VG_(deleteXA), NULL );

      /* and the tyents_to_keep cache */
      ML_(dinfo_free)( tyents_to_keep_cache );
      tyents_to_keep_cache = NULL;

      /* And the signatured type hash.  */
      VG_(HT_destruct) ( signature_types, ML_(dinfo_free) );

      /* record the GExprs in di so they can be freed later */
      vg_assert(!di->admin_gexprs);
      di->admin_gexprs = gexprs;
   }

   // Free up dynamically allocated memory
   if (VG_(clo_read_var_info)) {
      type_parser_release(&typarser);
      var_parser_release(&varparser);
   }
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The "new" DWARF3 reader -- top level control logic   ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

static Bool               d3rd_jmpbuf_valid  = False;
static const HChar*       d3rd_jmpbuf_reason = NULL;
static VG_MINIMAL_JMP_BUF(d3rd_jmpbuf);

static __attribute__((noreturn)) void barf ( const HChar* reason ) {
   vg_assert(d3rd_jmpbuf_valid);
   d3rd_jmpbuf_reason = reason;
   VG_MINIMAL_LONGJMP(d3rd_jmpbuf);
   /*NOTREACHED*/
   vg_assert(0);
}


void 
ML_(new_dwarf3_reader) (
   DebugInfo* di,
   DiSlice escn_debug_info,      DiSlice escn_debug_types,
   DiSlice escn_debug_abbv,      DiSlice escn_debug_line,
   DiSlice escn_debug_str,       DiSlice escn_debug_ranges,
   DiSlice escn_debug_rnglists,  DiSlice escn_debug_loclists,
   DiSlice escn_debug_loc,       DiSlice escn_debug_info_alt,
   DiSlice escn_debug_abbv_alt,  DiSlice escn_debug_line_alt,
   DiSlice escn_debug_str_alt,   DiSlice escn_debug_line_str,
   DiSlice escn_debug_addr,      DiSlice escn_debug_str_offsets
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
   jumped = VG_MINIMAL_SETJMP(d3rd_jmpbuf);
   if (jumped == 0) {
      /* try this ... */
      new_dwarf3_reader_wrk( di, barf,
                             escn_debug_info,     escn_debug_types,
                             escn_debug_abbv,     escn_debug_line,
                             escn_debug_str,      escn_debug_ranges,
                             escn_debug_rnglists, escn_debug_loclists,
                             escn_debug_loc,      escn_debug_info_alt,
                             escn_debug_abbv_alt, escn_debug_line_alt,
                             escn_debug_str_alt,  escn_debug_line_str,
                             escn_debug_addr,     escn_debug_str_offsets );
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

#endif // defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
