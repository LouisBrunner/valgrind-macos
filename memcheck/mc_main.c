/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- MemCheck: Maintain bitmaps of memory, tracking the           ---*/
/*--- accessibility (A) and validity (V) status of each byte.      ---*/
/*---                                                    mc_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_tool_basics.h"
#include "pub_tool_aspacemgr.h"
#include "pub_tool_gdbserver.h"
#include "pub_tool_poolalloc.h"
#include "pub_tool_hashtable.h"     // For mc_include.h
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_oset.h"
#include "pub_tool_rangemap.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_xarray.h"
#include "pub_tool_xtree.h"
#include "pub_tool_xtmemory.h"

#include "mc_include.h"
#include "memcheck.h"   /* for client requests */

/* Set to 1 to do a little more sanity checking */
#define VG_DEBUG_MEMORY 0

#define DEBUG(fmt, args...) //VG_(printf)(fmt, ## args)

static void ocache_sarp_Set_Origins ( Addr, UWord, UInt ); /* fwds */
static void ocache_sarp_Clear_Origins ( Addr, UWord ); /* fwds */


/*------------------------------------------------------------*/
/*--- Fast-case knobs                                      ---*/
/*------------------------------------------------------------*/

// Comment these out to disable the fast cases (don't just set them to zero).

/* PERF_FAST_LOADV is in mc_include.h */
#define PERF_FAST_STOREV   1

#define PERF_FAST_SARP     1

#define PERF_FAST_STACK    1
#define PERF_FAST_STACK2   1

/* Change this to 1 to enable assertions on origin tracking cache fast
   paths */
#define OC_ENABLE_ASSERTIONS 0

/* Change this to 1 for experimental, higher precision origin tracking
   8- and 16-bit store handling. */
#define OC_PRECISION_STORE 1


/*------------------------------------------------------------*/
/*--- Comments on the origin tracking implementation       ---*/
/*------------------------------------------------------------*/

/* See detailed comment entitled
   AN OVERVIEW OF THE ORIGIN TRACKING IMPLEMENTATION
   which is contained further on in this file. */


/*------------------------------------------------------------*/
/*--- V bits and A bits                                    ---*/
/*------------------------------------------------------------*/

/* Conceptually, every byte value has 8 V bits, which track whether Memcheck
   thinks the corresponding value bit is defined.  And every memory byte
   has an A bit, which tracks whether Memcheck thinks the program can access
   it safely (ie. it's mapped, and has at least one of the RWX permission bits
   set).  So every N-bit register is shadowed with N V bits, and every memory
   byte is shadowed with 8 V bits and one A bit.

   In the implementation, we use two forms of compression (compressed V bits
   and distinguished secondary maps) to avoid the 9-bit-per-byte overhead
   for memory.

   Memcheck also tracks extra information about each heap block that is
   allocated, for detecting memory leaks and other purposes.
*/

/*------------------------------------------------------------*/
/*--- Basic A/V bitmap representation.                     ---*/
/*------------------------------------------------------------*/

/* All reads and writes are checked against a memory map (a.k.a. shadow
   memory), which records the state of all memory in the process.

   On 32-bit machines the memory map is organised as follows.
   The top 16 bits of an address are used to index into a top-level
   map table, containing 65536 entries.  Each entry is a pointer to a
   second-level map, which records the accesibililty and validity
   permissions for the 65536 bytes indexed by the lower 16 bits of the
   address.  Each byte is represented by two bits (details are below).  So
   each second-level map contains 16384 bytes.  This two-level arrangement
   conveniently divides the 4G address space into 64k lumps, each size 64k
   bytes.

   All entries in the primary (top-level) map must point to a valid
   secondary (second-level) map.  Since many of the 64kB chunks will
   have the same status for every bit -- ie. noaccess (for unused
   address space) or entirely addressable and defined (for code segments) --
   there are three distinguished secondary maps, which indicate 'noaccess',
   'undefined' and 'defined'.  For these uniform 64kB chunks, the primary
   map entry points to the relevant distinguished map.  In practice,
   typically more than half of the addressable memory is represented with
   the 'undefined' or 'defined' distinguished secondary map, so it gives a
   good saving.  It also lets us set the V+A bits of large address regions
   quickly in set_address_range_perms().

   On 64-bit machines it's more complicated.  If we followed the same basic
   scheme we'd have a four-level table which would require too many memory
   accesses.  So instead the top-level map table has 2^20 entries (indexed
   using bits 16..35 of the address);  this covers the bottom 64GB.  Any
   accesses above 64GB are handled with a slow, sparse auxiliary table.
   Valgrind's address space manager tries very hard to keep things below
   this 64GB barrier so that performance doesn't suffer too much.

   Note that this file has a lot of different functions for reading and
   writing shadow memory.  Only a couple are strictly necessary (eg.
   get_vabits2 and set_vabits2), most are just specialised for specific
   common cases to improve performance.

   Aside: the V+A bits are less precise than they could be -- we have no way
   of marking memory as read-only.  It would be great if we could add an
   extra state VA_BITSn_READONLY.  But then we'd have 5 different states,
   which requires 2.3 bits to hold, and there's no way to do that elegantly
   -- we'd have to double up to 4 bits of metadata per byte, which doesn't
   seem worth it.
*/

/* --------------- Basic configuration --------------- */

/* Only change this.  N_PRIMARY_MAP *must* be a power of 2. */

#if VG_WORDSIZE == 4

/* cover the entire address space */
#  define N_PRIMARY_BITS  16

#else

/* Just handle the first 128G fast and the rest via auxiliary
   primaries.  If you change this, Memcheck will assert at startup.
   See the definition of UNALIGNED_OR_HIGH for extensive comments. */
#  define N_PRIMARY_BITS  21

#endif


/* Do not change this. */
#define N_PRIMARY_MAP  ( ((UWord)1) << N_PRIMARY_BITS)

/* Do not change this. */
#define MAX_PRIMARY_ADDRESS (Addr)((((Addr)65536) * N_PRIMARY_MAP)-1)


/* --------------- Secondary maps --------------- */

// Each byte of memory conceptually has an A bit, which indicates its
// addressability, and 8 V bits, which indicates its definedness.
//
// But because very few bytes are partially defined, we can use a nice
// compression scheme to reduce the size of shadow memory.  Each byte of
// memory has 2 bits which indicates its state (ie. V+A bits):
//
//   00:  noaccess    (unaddressable but treated as fully defined)
//   01:  undefined   (addressable and fully undefined)
//   10:  defined     (addressable and fully defined)
//   11:  partdefined (addressable and partially defined)
//
// In the "partdefined" case, we use a secondary table to store the V bits.
// Each entry in the secondary-V-bits table maps a byte address to its 8 V
// bits.
//
// We store the compressed V+A bits in 8-bit chunks, ie. the V+A bits for
// four bytes (32 bits) of memory are in each chunk.  Hence the name
// "vabits8".  This lets us get the V+A bits for four bytes at a time
// easily (without having to do any shifting and/or masking), and that is a
// very common operation.  (Note that although each vabits8 chunk
// is 8 bits in size, it represents 32 bits of memory.)
//
// The representation is "inverse" little-endian... each 4 bytes of
// memory is represented by a 1 byte value, where:
//
// - the status of byte (a+0) is held in bits [1..0]
// - the status of byte (a+1) is held in bits [3..2]
// - the status of byte (a+2) is held in bits [5..4]
// - the status of byte (a+3) is held in bits [7..6]
//
// It's "inverse" because endianness normally describes a mapping from
// value bits to memory addresses;  in this case the mapping is inverted.
// Ie. instead of particular value bits being held in certain addresses, in
// this case certain addresses are represented by particular value bits.
// See insert_vabits2_into_vabits8() for an example.
//
// But note that we don't compress the V bits stored in registers;  they
// need to be explicit to made the shadow operations possible.  Therefore
// when moving values between registers and memory we need to convert
// between the expanded in-register format and the compressed in-memory
// format.  This isn't so difficult, it just requires careful attention in a
// few places.

// These represent eight bits of memory.
#define VA_BITS2_NOACCESS     0x0      // 00b
#define VA_BITS2_UNDEFINED    0x1      // 01b
#define VA_BITS2_DEFINED      0x2      // 10b
#define VA_BITS2_PARTDEFINED  0x3      // 11b

// These represent 16 bits of memory.
#define VA_BITS4_NOACCESS     0x0      // 00_00b
#define VA_BITS4_UNDEFINED    0x5      // 01_01b
#define VA_BITS4_DEFINED      0xa      // 10_10b

// These represent 32 bits of memory.
#define VA_BITS8_NOACCESS     0x00     // 00_00_00_00b
#define VA_BITS8_UNDEFINED    0x55     // 01_01_01_01b
#define VA_BITS8_DEFINED      0xaa     // 10_10_10_10b

// These represent 64 bits of memory.
#define VA_BITS16_NOACCESS    0x0000   // 00_00_00_00b x 2
#define VA_BITS16_UNDEFINED   0x5555   // 01_01_01_01b x 2
#define VA_BITS16_DEFINED     0xaaaa   // 10_10_10_10b x 2

// These represent 128 bits of memory.
#define VA_BITS32_UNDEFINED   0x55555555  // 01_01_01_01b x 4


#define SM_CHUNKS             16384    // Each SM covers 64k of memory.
#define SM_OFF(aaa)           (((aaa) & 0xffff) >> 2)
#define SM_OFF_16(aaa)        (((aaa) & 0xffff) >> 3)

// Paranoia:  it's critical for performance that the requested inlining
// occurs.  So try extra hard.
#define INLINE    inline __attribute__((always_inline))

static INLINE Addr start_of_this_sm ( Addr a ) {
   return (a & (~SM_MASK));
}
static INLINE Bool is_start_of_sm ( Addr a ) {
   return (start_of_this_sm(a) == a);
}

STATIC_ASSERT(SM_CHUNKS % 2 == 0);

typedef
   union {
      UChar vabits8[SM_CHUNKS];
      UShort vabits16[SM_CHUNKS/2];
   }
   SecMap;

// 3 distinguished secondary maps, one for no-access, one for
// accessible but undefined, and one for accessible and defined.
// Distinguished secondaries may never be modified.
#define SM_DIST_NOACCESS   0
#define SM_DIST_UNDEFINED  1
#define SM_DIST_DEFINED    2

static SecMap sm_distinguished[3];

static INLINE Bool is_distinguished_sm ( SecMap* sm ) {
   return sm >= &sm_distinguished[0] && sm <= &sm_distinguished[2];
}

// Forward declaration
static void update_SM_counts(SecMap* oldSM, SecMap* newSM);

/* dist_sm points to one of our three distinguished secondaries.  Make
   a copy of it so that we can write to it.
*/
static SecMap* copy_for_writing ( SecMap* dist_sm )
{
   SecMap* new_sm;
   tl_assert(dist_sm == &sm_distinguished[0]
          || dist_sm == &sm_distinguished[1]
          || dist_sm == &sm_distinguished[2]);

   SysRes sres = VG_(am_shadow_alloc)(sizeof(SecMap));
   if (sr_isError(sres))
      VG_(out_of_memory_NORETURN)( "memcheck:allocate new SecMap",
                                   sizeof(SecMap), sr_Err(sres) );
   new_sm = (void *)(Addr)sr_Res(sres);
   VG_(memcpy)(new_sm, dist_sm, sizeof(SecMap));
   update_SM_counts(dist_sm, new_sm);
   return new_sm;
}

/* --------------- Stats --------------- */

static Int   n_issued_SMs      = 0;
static Int   n_deissued_SMs    = 0;
static Int   n_noaccess_SMs    = N_PRIMARY_MAP; // start with many noaccess DSMs
static Int   n_undefined_SMs   = 0;
static Int   n_defined_SMs     = 0;
static Int   n_non_DSM_SMs     = 0;
static Int   max_noaccess_SMs  = 0;
static Int   max_undefined_SMs = 0;
static Int   max_defined_SMs   = 0;
static Int   max_non_DSM_SMs   = 0;

/* # searches initiated in auxmap_L1, and # base cmps required */
static ULong n_auxmap_L1_searches  = 0;
static ULong n_auxmap_L1_cmps      = 0;
/* # of searches that missed in auxmap_L1 and therefore had to
   be handed to auxmap_L2. And the number of nodes inserted. */
static ULong n_auxmap_L2_searches  = 0;
static ULong n_auxmap_L2_nodes     = 0;

static Int   n_sanity_cheap     = 0;
static Int   n_sanity_expensive = 0;

static Int   n_secVBit_nodes   = 0;
static Int   max_secVBit_nodes = 0;

static void update_SM_counts(SecMap* oldSM, SecMap* newSM)
{
   if      (oldSM == &sm_distinguished[SM_DIST_NOACCESS ]) n_noaccess_SMs --;
   else if (oldSM == &sm_distinguished[SM_DIST_UNDEFINED]) n_undefined_SMs--;
   else if (oldSM == &sm_distinguished[SM_DIST_DEFINED  ]) n_defined_SMs  --;
   else                                                  { n_non_DSM_SMs  --;
                                                           n_deissued_SMs ++; }

   if      (newSM == &sm_distinguished[SM_DIST_NOACCESS ]) n_noaccess_SMs ++;
   else if (newSM == &sm_distinguished[SM_DIST_UNDEFINED]) n_undefined_SMs++;
   else if (newSM == &sm_distinguished[SM_DIST_DEFINED  ]) n_defined_SMs  ++;
   else                                                  { n_non_DSM_SMs  ++;
                                                           n_issued_SMs   ++; }

   if (n_noaccess_SMs  > max_noaccess_SMs ) max_noaccess_SMs  = n_noaccess_SMs;
   if (n_undefined_SMs > max_undefined_SMs) max_undefined_SMs = n_undefined_SMs;
   if (n_defined_SMs   > max_defined_SMs  ) max_defined_SMs   = n_defined_SMs;
   if (n_non_DSM_SMs   > max_non_DSM_SMs  ) max_non_DSM_SMs   = n_non_DSM_SMs;
}

/* --------------- Primary maps --------------- */

/* The main primary map.  This covers some initial part of the address
   space, addresses 0 .. (N_PRIMARY_MAP << 16)-1.  The rest of it is
   handled using the auxiliary primary map.
*/
#if ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
    && (defined(VGP_arm_linux) \
        || defined(VGP_x86_linux) || defined(VGP_x86_solaris) || defined(VGP_x86_freebsd))
/* mc_main_asm.c needs visibility on a few things declared in this file.
   MC_MAIN_STATIC allows to define them static if ok, i.e. on
   platforms that are not using hand-coded asm statements. */
#define MC_MAIN_STATIC
#else
#define MC_MAIN_STATIC static
#endif
MC_MAIN_STATIC SecMap* primary_map[N_PRIMARY_MAP];


/* An entry in the auxiliary primary map.  base must be a 64k-aligned
   value, and sm points at the relevant secondary map.  As with the
   main primary map, the secondary may be either a real secondary, or
   one of the three distinguished secondaries.  DO NOT CHANGE THIS
   LAYOUT: the first word has to be the key for OSet fast lookups.
*/
typedef
   struct {
      Addr    base;
      SecMap* sm;
   }
   AuxMapEnt;

/* Tunable parameter: How big is the L1 queue? */
#define N_AUXMAP_L1 24

/* Tunable parameter: How far along the L1 queue to insert
   entries resulting from L2 lookups? */
#define AUXMAP_L1_INSERT_IX 12

static struct {
          Addr       base;
          AuxMapEnt* ent; // pointer to the matching auxmap_L2 node
       }
       auxmap_L1[N_AUXMAP_L1];

static OSet* auxmap_L2 = NULL;

static void init_auxmap_L1_L2 ( void )
{
   Int i;
   for (i = 0; i < N_AUXMAP_L1; i++) {
      auxmap_L1[i].base = 0;
      auxmap_L1[i].ent  = NULL;
   }

   tl_assert(0 == offsetof(AuxMapEnt,base));
   tl_assert(sizeof(Addr) == sizeof(void*));
   auxmap_L2 = VG_(OSetGen_Create)( /*keyOff*/  offsetof(AuxMapEnt,base),
                                    /*fastCmp*/ NULL,
                                    VG_(malloc), "mc.iaLL.1", VG_(free) );
}

/* Check representation invariants; if OK return NULL; else a
   descriptive bit of text.  Also return the number of
   non-distinguished secondary maps referred to from the auxiliary
   primary maps. */

static const HChar* check_auxmap_L1_L2_sanity ( Word* n_secmaps_found )
{
   Word i, j;
   /* On a 32-bit platform, the L2 and L1 tables should
      both remain empty forever.

      On a 64-bit platform:
      In the L2 table:
       all .base & 0xFFFF == 0
       all .base > MAX_PRIMARY_ADDRESS
      In the L1 table:
       all .base & 0xFFFF == 0
       all (.base > MAX_PRIMARY_ADDRESS
            .base & 0xFFFF == 0
            and .ent points to an AuxMapEnt with the same .base)
           or
           (.base == 0 and .ent == NULL)
   */
   *n_secmaps_found = 0;
   if (sizeof(void*) == 4) {
      /* 32-bit platform */
      if (VG_(OSetGen_Size)(auxmap_L2) != 0)
         return "32-bit: auxmap_L2 is non-empty";
      for (i = 0; i < N_AUXMAP_L1; i++)
        if (auxmap_L1[i].base != 0 || auxmap_L1[i].ent != NULL)
      return "32-bit: auxmap_L1 is non-empty";
   } else {
      /* 64-bit platform */
      UWord elems_seen = 0;
      AuxMapEnt *elem, *res;
      AuxMapEnt key;
      /* L2 table */
      VG_(OSetGen_ResetIter)(auxmap_L2);
      while ( (elem = VG_(OSetGen_Next)(auxmap_L2)) ) {
         elems_seen++;
         if (0 != (elem->base & (Addr)0xFFFF))
            return "64-bit: nonzero .base & 0xFFFF in auxmap_L2";
         if (elem->base <= MAX_PRIMARY_ADDRESS)
            return "64-bit: .base <= MAX_PRIMARY_ADDRESS in auxmap_L2";
         if (elem->sm == NULL)
            return "64-bit: .sm in _L2 is NULL";
         if (!is_distinguished_sm(elem->sm))
            (*n_secmaps_found)++;
      }
      if (elems_seen != n_auxmap_L2_nodes)
         return "64-bit: disagreement on number of elems in _L2";
      /* Check L1-L2 correspondence */
      for (i = 0; i < N_AUXMAP_L1; i++) {
         if (auxmap_L1[i].base == 0 && auxmap_L1[i].ent == NULL)
            continue;
         if (0 != (auxmap_L1[i].base & (Addr)0xFFFF))
            return "64-bit: nonzero .base & 0xFFFF in auxmap_L1";
         if (auxmap_L1[i].base <= MAX_PRIMARY_ADDRESS)
            return "64-bit: .base <= MAX_PRIMARY_ADDRESS in auxmap_L1";
         if (auxmap_L1[i].ent == NULL)
            return "64-bit: .ent is NULL in auxmap_L1";
         if (auxmap_L1[i].ent->base != auxmap_L1[i].base)
            return "64-bit: _L1 and _L2 bases are inconsistent";
         /* Look it up in auxmap_L2. */
         key.base = auxmap_L1[i].base;
         key.sm   = 0;
         res = VG_(OSetGen_Lookup)(auxmap_L2, &key);
         if (res == NULL)
            return "64-bit: _L1 .base not found in _L2";
         if (res != auxmap_L1[i].ent)
            return "64-bit: _L1 .ent disagrees with _L2 entry";
      }
      /* Check L1 contains no duplicates */
      for (i = 0; i < N_AUXMAP_L1; i++) {
         if (auxmap_L1[i].base == 0)
            continue;
	 for (j = i+1; j < N_AUXMAP_L1; j++) {
            if (auxmap_L1[j].base == 0)
               continue;
            if (auxmap_L1[j].base == auxmap_L1[i].base)
               return "64-bit: duplicate _L1 .base entries";
         }
      }
   }
   return NULL; /* ok */
}

static void insert_into_auxmap_L1_at ( Word rank, AuxMapEnt* ent )
{
   Word i;
   tl_assert(ent);
   tl_assert(rank >= 0 && rank < N_AUXMAP_L1);
   for (i = N_AUXMAP_L1-1; i > rank; i--)
      auxmap_L1[i] = auxmap_L1[i-1];
   auxmap_L1[rank].base = ent->base;
   auxmap_L1[rank].ent  = ent;
}

static INLINE AuxMapEnt* maybe_find_in_auxmap ( Addr a )
{
   AuxMapEnt  key;
   AuxMapEnt* res;
   Word       i;

   tl_assert(a > MAX_PRIMARY_ADDRESS);
   a &= ~(Addr)0xFFFF;

   /* First search the front-cache, which is a self-organising
      list containing the most popular entries. */

   if (LIKELY(auxmap_L1[0].base == a))
      return auxmap_L1[0].ent;
   if (LIKELY(auxmap_L1[1].base == a)) {
      Addr       t_base = auxmap_L1[0].base;
      AuxMapEnt* t_ent  = auxmap_L1[0].ent;
      auxmap_L1[0].base = auxmap_L1[1].base;
      auxmap_L1[0].ent  = auxmap_L1[1].ent;
      auxmap_L1[1].base = t_base;
      auxmap_L1[1].ent  = t_ent;
      return auxmap_L1[0].ent;
   }

   n_auxmap_L1_searches++;

   for (i = 0; i < N_AUXMAP_L1; i++) {
      if (auxmap_L1[i].base == a) {
         break;
      }
   }
   tl_assert(i >= 0 && i <= N_AUXMAP_L1);

   n_auxmap_L1_cmps += (ULong)(i+1);

   if (i < N_AUXMAP_L1) {
      if (i > 0) {
         Addr       t_base = auxmap_L1[i-1].base;
         AuxMapEnt* t_ent  = auxmap_L1[i-1].ent;
         auxmap_L1[i-1].base = auxmap_L1[i-0].base;
         auxmap_L1[i-1].ent  = auxmap_L1[i-0].ent;
         auxmap_L1[i-0].base = t_base;
         auxmap_L1[i-0].ent  = t_ent;
         i--;
      }
      return auxmap_L1[i].ent;
   }

   n_auxmap_L2_searches++;

   /* First see if we already have it. */
   key.base = a;
   key.sm   = 0;

   res = VG_(OSetGen_Lookup)(auxmap_L2, &key);
   if (res)
      insert_into_auxmap_L1_at( AUXMAP_L1_INSERT_IX, res );
   return res;
}

static AuxMapEnt* find_or_alloc_in_auxmap ( Addr a )
{
   AuxMapEnt *nyu, *res;

   /* First see if we already have it. */
   res = maybe_find_in_auxmap( a );
   if (LIKELY(res))
      return res;

   /* Ok, there's no entry in the secondary map, so we'll have
      to allocate one. */
   a &= ~(Addr)0xFFFF;

   nyu = (AuxMapEnt*) VG_(OSetGen_AllocNode)( auxmap_L2, sizeof(AuxMapEnt) );
   nyu->base = a;
   nyu->sm   = &sm_distinguished[SM_DIST_NOACCESS];
   VG_(OSetGen_Insert)( auxmap_L2, nyu );
   insert_into_auxmap_L1_at( AUXMAP_L1_INSERT_IX, nyu );
   n_auxmap_L2_nodes++;
   return nyu;
}

/* --------------- SecMap fundamentals --------------- */

// In all these, 'low' means it's definitely in the main primary map,
// 'high' means it's definitely in the auxiliary table.

static INLINE UWord get_primary_map_low_offset ( Addr a )
{
  UWord pm_off = a >> 16;
  return pm_off;
}

static INLINE SecMap** get_secmap_low_ptr ( Addr a )
{
   UWord pm_off = a >> 16;
#  if VG_DEBUG_MEMORY >= 1
   tl_assert(pm_off < N_PRIMARY_MAP);
#  endif
   return &primary_map[ pm_off ];
}

static INLINE SecMap** get_secmap_high_ptr ( Addr a )
{
   AuxMapEnt* am = find_or_alloc_in_auxmap(a);
   return &am->sm;
}

static INLINE SecMap** get_secmap_ptr ( Addr a )
{
   return ( a <= MAX_PRIMARY_ADDRESS
          ? get_secmap_low_ptr(a)
          : get_secmap_high_ptr(a));
}

static INLINE SecMap* get_secmap_for_reading_low ( Addr a )
{
   return *get_secmap_low_ptr(a);
}

static INLINE SecMap* get_secmap_for_reading_high ( Addr a )
{
   return *get_secmap_high_ptr(a);
}

static INLINE SecMap* get_secmap_for_writing_low(Addr a)
{
   SecMap** p = get_secmap_low_ptr(a);
   if (UNLIKELY(is_distinguished_sm(*p)))
      *p = copy_for_writing(*p);
   return *p;
}

static INLINE SecMap* get_secmap_for_writing_high ( Addr a )
{
   SecMap** p = get_secmap_high_ptr(a);
   if (UNLIKELY(is_distinguished_sm(*p)))
      *p = copy_for_writing(*p);
   return *p;
}

/* Produce the secmap for 'a', either from the primary map or by
   ensuring there is an entry for it in the aux primary map.  The
   secmap may be a distinguished one as the caller will only want to
   be able to read it.
*/
static INLINE SecMap* get_secmap_for_reading ( Addr a )
{
   return ( a <= MAX_PRIMARY_ADDRESS
          ? get_secmap_for_reading_low (a)
          : get_secmap_for_reading_high(a) );
}

/* Produce the secmap for 'a', either from the primary map or by
   ensuring there is an entry for it in the aux primary map.  The
   secmap may not be a distinguished one, since the caller will want
   to be able to write it.  If it is a distinguished secondary, make a
   writable copy of it, install it, and return the copy instead.  (COW
   semantics).
*/
static INLINE SecMap* get_secmap_for_writing ( Addr a )
{
   return ( a <= MAX_PRIMARY_ADDRESS
          ? get_secmap_for_writing_low (a)
          : get_secmap_for_writing_high(a) );
}

/* If 'a' has a SecMap, produce it.  Else produce NULL.  But don't
   allocate one if one doesn't already exist.  This is used by the
   leak checker.
*/
static SecMap* maybe_get_secmap_for ( Addr a )
{
   if (a <= MAX_PRIMARY_ADDRESS) {
      return get_secmap_for_reading_low(a);
   } else {
      AuxMapEnt* am = maybe_find_in_auxmap(a);
      return am ? am->sm : NULL;
   }
}

/* --------------- Fundamental functions --------------- */

static INLINE
void insert_vabits2_into_vabits8 ( Addr a, UChar vabits2, UChar* vabits8 )
{
   UInt shift =  (a & 3)  << 1;        // shift by 0, 2, 4, or 6
   *vabits8  &= ~(0x3     << shift);   // mask out the two old bits
   *vabits8  |=  (vabits2 << shift);   // mask  in the two new bits
}

static INLINE
void insert_vabits4_into_vabits8 ( Addr a, UChar vabits4, UChar* vabits8 )
{
   UInt shift;
   tl_assert(VG_IS_2_ALIGNED(a));      // Must be 2-aligned
   shift     =  (a & 2)   << 1;        // shift by 0 or 4
   *vabits8 &= ~(0xf      << shift);   // mask out the four old bits
   *vabits8 |=  (vabits4 << shift);    // mask  in the four new bits
}

static INLINE
UChar extract_vabits2_from_vabits8 ( Addr a, UChar vabits8 )
{
   UInt shift = (a & 3) << 1;          // shift by 0, 2, 4, or 6
   vabits8 >>= shift;                  // shift the two bits to the bottom
   return 0x3 & vabits8;               // mask out the rest
}

static INLINE
UChar extract_vabits4_from_vabits8 ( Addr a, UChar vabits8 )
{
   UInt shift;
   tl_assert(VG_IS_2_ALIGNED(a));      // Must be 2-aligned
   shift = (a & 2) << 1;               // shift by 0 or 4
   vabits8 >>= shift;                  // shift the four bits to the bottom
   return 0xf & vabits8;               // mask out the rest
}

// Note that these four are only used in slow cases.  The fast cases do
// clever things like combine the auxmap check (in
// get_secmap_{read,writ}able) with alignment checks.

// *** WARNING! ***
// Any time this function is called, if it is possible that vabits2
// is equal to VA_BITS2_PARTDEFINED, then the corresponding entry in the
// sec-V-bits table must also be set!
static INLINE
void set_vabits2 ( Addr a, UChar vabits2 )
{
   SecMap* sm       = get_secmap_for_writing(a);
   UWord   sm_off   = SM_OFF(a);
   insert_vabits2_into_vabits8( a, vabits2, &(sm->vabits8[sm_off]) );
}

static INLINE
UChar get_vabits2 ( Addr a )
{
   SecMap* sm       = get_secmap_for_reading(a);
   UWord   sm_off   = SM_OFF(a);
   UChar   vabits8  = sm->vabits8[sm_off];
   return extract_vabits2_from_vabits8(a, vabits8);
}

// *** WARNING! ***
// Any time this function is called, if it is possible that any of the
// 4 2-bit fields in vabits8 are equal to VA_BITS2_PARTDEFINED, then the
// corresponding entry(s) in the sec-V-bits table must also be set!
static INLINE
UChar get_vabits8_for_aligned_word32 ( Addr a )
{
   SecMap* sm       = get_secmap_for_reading(a);
   UWord   sm_off   = SM_OFF(a);
   UChar   vabits8  = sm->vabits8[sm_off];
   return vabits8;
}

static INLINE
void set_vabits8_for_aligned_word32 ( Addr a, UChar vabits8 )
{
   SecMap* sm       = get_secmap_for_writing(a);
   UWord   sm_off   = SM_OFF(a);
   sm->vabits8[sm_off] = vabits8;
}


// Forward declarations
static UWord get_sec_vbits8(Addr a);
static void  set_sec_vbits8(Addr a, UWord vbits8);

// Returns False if there was an addressability error.
static INLINE
Bool set_vbits8 ( Addr a, UChar vbits8 )
{
   Bool  ok      = True;
   UChar vabits2 = get_vabits2(a);
   if ( VA_BITS2_NOACCESS != vabits2 ) {
      // Addressable.  Convert in-register format to in-memory format.
      // Also remove any existing sec V bit entry for the byte if no
      // longer necessary.
      if      ( V_BITS8_DEFINED   == vbits8 ) { vabits2 = VA_BITS2_DEFINED;   }
      else if ( V_BITS8_UNDEFINED == vbits8 ) { vabits2 = VA_BITS2_UNDEFINED; }
      else                                    { vabits2 = VA_BITS2_PARTDEFINED;
                                                set_sec_vbits8(a, vbits8);  }
      set_vabits2(a, vabits2);

   } else {
      // Unaddressable!  Do nothing -- when writing to unaddressable
      // memory it acts as a black hole, and the V bits can never be seen
      // again.  So we don't have to write them at all.
      ok = False;
   }
   return ok;
}

// Returns False if there was an addressability error.  In that case, we put
// all defined bits into vbits8.
static INLINE
Bool get_vbits8 ( Addr a, UChar* vbits8 )
{
   Bool  ok      = True;
   UChar vabits2 = get_vabits2(a);

   // Convert the in-memory format to in-register format.
   if      ( VA_BITS2_DEFINED   == vabits2 ) { *vbits8 = V_BITS8_DEFINED;   }
   else if ( VA_BITS2_UNDEFINED == vabits2 ) { *vbits8 = V_BITS8_UNDEFINED; }
   else if ( VA_BITS2_NOACCESS  == vabits2 ) {
      *vbits8 = V_BITS8_DEFINED;    // Make V bits defined!
      ok = False;
   } else {
      tl_assert( VA_BITS2_PARTDEFINED == vabits2 );
      *vbits8 = get_sec_vbits8(a);
   }
   return ok;
}


/* --------------- Secondary V bit table ------------ */

// This table holds the full V bit pattern for partially-defined bytes
// (PDBs) that are represented by VA_BITS2_PARTDEFINED in the main shadow
// memory.
//
// Note: the nodes in this table can become stale.  Eg. if you write a PDB,
// then overwrite the same address with a fully defined byte, the sec-V-bit
// node will not necessarily be removed.  This is because checking for
// whether removal is necessary would slow down the fast paths.
//
// To avoid the stale nodes building up too much, we periodically (once the
// table reaches a certain size) garbage collect (GC) the table by
// traversing it and evicting any nodes not having PDB.
// If more than a certain proportion of nodes survived, we increase the
// table size so that GCs occur less often.
//
// This policy is designed to avoid bad table bloat in the worst case where
// a program creates huge numbers of stale PDBs -- we would get this bloat
// if we had no GC -- while handling well the case where a node becomes
// stale but shortly afterwards is rewritten with a PDB and so becomes
// non-stale again (which happens quite often, eg. in perf/bz2).  If we just
// remove all stale nodes as soon as possible, we just end up re-adding a
// lot of them in later again.  The "sufficiently stale" approach avoids
// this.  (If a program has many live PDBs, performance will just suck,
// there's no way around that.)
//
// Further comments, JRS 14 Feb 2012.  It turns out that the policy of
// holding on to stale entries for 2 GCs before discarding them can lead
// to massive space leaks.  So we're changing to an arrangement where
// lines are evicted as soon as they are observed to be stale during a
// GC.  This also has a side benefit of allowing the sufficiently_stale
// field to be removed from the SecVBitNode struct, reducing its size by
// 8 bytes, which is a substantial space saving considering that the
// struct was previously 32 or so bytes, on a 64 bit target.
//
// In order to try and mitigate the problem that the "sufficiently stale"
// heuristic was designed to avoid, the table size is allowed to drift
// up ("DRIFTUP") slowly to 80000, even if the residency is low.  This
// means that nodes will exist in the table longer on average, and hopefully
// will be deleted and re-added less frequently.
//
// The previous scaling up mechanism (now called STEPUP) is retained:
// if residency exceeds 50%, the table is scaled up, although by a
// factor sqrt(2) rather than 2 as before.  This effectively doubles the
// frequency of GCs when there are many PDBs at reduces the tendency of
// stale PDBs to reside for long periods in the table.

static OSet* secVBitTable;

// Stats
static ULong sec_vbits_new_nodes = 0;
static ULong sec_vbits_updates   = 0;

// This must be a power of two;  this is checked in mc_pre_clo_init().
// The size chosen here is a trade-off:  if the nodes are bigger (ie. cover
// a larger address range) they take more space but we can get multiple
// partially-defined bytes in one if they are close to each other, reducing
// the number of total nodes.  In practice sometimes they are clustered (eg.
// perf/bz2 repeatedly writes then reads more than 20,000 in a contiguous
// row), but often not.  So we choose something intermediate.
#define BYTES_PER_SEC_VBIT_NODE     16

// We make the table bigger by a factor of STEPUP_GROWTH_FACTOR if
// more than this many nodes survive a GC.
#define STEPUP_SURVIVOR_PROPORTION  0.5
#define STEPUP_GROWTH_FACTOR        1.414213562

// If the above heuristic doesn't apply, then we may make the table
// slightly bigger, by a factor of DRIFTUP_GROWTH_FACTOR, if more than
// this many nodes survive a GC, _and_ the total table size does
// not exceed a fixed limit.  The numbers are somewhat arbitrary, but
// work tolerably well on long Firefox runs.  The scaleup ratio of 1.5%
// effectively although gradually reduces residency and increases time
// between GCs for programs with small numbers of PDBs.  The 80000 limit
// effectively limits the table size to around 2MB for programs with
// small numbers of PDBs, whilst giving a reasonably long lifetime to
// entries, to try and reduce the costs resulting from deleting and
// re-adding of entries.
#define DRIFTUP_SURVIVOR_PROPORTION 0.15
#define DRIFTUP_GROWTH_FACTOR       1.015
#define DRIFTUP_MAX_SIZE            80000

// We GC the table when it gets this many nodes in it, ie. it's effectively
// the table size.  It can change.
static Int  secVBitLimit = 1000;

// The number of GCs done, used to age sec-V-bit nodes for eviction.
// Because it's unsigned, wrapping doesn't matter -- the right answer will
// come out anyway.
static UInt GCs_done = 0;

typedef
   struct {
      Addr  a;
      UChar vbits8[BYTES_PER_SEC_VBIT_NODE];
   }
   SecVBitNode;

static OSet* createSecVBitTable(void)
{
   OSet* newSecVBitTable;
   newSecVBitTable = VG_(OSetGen_Create_With_Pool)
      ( offsetof(SecVBitNode, a),
        NULL, // use fast comparisons
        VG_(malloc), "mc.cSVT.1 (sec VBit table)",
        VG_(free),
        1000,
        sizeof(SecVBitNode));
   return newSecVBitTable;
}

static void gcSecVBitTable(void)
{
   OSet*        secVBitTable2;
   SecVBitNode* n;
   Int          i, n_nodes = 0, n_survivors = 0;

   GCs_done++;

   // Create the new table.
   secVBitTable2 = createSecVBitTable();

   // Traverse the table, moving fresh nodes into the new table.
   VG_(OSetGen_ResetIter)(secVBitTable);
   while ( (n = VG_(OSetGen_Next)(secVBitTable)) ) {
      // Keep node if any of its bytes are non-stale.  Using
      // get_vabits2() for the lookup is not very efficient, but I don't
      // think it matters.
      for (i = 0; i < BYTES_PER_SEC_VBIT_NODE; i++) {
         if (VA_BITS2_PARTDEFINED == get_vabits2(n->a + i)) {
            // Found a non-stale byte, so keep =>
            // Insert a copy of the node into the new table.
            SecVBitNode* n2 =
               VG_(OSetGen_AllocNode)(secVBitTable2, sizeof(SecVBitNode));
            *n2 = *n;
            VG_(OSetGen_Insert)(secVBitTable2, n2);
            break;
         }
      }
   }

   // Get the before and after sizes.
   n_nodes     = VG_(OSetGen_Size)(secVBitTable);
   n_survivors = VG_(OSetGen_Size)(secVBitTable2);

   // Destroy the old table, and put the new one in its place.
   VG_(OSetGen_Destroy)(secVBitTable);
   secVBitTable = secVBitTable2;

   if (VG_(clo_verbosity) > 1 && n_nodes != 0) {
      VG_(message)(Vg_DebugMsg, "memcheck GC: %d nodes, %d survivors (%.1f%%)\n",
                   n_nodes, n_survivors, n_survivors * 100.0 / n_nodes);
   }

   // Increase table size if necessary.
   if ((Double)n_survivors
       > ((Double)secVBitLimit * STEPUP_SURVIVOR_PROPORTION)) {
      secVBitLimit = (Int)((Double)secVBitLimit * (Double)STEPUP_GROWTH_FACTOR);
      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg,
                      "memcheck GC: %d new table size (stepup)\n",
                      secVBitLimit);
   }
   else
   if (secVBitLimit < DRIFTUP_MAX_SIZE
       && (Double)n_survivors
          > ((Double)secVBitLimit * DRIFTUP_SURVIVOR_PROPORTION)) {
      secVBitLimit = (Int)((Double)secVBitLimit * (Double)DRIFTUP_GROWTH_FACTOR);
      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg,
                      "memcheck GC: %d new table size (driftup)\n",
                      secVBitLimit);
   }
}

static UWord get_sec_vbits8(Addr a)
{
   Addr         aAligned = VG_ROUNDDN(a, BYTES_PER_SEC_VBIT_NODE);
   Int          amod     = a % BYTES_PER_SEC_VBIT_NODE;
   SecVBitNode* n        = VG_(OSetGen_Lookup)(secVBitTable, &aAligned);
   UChar        vbits8;
   tl_assert2(n, "get_sec_vbits8: no node for address %p (%p)\n", aAligned, a);
   // Shouldn't be fully defined or fully undefined -- those cases shouldn't
   // make it to the secondary V bits table.
   vbits8 = n->vbits8[amod];
   tl_assert(V_BITS8_DEFINED != vbits8 && V_BITS8_UNDEFINED != vbits8);
   return vbits8;
}

static void set_sec_vbits8(Addr a, UWord vbits8)
{
   Addr         aAligned = VG_ROUNDDN(a, BYTES_PER_SEC_VBIT_NODE);
   Int          i, amod  = a % BYTES_PER_SEC_VBIT_NODE;
   SecVBitNode* n        = VG_(OSetGen_Lookup)(secVBitTable, &aAligned);
   // Shouldn't be fully defined or fully undefined -- those cases shouldn't
   // make it to the secondary V bits table.
   tl_assert(V_BITS8_DEFINED != vbits8 && V_BITS8_UNDEFINED != vbits8);
   if (n) {
      n->vbits8[amod] = vbits8;     // update
      sec_vbits_updates++;
   } else {
      // Do a table GC if necessary.  Nb: do this before creating and
      // inserting the new node, to avoid erroneously GC'ing the new node.
      if (secVBitLimit == VG_(OSetGen_Size)(secVBitTable)) {
         gcSecVBitTable();
      }

      // New node:  assign the specific byte, make the rest invalid (they
      // should never be read as-is, but be cautious).
      n = VG_(OSetGen_AllocNode)(secVBitTable, sizeof(SecVBitNode));
      n->a            = aAligned;
      for (i = 0; i < BYTES_PER_SEC_VBIT_NODE; i++) {
         n->vbits8[i] = V_BITS8_UNDEFINED;
      }
      n->vbits8[amod] = vbits8;

      // Insert the new node.
      VG_(OSetGen_Insert)(secVBitTable, n);
      sec_vbits_new_nodes++;

      n_secVBit_nodes = VG_(OSetGen_Size)(secVBitTable);
      if (n_secVBit_nodes > max_secVBit_nodes)
         max_secVBit_nodes = n_secVBit_nodes;
   }
}

/* --------------- Endianness helpers --------------- */

/* Returns the offset in memory of the byteno-th most significant byte
   in a wordszB-sized word, given the specified endianness. */
static INLINE UWord byte_offset_w ( UWord wordszB, Bool bigendian,
                                    UWord byteno ) {
   return bigendian ? (wordszB-1-byteno) : byteno;
}


/* --------------- Ignored address ranges --------------- */

/* Denotes the address-error-reportability status for address ranges:
   IAR_NotIgnored:  the usual case -- report errors in this range
   IAR_CommandLine: don't report errors -- from command line setting
   IAR_ClientReq:   don't report errors -- from client request
*/
typedef
   enum { IAR_INVALID=99,
          IAR_NotIgnored,
          IAR_CommandLine,
          IAR_ClientReq }
   IARKind;

static const HChar* showIARKind ( IARKind iark )
{
   switch (iark) {
      case IAR_INVALID:     return "INVALID";
      case IAR_NotIgnored:  return "NotIgnored";
      case IAR_CommandLine: return "CommandLine";
      case IAR_ClientReq:   return "ClientReq";
      default:              return "???";
   }
}

// RangeMap<IARKind>
static RangeMap* gIgnoredAddressRanges = NULL;

static void init_gIgnoredAddressRanges ( void )
{
   if (LIKELY(gIgnoredAddressRanges != NULL))
      return;
   gIgnoredAddressRanges = VG_(newRangeMap)( VG_(malloc), "mc.igIAR.1",
                                             VG_(free), IAR_NotIgnored );
}

Bool MC_(in_ignored_range) ( Addr a )
{
   if (LIKELY(gIgnoredAddressRanges == NULL))
      return False;
   UWord how     = IAR_INVALID;
   UWord key_min = ~(UWord)0;
   UWord key_max =  (UWord)0;
   VG_(lookupRangeMap)(&key_min, &key_max, &how, gIgnoredAddressRanges, a);
   tl_assert(key_min <= a && a <= key_max);
   switch (how) {
      case IAR_NotIgnored:  return False;
      case IAR_CommandLine: return True;
      case IAR_ClientReq:   return True;
      default: break; /* invalid */
   }
   VG_(tool_panic)("MC_(in_ignore_range)");
   /*NOTREACHED*/
}

Bool MC_(in_ignored_range_below_sp) ( Addr sp, Addr a, UInt szB )
{
   if (LIKELY(!MC_(clo_ignore_range_below_sp)))
       return False;
   tl_assert(szB >= 1 && szB <= 32);
   tl_assert(MC_(clo_ignore_range_below_sp__first_offset)
             > MC_(clo_ignore_range_below_sp__last_offset));
   Addr range_lo = sp - MC_(clo_ignore_range_below_sp__first_offset);
   Addr range_hi = sp - MC_(clo_ignore_range_below_sp__last_offset);
   if (range_lo >= range_hi) {
      /* Bizarre.  We have a wraparound situation.  What should we do? */
      return False; // Play safe
   } else {
      /* This is the expected case. */
      if (range_lo <= a && a + szB - 1 <= range_hi)
         return True;
      else
         return False;
   }
   /*NOTREACHED*/
   tl_assert(0);
}

/* Parse two Addrs (in hex) separated by a dash, or fail. */

static Bool parse_Addr_pair ( const HChar** ppc, Addr* result1, Addr* result2 )
{
   Bool ok = VG_(parse_Addr) (ppc, result1);
   if (!ok)
      return False;
   if (**ppc != '-')
      return False;
   (*ppc)++;
   ok = VG_(parse_Addr) (ppc, result2);
   if (!ok)
      return False;
   return True;
}

/* Parse two UInts (32 bit unsigned, in decimal) separated by a dash,
   or fail. */

static Bool parse_UInt_pair ( const HChar** ppc, UInt* result1, UInt* result2 )
{
   Bool ok = VG_(parse_UInt) (ppc, result1);
   if (!ok)
      return False;
   if (**ppc != '-')
      return False;
   (*ppc)++;
   ok = VG_(parse_UInt) (ppc, result2);
   if (!ok)
      return False;
   return True;
}

/* Parse a set of ranges separated by commas into 'ignoreRanges', or
   fail.  If they are valid, add them to the global set of ignored
   ranges. */
static Bool parse_ignore_ranges ( const HChar* str0 )
{
   init_gIgnoredAddressRanges();
   const HChar*  str = str0;
   const HChar** ppc = &str;
   while (1) {
      Addr start = ~(Addr)0;
      Addr end   = (Addr)0;
      Bool ok    = parse_Addr_pair(ppc, &start, &end);
      if (!ok)
         return False;
      if (start > end)
         return False;
      VG_(bindRangeMap)( gIgnoredAddressRanges, start, end, IAR_CommandLine );
      if (**ppc == 0)
         return True;
      if (**ppc != ',')
         return False;
      (*ppc)++;
   }
   /*NOTREACHED*/
   return False;
}

/* Add or remove [start, +len) from the set of ignored ranges. */
static Bool modify_ignore_ranges ( Bool addRange, Addr start, Addr len )
{
   init_gIgnoredAddressRanges();
   const Bool verbose = (VG_(clo_verbosity) > 1);
   if (len == 0) {
      return False;
   }
   if (addRange) {
      VG_(bindRangeMap)(gIgnoredAddressRanges,
                        start, start+len-1, IAR_ClientReq);
      if (verbose)
         VG_(dmsg)("memcheck: modify_ignore_ranges: add %p %p\n",
                   (void*)start, (void*)(start+len-1));
   } else {
      VG_(bindRangeMap)(gIgnoredAddressRanges,
                        start, start+len-1, IAR_NotIgnored);
      if (verbose)
         VG_(dmsg)("memcheck: modify_ignore_ranges: del %p %p\n",
                   (void*)start, (void*)(start+len-1));
   }
   if (verbose) {
      VG_(dmsg)("memcheck:   now have %u ranges:\n",
                VG_(sizeRangeMap)(gIgnoredAddressRanges));
      UInt i;
      for (i = 0; i < VG_(sizeRangeMap)(gIgnoredAddressRanges); i++) {
         UWord val     = IAR_INVALID;
         UWord key_min = ~(UWord)0;
         UWord key_max = (UWord)0;
         VG_(indexRangeMap)( &key_min, &key_max, &val,
                             gIgnoredAddressRanges, i );
         VG_(dmsg)("memcheck:      [%u]  %016lx-%016lx  %s\n",
                   i, key_min, key_max, showIARKind(val));
      }
   }
   return True;
}


/* --------------- Load/store slow cases. --------------- */

static
__attribute__((noinline))
void mc_LOADV_128_or_256_slow ( /*OUT*/ULong* res,
                                Addr a, SizeT nBits, Bool bigendian )
{
   ULong  pessim[4];     /* only used when p-l-ok=yes */
   SSizeT szB            = nBits / 8;
   SSizeT szL            = szB / 8;  /* Size in Longs (64-bit units) */
   SSizeT i, j;          /* Must be signed. */
   SizeT  n_addrs_bad = 0;
   Addr   ai;
   UChar  vbits8;
   Bool   ok;

   /* Code below assumes load size is a power of two and at least 64
      bits. */
   tl_assert((szB & (szB-1)) == 0 && szL > 0);

   /* If this triggers, you probably just need to increase the size of
      the pessim array. */
   tl_assert(szL <= sizeof(pessim) / sizeof(pessim[0]));

   for (j = 0; j < szL; j++) {
      pessim[j] = V_BITS64_DEFINED;
      res[j] = V_BITS64_UNDEFINED;
   }

   /* Make up a result V word, which contains the loaded data for
      valid addresses and Defined for invalid addresses.  Iterate over
      the bytes in the word, from the most significant down to the
      least.  The vbits to return are calculated into vbits128.  Also
      compute the pessimising value to be used when
      --partial-loads-ok=yes.  n_addrs_bad is redundant (the relevant
      info can be gleaned from the pessim array) but is used as a
      cross-check. */
   for (j = szL-1; j >= 0; j--) {
      ULong vbits64    = V_BITS64_UNDEFINED;
      ULong pessim64   = V_BITS64_DEFINED;
      UWord long_index = byte_offset_w(szL, bigendian, j);
      for (i = 8-1; i >= 0; i--) {
         PROF_EVENT(MCPE_LOADV_128_OR_256_SLOW_LOOP);
         ai = a + 8*long_index + byte_offset_w(8, bigendian, i);
         ok = get_vbits8(ai, &vbits8);
         vbits64 <<= 8;
         vbits64 |= vbits8;
         if (!ok) n_addrs_bad++;
         pessim64 <<= 8;
         pessim64 |= (ok ? V_BITS8_DEFINED : V_BITS8_UNDEFINED);
      }
      res[long_index] = vbits64;
      pessim[long_index] = pessim64;
   }

   /* In the common case, all the addresses involved are valid, so we
      just return the computed V bits and have done. */
   if (LIKELY(n_addrs_bad == 0))
      return;

   /* If there's no possibility of getting a partial-loads-ok
      exemption, report the error and quit. */
   if (!MC_(clo_partial_loads_ok)) {
      MC_(record_address_error)( VG_(get_running_tid)(), a, szB, False );
      return;
   }

   /* The partial-loads-ok excemption might apply.  Find out if it
      does.  If so, don't report an addressing error, but do return
      Undefined for the bytes that are out of range, so as to avoid
      false negatives.  If it doesn't apply, just report an addressing
      error in the usual way. */

   /* Some code steps along byte strings in aligned chunks
      even when there is only a partially defined word at the end (eg,
      optimised strlen).  This is allowed by the memory model of
      modern machines, since an aligned load cannot span two pages and
      thus cannot "partially fault".

      Therefore, a load from a partially-addressible place is allowed
      if all of the following hold:
      - the command-line flag is set [by default, it isn't]
      - it's an aligned load
      - at least one of the addresses in the word *is* valid

      Since this suppresses the addressing error, we avoid false
      negatives by marking bytes undefined when they come from an
      invalid address.
   */

   /* "at least one of the addresses is invalid" */
   ok = False;
   for (j = 0; j < szL; j++)
      ok |= pessim[j] != V_BITS64_DEFINED;
   tl_assert(ok);

#  if defined(VGP_s390x_linux)
   tl_assert(szB == 16); // s390 doesn't have > 128 bit SIMD
   /* OK if all loaded bytes are from the same page. */
   Bool alignedOK = ((a & 0xfff) <= 0x1000 - szB);
#  elif defined(VGA_ppc64be) || defined(VGA_ppc64le)
   /* lxvd2x might generate an unaligned 128 bit vector load.  */
   Bool alignedOK = (szB == 16);
#  else
   /* OK if the address is aligned by the load size. */
   Bool alignedOK = (0 == (a & (szB - 1)));
#  endif

   if (alignedOK && n_addrs_bad < szB) {
      /* Exemption applies.  Use the previously computed pessimising
         value and return the combined result, but don't flag an
         addressing error.  The pessimising value is Defined for valid
         addresses and Undefined for invalid addresses. */
      /* for assumption that doing bitwise or implements UifU */
      tl_assert(V_BIT_UNDEFINED == 1 && V_BIT_DEFINED == 0);
      /* (really need "UifU" here...)
         vbits[j] UifU= pessim[j]  (is pessimised by it, iow) */
      for (j = szL-1; j >= 0; j--)
         res[j] |= pessim[j];
      return;
   }

   /* Exemption doesn't apply.  Flag an addressing error in the normal
      way. */
   MC_(record_address_error)( VG_(get_running_tid)(), a, szB, False );
}

MC_MAIN_STATIC
__attribute__((noinline))
__attribute__((used))
VG_REGPARM(3)
ULong mc_LOADVn_slow ( Addr a, SizeT nBits, Bool bigendian );

MC_MAIN_STATIC
__attribute__((noinline))
__attribute__((used))
VG_REGPARM(3) /* make sure we're using a fixed calling convention, since
                 this function may get called from hand written assembly. */
ULong mc_LOADVn_slow ( Addr a, SizeT nBits, Bool bigendian )
{
   PROF_EVENT(MCPE_LOADVN_SLOW);

   /* ------------ BEGIN semi-fast cases ------------ */
   /* These deal quickly-ish with the common auxiliary primary map
      cases on 64-bit platforms.  Are merely a speedup hack; can be
      omitted without loss of correctness/functionality.  Note that in
      both cases the "sizeof(void*) == 8" causes these cases to be
      folded out by compilers on 32-bit platforms.  These are derived
      from LOADV64 and LOADV32.
   */

#  if defined(VGA_mips64) && defined(VGABI_N32)
   if (LIKELY(sizeof(void*) == 4 && nBits == 64 && VG_IS_8_ALIGNED(a)))
#  else
   if (LIKELY(sizeof(void*) == 8 && nBits == 64 && VG_IS_8_ALIGNED(a)))
#  endif
   {
      SecMap* sm       = get_secmap_for_reading(a);
      UWord   sm_off16 = SM_OFF_16(a);
      UWord   vabits16 = sm->vabits16[sm_off16];
      if (LIKELY(vabits16 == VA_BITS16_DEFINED))
         return V_BITS64_DEFINED;
      if (LIKELY(vabits16 == VA_BITS16_UNDEFINED))
         return V_BITS64_UNDEFINED;
      /* else fall into the slow case */
   }

#  if defined(VGA_mips64) && defined(VGABI_N32)
   if (LIKELY(sizeof(void*) == 4 && nBits == 32 && VG_IS_4_ALIGNED(a)))
#  else
   if (LIKELY(sizeof(void*) == 8 && nBits == 32 && VG_IS_4_ALIGNED(a)))
#  endif
   {
      SecMap* sm = get_secmap_for_reading(a);
      UWord sm_off = SM_OFF(a);
      UWord vabits8 = sm->vabits8[sm_off];
      if (LIKELY(vabits8 == VA_BITS8_DEFINED))
         return ((UWord)0xFFFFFFFF00000000ULL | (UWord)V_BITS32_DEFINED);
      if (LIKELY(vabits8 == VA_BITS8_UNDEFINED))
         return ((UWord)0xFFFFFFFF00000000ULL | (UWord)V_BITS32_UNDEFINED);
      /* else fall into slow case */
   }

   /* ------------ END semi-fast cases ------------ */

   ULong  vbits64     = V_BITS64_UNDEFINED; /* result */
   ULong  pessim64    = V_BITS64_DEFINED;   /* only used when p-l-ok=yes */
   SSizeT szB         = nBits / 8;
   SSizeT i;          /* Must be signed. */
   SizeT  n_addrs_bad = 0;
   Addr   ai;
   UChar  vbits8;
   Bool   ok;

   tl_assert(nBits == 64 || nBits == 32 || nBits == 16 || nBits == 8);

   /* Make up a 64-bit result V word, which contains the loaded data
      for valid addresses and Defined for invalid addresses.  Iterate
      over the bytes in the word, from the most significant down to
      the least.  The vbits to return are calculated into vbits64.
      Also compute the pessimising value to be used when
      --partial-loads-ok=yes.  n_addrs_bad is redundant (the relevant
      info can be gleaned from pessim64) but is used as a
      cross-check. */
   for (i = szB-1; i >= 0; i--) {
      PROF_EVENT(MCPE_LOADVN_SLOW_LOOP);
      ai = a + byte_offset_w(szB, bigendian, i);
      ok = get_vbits8(ai, &vbits8);
      vbits64 <<= 8;
      vbits64 |= vbits8;
      if (!ok) n_addrs_bad++;
      pessim64 <<= 8;
      pessim64 |= (ok ? V_BITS8_DEFINED : V_BITS8_UNDEFINED);
   }

   /* In the common case, all the addresses involved are valid, so we
      just return the computed V bits and have done. */
   if (LIKELY(n_addrs_bad == 0))
      return vbits64;

   /* If there's no possibility of getting a partial-loads-ok
      exemption, report the error and quit. */
   if (!MC_(clo_partial_loads_ok)) {
      MC_(record_address_error)( VG_(get_running_tid)(), a, szB, False );
      return vbits64;
   }

   /* The partial-loads-ok excemption might apply.  Find out if it
      does.  If so, don't report an addressing error, but do return
      Undefined for the bytes that are out of range, so as to avoid
      false negatives.  If it doesn't apply, just report an addressing
      error in the usual way. */

   /* Some code steps along byte strings in aligned word-sized chunks
      even when there is only a partially defined word at the end (eg,
      optimised strlen).  This is allowed by the memory model of
      modern machines, since an aligned load cannot span two pages and
      thus cannot "partially fault".  Despite such behaviour being
      declared undefined by ANSI C/C++.

      Therefore, a load from a partially-addressible place is allowed
      if all of the following hold:
      - the command-line flag is set [by default, it isn't]
      - it's a word-sized, word-aligned load
      - at least one of the addresses in the word *is* valid

      Since this suppresses the addressing error, we avoid false
      negatives by marking bytes undefined when they come from an
      invalid address.
   */

   /* "at least one of the addresses is invalid" */
   tl_assert(pessim64 != V_BITS64_DEFINED);

#  if defined(VGA_mips64) && defined(VGABI_N32)
   if (szB == VG_WORDSIZE * 2 && VG_IS_WORD_ALIGNED(a)
       && n_addrs_bad < VG_WORDSIZE * 2)
#  elif defined(VGA_ppc64be) || defined(VGA_ppc64le)
   /* On power unaligned loads of words are OK. */
   if (szB == VG_WORDSIZE && n_addrs_bad < VG_WORDSIZE)
#  else
   if (szB == VG_WORDSIZE && VG_IS_WORD_ALIGNED(a)
       && n_addrs_bad < VG_WORDSIZE)
#  endif
   {
      /* Exemption applies.  Use the previously computed pessimising
         value for vbits64 and return the combined result, but don't
         flag an addressing error.  The pessimising value is Defined
         for valid addresses and Undefined for invalid addresses. */
      /* for assumption that doing bitwise or implements UifU */
      tl_assert(V_BIT_UNDEFINED == 1 && V_BIT_DEFINED == 0);
      /* (really need "UifU" here...)
         vbits64 UifU= pessim64  (is pessimised by it, iow) */
      vbits64 |= pessim64;
      return vbits64;
   }

   /* Also, in appears that gcc generates string-stepping code in
      32-bit chunks on 64 bit platforms.  So, also grant an exception
      for this case.  Note that the first clause of the conditional
      (VG_WORDSIZE == 8) is known at compile time, so the whole clause
      will get folded out in 32 bit builds. */
#  if defined(VGA_mips64) && defined(VGABI_N32)
   if (VG_WORDSIZE == 4
       && VG_IS_4_ALIGNED(a) && nBits == 32 && n_addrs_bad < 4)
#  else
   if (VG_WORDSIZE == 8
       && VG_IS_4_ALIGNED(a) && nBits == 32 && n_addrs_bad < 4)
#  endif
   {
      tl_assert(V_BIT_UNDEFINED == 1 && V_BIT_DEFINED == 0);
      /* (really need "UifU" here...)
         vbits64 UifU= pessim64  (is pessimised by it, iow) */
      vbits64 |= pessim64;
      /* Mark the upper 32 bits as undefined, just to be on the safe
         side. */
      vbits64 |= (((ULong)V_BITS32_UNDEFINED) << 32);
      return vbits64;
   }

   /* Exemption doesn't apply.  Flag an addressing error in the normal
      way. */
   MC_(record_address_error)( VG_(get_running_tid)(), a, szB, False );

   return vbits64;
}


static
__attribute__((noinline))
void mc_STOREVn_slow ( Addr a, SizeT nBits, ULong vbytes, Bool bigendian )
{
   SizeT szB = nBits / 8;
   SizeT i, n_addrs_bad = 0;
   UChar vbits8;
   Addr  ai;
   Bool  ok;

   PROF_EVENT(MCPE_STOREVN_SLOW);

   /* ------------ BEGIN semi-fast cases ------------ */
   /* These deal quickly-ish with the common auxiliary primary map
      cases on 64-bit platforms.  Are merely a speedup hack; can be
      omitted without loss of correctness/functionality.  Note that in
      both cases the "sizeof(void*) == 8" causes these cases to be
      folded out by compilers on 32-bit platforms.  The logic below
      is somewhat similar to some cases extensively commented in
      MC_(helperc_STOREV8).
   */
#  if defined(VGA_mips64) && defined(VGABI_N32)
   if (LIKELY(sizeof(void*) == 4 && nBits == 64 && VG_IS_8_ALIGNED(a)))
#  else
   if (LIKELY(sizeof(void*) == 8 && nBits == 64 && VG_IS_8_ALIGNED(a)))
#  endif
   {
      SecMap* sm       = get_secmap_for_reading(a);
      UWord   sm_off16 = SM_OFF_16(a);
      UWord   vabits16 = sm->vabits16[sm_off16];
      if (LIKELY( !is_distinguished_sm(sm) &&
                          (VA_BITS16_DEFINED   == vabits16 ||
                           VA_BITS16_UNDEFINED == vabits16) )) {
         /* Handle common case quickly: a is suitably aligned, */
         /* is mapped, and is addressible. */
         // Convert full V-bits in register to compact 2-bit form.
         if (LIKELY(V_BITS64_DEFINED == vbytes)) {
            sm->vabits16[sm_off16] = VA_BITS16_DEFINED;
            return;
         } else if (V_BITS64_UNDEFINED == vbytes) {
            sm->vabits16[sm_off16] = VA_BITS16_UNDEFINED;
            return;
         }
         /* else fall into the slow case */
      }
      /* else fall into the slow case */
   }

#  if defined(VGA_mips64) && defined(VGABI_N32)
   if (LIKELY(sizeof(void*) == 4 && nBits == 32 && VG_IS_4_ALIGNED(a)))
#  else
   if (LIKELY(sizeof(void*) == 8 && nBits == 32 && VG_IS_4_ALIGNED(a)))
#  endif
   {
      SecMap* sm      = get_secmap_for_reading(a);
      UWord   sm_off  = SM_OFF(a);
      UWord   vabits8 = sm->vabits8[sm_off];
      if (LIKELY( !is_distinguished_sm(sm) &&
                          (VA_BITS8_DEFINED   == vabits8 ||
                           VA_BITS8_UNDEFINED == vabits8) )) {
         /* Handle common case quickly: a is suitably aligned, */
         /* is mapped, and is addressible. */
         // Convert full V-bits in register to compact 2-bit form.
         if (LIKELY(V_BITS32_DEFINED == (vbytes & 0xFFFFFFFF))) {
            sm->vabits8[sm_off] = VA_BITS8_DEFINED;
            return;
         } else if (V_BITS32_UNDEFINED == (vbytes & 0xFFFFFFFF)) {
            sm->vabits8[sm_off] = VA_BITS8_UNDEFINED;
            return;
         }
         /* else fall into the slow case */
      }
      /* else fall into the slow case */
   }
   /* ------------ END semi-fast cases ------------ */

   tl_assert(nBits == 64 || nBits == 32 || nBits == 16 || nBits == 8);

   /* Dump vbytes in memory, iterating from least to most significant
      byte.  At the same time establish addressibility of the location. */
   for (i = 0; i < szB; i++) {
      PROF_EVENT(MCPE_STOREVN_SLOW_LOOP);
      ai     = a + byte_offset_w(szB, bigendian, i);
      vbits8 = vbytes & 0xff;
      ok     = set_vbits8(ai, vbits8);
      if (!ok) n_addrs_bad++;
      vbytes >>= 8;
   }

   /* If an address error has happened, report it. */
   if (n_addrs_bad > 0)
      MC_(record_address_error)( VG_(get_running_tid)(), a, szB, True );
}


/*------------------------------------------------------------*/
/*--- Setting permissions over address ranges.             ---*/
/*------------------------------------------------------------*/

static void set_address_range_perms ( Addr a, SizeT lenT, UWord vabits16,
                                      UWord dsm_num )
{
   UWord    sm_off, sm_off16;
   UWord    vabits2 = vabits16 & 0x3;
   SizeT    lenA, lenB, len_to_next_secmap;
   Addr     aNext;
   SecMap*  sm;
   SecMap** sm_ptr;
   SecMap*  example_dsm;

   PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS);

   /* Check the V+A bits make sense. */
   tl_assert(VA_BITS16_NOACCESS  == vabits16 ||
             VA_BITS16_UNDEFINED == vabits16 ||
             VA_BITS16_DEFINED   == vabits16);

   // This code should never write PDBs;  ensure this.  (See comment above
   // set_vabits2().)
   tl_assert(VA_BITS2_PARTDEFINED != vabits2);

   if (lenT == 0)
      return;

   if (lenT > 256 * 1024 * 1024) {
      if (VG_(clo_verbosity) > 0 && !VG_(clo_xml)) {
         const HChar* s = "unknown???";
         if (vabits16 == VA_BITS16_NOACCESS ) s = "noaccess";
         if (vabits16 == VA_BITS16_UNDEFINED) s = "undefined";
         if (vabits16 == VA_BITS16_DEFINED  ) s = "defined";
         VG_(message)(Vg_UserMsg, "Warning: set address range perms: "
                                  "large range [0x%lx, 0x%lx) (%s)\n",
                                  a, a + lenT, s);
      }
   }

#ifndef PERF_FAST_SARP
   /*------------------ debug-only case ------------------ */
   {
      // Endianness doesn't matter here because all bytes are being set to
      // the same value.
      // Nb: We don't have to worry about updating the sec-V-bits table
      // after these set_vabits2() calls because this code never writes
      // VA_BITS2_PARTDEFINED values.
      SizeT i;
      for (i = 0; i < lenT; i++) {
         set_vabits2(a + i, vabits2);
      }
      return;
   }
#endif

   /*------------------ standard handling ------------------ */

   /* Get the distinguished secondary that we might want
      to use (part of the space-compression scheme). */
   example_dsm = &sm_distinguished[dsm_num];

   // We have to handle ranges covering various combinations of partial and
   // whole sec-maps.  Here is how parts 1, 2 and 3 are used in each case.
   // Cases marked with a '*' are common.
   //
   //   TYPE                                             PARTS USED
   //   ----                                             ----------
   // * one partial sec-map                  (p)         1
   // - one whole sec-map                    (P)         2
   //
   // * two partial sec-maps                 (pp)        1,3
   // - one partial, one whole sec-map       (pP)        1,2
   // - one whole, one partial sec-map       (Pp)        2,3
   // - two whole sec-maps                   (PP)        2,2
   //
   // * one partial, one whole, one partial  (pPp)       1,2,3
   // - one partial, two whole               (pPP)       1,2,2
   // - two whole, one partial               (PPp)       2,2,3
   // - three whole                          (PPP)       2,2,2
   //
   // * one partial, N-2 whole, one partial  (pP...Pp)   1,2...2,3
   // - one partial, N-1 whole               (pP...PP)   1,2...2,2
   // - N-1 whole, one partial               (PP...Pp)   2,2...2,3
   // - N whole                              (PP...PP)   2,2...2,3

   // Break up total length (lenT) into two parts:  length in the first
   // sec-map (lenA), and the rest (lenB);   lenT == lenA + lenB.
   aNext = start_of_this_sm(a) + SM_SIZE;
   len_to_next_secmap = aNext - a;
   if ( lenT <= len_to_next_secmap ) {
      // Range entirely within one sec-map.  Covers almost all cases.
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_SINGLE_SECMAP);
      lenA = lenT;
      lenB = 0;
   } else if (is_start_of_sm(a)) {
      // Range spans at least one whole sec-map, and starts at the beginning
      // of a sec-map; skip to Part 2.
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_STARTOF_SECMAP);
      lenA = 0;
      lenB = lenT;
      goto part2;
   } else {
      // Range spans two or more sec-maps, first one is partial.
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_MULTIPLE_SECMAPS);
      lenA = len_to_next_secmap;
      lenB = lenT - lenA;
   }

   //------------------------------------------------------------------------
   // Part 1: Deal with the first sec_map.  Most of the time the range will be
   // entirely within a sec_map and this part alone will suffice.  Also,
   // doing it this way lets us avoid repeatedly testing for the crossing of
   // a sec-map boundary within these loops.
   //------------------------------------------------------------------------

   // If it's distinguished, make it undistinguished if necessary.
   sm_ptr = get_secmap_ptr(a);
   if (is_distinguished_sm(*sm_ptr)) {
      if (*sm_ptr == example_dsm) {
         // Sec-map already has the V+A bits that we want, so skip.
         PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_DIST_SM1_QUICK);
         a    = aNext;
         lenA = 0;
      } else {
         PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_DIST_SM1);
         *sm_ptr = copy_for_writing(*sm_ptr);
      }
   }
   sm = *sm_ptr;

   // 1 byte steps
   while (True) {
      if (VG_IS_8_ALIGNED(a)) break;
      if (lenA < 1)           break;
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_LOOP1A);
      sm_off = SM_OFF(a);
      insert_vabits2_into_vabits8( a, vabits2, &(sm->vabits8[sm_off]) );
      a    += 1;
      lenA -= 1;
   }
   // 8-aligned, 8 byte steps
   while (True) {
      if (lenA < 8) break;
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_LOOP8A);
      sm_off16 = SM_OFF_16(a);
      sm->vabits16[sm_off16] = vabits16;
      a    += 8;
      lenA -= 8;
   }
   // 1 byte steps
   while (True) {
      if (lenA < 1) break;
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_LOOP1B);
      sm_off = SM_OFF(a);
      insert_vabits2_into_vabits8( a, vabits2, &(sm->vabits8[sm_off]) );
      a    += 1;
      lenA -= 1;
   }

   // We've finished the first sec-map.  Is that it?
   if (lenB == 0)
      return;

   //------------------------------------------------------------------------
   // Part 2: Fast-set entire sec-maps at a time.
   //------------------------------------------------------------------------
  part2:
   // 64KB-aligned, 64KB steps.
   // Nb: we can reach here with lenB < SM_SIZE
   tl_assert(0 == lenA);
   while (True) {
      if (lenB < SM_SIZE) break;
      tl_assert(is_start_of_sm(a));
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_LOOP64K);
      sm_ptr = get_secmap_ptr(a);
      if (!is_distinguished_sm(*sm_ptr)) {
         PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_LOOP64K_FREE_DIST_SM);
         // Free the non-distinguished sec-map that we're replacing.  This
         // case happens moderately often, enough to be worthwhile.
         SysRes sres = VG_(am_munmap_valgrind)((Addr)*sm_ptr, sizeof(SecMap));
         tl_assert2(! sr_isError(sres), "SecMap valgrind munmap failure\n");
      }
      update_SM_counts(*sm_ptr, example_dsm);
      // Make the sec-map entry point to the example DSM
      *sm_ptr = example_dsm;
      lenB -= SM_SIZE;
      a    += SM_SIZE;
   }

   // We've finished the whole sec-maps.  Is that it?
   if (lenB == 0)
      return;

   //------------------------------------------------------------------------
   // Part 3: Finish off the final partial sec-map, if necessary.
   //------------------------------------------------------------------------

   tl_assert(is_start_of_sm(a) && lenB < SM_SIZE);

   // If it's distinguished, make it undistinguished if necessary.
   sm_ptr = get_secmap_ptr(a);
   if (is_distinguished_sm(*sm_ptr)) {
      if (*sm_ptr == example_dsm) {
         // Sec-map already has the V+A bits that we want, so stop.
         PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_DIST_SM2_QUICK);
         return;
      } else {
         PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_DIST_SM2);
         *sm_ptr = copy_for_writing(*sm_ptr);
      }
   }
   sm = *sm_ptr;

   // 8-aligned, 8 byte steps
   while (True) {
      if (lenB < 8) break;
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_LOOP8B);
      sm_off16 = SM_OFF_16(a);
      sm->vabits16[sm_off16] = vabits16;
      a    += 8;
      lenB -= 8;
   }
   // 1 byte steps
   while (True) {
      if (lenB < 1) return;
      PROF_EVENT(MCPE_SET_ADDRESS_RANGE_PERMS_LOOP1C);
      sm_off = SM_OFF(a);
      insert_vabits2_into_vabits8( a, vabits2, &(sm->vabits8[sm_off]) );
      a    += 1;
      lenB -= 1;
   }
}


/* --- Set permissions for arbitrary address ranges --- */

void MC_(make_mem_noaccess) ( Addr a, SizeT len )
{
   PROF_EVENT(MCPE_MAKE_MEM_NOACCESS);
   DEBUG("MC_(make_mem_noaccess)(%p, %lu)\n", a, len);
   set_address_range_perms ( a, len, VA_BITS16_NOACCESS, SM_DIST_NOACCESS );
   if (UNLIKELY( MC_(clo_mc_level) == 3 ))
      ocache_sarp_Clear_Origins ( a, len );
}

static void make_mem_undefined ( Addr a, SizeT len )
{
   PROF_EVENT(MCPE_MAKE_MEM_UNDEFINED);
   DEBUG("make_mem_undefined(%p, %lu)\n", a, len);
   set_address_range_perms ( a, len, VA_BITS16_UNDEFINED, SM_DIST_UNDEFINED );
}

void MC_(make_mem_undefined_w_otag) ( Addr a, SizeT len, UInt otag )
{
   PROF_EVENT(MCPE_MAKE_MEM_UNDEFINED_W_OTAG);
   DEBUG("MC_(make_mem_undefined)(%p, %lu)\n", a, len);
   set_address_range_perms ( a, len, VA_BITS16_UNDEFINED, SM_DIST_UNDEFINED );
   if (UNLIKELY( MC_(clo_mc_level) == 3 ))
      ocache_sarp_Set_Origins ( a, len, otag );
}

static
void make_mem_undefined_w_tid_and_okind ( Addr a, SizeT len,
                                          ThreadId tid, UInt okind )
{
   UInt        ecu;
   ExeContext* here;
   /* VG_(record_ExeContext) checks for validity of tid, and asserts
      if it is invalid.  So no need to do it here. */
   tl_assert(okind <= 3);
   here = VG_(record_ExeContext)( tid, 0/*first_ip_delta*/ );
   tl_assert(here);
   ecu = VG_(get_ECU_from_ExeContext)(here);
   tl_assert(VG_(is_plausible_ECU)(ecu));
   MC_(make_mem_undefined_w_otag) ( a, len, ecu | okind );
}

static
void mc_new_mem_w_tid_make_ECU  ( Addr a, SizeT len, ThreadId tid )
{
   make_mem_undefined_w_tid_and_okind ( a, len, tid, MC_OKIND_UNKNOWN );
}

static
void mc_new_mem_w_tid_no_ECU  ( Addr a, SizeT len, ThreadId tid )
{
   MC_(make_mem_undefined_w_otag) ( a, len, MC_OKIND_UNKNOWN );
}

void MC_(make_mem_defined) ( Addr a, SizeT len )
{
   PROF_EVENT(MCPE_MAKE_MEM_DEFINED);
   DEBUG("MC_(make_mem_defined)(%p, %lu)\n", a, len);
   set_address_range_perms ( a, len, VA_BITS16_DEFINED, SM_DIST_DEFINED );
   if (UNLIKELY( MC_(clo_mc_level) == 3 ))
      ocache_sarp_Clear_Origins ( a, len );
}

__attribute__((unused))
static void make_mem_defined_w_tid ( Addr a, SizeT len, ThreadId tid )
{
   MC_(make_mem_defined)(a, len);
}

/* For each byte in [a,a+len), if the byte is addressable, make it be
   defined, but if it isn't addressible, leave it alone.  In other
   words a version of MC_(make_mem_defined) that doesn't mess with
   addressibility.  Low-performance implementation. */
static void make_mem_defined_if_addressable ( Addr a, SizeT len )
{
   SizeT i;
   UChar vabits2;
   DEBUG("make_mem_defined_if_addressable(%p, %llu)\n", a, (ULong)len);
   for (i = 0; i < len; i++) {
      vabits2 = get_vabits2( a+i );
      if (LIKELY(VA_BITS2_NOACCESS != vabits2)) {
         set_vabits2(a+i, VA_BITS2_DEFINED);
         if (UNLIKELY(MC_(clo_mc_level) >= 3)) {
            MC_(helperc_b_store1)( a+i, 0 ); /* clear the origin tag */
         }
      }
   }
}

/* Similarly (needed for mprotect handling ..) */
static void make_mem_defined_if_noaccess ( Addr a, SizeT len )
{
   SizeT i;
   UChar vabits2;
   DEBUG("make_mem_defined_if_noaccess(%p, %llu)\n", a, (ULong)len);
   for (i = 0; i < len; i++) {
      vabits2 = get_vabits2( a+i );
      if (LIKELY(VA_BITS2_NOACCESS == vabits2)) {
         set_vabits2(a+i, VA_BITS2_DEFINED);
         if (UNLIKELY(MC_(clo_mc_level) >= 3)) {
            MC_(helperc_b_store1)( a+i, 0 ); /* clear the origin tag */
         }
      }
   }
}

/* --- Block-copy permissions (needed for implementing realloc() and
       sys_mremap). --- */

void MC_(copy_address_range_state) ( Addr src, Addr dst, SizeT len )
{
   SizeT i, j;
   UChar vabits2, vabits8;
   Bool  aligned, nooverlap;

   DEBUG("MC_(copy_address_range_state)\n");
   PROF_EVENT(MCPE_COPY_ADDRESS_RANGE_STATE);

   if (len == 0 || src == dst)
      return;

   aligned   = VG_IS_4_ALIGNED(src) && VG_IS_4_ALIGNED(dst);
   nooverlap = src+len <= dst || dst+len <= src;

   if (nooverlap && aligned) {

      /* Vectorised fast case, when no overlap and suitably aligned */
      /* vector loop */
      i = 0;
      while (len >= 4) {
         vabits8 = get_vabits8_for_aligned_word32( src+i );
         set_vabits8_for_aligned_word32( dst+i, vabits8 );
         if (LIKELY(VA_BITS8_DEFINED == vabits8
                            || VA_BITS8_UNDEFINED == vabits8
                            || VA_BITS8_NOACCESS == vabits8)) {
            /* do nothing */
         } else {
            /* have to copy secondary map info */
            if (VA_BITS2_PARTDEFINED == get_vabits2( src+i+0 ))
               set_sec_vbits8( dst+i+0, get_sec_vbits8( src+i+0 ) );
            if (VA_BITS2_PARTDEFINED == get_vabits2( src+i+1 ))
               set_sec_vbits8( dst+i+1, get_sec_vbits8( src+i+1 ) );
            if (VA_BITS2_PARTDEFINED == get_vabits2( src+i+2 ))
               set_sec_vbits8( dst+i+2, get_sec_vbits8( src+i+2 ) );
            if (VA_BITS2_PARTDEFINED == get_vabits2( src+i+3 ))
               set_sec_vbits8( dst+i+3, get_sec_vbits8( src+i+3 ) );
         }
         i += 4;
         len -= 4;
      }
      /* fixup loop */
      while (len >= 1) {
         vabits2 = get_vabits2( src+i );
         set_vabits2( dst+i, vabits2 );
         if (VA_BITS2_PARTDEFINED == vabits2) {
            set_sec_vbits8( dst+i, get_sec_vbits8( src+i ) );
         }
         i++;
         len--;
      }

   } else {

      /* We have to do things the slow way */
      if (src < dst) {
         for (i = 0, j = len-1; i < len; i++, j--) {
            PROF_EVENT(MCPE_COPY_ADDRESS_RANGE_STATE_LOOP1);
            vabits2 = get_vabits2( src+j );
            set_vabits2( dst+j, vabits2 );
            if (VA_BITS2_PARTDEFINED == vabits2) {
               set_sec_vbits8( dst+j, get_sec_vbits8( src+j ) );
            }
         }
      }

      if (src > dst) {
         for (i = 0; i < len; i++) {
            PROF_EVENT(MCPE_COPY_ADDRESS_RANGE_STATE_LOOP2);
            vabits2 = get_vabits2( src+i );
            set_vabits2( dst+i, vabits2 );
            if (VA_BITS2_PARTDEFINED == vabits2) {
               set_sec_vbits8( dst+i, get_sec_vbits8( src+i ) );
            }
         }
      }
   }

}


/*------------------------------------------------------------*/
/*--- Origin tracking stuff - cache basics                 ---*/
/*------------------------------------------------------------*/

/* AN OVERVIEW OF THE ORIGIN TRACKING IMPLEMENTATION
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Note that this implementation draws inspiration from the "origin
   tracking by value piggybacking" scheme described in "Tracking Bad
   Apples: Reporting the Origin of Null and Undefined Value Errors"
   (Michael Bond, Nicholas Nethercote, Stephen Kent, Samuel Guyer,
   Kathryn McKinley, OOPSLA07, Montreal, Oct 2007) but in fact it is
   implemented completely differently.

   Origin tags and ECUs -- about the shadow values
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   This implementation tracks the defining point of all uninitialised
   values using so called "origin tags", which are 32-bit integers,
   rather than using the values themselves to encode the origins.  The
   latter, so-called value piggybacking", is what the OOPSLA07 paper
   describes.

   Origin tags, as tracked by the machinery below, are 32-bit unsigned
   ints (UInts), regardless of the machine's word size.  Each tag
   comprises an upper 30-bit ECU field and a lower 2-bit
   'kind' field.  The ECU field is a number given out by m_execontext
   and has a 1-1 mapping with ExeContext*s.  An ECU can be used
   directly as an origin tag (otag), but in fact we want to put
   additional information 'kind' field to indicate roughly where the
   tag came from.  This helps print more understandable error messages
   for the user -- it has no other purpose.  In summary:

   * Both ECUs and origin tags are represented as 32-bit words

   * m_execontext and the core-tool interface deal purely in ECUs.
     They have no knowledge of origin tags - that is a purely
     Memcheck-internal matter.

   * all valid ECUs have the lowest 2 bits zero and at least
     one of the upper 30 bits nonzero (see VG_(is_plausible_ECU))

   * to convert from an ECU to an otag, OR in one of the MC_OKIND_
     constants defined in mc_include.h.

   * to convert an otag back to an ECU, AND it with ~3

   One important fact is that no valid otag is zero.  A zero otag is
   used by the implementation to indicate "no origin", which could
   mean that either the value is defined, or it is undefined but the
   implementation somehow managed to lose the origin.

   The ECU used for memory created by malloc etc is derived from the
   stack trace at the time the malloc etc happens.  This means the
   mechanism can show the exact allocation point for heap-created
   uninitialised values.

   In contrast, it is simply too expensive to create a complete
   backtrace for each stack allocation.  Therefore we merely use a
   depth-1 backtrace for stack allocations, which can be done once at
   translation time, rather than N times at run time.  The result of
   this is that, for stack created uninitialised values, Memcheck can
   only show the allocating function, and not what called it.
   Furthermore, compilers tend to move the stack pointer just once at
   the start of the function, to allocate all locals, and so in fact
   the stack origin almost always simply points to the opening brace
   of the function.  Net result is, for stack origins, the mechanism
   can tell you in which function the undefined value was created, but
   that's all.  Users will need to carefully check all locals in the
   specified function.

   Shadowing registers and memory
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Memory is shadowed using a two level cache structure (ocacheL1 and
   ocacheL2).  Memory references are first directed to ocacheL1.  This
   is a traditional 2-way set associative cache with 32-byte lines and
   approximate LRU replacement within each set.

   A naive implementation would require storing one 32 bit otag for
   each byte of memory covered, a 4:1 space overhead.  Instead, there
   is one otag for every 4 bytes of memory covered, plus a 4-bit mask
   that shows which of the 4 bytes have that shadow value and which
   have a shadow value of zero (indicating no origin).  Hence a lot of
   space is saved, but the cost is that only one different origin per
   4 bytes of address space can be represented.  This is a source of
   imprecision, but how much of a problem it really is remains to be
   seen.

   A cache line that contains all zeroes ("no origins") contains no
   useful information, and can be ejected from the L1 cache "for
   free", in the sense that a read miss on the L1 causes a line of
   zeroes to be installed.  However, ejecting a line containing
   nonzeroes risks losing origin information permanently.  In order to
   prevent such lossage, ejected nonzero lines are placed in a
   secondary cache (ocacheL2), which is an OSet (AVL tree) of cache
   lines.  This can grow arbitrarily large, and so should ensure that
   Memcheck runs out of memory in preference to losing useful origin
   info due to cache size limitations.

   Shadowing registers is a bit tricky, because the shadow values are
   32 bits, regardless of the size of the register.  That gives a
   problem for registers smaller than 32 bits.  The solution is to
   find spaces in the guest state that are unused, and use those to
   shadow guest state fragments smaller than 32 bits.  For example, on
   ppc32/64, each vector register is 16 bytes long.  If 4 bytes of the
   shadow are allocated for the register's otag, then there are still
   12 bytes left over which could be used to shadow 3 other values.

   This implies there is some non-obvious mapping from guest state
   (start,length) pairs to the relevant shadow offset (for the origin
   tags).  And it is unfortunately guest-architecture specific.  The
   mapping is contained in mc_machine.c, which is quite lengthy but
   straightforward.

   Instrumenting the IR
   ~~~~~~~~~~~~~~~~~~~~

   Instrumentation is largely straightforward, and done by the
   functions schemeE and schemeS in mc_translate.c.  These generate
   code for handling the origin tags of expressions (E) and statements
   (S) respectively.  The rather strange names are a reference to the
   "compilation schemes" shown in Simon Peyton Jones' book "The
   Implementation of Functional Programming Languages" (Prentice Hall,
   1987, see
   http://research.microsoft.com/~simonpj/papers/slpj-book-1987/index.htm).

   schemeS merely arranges to move shadow values around the guest
   state to track the incoming IR.  schemeE is largely trivial too.
   The only significant point is how to compute the otag corresponding
   to binary (or ternary, quaternary, etc) operator applications.  The
   rule is simple: just take whichever value is larger (32-bit
   unsigned max).  Constants get the special value zero.  Hence this
   rule always propagates a nonzero (known) otag in preference to a
   zero (unknown, or more likely, value-is-defined) tag, as we want.
   If two different undefined values are inputs to a binary operator
   application, then which is propagated is arbitrary, but that
   doesn't matter, since the program is erroneous in using either of
   the values, and so there's no point in attempting to propagate
   both.

   Since constants are abstracted to (otag) zero, much of the
   instrumentation code can be folded out without difficulty by the
   generic post-instrumentation IR cleanup pass, using these rules:
   Max32U(0,x) -> x, Max32U(x,0) -> x, Max32(x,y) where x and y are
   constants is evaluated at JIT time.  And the resulting dead code
   removal.  In practice this causes surprisingly few Max32Us to
   survive through to backend code generation.

   Integration with the V-bits machinery
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   This is again largely straightforward.  Mostly the otag and V bits
   stuff are independent.  The only point of interaction is when the V
   bits instrumenter creates a call to a helper function to report an
   uninitialised value error -- in that case it must first use schemeE
   to get hold of the origin tag expression for the value, and pass
   that to the helper too.

   There is the usual stuff to do with setting address range
   permissions.  When memory is painted undefined, we must also know
   the origin tag to paint with, which involves some tedious plumbing,
   particularly to do with the fast case stack handlers.  When memory
   is painted defined or noaccess then the origin tags must be forced
   to zero.

   One of the goals of the implementation was to ensure that the
   non-origin tracking mode isn't slowed down at all.  To do this,
   various functions to do with memory permissions setting (again,
   mostly pertaining to the stack) are duplicated for the with- and
   without-otag case.

   Dealing with stack redzones, and the NIA cache
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   This is one of the few non-obvious parts of the implementation.

   Some ABIs (amd64-ELF, ppc64-ELF, ppc32/64-XCOFF) define a small
   reserved area below the stack pointer, that can be used as scratch
   space by compiler generated code for functions.  In the Memcheck
   sources this is referred to as the "stack redzone".  The important
   thing here is that such redzones are considered volatile across
   function calls and returns.  So Memcheck takes care to mark them as
   undefined for each call and return, on the afflicted platforms.
   Past experience shows this is essential in order to get reliable
   messages about uninitialised values that come from the stack.

   So the question is, when we paint a redzone undefined, what origin
   tag should we use for it?  Consider a function f() calling g().  If
   we paint the redzone using an otag derived from the ExeContext of
   the CALL/BL instruction in f, then any errors in g causing it to
   use uninitialised values that happen to lie in the redzone, will be
   reported as having their origin in f.  Which is highly confusing.

   The same applies for returns: if, on a return, we paint the redzone
   using a origin tag derived from the ExeContext of the RET/BLR
   instruction in g, then any later errors in f causing it to use
   uninitialised values in the redzone, will be reported as having
   their origin in g.  Which is just as confusing.

   To do it right, in both cases we need to use an origin tag which
   pertains to the instruction which dynamically follows the CALL/BL
   or RET/BLR.  In short, one derived from the NIA - the "next
   instruction address".

   To make this work, Memcheck's redzone-painting helper,
   MC_(helperc_MAKE_STACK_UNINIT), now takes a third argument, the
   NIA.  It converts the NIA to a 1-element ExeContext, and uses that
   ExeContext's ECU as the basis for the otag used to paint the
   redzone.  The expensive part of this is converting an NIA into an
   ECU, since this happens once for every call and every return.  So
   we use a simple 511-line, 2-way set associative cache
   (nia_to_ecu_cache) to cache the mappings, and that knocks most of
   the cost out.

   Further background comments
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~

   > Question: why is otag a UInt?  Wouldn't a UWord be better?  Isn't
   > it really just the address of the relevant ExeContext?

   Well, it's not the address, but a value which has a 1-1 mapping
   with ExeContexts, and is guaranteed not to be zero, since zero
   denotes (to memcheck) "unknown origin or defined value".  So these
   UInts are just numbers starting at 4 and incrementing by 4; each
   ExeContext is given a number when it is created.  (*** NOTE this
   confuses otags and ECUs; see comments above ***).

   Making these otags 32-bit regardless of the machine's word size
   makes the 64-bit implementation easier (next para).  And it doesn't
   really limit us in any way, since for the tags to overflow would
   require that the program somehow caused 2^30-1 different
   ExeContexts to be created, in which case it is probably in deep
   trouble.  Not to mention V will have soaked up many tens of
   gigabytes of memory merely to store them all.

   So having 64-bit origins doesn't really buy you anything, and has
   the following downsides:

   Suppose that instead, an otag is a UWord.  This would mean that, on
   a 64-bit target,

   1. It becomes hard to shadow any element of guest state which is
      smaller than 8 bytes.  To do so means you'd need to find some
      8-byte-sized hole in the guest state which you don't want to
      shadow, and use that instead to hold the otag.  On ppc64, the
      condition code register(s) are split into 20 UChar sized pieces,
      all of which need to be tracked (guest_XER_SO .. guest_CR7_0)
      and so that would entail finding 160 bytes somewhere else in the
      guest state.

      Even on x86, I want to track origins for %AH .. %DH (bits 15:8
      of %EAX .. %EDX) that are separate from %AL .. %DL (bits 7:0 of
      same) and so I had to look for 4 untracked otag-sized areas in
      the guest state to make that possible.

      The same problem exists of course when origin tags are only 32
      bits, but it's less extreme.

   2. (More compelling) it doubles the size of the origin shadow
      memory.  Given that the shadow memory is organised as a fixed
      size cache, and that accuracy of tracking is limited by origins
      falling out the cache due to space conflicts, this isn't good.

   > Another question: is the origin tracking perfect, or are there
   > cases where it fails to determine an origin?

   It is imperfect for at least for the following reasons, and
   probably more:

   * Insufficient capacity in the origin cache.  When a line is
     evicted from the cache it is gone forever, and so subsequent
     queries for the line produce zero, indicating no origin
     information.  Interestingly, a line containing all zeroes can be
     evicted "free" from the cache, since it contains no useful
     information, so there is scope perhaps for some cleverer cache
     management schemes.  (*** NOTE, with the introduction of the
     second level origin tag cache, ocacheL2, this is no longer a
     problem. ***)

   * The origin cache only stores one otag per 32-bits of address
     space, plus 4 bits indicating which of the 4 bytes has that tag
     and which are considered defined.  The result is that if two
     undefined bytes in the same word are stored in memory, the first
     stored byte's origin will be lost and replaced by the origin for
     the second byte.

   * Nonzero origin tags for defined values.  Consider a binary
     operator application op(x,y).  Suppose y is undefined (and so has
     a valid nonzero origin tag), and x is defined, but erroneously
     has a nonzero origin tag (defined values should have tag zero).
     If the erroneous tag has a numeric value greater than y's tag,
     then the rule for propagating origin tags though binary
     operations, which is simply to take the unsigned max of the two
     tags, will erroneously propagate x's tag rather than y's.

   * Some obscure uses of x86/amd64 byte registers can cause lossage
     or confusion of origins.  %AH .. %DH are treated as different
     from, and unrelated to, their parent registers, %EAX .. %EDX.
     So some weird sequences like

        movb undefined-value, %AH
        movb defined-value, %AL
        .. use %AX or %EAX ..

     will cause the origin attributed to %AH to be ignored, since %AL,
     %AX, %EAX are treated as the same register, and %AH as a
     completely separate one.

   But having said all that, it actually seems to work fairly well in
   practice.
*/

static UWord stats_ocacheL1_find           = 0;
static UWord stats_ocacheL1_found_at_1     = 0;
static UWord stats_ocacheL1_found_at_N     = 0;
static UWord stats_ocacheL1_misses         = 0;
static UWord stats_ocacheL1_lossage        = 0;
static UWord stats_ocacheL1_movefwds       = 0;

static UWord stats__ocacheL2_finds         = 0;
static UWord stats__ocacheL2_adds          = 0;
static UWord stats__ocacheL2_dels          = 0;
static UWord stats__ocacheL2_misses        = 0;
static UWord stats__ocacheL2_n_nodes_max   = 0;

/* Cache of 32-bit values, one every 32 bits of address space */

#define OC_BITS_PER_LINE 5
#define OC_W32S_PER_LINE (1 << (OC_BITS_PER_LINE - 2))

static INLINE UWord oc_line_offset ( Addr a ) {
   return (a >> 2) & (OC_W32S_PER_LINE - 1);
}
static INLINE Bool is_valid_oc_tag ( Addr tag ) {
   return 0 == (tag & ((1 << OC_BITS_PER_LINE) - 1));
}

#define OC_LINES_PER_SET 2

#define OC_N_SET_BITS    20
#define OC_N_SETS        (1 << OC_N_SET_BITS)

/* These settings give:
   64 bit host: ocache:  100,663,296 sizeB    67,108,864 useful
   32 bit host: ocache:   92,274,688 sizeB    67,108,864 useful
*/

#define OC_MOVE_FORWARDS_EVERY_BITS 7


/* Originally (pre Dec 2021) it was the case that this code had a
   parameterizable cache line size, set by changing OC_BITS_PER_LINE.
   However, as a result of the speedup fixes necessitated by bug 446103, that
   is no longer really the case, and much of the L1 and L2 cache code has been
   tuned specifically for the case OC_BITS_PER_LINE == 5 (that is, the line
   size is 32 bytes).  Changing that would require a bunch of re-tuning
   effort.  So let's set it in stone for now. */
STATIC_ASSERT(OC_BITS_PER_LINE == 5);
STATIC_ASSERT(OC_LINES_PER_SET == 2);

/* Fundamentally we want an OCacheLine structure (see below) as follows:
      struct {
         Addr tag;
         UInt  w32  [OC_W32S_PER_LINE];
         UChar descr[OC_W32S_PER_LINE];
      }
   However, in various places, we want to set the w32[] and descr[] arrays to
   zero, or check if they are zero.  This can be a very hot path (per bug
   446103).  So, instead, we have a union which is either those two arrays
   (OCacheLine_Main) or simply an array of ULongs (OCacheLine_W64s).  For the
   set-zero/test-zero operations, the OCacheLine_W64s are used.
*/

// To ensure that OCacheLine.descr[] will fit in an integral number of ULongs.
STATIC_ASSERT(0 == (OC_W32S_PER_LINE % 8));

#define OC_W64S_PER_MAIN /* "MAIN" meaning "struct OCacheLine_Main" */   \
           (OC_W32S_PER_LINE / 2    /* covers OCacheLine_Main.w32[] */   \
            + OC_W32S_PER_LINE / 8) /* covers OCacheLine_Main.descr[] */
STATIC_ASSERT(OC_W64S_PER_MAIN == 5);

typedef
   ULong OCacheLine_W64s[OC_W64S_PER_MAIN];

typedef
   struct {
      UInt  w32  [OC_W32S_PER_LINE];
      UChar descr[OC_W32S_PER_LINE];
   }
   OCacheLine_Main;

STATIC_ASSERT(sizeof(OCacheLine_W64s) == sizeof(OCacheLine_Main));

typedef
   struct {
      Addr  tag;
      union {
         OCacheLine_W64s w64s;
         OCacheLine_Main main;
      } u;
   }
   OCacheLine;

/* Classify and also sanity-check 'line'.  Return 'e' (empty) if not
   in use, 'n' (nonzero) if it contains at least one valid origin tag,
   and 'z' if all the represented tags are zero. */
static inline UChar classify_OCacheLine ( OCacheLine* line )
{
   UWord i;
   if (line->tag == 1/*invalid*/)
      return 'e'; /* EMPTY */
   tl_assert(is_valid_oc_tag(line->tag));

   // BEGIN fast special-case of the test loop below.  This will detect
   // zero-ness (case 'z') for a subset of cases that the loop below will,
   // hence is safe.
   if (OC_W64S_PER_MAIN == 5) {
      if (line->u.w64s[0] == 0
          && line->u.w64s[1] == 0 && line->u.w64s[2] == 0
          && line->u.w64s[3] == 0 && line->u.w64s[4] == 0) {
         return 'z';
      }
   } else {
      tl_assert2(0, "unsupported line size (classify_OCacheLine)");
   }
   // END fast special-case of the test loop below.

   for (i = 0; i < OC_W32S_PER_LINE; i++) {
      tl_assert(0 == ((~0xF) & line->u.main.descr[i]));
      if (line->u.main.w32[i] > 0 && line->u.main.descr[i] > 0)
         return 'n'; /* NONZERO - contains useful info */
   }
   return 'z'; /* ZERO - no useful info */
}

typedef
   struct {
      OCacheLine line[OC_LINES_PER_SET];
   }
   OCacheSet;

typedef
   struct {
      OCacheSet set[OC_N_SETS];
   }
   OCache;

static OCache* ocacheL1 = NULL;
static UWord   ocacheL1_event_ctr = 0;

static void init_ocacheL2 ( void ); /* fwds */
static void init_OCache ( void )
{
   UWord line, set;
   tl_assert(MC_(clo_mc_level) >= 3);
   tl_assert(ocacheL1 == NULL);
   SysRes sres = VG_(am_shadow_alloc)(sizeof(OCache));
   if (sr_isError(sres)) {
      VG_(out_of_memory_NORETURN)( "memcheck:allocating ocacheL1",
                                   sizeof(OCache), sr_Err(sres) );
   }
   ocacheL1 = (void *)(Addr)sr_Res(sres);
   tl_assert(ocacheL1 != NULL);
   for (set = 0; set < OC_N_SETS; set++) {
      for (line = 0; line < OC_LINES_PER_SET; line++) {
         ocacheL1->set[set].line[line].tag = 1/*invalid*/;
      }
   }
   init_ocacheL2();
}

static inline void moveLineForwards ( OCacheSet* set, UWord lineno )
{
   OCacheLine tmp;
   stats_ocacheL1_movefwds++;
   tl_assert(lineno > 0 && lineno < OC_LINES_PER_SET);
   tmp = set->line[lineno-1];
   set->line[lineno-1] = set->line[lineno];
   set->line[lineno] = tmp;
}

static inline void zeroise_OCacheLine ( OCacheLine* line, Addr tag ) {
   UWord i;
   if (OC_W32S_PER_LINE == 8) {
      // BEGIN fast special-case of the loop below
      tl_assert(OC_W64S_PER_MAIN == 5);
      line->u.w64s[0] = 0;
      line->u.w64s[1] = 0;
      line->u.w64s[2] = 0;
      line->u.w64s[3] = 0;
      line->u.w64s[4] = 0;
      // END fast special-case of the loop below
   } else {
      tl_assert2(0, "unsupported line size (zeroise_OCacheLine)");
      for (i = 0; i < OC_W32S_PER_LINE; i++) {
         line->u.main.w32[i] = 0; /* NO ORIGIN */
         line->u.main.descr[i] = 0; /* REALLY REALLY NO ORIGIN! */
      }
   }
   line->tag = tag;
}

//////////////////////////////////////////////////////////////
//// OCache backing store

// The backing store for ocacheL1 is, conceptually, an AVL tree of lines that
// got ejected from the L1 (a "victim cache"), and which actually contain
// useful info -- that is, for which classify_OCacheLine would return 'n' and
// no other value.  However, the tree can grow large, and searching/updating
// it can be hot paths.  Hence we "take out" 12 significant bits of the key by
// having 4096 trees, and select one using HASH_OCACHE_TAG.
//
// What that hash function returns isn't important so long as it is a pure
// function of the tag values, and is < 4096.  However, it is critical for
// performance of long SARPs.  Hence the extra shift of 11 bits.  This means
// each tree conceptually is assigned to contiguous sequences of 2048 lines in
// the "line address space", giving some locality of reference when scanning
// linearly through address space, as is done by a SARP.  Changing that 11 to
// 0 gives terrible performance on long SARPs, presumably because each new
// line is in a different tree, hence we wind up thrashing the (CPU's) caches.
//
// On 32-bit targets, we have to be a bit careful not to shift out so many
// bits that not all 2^12 trees get used.  That leads to the constraint
// (OC_BITS_PER_LINE + 11 + 12) < 32.  Note that the 11 is the only thing we
// can change here.  In this case we have OC_BITS_PER_LINE == 5, hence the
// inequality is (28 < 32) and so we're good.
//
// The value 11 was determined empirically from various Firefox runs.  10 or
// 12 also work pretty well.

static OSet* ocachesL2[4096];

STATIC_ASSERT((OC_BITS_PER_LINE + 11 + 12) < 32);
static inline UInt HASH_OCACHE_TAG ( Addr tag ) {
   return (UInt)((tag >> (OC_BITS_PER_LINE + 11)) & 0xFFF);
}

static void* ocacheL2_malloc ( const HChar* cc, SizeT szB ) {
   return VG_(malloc)(cc, szB);
}
static void ocacheL2_free ( void* v ) {
   VG_(free)( v );
}

/* Stats: # nodes currently in tree */
static UWord stats__ocacheL2_n_nodes = 0;

static void init_ocacheL2 ( void )
{
   tl_assert(sizeof(Word) == sizeof(Addr)); /* since OCacheLine.tag :: Addr */
   tl_assert(0 == offsetof(OCacheLine,tag));
   for (UInt i = 0; i < 4096; i++) {
      tl_assert(!ocachesL2[i]);
      ocachesL2[i]
         = VG_(OSetGen_Create)( offsetof(OCacheLine,tag),
                                NULL, /* fast cmp */
                                ocacheL2_malloc, "mc.ioL2", ocacheL2_free);
   }
   stats__ocacheL2_n_nodes = 0;
}

/* Find line with the given tag in the tree, or NULL if not found. */
static inline OCacheLine* ocacheL2_find_tag ( Addr tag )
{
   OCacheLine* line;
   tl_assert(is_valid_oc_tag(tag));
   stats__ocacheL2_finds++;
   OSet* oset = ocachesL2[HASH_OCACHE_TAG(tag)];
   line = VG_(OSetGen_Lookup)( oset, &tag );
   return line;
}

/* Delete the line with the given tag from the tree, if it is present, and
   free up the associated memory. */
static void ocacheL2_del_tag ( Addr tag )
{
   OCacheLine* line;
   tl_assert(is_valid_oc_tag(tag));
   stats__ocacheL2_dels++;
   OSet* oset = ocachesL2[HASH_OCACHE_TAG(tag)];
   line = VG_(OSetGen_Remove)( oset, &tag );
   if (line) {
      VG_(OSetGen_FreeNode)(oset, line);
      tl_assert(stats__ocacheL2_n_nodes > 0);
      stats__ocacheL2_n_nodes--;
   }
}

/* Add a copy of the given line to the tree.  It must not already be
   present. */
static void ocacheL2_add_line ( OCacheLine* line )
{
   OCacheLine* copy;
   tl_assert(is_valid_oc_tag(line->tag));
   OSet* oset = ocachesL2[HASH_OCACHE_TAG(line->tag)];
   copy = VG_(OSetGen_AllocNode)( oset, sizeof(OCacheLine) );
   *copy = *line;
   stats__ocacheL2_adds++;
   VG_(OSetGen_Insert)( oset, copy );
   stats__ocacheL2_n_nodes++;
   if (stats__ocacheL2_n_nodes > stats__ocacheL2_n_nodes_max)
      stats__ocacheL2_n_nodes_max = stats__ocacheL2_n_nodes;
}

////
//////////////////////////////////////////////////////////////

__attribute__((noinline))
static OCacheLine* find_OCacheLine_SLOW ( Addr a )
{
   OCacheLine *victim, *inL2;
   UChar c;
   UWord line;
   UWord setno   = (a >> OC_BITS_PER_LINE) & (OC_N_SETS - 1);
   UWord tagmask = ~((1 << OC_BITS_PER_LINE) - 1);
   UWord tag     = a & tagmask;
   tl_assert(setno < OC_N_SETS);

   /* we already tried line == 0; skip therefore. */
   for (line = 1; line < OC_LINES_PER_SET; line++) {
      if (ocacheL1->set[setno].line[line].tag == tag) {
         switch (line) {
         // with OC_LINES_PER_SET equal to 2 this is the only possible case
         case 1:
            stats_ocacheL1_found_at_1++;
            break;
#if OC_LINES_PER_SET > 2
         default:
            stats_ocacheL1_found_at_N++;
            break;
#endif
         }
         if (UNLIKELY(0 == (ocacheL1_event_ctr++
                            & ((1<<OC_MOVE_FORWARDS_EVERY_BITS)-1)))) {
            moveLineForwards( &ocacheL1->set[setno], line );
            line--;
         }
         return &ocacheL1->set[setno].line[line];
      }
   }

   /* A miss.  Use the last slot.  Implicitly this means we're
      ejecting the line in the last slot. */
   stats_ocacheL1_misses++;
   tl_assert(line == OC_LINES_PER_SET);
   line--;
   tl_assert(line > 0);

   /* First, move the to-be-ejected line to the L2 cache. */
   victim = &ocacheL1->set[setno].line[line];
   c = classify_OCacheLine(victim);
   switch (c) {
      case 'e':
         /* the line is empty (has invalid tag); ignore it. */
         break;
      case 'z':
         /* line contains zeroes.  We must ensure the backing store is
            updated accordingly, either by copying the line there
            verbatim, or by ensuring it isn't present there.  We
            choose the latter on the basis that it reduces the size of
            the backing store. */
         ocacheL2_del_tag( victim->tag );
         break;
      case 'n':
         /* line contains at least one real, useful origin.  Copy it
            to the backing store. */
         stats_ocacheL1_lossage++;
         inL2 = ocacheL2_find_tag( victim->tag );
         if (inL2) {
            *inL2 = *victim;
         } else {
            ocacheL2_add_line( victim );
         }
         break;
      default:
         tl_assert(0);
   }

   /* Now we must reload the L1 cache from the backing tree, if
      possible. */
   tl_assert(tag != victim->tag); /* stay sane */
   inL2 = ocacheL2_find_tag( tag );
   if (inL2) {
      /* We're in luck.  It's in the L2. */
      ocacheL1->set[setno].line[line] = *inL2;
   } else {
      /* Missed at both levels of the cache hierarchy.  We have to
         declare it as full of zeroes (unknown origins). */
      stats__ocacheL2_misses++;
      zeroise_OCacheLine( &ocacheL1->set[setno].line[line], tag );
   }

   /* Move it one forwards */
   moveLineForwards( &ocacheL1->set[setno], line );
   line--;

   return &ocacheL1->set[setno].line[line];
}

static INLINE OCacheLine* find_OCacheLine ( Addr a )
{
   UWord setno   = (a >> OC_BITS_PER_LINE) & (OC_N_SETS - 1);
   UWord tagmask = ~((1 << OC_BITS_PER_LINE) - 1);
   UWord tag     = a & tagmask;

   stats_ocacheL1_find++;

   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(setno >= 0 && setno < OC_N_SETS);
      tl_assert(0 == (tag & (4 * OC_W32S_PER_LINE - 1)));
   }

   if (LIKELY(ocacheL1->set[setno].line[0].tag == tag)) {
      return &ocacheL1->set[setno].line[0];
   }

   return find_OCacheLine_SLOW( a );
}

static INLINE void set_aligned_word64_Origin_to_undef ( Addr a, UInt otag )
{
   //// BEGIN inlined, specialised version of MC_(helperc_b_store8)
   //// Set the origins for a+0 .. a+7
   { OCacheLine* line;
     UWord lineoff = oc_line_offset(a);
     if (OC_ENABLE_ASSERTIONS) {
        tl_assert(lineoff >= 0
                  && lineoff < OC_W32S_PER_LINE -1/*'cos 8-aligned*/);
     }
     line = find_OCacheLine( a );
     line->u.main.descr[lineoff+0] = 0xF;
     line->u.main.descr[lineoff+1] = 0xF;
     line->u.main.w32[lineoff+0]   = otag;
     line->u.main.w32[lineoff+1]   = otag;
   }
   //// END inlined, specialised version of MC_(helperc_b_store8)
}


/*------------------------------------------------------------*/
/*--- Aligned fast case permission setters,                ---*/
/*--- for dealing with stacks                              ---*/
/*------------------------------------------------------------*/

/*--------------------- 32-bit ---------------------*/

/* Nb: by "aligned" here we mean 4-byte aligned */

static INLINE void make_aligned_word32_undefined ( Addr a )
{
  PROF_EVENT(MCPE_MAKE_ALIGNED_WORD32_UNDEFINED);

#ifndef PERF_FAST_STACK2
   make_mem_undefined(a, 4);
#else
   {
      UWord   sm_off;
      SecMap* sm;

      if (UNLIKELY(a > MAX_PRIMARY_ADDRESS)) {
         PROF_EVENT(MCPE_MAKE_ALIGNED_WORD32_UNDEFINED_SLOW);
         make_mem_undefined(a, 4);
         return;
      }

      sm                  = get_secmap_for_writing_low(a);
      sm_off              = SM_OFF(a);
      sm->vabits8[sm_off] = VA_BITS8_UNDEFINED;
   }
#endif
}

static INLINE
void make_aligned_word32_undefined_w_otag ( Addr a, UInt otag )
{
   make_aligned_word32_undefined(a);
   //// BEGIN inlined, specialised version of MC_(helperc_b_store4)
   //// Set the origins for a+0 .. a+3
   { OCacheLine* line;
     UWord lineoff = oc_line_offset(a);
     if (OC_ENABLE_ASSERTIONS) {
        tl_assert(lineoff >= 0 && lineoff < OC_W32S_PER_LINE);
     }
     line = find_OCacheLine( a );
     line->u.main.descr[lineoff] = 0xF;
     line->u.main.w32[lineoff]   = otag;
   }
   //// END inlined, specialised version of MC_(helperc_b_store4)
}

static INLINE
void make_aligned_word32_noaccess ( Addr a )
{
   PROF_EVENT(MCPE_MAKE_ALIGNED_WORD32_NOACCESS);

#ifndef PERF_FAST_STACK2
   MC_(make_mem_noaccess)(a, 4);
#else
   {
      UWord   sm_off;
      SecMap* sm;

      if (UNLIKELY(a > MAX_PRIMARY_ADDRESS)) {
         PROF_EVENT(MCPE_MAKE_ALIGNED_WORD32_NOACCESS_SLOW);
         MC_(make_mem_noaccess)(a, 4);
         return;
      }

      sm                  = get_secmap_for_writing_low(a);
      sm_off              = SM_OFF(a);
      sm->vabits8[sm_off] = VA_BITS8_NOACCESS;

      //// BEGIN inlined, specialised version of MC_(helperc_b_store4)
      //// Set the origins for a+0 .. a+3.
      if (UNLIKELY( MC_(clo_mc_level) == 3 )) {
         OCacheLine* line;
         UWord lineoff = oc_line_offset(a);
         if (OC_ENABLE_ASSERTIONS) {
            tl_assert(lineoff >= 0 && lineoff < OC_W32S_PER_LINE);
         }
         line = find_OCacheLine( a );
         line->u.main.descr[lineoff] = 0;
      }
      //// END inlined, specialised version of MC_(helperc_b_store4)
   }
#endif
}

/*--------------------- 64-bit ---------------------*/

/* Nb: by "aligned" here we mean 8-byte aligned */

static INLINE void make_aligned_word64_undefined ( Addr a )
{
   PROF_EVENT(MCPE_MAKE_ALIGNED_WORD64_UNDEFINED);

#ifndef PERF_FAST_STACK2
   make_mem_undefined(a, 8);
#else
   {
      UWord   sm_off16;
      SecMap* sm;

      if (UNLIKELY(a > MAX_PRIMARY_ADDRESS)) {
         PROF_EVENT(MCPE_MAKE_ALIGNED_WORD64_UNDEFINED_SLOW);
         make_mem_undefined(a, 8);
         return;
      }

      sm       = get_secmap_for_writing_low(a);
      sm_off16 = SM_OFF_16(a);
      sm->vabits16[sm_off16] = VA_BITS16_UNDEFINED;
   }
#endif
}

static INLINE
void make_aligned_word64_undefined_w_otag ( Addr a, UInt otag )
{
   make_aligned_word64_undefined(a);
   //// BEGIN inlined, specialised version of MC_(helperc_b_store8)
   //// Set the origins for a+0 .. a+7
   { OCacheLine* line;
     UWord lineoff = oc_line_offset(a);
     tl_assert(lineoff < OC_W32S_PER_LINE -1/*'cos 8-aligned*/);
     line = find_OCacheLine( a );
     line->u.main.descr[lineoff+0] = 0xF;
     line->u.main.descr[lineoff+1] = 0xF;
     line->u.main.w32[lineoff+0]   = otag;
     line->u.main.w32[lineoff+1]   = otag;
   }
   //// END inlined, specialised version of MC_(helperc_b_store8)
}

static INLINE
void make_aligned_word64_noaccess ( Addr a )
{
   PROF_EVENT(MCPE_MAKE_ALIGNED_WORD64_NOACCESS);

#ifndef PERF_FAST_STACK2
   MC_(make_mem_noaccess)(a, 8);
#else
   {
      UWord   sm_off16;
      SecMap* sm;

      if (UNLIKELY(a > MAX_PRIMARY_ADDRESS)) {
         PROF_EVENT(MCPE_MAKE_ALIGNED_WORD64_NOACCESS_SLOW);
         MC_(make_mem_noaccess)(a, 8);
         return;
      }

      sm       = get_secmap_for_writing_low(a);
      sm_off16 = SM_OFF_16(a);
      sm->vabits16[sm_off16] = VA_BITS16_NOACCESS;

      //// BEGIN inlined, specialised version of MC_(helperc_b_store8)
      //// Clear the origins for a+0 .. a+7.
      if (UNLIKELY( MC_(clo_mc_level) == 3 )) {
         OCacheLine* line;
         UWord lineoff = oc_line_offset(a);
         tl_assert(lineoff < OC_W32S_PER_LINE -1/*'cos 8-aligned*/);
         line = find_OCacheLine( a );
         line->u.main.descr[lineoff+0] = 0;
         line->u.main.descr[lineoff+1] = 0;
      }
      //// END inlined, specialised version of MC_(helperc_b_store8)
   }
#endif
}


/*------------------------------------------------------------*/
/*--- Stack pointer adjustment                             ---*/
/*------------------------------------------------------------*/

#ifdef PERF_FAST_STACK
#  define MAYBE_USED
#else
#  define MAYBE_USED __attribute__((unused))
#endif

/*--------------- adjustment by 4 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_4_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_4);
   if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 4, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_4(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_4);
   if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 4 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_4(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_4);
   if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-4, 4 );
   }
}

/*--------------- adjustment by 8 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_8_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_8);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP, otag );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP  , otag );
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+4, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 8, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_8(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_8);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4 );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 8 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_8(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_8);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8 );
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-8, 8 );
   }
}

/*--------------- adjustment by 12 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_12_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_12);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP  , otag );
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+8, otag );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* from previous test we don't have 8-alignment at offset +0,
         hence must have 8 alignment at offsets +4/-4.  Hence safe to
         do 4 at +0 and then 8 at +4/. */
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP  , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+4, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 12, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_12(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_12);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* from previous test we don't have 8-alignment at offset +0,
         hence must have 8 alignment at offsets +4/-4.  Hence safe to
         do 4 at +0 and then 8 at +4/. */
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4 );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 12 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_12(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_12);
   /* Note the -12 in the test */
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP-12 )) {
      /* We have 8-alignment at -12, hence ok to do 8 at -12 and 4 at
         -4. */
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-12 );
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4  );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* We have 4-alignment at +0, but we don't have 8-alignment at
         -12.  So we must have 8-alignment at -8.  Hence do 4 at -12
         and then 8 at -8. */
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-12 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8  );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-12, 12 );
   }
}

/*--------------- adjustment by 16 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_16_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_16);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Have 8-alignment at +0, hence do 8 at +0 and 8 at +8. */
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP  , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+8, otag );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Have 4 alignment at +0 but not 8; hence 8 must be at +4.
         Hence do 4 at +0, 8 at +4, 4 at +12. */
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP   , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+4 , otag );
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+12, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 16, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_16(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_16);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Have 8-alignment at +0, hence do 8 at +0 and 8 at +8. */
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Have 4 alignment at +0 but not 8; hence 8 must be at +4.
         Hence do 4 at +0, 8 at +4, 4 at +12. */
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4  );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+12 );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 16 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_16(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_16);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Have 8-alignment at +0, hence do 8 at -16 and 8 at -8. */
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8  );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* 8 alignment must be at -12.  Do 4 at -16, 8 at -12, 4 at -4. */
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-12 );
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4  );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-16, 16 );
   }
}

/*--------------- adjustment by 32 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_32_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_32);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Straightforward */
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP   , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+8 , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+16, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+24, otag );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* 8 alignment must be at +4.  Hence do 8 at +4,+12,+20 and 4 at
         +0,+28. */
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP   , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+4 , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+12, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+20, otag );
      make_aligned_word32_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+28, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 32, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_32(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_32);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Straightforward */
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+16 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+24 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* 8 alignment must be at +4.  Hence do 8 at +4,+12,+20 and 4 at
         +0,+28. */
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+12 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+20 );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+28 );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 32 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_32(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_32);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Straightforward */
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-32 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-24 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP- 8 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* 8 alignment must be at -4 etc.  Hence do 8 at -12,-20,-28 and
         4 at -32,-4. */
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-32 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-28 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-20 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-12 );
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4  );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-32, 32 );
   }
}

/*--------------- adjustment by 112 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_112_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_112);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP   , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+8 , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+16, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+24, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+32, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+40, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+48, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+56, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+64, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+72, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+80, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+88, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+96, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+104, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 112, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_112(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_112);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+16 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+24 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+32 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+40 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+48 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+56 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+64 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+72 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+80 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+88 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+96 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+104 );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 112 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_112(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_112);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-112);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-104);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-96 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-88 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-80 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-72 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-64 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-56 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-48 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-40 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-32 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-24 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP- 8 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-112, 112 );
   }
}

/*--------------- adjustment by 128 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_128_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_128);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP   , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+8 , otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+16, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+24, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+32, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+40, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+48, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+56, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+64, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+72, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+80, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+88, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+96, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+104, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+112, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+120, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 128, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_128(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_128);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+16 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+24 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+32 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+40 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+48 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+56 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+64 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+72 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+80 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+88 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+96 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+104 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+112 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+120 );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 128 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_128(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_128);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-128);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-120);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-112);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-104);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-96 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-88 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-80 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-72 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-64 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-56 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-48 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-40 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-32 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-24 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP- 8 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-128, 128 );
   }
}

/*--------------- adjustment by 144 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_144_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_144);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP,     otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+8,   otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+16,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+24,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+32,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+40,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+48,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+56,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+64,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+72,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+80,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+88,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+96,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+104, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+112, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+120, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+128, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+136, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 144, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_144(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_144);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+16 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+24 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+32 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+40 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+48 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+56 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+64 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+72 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+80 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+88 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+96 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+104 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+112 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+120 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+128 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+136 );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 144 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_144(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_144);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-144);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-136);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-128);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-120);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-112);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-104);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-96 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-88 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-80 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-72 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-64 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-56 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-48 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-40 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-32 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-24 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP- 8 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-144, 144 );
   }
}

/*--------------- adjustment by 160 bytes ---------------*/

MAYBE_USED
static void VG_REGPARM(2) mc_new_mem_stack_160_w_ECU(Addr new_SP, UInt ecu)
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK_160);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP,     otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+8,   otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+16,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+24,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+32,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+40,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+48,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+56,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+64,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+72,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+80,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+88,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+96,  otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+104, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+112, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+120, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+128, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+136, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+144, otag );
      make_aligned_word64_undefined_w_otag ( -VG_STACK_REDZONE_SZB + new_SP+152, otag );
   } else {
      MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + new_SP, 160, otag );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_new_mem_stack_160(Addr new_SP)
{
   PROF_EVENT(MCPE_NEW_MEM_STACK_160);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+16 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+24 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+32 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+40 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+48 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+56 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+64 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+72 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+80 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+88 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+96 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+104 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+112 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+120 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+128 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+136 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+144 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+152 );
   } else {
      make_mem_undefined ( -VG_STACK_REDZONE_SZB + new_SP, 160 );
   }
}

MAYBE_USED
static void VG_REGPARM(1) mc_die_mem_stack_160(Addr new_SP)
{
   PROF_EVENT(MCPE_DIE_MEM_STACK_160);
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-160);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-152);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-144);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-136);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-128);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-120);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-112);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-104);
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-96 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-88 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-80 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-72 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-64 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-56 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-48 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-40 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-32 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-24 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP- 8 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-160, 160 );
   }
}

/*--------------- adjustment by N bytes ---------------*/

static void mc_new_mem_stack_w_ECU ( Addr a, SizeT len, UInt ecu )
{
   UInt otag = ecu | MC_OKIND_STACK;
   PROF_EVENT(MCPE_NEW_MEM_STACK);
   MC_(make_mem_undefined_w_otag) ( -VG_STACK_REDZONE_SZB + a, len, otag );
}

static void mc_new_mem_stack ( Addr a, SizeT len )
{
   PROF_EVENT(MCPE_NEW_MEM_STACK);
   make_mem_undefined ( -VG_STACK_REDZONE_SZB + a, len );
}

static void mc_die_mem_stack ( Addr a, SizeT len )
{
   PROF_EVENT(MCPE_DIE_MEM_STACK);
   MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + a, len );
}


/* The AMD64 ABI says:

   "The 128-byte area beyond the location pointed to by %rsp is considered
    to be reserved and shall not be modified by signal or interrupt
    handlers.  Therefore, functions may use this area for temporary data
    that is not needed across function calls.  In particular, leaf functions
    may use this area for their entire stack frame, rather than adjusting
    the stack pointer in the prologue and epilogue.  This area is known as
    red zone [sic]."

   So after any call or return we need to mark this redzone as containing
   undefined values.

   Consider this:  we're in function f.  f calls g.  g moves rsp down
   modestly (say 16 bytes) and writes stuff all over the red zone, making it
   defined.  g returns.  f is buggy and reads from parts of the red zone
   that it didn't write on.  But because g filled that area in, f is going
   to be picking up defined V bits and so any errors from reading bits of
   the red zone it didn't write, will be missed.  The only solution I could
   think of was to make the red zone undefined when g returns to f.

   This is in accordance with the ABI, which makes it clear the redzone
   is volatile across function calls.

   The problem occurs the other way round too: f could fill the RZ up
   with defined values and g could mistakenly read them.  So the RZ
   also needs to be nuked on function calls.
*/


/* Here's a simple cache to hold nia -> ECU mappings.  It could be
   improved so as to have a lower miss rate. */

static UWord stats__nia_cache_queries = 0;
static UWord stats__nia_cache_misses  = 0;

typedef
   struct { UWord nia0; UWord ecu0;   /* nia0 maps to ecu0 */
            UWord nia1; UWord ecu1; } /* nia1 maps to ecu1 */
   WCacheEnt;

#define N_NIA_TO_ECU_CACHE 511

static WCacheEnt nia_to_ecu_cache[N_NIA_TO_ECU_CACHE];

static void init_nia_to_ecu_cache ( void )
{
   UWord       i;
   Addr        zero_addr = 0;
   ExeContext* zero_ec;
   UInt        zero_ecu;
   /* Fill all the slots with an entry for address zero, and the
      relevant otags accordingly.  Hence the cache is initially filled
      with valid data. */
   zero_ec = VG_(make_depth_1_ExeContext_from_Addr)(zero_addr);
   tl_assert(zero_ec);
   zero_ecu = VG_(get_ECU_from_ExeContext)(zero_ec);
   tl_assert(VG_(is_plausible_ECU)(zero_ecu));
   for (i = 0; i < N_NIA_TO_ECU_CACHE; i++) {
      nia_to_ecu_cache[i].nia0 = zero_addr;
      nia_to_ecu_cache[i].ecu0 = zero_ecu;
      nia_to_ecu_cache[i].nia1 = zero_addr;
      nia_to_ecu_cache[i].ecu1 = zero_ecu;
   }
}

static inline UInt convert_nia_to_ecu ( Addr nia )
{
   UWord i;
   UInt        ecu;
   ExeContext* ec;

   tl_assert( sizeof(nia_to_ecu_cache[0].nia1) == sizeof(nia) );

   stats__nia_cache_queries++;
   i = nia % N_NIA_TO_ECU_CACHE;
   tl_assert(i < N_NIA_TO_ECU_CACHE);

   if (LIKELY( nia_to_ecu_cache[i].nia0 == nia ))
      return nia_to_ecu_cache[i].ecu0;

   if (LIKELY( nia_to_ecu_cache[i].nia1 == nia )) {
#     define SWAP(_w1,_w2) { UWord _t = _w1; _w1 = _w2; _w2 = _t; }
      SWAP( nia_to_ecu_cache[i].nia0, nia_to_ecu_cache[i].nia1 );
      SWAP( nia_to_ecu_cache[i].ecu0, nia_to_ecu_cache[i].ecu1 );
#     undef SWAP
      return nia_to_ecu_cache[i].ecu0;
   }

   stats__nia_cache_misses++;
   ec = VG_(make_depth_1_ExeContext_from_Addr)(nia);
   tl_assert(ec);
   ecu = VG_(get_ECU_from_ExeContext)(ec);
   tl_assert(VG_(is_plausible_ECU)(ecu));

   nia_to_ecu_cache[i].nia1 = nia_to_ecu_cache[i].nia0;
   nia_to_ecu_cache[i].ecu1 = nia_to_ecu_cache[i].ecu0;

   nia_to_ecu_cache[i].nia0 = nia;
   nia_to_ecu_cache[i].ecu0 = (UWord)ecu;
   return ecu;
}


/* This marks the stack as addressible but undefined, after a call or
   return for a target that has an ABI defined stack redzone.  It
   happens quite a lot and needs to be fast.  This is the version for
   origin tracking.  The non-origin-tracking version is below. */
VG_REGPARM(3)
void MC_(helperc_MAKE_STACK_UNINIT_w_o) ( Addr base, UWord len, Addr nia )
{
   PROF_EVENT(MCPE_MAKE_STACK_UNINIT_W_O);
   if (0)
      VG_(printf)("helperc_MAKE_STACK_UNINIT_w_o (%#lx,%lu,nia=%#lx)\n",
                  base, len, nia );

   UInt ecu = convert_nia_to_ecu ( nia );
   tl_assert(VG_(is_plausible_ECU)(ecu));

   UInt otag = ecu | MC_OKIND_STACK;

#  if 0
   /* Slow(ish) version, which is fairly easily seen to be correct.
   */
   if (LIKELY( VG_IS_8_ALIGNED(base) && len==128 )) {
      make_aligned_word64_undefined_w_otag(base +   0, otag);
      make_aligned_word64_undefined_w_otag(base +   8, otag);
      make_aligned_word64_undefined_w_otag(base +  16, otag);
      make_aligned_word64_undefined_w_otag(base +  24, otag);

      make_aligned_word64_undefined_w_otag(base +  32, otag);
      make_aligned_word64_undefined_w_otag(base +  40, otag);
      make_aligned_word64_undefined_w_otag(base +  48, otag);
      make_aligned_word64_undefined_w_otag(base +  56, otag);

      make_aligned_word64_undefined_w_otag(base +  64, otag);
      make_aligned_word64_undefined_w_otag(base +  72, otag);
      make_aligned_word64_undefined_w_otag(base +  80, otag);
      make_aligned_word64_undefined_w_otag(base +  88, otag);

      make_aligned_word64_undefined_w_otag(base +  96, otag);
      make_aligned_word64_undefined_w_otag(base + 104, otag);
      make_aligned_word64_undefined_w_otag(base + 112, otag);
      make_aligned_word64_undefined_w_otag(base + 120, otag);
   } else {
      MC_(make_mem_undefined_w_otag)(base, len, otag);
   }
#  endif

   /* Idea is: go fast when
         * 8-aligned and length is 128
         * the sm is available in the main primary map
         * the address range falls entirely with a single secondary map
      If all those conditions hold, just update the V+A bits by writing
      directly into the vabits array.  (If the sm was distinguished, this
      will make a copy and then write to it.)
   */
   if (LIKELY( len == 128 && VG_IS_8_ALIGNED(base) )) {
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 128 - 1);
      tl_assert(a_lo < a_hi);             // paranoia: detect overflow
      if (LIKELY(a_hi <= MAX_PRIMARY_ADDRESS)) {
         /* Now we know the entire range is within the main primary map. */
         UWord pm_off_lo = get_primary_map_low_offset(a_lo);
         UWord pm_off_hi = get_primary_map_low_offset(a_hi);
         if (LIKELY(pm_off_lo == pm_off_hi)) {
           /* Now we know that the entire address range falls within a
              single secondary map, and that that secondary 'lives' in
              the main primary map. */
            SecMap* sm      = get_secmap_for_writing_low(a_lo);
            UWord   v_off16 = SM_OFF_16(a_lo);
            UShort* p       = &sm->vabits16[v_off16];
            p[ 0] = VA_BITS16_UNDEFINED;
            p[ 1] = VA_BITS16_UNDEFINED;
            p[ 2] = VA_BITS16_UNDEFINED;
            p[ 3] = VA_BITS16_UNDEFINED;
            p[ 4] = VA_BITS16_UNDEFINED;
            p[ 5] = VA_BITS16_UNDEFINED;
            p[ 6] = VA_BITS16_UNDEFINED;
            p[ 7] = VA_BITS16_UNDEFINED;
            p[ 8] = VA_BITS16_UNDEFINED;
            p[ 9] = VA_BITS16_UNDEFINED;
            p[10] = VA_BITS16_UNDEFINED;
            p[11] = VA_BITS16_UNDEFINED;
            p[12] = VA_BITS16_UNDEFINED;
            p[13] = VA_BITS16_UNDEFINED;
            p[14] = VA_BITS16_UNDEFINED;
            p[15] = VA_BITS16_UNDEFINED;
            set_aligned_word64_Origin_to_undef( base + 8 * 0, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 1, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 2, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 3, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 4, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 5, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 6, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 7, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 8, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 9, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 10, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 11, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 12, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 13, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 14, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 15, otag );
            return;
         }
      }
   }

   /* 288 bytes (36 ULongs) is the magic value for ELF ppc64. */
   if (LIKELY( len == 288 && VG_IS_8_ALIGNED(base) )) {
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 288 - 1);
      tl_assert(a_lo < a_hi);             // paranoia: detect overflow
      if (a_hi <= MAX_PRIMARY_ADDRESS) {
         UWord pm_off_lo = get_primary_map_low_offset(a_lo);
         UWord pm_off_hi = get_primary_map_low_offset(a_hi);
         if (LIKELY(pm_off_lo == pm_off_hi)) {
           /* Now we know that the entire address range falls within a
              single secondary map, and that that secondary 'lives' in
              the main primary map. */
            SecMap* sm      = get_secmap_for_writing_low(a_lo);
            UWord   v_off16 = SM_OFF_16(a_lo);
            UShort* p       = &sm->vabits16[v_off16];
            p[ 0] = VA_BITS16_UNDEFINED;
            p[ 1] = VA_BITS16_UNDEFINED;
            p[ 2] = VA_BITS16_UNDEFINED;
            p[ 3] = VA_BITS16_UNDEFINED;
            p[ 4] = VA_BITS16_UNDEFINED;
            p[ 5] = VA_BITS16_UNDEFINED;
            p[ 6] = VA_BITS16_UNDEFINED;
            p[ 7] = VA_BITS16_UNDEFINED;
            p[ 8] = VA_BITS16_UNDEFINED;
            p[ 9] = VA_BITS16_UNDEFINED;
            p[10] = VA_BITS16_UNDEFINED;
            p[11] = VA_BITS16_UNDEFINED;
            p[12] = VA_BITS16_UNDEFINED;
            p[13] = VA_BITS16_UNDEFINED;
            p[14] = VA_BITS16_UNDEFINED;
            p[15] = VA_BITS16_UNDEFINED;
            p[16] = VA_BITS16_UNDEFINED;
            p[17] = VA_BITS16_UNDEFINED;
            p[18] = VA_BITS16_UNDEFINED;
            p[19] = VA_BITS16_UNDEFINED;
            p[20] = VA_BITS16_UNDEFINED;
            p[21] = VA_BITS16_UNDEFINED;
            p[22] = VA_BITS16_UNDEFINED;
            p[23] = VA_BITS16_UNDEFINED;
            p[24] = VA_BITS16_UNDEFINED;
            p[25] = VA_BITS16_UNDEFINED;
            p[26] = VA_BITS16_UNDEFINED;
            p[27] = VA_BITS16_UNDEFINED;
            p[28] = VA_BITS16_UNDEFINED;
            p[29] = VA_BITS16_UNDEFINED;
            p[30] = VA_BITS16_UNDEFINED;
            p[31] = VA_BITS16_UNDEFINED;
            p[32] = VA_BITS16_UNDEFINED;
            p[33] = VA_BITS16_UNDEFINED;
            p[34] = VA_BITS16_UNDEFINED;
            p[35] = VA_BITS16_UNDEFINED;
            set_aligned_word64_Origin_to_undef( base + 8 * 0, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 1, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 2, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 3, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 4, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 5, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 6, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 7, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 8, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 9, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 10, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 11, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 12, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 13, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 14, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 15, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 16, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 17, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 18, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 19, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 20, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 21, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 22, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 23, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 24, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 25, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 26, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 27, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 28, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 29, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 30, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 31, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 32, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 33, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 34, otag );
            set_aligned_word64_Origin_to_undef( base + 8 * 35, otag );
            return;
         }
      }
   }

   /* else fall into slow case */
   MC_(make_mem_undefined_w_otag)(base, len, otag);
}


/* This is a version of MC_(helperc_MAKE_STACK_UNINIT_w_o) that is
   specialised for the non-origin-tracking case. */
VG_REGPARM(2)
void MC_(helperc_MAKE_STACK_UNINIT_no_o) ( Addr base, UWord len )
{
   PROF_EVENT(MCPE_MAKE_STACK_UNINIT_NO_O);
   if (0)
      VG_(printf)("helperc_MAKE_STACK_UNINIT_no_o (%#lx,%lu)\n",
                  base, len );

#  if 0
   /* Slow(ish) version, which is fairly easily seen to be correct.
   */
   if (LIKELY( VG_IS_8_ALIGNED(base) && len==128 )) {
      make_aligned_word64_undefined(base +   0);
      make_aligned_word64_undefined(base +   8);
      make_aligned_word64_undefined(base +  16);
      make_aligned_word64_undefined(base +  24);

      make_aligned_word64_undefined(base +  32);
      make_aligned_word64_undefined(base +  40);
      make_aligned_word64_undefined(base +  48);
      make_aligned_word64_undefined(base +  56);

      make_aligned_word64_undefined(base +  64);
      make_aligned_word64_undefined(base +  72);
      make_aligned_word64_undefined(base +  80);
      make_aligned_word64_undefined(base +  88);

      make_aligned_word64_undefined(base +  96);
      make_aligned_word64_undefined(base + 104);
      make_aligned_word64_undefined(base + 112);
      make_aligned_word64_undefined(base + 120);
   } else {
      make_mem_undefined(base, len);
   }
#  endif

   /* Idea is: go fast when
         * 8-aligned and length is 128
         * the sm is available in the main primary map
         * the address range falls entirely with a single secondary map
      If all those conditions hold, just update the V+A bits by writing
      directly into the vabits array.  (If the sm was distinguished, this
      will make a copy and then write to it.)
   */
   if (LIKELY( len == 128 && VG_IS_8_ALIGNED(base) )) {
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 128 - 1);
      tl_assert(a_lo < a_hi);             // paranoia: detect overflow
      if (LIKELY(a_hi <= MAX_PRIMARY_ADDRESS)) {
         /* Now we know the entire range is within the main primary map. */
         UWord pm_off_lo = get_primary_map_low_offset(a_lo);
         UWord pm_off_hi = get_primary_map_low_offset(a_hi);
         if (LIKELY(pm_off_lo == pm_off_hi)) {
           /* Now we know that the entire address range falls within a
              single secondary map, and that that secondary 'lives' in
              the main primary map. */
            SecMap* sm      = get_secmap_for_writing_low(a_lo);
            UWord   v_off16 = SM_OFF_16(a_lo);
            UShort* p       = &sm->vabits16[v_off16];
            p[ 0] = VA_BITS16_UNDEFINED;
            p[ 1] = VA_BITS16_UNDEFINED;
            p[ 2] = VA_BITS16_UNDEFINED;
            p[ 3] = VA_BITS16_UNDEFINED;
            p[ 4] = VA_BITS16_UNDEFINED;
            p[ 5] = VA_BITS16_UNDEFINED;
            p[ 6] = VA_BITS16_UNDEFINED;
            p[ 7] = VA_BITS16_UNDEFINED;
            p[ 8] = VA_BITS16_UNDEFINED;
            p[ 9] = VA_BITS16_UNDEFINED;
            p[10] = VA_BITS16_UNDEFINED;
            p[11] = VA_BITS16_UNDEFINED;
            p[12] = VA_BITS16_UNDEFINED;
            p[13] = VA_BITS16_UNDEFINED;
            p[14] = VA_BITS16_UNDEFINED;
            p[15] = VA_BITS16_UNDEFINED;
            return;
         }
      }
   }

   /* 288 bytes (36 ULongs) is the magic value for ELF ppc64. */
   if (LIKELY( len == 288 && VG_IS_8_ALIGNED(base) )) {
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 288 - 1);
      tl_assert(a_lo < a_hi);             // paranoia: detect overflow
      if (a_hi <= MAX_PRIMARY_ADDRESS) {
         UWord pm_off_lo = get_primary_map_low_offset(a_lo);
         UWord pm_off_hi = get_primary_map_low_offset(a_hi);
         if (LIKELY(pm_off_lo == pm_off_hi)) {
           /* Now we know that the entire address range falls within a
              single secondary map, and that that secondary 'lives' in
              the main primary map. */
            SecMap* sm      = get_secmap_for_writing_low(a_lo);
            UWord   v_off16 = SM_OFF_16(a_lo);
            UShort* p       = &sm->vabits16[v_off16];
            p[ 0] = VA_BITS16_UNDEFINED;
            p[ 1] = VA_BITS16_UNDEFINED;
            p[ 2] = VA_BITS16_UNDEFINED;
            p[ 3] = VA_BITS16_UNDEFINED;
            p[ 4] = VA_BITS16_UNDEFINED;
            p[ 5] = VA_BITS16_UNDEFINED;
            p[ 6] = VA_BITS16_UNDEFINED;
            p[ 7] = VA_BITS16_UNDEFINED;
            p[ 8] = VA_BITS16_UNDEFINED;
            p[ 9] = VA_BITS16_UNDEFINED;
            p[10] = VA_BITS16_UNDEFINED;
            p[11] = VA_BITS16_UNDEFINED;
            p[12] = VA_BITS16_UNDEFINED;
            p[13] = VA_BITS16_UNDEFINED;
            p[14] = VA_BITS16_UNDEFINED;
            p[15] = VA_BITS16_UNDEFINED;
            p[16] = VA_BITS16_UNDEFINED;
            p[17] = VA_BITS16_UNDEFINED;
            p[18] = VA_BITS16_UNDEFINED;
            p[19] = VA_BITS16_UNDEFINED;
            p[20] = VA_BITS16_UNDEFINED;
            p[21] = VA_BITS16_UNDEFINED;
            p[22] = VA_BITS16_UNDEFINED;
            p[23] = VA_BITS16_UNDEFINED;
            p[24] = VA_BITS16_UNDEFINED;
            p[25] = VA_BITS16_UNDEFINED;
            p[26] = VA_BITS16_UNDEFINED;
            p[27] = VA_BITS16_UNDEFINED;
            p[28] = VA_BITS16_UNDEFINED;
            p[29] = VA_BITS16_UNDEFINED;
            p[30] = VA_BITS16_UNDEFINED;
            p[31] = VA_BITS16_UNDEFINED;
            p[32] = VA_BITS16_UNDEFINED;
            p[33] = VA_BITS16_UNDEFINED;
            p[34] = VA_BITS16_UNDEFINED;
            p[35] = VA_BITS16_UNDEFINED;
            return;
         }
      }
   }

   /* else fall into slow case */
   make_mem_undefined(base, len);
}


/* And this is an even more specialised case, for the case where there
   is no origin tracking, and the length is 128. */
VG_REGPARM(1)
void MC_(helperc_MAKE_STACK_UNINIT_128_no_o) ( Addr base )
{
   PROF_EVENT(MCPE_MAKE_STACK_UNINIT_128_NO_O);
   if (0)
      VG_(printf)("helperc_MAKE_STACK_UNINIT_128_no_o (%#lx)\n", base );

#  if 0
   /* Slow(ish) version, which is fairly easily seen to be correct.
   */
   if (LIKELY( VG_IS_8_ALIGNED(base) )) {
      make_aligned_word64_undefined(base +   0);
      make_aligned_word64_undefined(base +   8);
      make_aligned_word64_undefined(base +  16);
      make_aligned_word64_undefined(base +  24);

      make_aligned_word64_undefined(base +  32);
      make_aligned_word64_undefined(base +  40);
      make_aligned_word64_undefined(base +  48);
      make_aligned_word64_undefined(base +  56);

      make_aligned_word64_undefined(base +  64);
      make_aligned_word64_undefined(base +  72);
      make_aligned_word64_undefined(base +  80);
      make_aligned_word64_undefined(base +  88);

      make_aligned_word64_undefined(base +  96);
      make_aligned_word64_undefined(base + 104);
      make_aligned_word64_undefined(base + 112);
      make_aligned_word64_undefined(base + 120);
   } else {
      make_mem_undefined(base, 128);
   }
#  endif

   /* Idea is: go fast when
         * 16-aligned and length is 128
         * the sm is available in the main primary map
         * the address range falls entirely with a single secondary map
      If all those conditions hold, just update the V+A bits by writing
      directly into the vabits array.  (If the sm was distinguished, this
      will make a copy and then write to it.)

      Typically this applies to amd64 'ret' instructions, since RSP is
      16-aligned (0 % 16) after the instruction (per the amd64-ELF ABI).
   */
   if (LIKELY( VG_IS_16_ALIGNED(base) )) {
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 128 - 1);
      /* FIXME: come up with a sane story on the wraparound case
         (which of course cnanot happen, but still..) */
      /* tl_assert(a_lo < a_hi); */            // paranoia: detect overflow
      if (LIKELY(a_hi <= MAX_PRIMARY_ADDRESS)) {
         /* Now we know the entire range is within the main primary map. */
         UWord pm_off_lo = get_primary_map_low_offset(a_lo);
         UWord pm_off_hi = get_primary_map_low_offset(a_hi);
         if (LIKELY(pm_off_lo == pm_off_hi)) {
           /* Now we know that the entire address range falls within a
              single secondary map, and that that secondary 'lives' in
              the main primary map. */
            PROF_EVENT(MCPE_MAKE_STACK_UNINIT_128_NO_O_ALIGNED_16);
            SecMap* sm    = get_secmap_for_writing_low(a_lo);
            UWord   v_off = SM_OFF(a_lo);
            UInt*   w32   = ASSUME_ALIGNED(UInt*, &sm->vabits8[v_off]);
            w32[ 0] = VA_BITS32_UNDEFINED;
            w32[ 1] = VA_BITS32_UNDEFINED;
            w32[ 2] = VA_BITS32_UNDEFINED;
            w32[ 3] = VA_BITS32_UNDEFINED;
            w32[ 4] = VA_BITS32_UNDEFINED;
            w32[ 5] = VA_BITS32_UNDEFINED;
            w32[ 6] = VA_BITS32_UNDEFINED;
            w32[ 7] = VA_BITS32_UNDEFINED;
            return;
         }
      }
   }

   /* The same, but for when base is 8 % 16, which is the situation
      with RSP for amd64-ELF immediately after call instructions.
   */
   if (LIKELY( VG_IS_16_ALIGNED(base+8) )) { // restricts to 8 aligned
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 128 - 1);
      /* FIXME: come up with a sane story on the wraparound case
         (which of course cnanot happen, but still..) */
      /* tl_assert(a_lo < a_hi); */            // paranoia: detect overflow
      if (LIKELY(a_hi <= MAX_PRIMARY_ADDRESS)) {
         /* Now we know the entire range is within the main primary map. */
         UWord pm_off_lo = get_primary_map_low_offset(a_lo);
         UWord pm_off_hi = get_primary_map_low_offset(a_hi);
         if (LIKELY(pm_off_lo == pm_off_hi)) {
            PROF_EVENT(MCPE_MAKE_STACK_UNINIT_128_NO_O_ALIGNED_8);
           /* Now we know that the entire address range falls within a
              single secondary map, and that that secondary 'lives' in
              the main primary map. */
            SecMap* sm      = get_secmap_for_writing_low(a_lo);
            UWord   v_off16 = SM_OFF_16(a_lo);
            UShort* w16     = &sm->vabits16[v_off16];
            UInt*   w32     = ASSUME_ALIGNED(UInt*, &w16[1]);
            /* The following assertion is commented out for obvious
               performance reasons, but was verified as valid when
               running the entire testsuite and also Firefox. */
            /* tl_assert(VG_IS_4_ALIGNED(w32)); */
            w16[ 0] = VA_BITS16_UNDEFINED; // w16[0]
            w32[ 0] = VA_BITS32_UNDEFINED; // w16[1,2]
            w32[ 1] = VA_BITS32_UNDEFINED; // w16[3,4]
            w32[ 2] = VA_BITS32_UNDEFINED; // w16[5,6]
            w32[ 3] = VA_BITS32_UNDEFINED; // w16[7,8]
            w32[ 4] = VA_BITS32_UNDEFINED; // w16[9,10]
            w32[ 5] = VA_BITS32_UNDEFINED; // w16[11,12]
            w32[ 6] = VA_BITS32_UNDEFINED; // w16[13,14]
            w16[15] = VA_BITS16_UNDEFINED; // w16[15]
            return;
         }
      }
   }

   /* else fall into slow case */
   PROF_EVENT(MCPE_MAKE_STACK_UNINIT_128_NO_O_SLOWCASE);
   make_mem_undefined(base, 128);
}


/*------------------------------------------------------------*/
/*--- Checking memory                                      ---*/
/*------------------------------------------------------------*/

typedef
   enum {
      MC_Ok = 5,
      MC_AddrErr = 6,
      MC_ValueErr = 7
   }
   MC_ReadResult;


/* Check permissions for address range.  If inadequate permissions
   exist, *bad_addr is set to the offending address, so the caller can
   know what it is. */

/* Returns True if [a .. a+len) is not addressible.  Otherwise,
   returns False, and if bad_addr is non-NULL, sets *bad_addr to
   indicate the lowest failing address.  Functions below are
   similar. */
Bool MC_(check_mem_is_noaccess) ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UWord vabits2;

   PROF_EVENT(MCPE_CHECK_MEM_IS_NOACCESS);
   for (i = 0; i < len; i++) {
      PROF_EVENT(MCPE_CHECK_MEM_IS_NOACCESS_LOOP);
      vabits2 = get_vabits2(a);
      if (VA_BITS2_NOACCESS != vabits2) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
}

static Bool is_mem_addressable ( Addr a, SizeT len,
                                 /*OUT*/Addr* bad_addr )
{
   SizeT i;
   UWord vabits2;

   PROF_EVENT(MCPE_IS_MEM_ADDRESSABLE);
   for (i = 0; i < len; i++) {
      PROF_EVENT(MCPE_IS_MEM_ADDRESSABLE_LOOP);
      vabits2 = get_vabits2(a);
      if (VA_BITS2_NOACCESS == vabits2) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
}

static MC_ReadResult is_mem_defined ( Addr a, SizeT len,
                                      /*OUT*/Addr* bad_addr,
                                      /*OUT*/UInt* otag )
{
   SizeT i;
   UWord vabits2;

   PROF_EVENT(MCPE_IS_MEM_DEFINED);
   DEBUG("is_mem_defined\n");

   if (otag)     *otag = 0;
   if (bad_addr) *bad_addr = 0;
   for (i = 0; i < len; i++) {
      PROF_EVENT(MCPE_IS_MEM_DEFINED_LOOP);
      vabits2 = get_vabits2(a);
      if (VA_BITS2_DEFINED != vabits2) {
         // Error!  Nb: Report addressability errors in preference to
         // definedness errors.  And don't report definedeness errors unless
         // --undef-value-errors=yes.
         if (bad_addr) {
            *bad_addr = a;
         }
         if (VA_BITS2_NOACCESS == vabits2) {
            return MC_AddrErr;
         }
         if (MC_(clo_mc_level) >= 2) {
            if (otag && MC_(clo_mc_level) == 3) {
               *otag = MC_(helperc_b_load1)( a );
            }
            return MC_ValueErr;
         }
      }
      a++;
   }
   return MC_Ok;
}


/* Like is_mem_defined but doesn't give up at the first uninitialised
   byte -- the entire range is always checked.  This is important for
   detecting errors in the case where a checked range strays into
   invalid memory, but that fact is not detected by the ordinary
   is_mem_defined(), because of an undefined section that precedes the
   out of range section, possibly as a result of an alignment hole in
   the checked data.  This version always checks the entire range and
   can report both a definedness and an accessbility error, if
   necessary. */
static void is_mem_defined_comprehensive (
               Addr a, SizeT len,
               /*OUT*/Bool* errorV,    /* is there a definedness err? */
               /*OUT*/Addr* bad_addrV, /* if so where? */
               /*OUT*/UInt* otagV,     /* and what's its otag? */
               /*OUT*/Bool* errorA,    /* is there an addressability err? */
               /*OUT*/Addr* bad_addrA  /* if so where? */
            )
{
   SizeT i;
   UWord vabits2;
   Bool  already_saw_errV = False;

   PROF_EVENT(MCPE_IS_MEM_DEFINED_COMPREHENSIVE);
   DEBUG("is_mem_defined_comprehensive\n");

   tl_assert(!(*errorV || *errorA));

   for (i = 0; i < len; i++) {
      PROF_EVENT(MCPE_IS_MEM_DEFINED_COMPREHENSIVE_LOOP);
      vabits2 = get_vabits2(a);
      switch (vabits2) {
         case VA_BITS2_DEFINED:
            a++;
            break;
         case VA_BITS2_UNDEFINED:
         case VA_BITS2_PARTDEFINED:
            if (!already_saw_errV) {
               *errorV    = True;
               *bad_addrV = a;
               if (MC_(clo_mc_level) == 3) {
                  *otagV = MC_(helperc_b_load1)( a );
               } else {
                  *otagV = 0;
               }
               already_saw_errV = True;
            }
            a++; /* keep going */
            break;
         case VA_BITS2_NOACCESS:
            *errorA    = True;
            *bad_addrA = a;
            return; /* give up now. */
         default:
            tl_assert(0);
      }
   }
}


/* Check a zero-terminated ascii string.  Tricky -- don't want to
   examine the actual bytes, to find the end, until we're sure it is
   safe to do so. */

static Bool mc_is_defined_asciiz ( Addr a, Addr* bad_addr, UInt* otag )
{
   UWord vabits2;

   PROF_EVENT(MCPE_IS_DEFINED_ASCIIZ);
   DEBUG("mc_is_defined_asciiz\n");

   if (otag)     *otag = 0;
   if (bad_addr) *bad_addr = 0;
   while (True) {
      PROF_EVENT(MCPE_IS_DEFINED_ASCIIZ_LOOP);
      vabits2 = get_vabits2(a);
      if (VA_BITS2_DEFINED != vabits2) {
         // Error!  Nb: Report addressability errors in preference to
         // definedness errors.  And don't report definedeness errors unless
         // --undef-value-errors=yes.
         if (bad_addr) {
            *bad_addr = a;
         }
         if (VA_BITS2_NOACCESS == vabits2) {
            return MC_AddrErr;
         }
         if (MC_(clo_mc_level) >= 2) {
            if (otag && MC_(clo_mc_level) == 3) {
               *otag = MC_(helperc_b_load1)( a );
            }
            return MC_ValueErr;
         }
      }
      /* Ok, a is safe to read. */
      if (* ((UChar*)a) == 0) {
         return MC_Ok;
      }
      a++;
   }
}


/*------------------------------------------------------------*/
/*--- Memory event handlers                                ---*/
/*------------------------------------------------------------*/

static
void check_mem_is_addressable ( CorePart part, ThreadId tid, const HChar* s,
                                Addr base, SizeT size )
{
   Addr bad_addr;
   Bool ok = is_mem_addressable ( base, size, &bad_addr );

   if (!ok) {
      switch (part) {
      case Vg_CoreSysCall:
         MC_(record_memparam_error) ( tid, bad_addr,
                                      /*isAddrErr*/True, s, 0/*otag*/ );
         break;

      case Vg_CoreSignal:
         MC_(record_core_mem_error)( tid, s );
         break;

      default:
         VG_(tool_panic)("check_mem_is_addressable: unexpected CorePart");
      }
   }
}

static
void check_mem_is_defined ( CorePart part, ThreadId tid, const HChar* s,
                            Addr base, SizeT size )
{
   UInt otag = 0;
   Addr bad_addr;
   MC_ReadResult res = is_mem_defined ( base, size, &bad_addr, &otag );

   if (MC_Ok != res) {
      Bool isAddrErr = ( MC_AddrErr == res ? True : False );

      switch (part) {
      case Vg_CoreSysCall:
         MC_(record_memparam_error) ( tid, bad_addr, isAddrErr, s,
                                      isAddrErr ? 0 : otag );
         break;

      case Vg_CoreSysCallArgInMem:
         MC_(record_regparam_error) ( tid, s, otag );
         break;

      /* If we're being asked to jump to a silly address, record an error
         message before potentially crashing the entire system. */
      case Vg_CoreTranslate:
         MC_(record_jump_error)( tid, bad_addr );
         break;

      default:
         VG_(tool_panic)("check_mem_is_defined: unexpected CorePart");
      }
   }
}

static
void check_mem_is_defined_asciiz ( CorePart part, ThreadId tid,
                                   const HChar* s, Addr str )
{
   MC_ReadResult res;
   Addr bad_addr = 0;   // shut GCC up
   UInt otag = 0;

   tl_assert(part == Vg_CoreSysCall);
   res = mc_is_defined_asciiz ( (Addr)str, &bad_addr, &otag );
   if (MC_Ok != res) {
      Bool isAddrErr = ( MC_AddrErr == res ? True : False );
      MC_(record_memparam_error) ( tid, bad_addr, isAddrErr, s,
                                   isAddrErr ? 0 : otag );
   }
}

/* Handling of mmap and mprotect is not as simple as it seems.

   The underlying semantics are that memory obtained from mmap is
   always initialised, but may be inaccessible.  And changes to the
   protection of memory do not change its contents and hence not its
   definedness state.  Problem is we can't model
   inaccessible-but-with-some-definedness state; once we mark memory
   as inaccessible we lose all info about definedness, and so can't
   restore that if it is later made accessible again.

   One obvious thing to do is this:

      mmap/mprotect NONE  -> noaccess
      mmap/mprotect other -> defined

   The problem case here is: taking accessible memory, writing
   uninitialised data to it, mprotecting it NONE and later mprotecting
   it back to some accessible state causes the undefinedness to be
   lost.

   A better proposal is:

     (1) mmap NONE       ->  make noaccess
     (2) mmap other      ->  make defined

     (3) mprotect NONE   ->  # no change
     (4) mprotect other  ->  change any "noaccess" to "defined"

   (2) is OK because memory newly obtained from mmap really is defined
       (zeroed out by the kernel -- doing anything else would
       constitute a massive security hole.)

   (1) is OK because the only way to make the memory usable is via
       (4), in which case we also wind up correctly marking it all as
       defined.

   (3) is the weak case.  We choose not to change memory state.
       (presumably the range is in some mixture of "defined" and
       "undefined", viz, accessible but with arbitrary V bits).  Doing
       nothing means we retain the V bits, so that if the memory is
       later mprotected "other", the V bits remain unchanged, so there
       can be no false negatives.  The bad effect is that if there's
       an access in the area, then MC cannot warn; but at least we'll
       get a SEGV to show, so it's better than nothing.

   Consider the sequence (3) followed by (4).  Any memory that was
   "defined" or "undefined" previously retains its state (as
   required).  Any memory that was "noaccess" before can only have
   been made that way by (1), and so it's OK to change it to
   "defined".

   See https://bugs.kde.org/show_bug.cgi?id=205541
   and https://bugs.kde.org/show_bug.cgi?id=210268
*/
static
void mc_new_mem_mmap ( Addr a, SizeT len, Bool rr, Bool ww, Bool xx,
                       ULong di_handle )
{
   if (rr || ww || xx) {
      /* (2) mmap/mprotect other -> defined */
      MC_(make_mem_defined)(a, len);
   } else {
      /* (1) mmap/mprotect NONE  -> noaccess */
      MC_(make_mem_noaccess)(a, len);
   }
}

static
void mc_new_mem_mprotect ( Addr a, SizeT len, Bool rr, Bool ww, Bool xx )
{
   if (rr || ww || xx) {
      /* (4) mprotect other  ->  change any "noaccess" to "defined" */
      make_mem_defined_if_noaccess(a, len);
   } else {
      /* (3) mprotect NONE   ->  # no change */
      /* do nothing */
   }
}


static
void mc_new_mem_startup( Addr a, SizeT len,
                         Bool rr, Bool ww, Bool xx, ULong di_handle )
{
   // Because code is defined, initialised variables get put in the data
   // segment and are defined, and uninitialised variables get put in the
   // bss segment and are auto-zeroed (and so defined).
   //
   // It's possible that there will be padding between global variables.
   // This will also be auto-zeroed, and marked as defined by Memcheck.  If
   // a program uses it, Memcheck will not complain.  This is arguably a
   // false negative, but it's a grey area -- the behaviour is defined (the
   // padding is zeroed) but it's probably not what the user intended.  And
   // we can't avoid it.
   //
   // Note: we generally ignore RWX permissions, because we can't track them
   // without requiring more than one A bit which would slow things down a
   // lot.  But on Darwin the 0th page is mapped but !R and !W and !X.
   // So we mark any such pages as "unaddressable".
   DEBUG("mc_new_mem_startup(%#lx, %llu, rr=%u, ww=%u, xx=%u)\n",
         a, (ULong)len, rr, ww, xx);
   mc_new_mem_mmap(a, len, rr, ww, xx, di_handle);
}

static
void mc_post_mem_write(CorePart part, ThreadId tid, Addr a, SizeT len)
{
   MC_(make_mem_defined)(a, len);
}


/*------------------------------------------------------------*/
/*--- Register event handlers                              ---*/
/*------------------------------------------------------------*/

/* Try and get a nonzero origin for the guest state section of thread
   tid characterised by (offset,size).  Return 0 if nothing to show
   for it. */
static UInt mb_get_origin_for_guest_offset ( ThreadId tid,
                                             Int offset, SizeT size )
{
   Int   sh2off;
   UInt  area[3];
   UInt  otag;
   sh2off = MC_(get_otrack_shadow_offset)( offset, size );
   if (sh2off == -1)
      return 0;  /* This piece of guest state is not tracked */
   tl_assert(sh2off >= 0);
   tl_assert(0 == (sh2off % 4));
   area[0] = 0x31313131;
   area[2] = 0x27272727;
   VG_(get_shadow_regs_area)( tid, (UChar *)&area[1], 2/*shadowno*/,sh2off,4 );
   tl_assert(area[0] == 0x31313131);
   tl_assert(area[2] == 0x27272727);
   otag = area[1];
   return otag;
}


/* When some chunk of guest state is written, mark the corresponding
   shadow area as valid.  This is used to initialise arbitrarily large
   chunks of guest state, hence the _SIZE value, which has to be as
   big as the biggest guest state.
*/
static void mc_post_reg_write ( CorePart part, ThreadId tid,
                                PtrdiffT offset, SizeT size)
{
#  define MAX_REG_WRITE_SIZE 2264
   UChar area[MAX_REG_WRITE_SIZE];
   tl_assert(size <= MAX_REG_WRITE_SIZE);
   VG_(memset)(area, V_BITS8_DEFINED, size);
   VG_(set_shadow_regs_area)( tid, 1/*shadowNo*/,offset,size, area );
#  undef MAX_REG_WRITE_SIZE
}

static
void mc_post_reg_write_clientcall ( ThreadId tid,
                                    PtrdiffT offset, SizeT size, Addr f)
{
   mc_post_reg_write(/*dummy*/0, tid, offset, size);
}

/* Look at the definedness of the guest's shadow state for
   [offset, offset+len).  If any part of that is undefined, record
   a parameter error.
*/
static void mc_pre_reg_read ( CorePart part, ThreadId tid, const HChar* s,
                              PtrdiffT offset, SizeT size)
{
   Int   i;
   Bool  bad;
   UInt  otag;

   UChar area[16];
   tl_assert(size <= 16);

   VG_(get_shadow_regs_area)( tid, area, 1/*shadowNo*/,offset,size );

   bad = False;
   for (i = 0; i < size; i++) {
      if (area[i] != V_BITS8_DEFINED) {
         bad = True;
         break;
      }
   }

   if (!bad)
      return;

   /* We've found some undefinedness.  See if we can also find an
      origin for it. */
   otag = mb_get_origin_for_guest_offset( tid, offset, size );
   MC_(record_regparam_error) ( tid, s, otag );
}


/*------------------------------------------------------------*/
/*--- Register-memory event handlers                       ---*/
/*------------------------------------------------------------*/

static void mc_copy_mem_to_reg ( CorePart part, ThreadId tid, Addr a,
                                 PtrdiffT guest_state_offset, SizeT size )
{
   SizeT i;
   UChar vbits8;
   Int offset;
   UInt d32;

   /* Slow loop. */
   for (i = 0; i < size; i++) {
      get_vbits8( a+i, &vbits8 );
      VG_(set_shadow_regs_area)( tid, 1/*shadowNo*/, guest_state_offset+i,
                                 1, &vbits8 );
   }

   if (MC_(clo_mc_level) != 3)
      return;

   /* Track origins. */
   offset = MC_(get_otrack_shadow_offset)( guest_state_offset, size );
   if (offset == -1)
      return;

   switch (size) {
   case 1:
      d32 = MC_(helperc_b_load1)( a );
      break;
   case 2:
      d32 = MC_(helperc_b_load2)( a );
      break;
   case 4:
      d32 = MC_(helperc_b_load4)( a );
      break;
   case 8:
      d32 = MC_(helperc_b_load8)( a );
      break;
   case 16:
      d32 = MC_(helperc_b_load16)( a );
      break;
   case 32:
      d32 = MC_(helperc_b_load32)( a );
      break;
   default:
      tl_assert(0);
   }

   VG_(set_shadow_regs_area)( tid, 2/*shadowNo*/, offset, 4, (UChar*)&d32 );
}

static void mc_copy_reg_to_mem ( CorePart part, ThreadId tid,
                                 PtrdiffT guest_state_offset, Addr a,
                                 SizeT size )
{
   SizeT i;
   UChar vbits8;
   Int offset;
   UInt d32;

   /* Slow loop. */
   for (i = 0; i < size; i++) {
      VG_(get_shadow_regs_area)( tid, &vbits8, 1/*shadowNo*/,
                                 guest_state_offset+i, 1 );
      set_vbits8( a+i, vbits8 );
   }

   if (MC_(clo_mc_level) != 3)
      return;

   /* Track origins. */
   offset = MC_(get_otrack_shadow_offset)( guest_state_offset, size );
   if (offset == -1)
      return;

   VG_(get_shadow_regs_area)( tid, (UChar*)&d32, 2/*shadowNo*/, offset, 4 );
   switch (size) {
   case 1:
      MC_(helperc_b_store1)( a, d32 );
      break;
   case 2:
      MC_(helperc_b_store2)( a, d32 );
      break;
   case 4:
      MC_(helperc_b_store4)( a, d32 );
      break;
   case 8:
      MC_(helperc_b_store8)( a, d32 );
      break;
   case 16:
      MC_(helperc_b_store16)( a, d32 );
      break;
   case 32:
      MC_(helperc_b_store32)( a, d32 );
      break;
   default:
      tl_assert(0);
   }
}


/*------------------------------------------------------------*/
/*--- Some static assertions                               ---*/
/*------------------------------------------------------------*/

/* The handwritten assembly helpers below have baked-in assumptions
   about various constant values.  These assertions attempt to make
   that a bit safer by checking those values and flagging changes that
   would make the assembly invalid.  Not perfect but it's better than
   nothing. */

STATIC_ASSERT(SM_CHUNKS * 4 == 65536);

STATIC_ASSERT(VA_BITS8_DEFINED   == 0xAA);
STATIC_ASSERT(VA_BITS8_UNDEFINED == 0x55);

STATIC_ASSERT(V_BITS32_DEFINED   == 0x00000000);
STATIC_ASSERT(V_BITS32_UNDEFINED == 0xFFFFFFFF);

STATIC_ASSERT(VA_BITS4_DEFINED == 0xA);
STATIC_ASSERT(VA_BITS4_UNDEFINED == 0x5);

STATIC_ASSERT(V_BITS16_DEFINED == 0x0000);
STATIC_ASSERT(V_BITS16_UNDEFINED == 0xFFFF);

STATIC_ASSERT(VA_BITS2_DEFINED == 2);
STATIC_ASSERT(VA_BITS2_UNDEFINED == 1);

STATIC_ASSERT(V_BITS8_DEFINED == 0x00);
STATIC_ASSERT(V_BITS8_UNDEFINED == 0xFF);


/*------------------------------------------------------------*/
/*--- Functions called directly from generated code:       ---*/
/*--- Load/store handlers.                                 ---*/
/*------------------------------------------------------------*/

/* Types:  LOADV32, LOADV16, LOADV8 are:
               UWord fn ( Addr a )
   so they return 32-bits on 32-bit machines and 64-bits on
   64-bit machines.  Addr has the same size as a host word.

   LOADV64 is always  ULong fn ( Addr a )

   Similarly for STOREV8, STOREV16, STOREV32, the supplied vbits
   are a UWord, and for STOREV64 they are a ULong.
*/

/* If any part of '_a' indicated by the mask is 1, either '_a' is not
   naturally '_sz/8'-aligned, or it exceeds the range covered by the
   primary map.  This is all very tricky (and important!), so let's
   work through the maths by hand (below), *and* assert for these
   values at startup. */
#define MASK(_szInBytes) \
   ( ~((0x10000UL-(_szInBytes)) | ((N_PRIMARY_MAP-1) << 16)) )

/* MASK only exists so as to define this macro. */
#define UNALIGNED_OR_HIGH(_a,_szInBits) \
   ((_a) & MASK((_szInBits>>3)))

/* On a 32-bit machine:

   N_PRIMARY_BITS          == 16, so
   N_PRIMARY_MAP           == 0x10000, so
   N_PRIMARY_MAP-1         == 0xFFFF, so
   (N_PRIMARY_MAP-1) << 16 == 0xFFFF0000, and so

   MASK(1) = ~ ( (0x10000 - 1) | 0xFFFF0000 )
           = ~ ( 0xFFFF | 0xFFFF0000 )
           = ~ 0xFFFF'FFFF
           = 0

   MASK(2) = ~ ( (0x10000 - 2) | 0xFFFF0000 )
           = ~ ( 0xFFFE | 0xFFFF0000 )
           = ~ 0xFFFF'FFFE
           = 1

   MASK(4) = ~ ( (0x10000 - 4) | 0xFFFF0000 )
           = ~ ( 0xFFFC | 0xFFFF0000 )
           = ~ 0xFFFF'FFFC
           = 3

   MASK(8) = ~ ( (0x10000 - 8) | 0xFFFF0000 )
           = ~ ( 0xFFF8 | 0xFFFF0000 )
           = ~ 0xFFFF'FFF8
           = 7

   Hence in the 32-bit case, "a & MASK(1/2/4/8)" is a nonzero value
   precisely when a is not 1/2/4/8-bytes aligned.  And obviously, for
   the 1-byte alignment case, it is always a zero value, since MASK(1)
   is zero.  All as expected.

   On a 64-bit machine, it's more complex, since we're testing
   simultaneously for misalignment and for the address being at or
   above 64G:

   N_PRIMARY_BITS          == 20, so
   N_PRIMARY_MAP           == 0x100000, so
   N_PRIMARY_MAP-1         == 0xFFFFF, so
   (N_PRIMARY_MAP-1) << 16 == 0xF'FFFF'0000, and so

   MASK(1) = ~ ( (0x10000 - 1) | 0xF'FFFF'0000 )
           = ~ ( 0xFFFF | 0xF'FFFF'0000 )
           = ~ 0xF'FFFF'FFFF
           = 0xFFFF'FFF0'0000'0000

   MASK(2) = ~ ( (0x10000 - 2) | 0xF'FFFF'0000 )
           = ~ ( 0xFFFE | 0xF'FFFF'0000 )
           = ~ 0xF'FFFF'FFFE
           = 0xFFFF'FFF0'0000'0001

   MASK(4) = ~ ( (0x10000 - 4) | 0xF'FFFF'0000 )
           = ~ ( 0xFFFC | 0xF'FFFF'0000 )
           = ~ 0xF'FFFF'FFFC
           = 0xFFFF'FFF0'0000'0003

   MASK(8) = ~ ( (0x10000 - 8) | 0xF'FFFF'0000 )
           = ~ ( 0xFFF8 | 0xF'FFFF'0000 )
           = ~ 0xF'FFFF'FFF8
           = 0xFFFF'FFF0'0000'0007
*/

/*------------------------------------------------------------*/
/*--- LOADV256 and LOADV128                                ---*/
/*------------------------------------------------------------*/

static INLINE
void mc_LOADV_128_or_256 ( /*OUT*/ULong* res,
                           Addr a, SizeT nBits, Bool isBigEndian )
{
   PROF_EVENT(MCPE_LOADV_128_OR_256);

#ifndef PERF_FAST_LOADV
   mc_LOADV_128_or_256_slow( res, a, nBits, isBigEndian );
   return;
#else
   {
      UWord   sm_off16, vabits16, j;
      UWord   nBytes  = nBits / 8;
      UWord   nULongs = nBytes / 8;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,nBits) )) {
         PROF_EVENT(MCPE_LOADV_128_OR_256_SLOW1);
         mc_LOADV_128_or_256_slow( res, a, nBits, isBigEndian );
         return;
      }

      /* Handle common cases quickly: a (and a+8 and a+16 etc.) is
         suitably aligned, is mapped, and addressible. */
      for (j = 0; j < nULongs; j++) {
         sm       = get_secmap_for_reading_low(a + 8*j);
         sm_off16 = SM_OFF_16(a + 8*j);
         vabits16 = sm->vabits16[sm_off16];

         // Convert V bits from compact memory form to expanded
         // register form.
         if (LIKELY(vabits16 == VA_BITS16_DEFINED)) {
            res[j] = V_BITS64_DEFINED;
         } else if (LIKELY(vabits16 == VA_BITS16_UNDEFINED)) {
            res[j] = V_BITS64_UNDEFINED;
         } else {
            /* Slow case: some block of 8 bytes are not all-defined or
               all-undefined. */
            PROF_EVENT(MCPE_LOADV_128_OR_256_SLOW2);
            mc_LOADV_128_or_256_slow( res, a, nBits, isBigEndian );
            return;
         }
      }
      return;
   }
#endif
}

VG_REGPARM(2) void MC_(helperc_LOADV256be) ( /*OUT*/V256* res, Addr a )
{
   mc_LOADV_128_or_256(&res->w64[0], a, 256, True);
}
VG_REGPARM(2) void MC_(helperc_LOADV256le) ( /*OUT*/V256* res, Addr a )
{
   mc_LOADV_128_or_256(&res->w64[0], a, 256, False);
}

VG_REGPARM(2) void MC_(helperc_LOADV128be) ( /*OUT*/V128* res, Addr a )
{
   mc_LOADV_128_or_256(&res->w64[0], a, 128, True);
}
VG_REGPARM(2) void MC_(helperc_LOADV128le) ( /*OUT*/V128* res, Addr a )
{
   mc_LOADV_128_or_256(&res->w64[0], a, 128, False);
}

/*------------------------------------------------------------*/
/*--- LOADV64                                              ---*/
/*------------------------------------------------------------*/

static INLINE
ULong mc_LOADV64 ( Addr a, Bool isBigEndian )
{
   PROF_EVENT(MCPE_LOADV64);

#ifndef PERF_FAST_LOADV
   return mc_LOADVn_slow( a, 64, isBigEndian );
#else
   {
      UWord   sm_off16, vabits16;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,64) )) {
         PROF_EVENT(MCPE_LOADV64_SLOW1);
         return (ULong)mc_LOADVn_slow( a, 64, isBigEndian );
      }

      sm       = get_secmap_for_reading_low(a);
      sm_off16 = SM_OFF_16(a);
      vabits16 = sm->vabits16[sm_off16];

      // Handle common case quickly: a is suitably aligned, is mapped, and
      // addressible.
      // Convert V bits from compact memory form to expanded register form.
      if (LIKELY(vabits16 == VA_BITS16_DEFINED)) {
         return V_BITS64_DEFINED;
      } else if (LIKELY(vabits16 == VA_BITS16_UNDEFINED)) {
         return V_BITS64_UNDEFINED;
      } else {
         /* Slow case: the 8 bytes are not all-defined or all-undefined. */
         PROF_EVENT(MCPE_LOADV64_SLOW2);
         return mc_LOADVn_slow( a, 64, isBigEndian );
      }
   }
#endif
}

// Generic for all platforms
VG_REGPARM(1) ULong MC_(helperc_LOADV64be) ( Addr a )
{
   return mc_LOADV64(a, True);
}

// Non-generic assembly for arm32-linux
#if ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
    && defined(VGP_arm_linux)
/* See mc_main_asm.c */

#elif ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
      && (defined(VGP_x86_linux) || defined(VGP_x86_solaris) || defined(VGP_x86_freebsd))
/* See mc_main_asm.c */

#else
// Generic for all platforms except {arm32,x86}-linux and x86-solaris
VG_REGPARM(1) ULong MC_(helperc_LOADV64le) ( Addr a )
{
   return mc_LOADV64(a, False);
}
#endif

/*------------------------------------------------------------*/
/*--- STOREV64                                             ---*/
/*------------------------------------------------------------*/

static INLINE
void mc_STOREV64 ( Addr a, ULong vbits64, Bool isBigEndian )
{
   PROF_EVENT(MCPE_STOREV64);

#ifndef PERF_FAST_STOREV
   // XXX: this slow case seems to be marginally faster than the fast case!
   // Investigate further.
   mc_STOREVn_slow( a, 64, vbits64, isBigEndian );
#else
   {
      UWord   sm_off16, vabits16;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,64) )) {
         PROF_EVENT(MCPE_STOREV64_SLOW1);
         mc_STOREVn_slow( a, 64, vbits64, isBigEndian );
         return;
      }

      sm       = get_secmap_for_reading_low(a);
      sm_off16 = SM_OFF_16(a);
      vabits16 = sm->vabits16[sm_off16];

      // To understand the below cleverness, see the extensive comments
      // in MC_(helperc_STOREV8).
      if (LIKELY(V_BITS64_DEFINED == vbits64)) {
         if (LIKELY(vabits16 == (UShort)VA_BITS16_DEFINED)) {
            return;
         }
         if (!is_distinguished_sm(sm) && VA_BITS16_UNDEFINED == vabits16) {
            sm->vabits16[sm_off16] = VA_BITS16_DEFINED;
            return;
         }
         PROF_EVENT(MCPE_STOREV64_SLOW2);
         mc_STOREVn_slow( a, 64, vbits64, isBigEndian );
         return;
      }
      if (V_BITS64_UNDEFINED == vbits64) {
         if (vabits16 == (UShort)VA_BITS16_UNDEFINED) {
            return;
         }
         if (!is_distinguished_sm(sm) && VA_BITS16_DEFINED == vabits16) {
            sm->vabits16[sm_off16] = VA_BITS16_UNDEFINED;
            return;
         }
         PROF_EVENT(MCPE_STOREV64_SLOW3);
         mc_STOREVn_slow( a, 64, vbits64, isBigEndian );
         return;
      }

      PROF_EVENT(MCPE_STOREV64_SLOW4);
      mc_STOREVn_slow( a, 64, vbits64, isBigEndian );
   }
#endif
}

VG_REGPARM(1) void MC_(helperc_STOREV64be) ( Addr a, ULong vbits64 )
{
   mc_STOREV64(a, vbits64, True);
}
VG_REGPARM(1) void MC_(helperc_STOREV64le) ( Addr a, ULong vbits64 )
{
   mc_STOREV64(a, vbits64, False);
}

/*------------------------------------------------------------*/
/*--- LOADV32                                              ---*/
/*------------------------------------------------------------*/

static INLINE
UWord mc_LOADV32 ( Addr a, Bool isBigEndian )
{
   PROF_EVENT(MCPE_LOADV32);

#ifndef PERF_FAST_LOADV
   return (UWord)mc_LOADVn_slow( a, 32, isBigEndian );
#else
   {
      UWord   sm_off, vabits8;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,32) )) {
         PROF_EVENT(MCPE_LOADV32_SLOW1);
         return (UWord)mc_LOADVn_slow( a, 32, isBigEndian );
      }

      sm      = get_secmap_for_reading_low(a);
      sm_off  = SM_OFF(a);
      vabits8 = sm->vabits8[sm_off];

      // Handle common case quickly: a is suitably aligned, is mapped, and the
      // entire word32 it lives in is addressible.
      // Convert V bits from compact memory form to expanded register form.
      // For 64-bit platforms, set the high 32 bits of retval to 1 (undefined).
      // Almost certainly not necessary, but be paranoid.
      if (LIKELY(vabits8 == VA_BITS8_DEFINED)) {
         return ((UWord)0xFFFFFFFF00000000ULL | (UWord)V_BITS32_DEFINED);
      } else if (LIKELY(vabits8 == VA_BITS8_UNDEFINED)) {
         return ((UWord)0xFFFFFFFF00000000ULL | (UWord)V_BITS32_UNDEFINED);
      } else {
         /* Slow case: the 4 bytes are not all-defined or all-undefined. */
         PROF_EVENT(MCPE_LOADV32_SLOW2);
         return (UWord)mc_LOADVn_slow( a, 32, isBigEndian );
      }
   }
#endif
}

// Generic for all platforms
VG_REGPARM(1) UWord MC_(helperc_LOADV32be) ( Addr a )
{
   return mc_LOADV32(a, True);
}

// Non-generic assembly for arm32-linux
#if ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
    && defined(VGP_arm_linux)
/* See mc_main_asm.c */

#elif ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
      && (defined(VGP_x86_linux) || defined(VGP_x86_solaris))
/* See mc_main_asm.c */

#else
// Generic for all platforms except {arm32,x86}-linux and x86-solaris
VG_REGPARM(1) UWord MC_(helperc_LOADV32le) ( Addr a )
{
   return mc_LOADV32(a, False);
}
#endif

/*------------------------------------------------------------*/
/*--- STOREV32                                             ---*/
/*------------------------------------------------------------*/

static INLINE
void mc_STOREV32 ( Addr a, UWord vbits32, Bool isBigEndian )
{
   PROF_EVENT(MCPE_STOREV32);

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
#else
   {
      UWord   sm_off, vabits8;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,32) )) {
         PROF_EVENT(MCPE_STOREV32_SLOW1);
         mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
         return;
      }

      sm      = get_secmap_for_reading_low(a);
      sm_off  = SM_OFF(a);
      vabits8 = sm->vabits8[sm_off];

      // To understand the below cleverness, see the extensive comments
      // in MC_(helperc_STOREV8).
      if (LIKELY(V_BITS32_DEFINED == vbits32)) {
         if (LIKELY(vabits8 == (UInt)VA_BITS8_DEFINED)) {
            return;
         }
         if (!is_distinguished_sm(sm)  && VA_BITS8_UNDEFINED == vabits8) {
            sm->vabits8[sm_off] = (UInt)VA_BITS8_DEFINED;
            return;
         }
         PROF_EVENT(MCPE_STOREV32_SLOW2);
         mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
         return;
      }
      if (V_BITS32_UNDEFINED == vbits32) {
         if (vabits8 == (UInt)VA_BITS8_UNDEFINED) {
            return;
         }
         if (!is_distinguished_sm(sm) && VA_BITS8_DEFINED == vabits8) {
            sm->vabits8[sm_off] = (UInt)VA_BITS8_UNDEFINED;
            return;
         }
         PROF_EVENT(MCPE_STOREV32_SLOW3);
         mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
         return;
      }

      PROF_EVENT(MCPE_STOREV32_SLOW4);
      mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
   }
#endif
}

VG_REGPARM(2) void MC_(helperc_STOREV32be) ( Addr a, UWord vbits32 )
{
   mc_STOREV32(a, vbits32, True);
}
VG_REGPARM(2) void MC_(helperc_STOREV32le) ( Addr a, UWord vbits32 )
{
   mc_STOREV32(a, vbits32, False);
}

/*------------------------------------------------------------*/
/*--- LOADV16                                              ---*/
/*------------------------------------------------------------*/

static INLINE
UWord mc_LOADV16 ( Addr a, Bool isBigEndian )
{
   PROF_EVENT(MCPE_LOADV16);

#ifndef PERF_FAST_LOADV
   return (UWord)mc_LOADVn_slow( a, 16, isBigEndian );
#else
   {
      UWord   sm_off, vabits8;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,16) )) {
         PROF_EVENT(MCPE_LOADV16_SLOW1);
         return (UWord)mc_LOADVn_slow( a, 16, isBigEndian );
      }

      sm      = get_secmap_for_reading_low(a);
      sm_off  = SM_OFF(a);
      vabits8 = sm->vabits8[sm_off];
      // Handle common case quickly: a is suitably aligned, is mapped, and is
      // addressible.
      // Convert V bits from compact memory form to expanded register form
      if      (LIKELY(vabits8 == VA_BITS8_DEFINED  )) { return V_BITS16_DEFINED;   }
      else if (LIKELY(vabits8 == VA_BITS8_UNDEFINED)) { return V_BITS16_UNDEFINED; }
      else {
         // The 4 (yes, 4) bytes are not all-defined or all-undefined, check
         // the two sub-bytes.
         UChar vabits4 = extract_vabits4_from_vabits8(a, vabits8);
         if      (vabits4 == VA_BITS4_DEFINED  ) { return V_BITS16_DEFINED;   }
         else if (vabits4 == VA_BITS4_UNDEFINED) { return V_BITS16_UNDEFINED; }
         else {
            /* Slow case: the two bytes are not all-defined or all-undefined. */
            PROF_EVENT(MCPE_LOADV16_SLOW2);
            return (UWord)mc_LOADVn_slow( a, 16, isBigEndian );
         }
      }
   }
#endif
}

// Generic for all platforms
VG_REGPARM(1) UWord MC_(helperc_LOADV16be) ( Addr a )
{
   return mc_LOADV16(a, True);
}

// Non-generic assembly for arm32-linux
#if ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
    && defined(VGP_arm_linux)
__asm__( /* Derived from NCode template */
".text                                  \n"
".align 2                               \n"
".global vgMemCheck_helperc_LOADV16le   \n"
".type   vgMemCheck_helperc_LOADV16le, %function \n"
"vgMemCheck_helperc_LOADV16le:          \n" //
"      tst    r0, #1                    \n" //
"      bne    .LLV16LEc12               \n" // if misaligned
"      lsr    r2, r0, #16               \n" // r2 = pri-map-ix
"      movw   r3, #:lower16:primary_map \n" //
"      uxth   r1, r0                    \n" // r1 = sec-map-offB
"      movt   r3, #:upper16:primary_map \n" //
"      ldr    r2, [r3, r2, lsl #2]      \n" // r2 = sec-map
"      ldrb   r1, [r2, r1, lsr #2]      \n" // r1 = sec-map-VABITS8
"      cmp    r1, #0xAA                 \n" // r1 == VA_BITS8_DEFINED?
"      bne    .LLV16LEc0                \n" // no, goto .LLV16LEc0
".LLV16LEh9:                            \n" //
"      mov    r0, #0xFFFFFFFF           \n" //
"      lsl    r0, r0, #16               \n" // V_BITS16_DEFINED | top16safe
"      bx     lr                        \n" //
".LLV16LEc0:                            \n" //
"      cmp    r1, #0x55                 \n" // VA_BITS8_UNDEFINED
"      bne    .LLV16LEc4                \n" //
".LLV16LEc2:                            \n" //
"      mov    r0, #0xFFFFFFFF           \n" // V_BITS16_UNDEFINED | top16safe
"      bx     lr                        \n" //
".LLV16LEc4:                            \n" //
       // r1 holds sec-map-VABITS8.  r0 holds the address and is 2-aligned.
       // Extract the relevant 4 bits and inspect.
"      and    r2, r0, #2       \n" // addr & 2
"      add    r2, r2, r2       \n" // 2 * (addr & 2)
"      lsr    r1, r1, r2       \n" // sec-map-VABITS8 >> (2 * (addr & 2))
"      and    r1, r1, #15      \n" // (sec-map-VABITS8 >> (2 * (addr & 2))) & 15

"      cmp    r1, #0xA                  \n" // VA_BITS4_DEFINED
"      beq    .LLV16LEh9                \n" //

"      cmp    r1, #0x5                  \n" // VA_BITS4_UNDEFINED
"      beq    .LLV16LEc2                \n" //

".LLV16LEc12:                           \n" //
"      push   {r4, lr}                  \n" //
"      mov    r2, #0                    \n" //
"      mov    r1, #16                   \n" //
"      bl     mc_LOADVn_slow            \n" //
"      pop    {r4, pc}                  \n" //
".size vgMemCheck_helperc_LOADV16le, .-vgMemCheck_helperc_LOADV16le \n"
".previous\n"
);

#elif ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
      && (defined(VGP_x86_linux) || defined(VGP_x86_solaris))
__asm__(
".text\n"
".align 16\n"
".global vgMemCheck_helperc_LOADV16le\n"
".type   vgMemCheck_helperc_LOADV16le, @function\n"
"vgMemCheck_helperc_LOADV16le:\n"
"      test   $0x1,  %eax\n"
"      jne    .LLV16LE5\n"          /* jump if not aligned */
"      mov    %eax,  %edx\n"
"      shr    $0x10, %edx\n"
"      mov    primary_map(,%edx,4), %ecx\n"
"      movzwl %ax,   %edx\n"
"      shr    $0x2,  %edx\n"
"      movzbl (%ecx,%edx,1), %edx\n"/* edx = VA bits for 32bit */
"      cmp    $0xaa, %edx\n"        /* compare to VA_BITS8_DEFINED */
"      jne    .LLV16LE2\n"          /* jump if not all 32bits defined */
".LLV16LE1:\n"
"      mov    $0xffff0000,%eax\n"   /* V_BITS16_DEFINED | top16safe */
"      ret\n"
".LLV16LE2:\n"
"      cmp    $0x55, %edx\n"        /* compare to VA_BITS8_UNDEFINED */
"      jne    .LLV16LE4\n"          /* jump if not all 32bits undefined */
".LLV16LE3:\n"
"      or     $0xffffffff,%eax\n"   /* V_BITS16_UNDEFINED | top16safe */
"      ret\n"
".LLV16LE4:\n"
"      mov    %eax,  %ecx\n"
"      and    $0x2,  %ecx\n"
"      add    %ecx,  %ecx\n"
"      sar    %cl,   %edx\n"
"      and    $0xf,  %edx\n"
"      cmp    $0xa,  %edx\n"
"      je     .LLV16LE1\n"          /* jump if all 16bits are defined */
"      cmp    $0x5,  %edx\n"
"      je     .LLV16LE3\n"          /* jump if all 16bits are undefined */
".LLV16LE5:\n"
"      xor    %ecx,  %ecx\n"        /* tail call mc_LOADVn_slow(a, 16, 0) */
"      mov    $16,   %edx\n"
"      jmp    mc_LOADVn_slow\n"
".size vgMemCheck_helperc_LOADV16le, .-vgMemCheck_helperc_LOADV16le \n"
".previous\n"
);

#else
// Generic for all platforms except {arm32,x86}-linux and x86-solaris
VG_REGPARM(1) UWord MC_(helperc_LOADV16le) ( Addr a )
{
   return mc_LOADV16(a, False);
}
#endif

/*------------------------------------------------------------*/
/*--- STOREV16                                             ---*/
/*------------------------------------------------------------*/

/* True if the vabits4 in vabits8 indicate a and a+1 are accessible. */
static INLINE
Bool accessible_vabits4_in_vabits8 ( Addr a, UChar vabits8 )
{
   UInt shift;
   tl_assert(VG_IS_2_ALIGNED(a));      // Must be 2-aligned
   shift = (a & 2) << 1;               // shift by 0 or 4
   vabits8 >>= shift;                  // shift the four bits to the bottom
    // check 2 x vabits2 != VA_BITS2_NOACCESS
   return ((0x3 & vabits8) != VA_BITS2_NOACCESS)
      &&  ((0xc & vabits8) != VA_BITS2_NOACCESS << 2);
}

static INLINE
void mc_STOREV16 ( Addr a, UWord vbits16, Bool isBigEndian )
{
   PROF_EVENT(MCPE_STOREV16);

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 16, (ULong)vbits16, isBigEndian );
#else
   {
      UWord   sm_off, vabits8;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,16) )) {
         PROF_EVENT(MCPE_STOREV16_SLOW1);
         mc_STOREVn_slow( a, 16, (ULong)vbits16, isBigEndian );
         return;
      }

      sm      = get_secmap_for_reading_low(a);
      sm_off  = SM_OFF(a);
      vabits8 = sm->vabits8[sm_off];

      // To understand the below cleverness, see the extensive comments
      // in MC_(helperc_STOREV8).
      if (LIKELY(V_BITS16_DEFINED == vbits16)) {
         if (LIKELY(vabits8 == VA_BITS8_DEFINED)) {
            return;
         }
         if (!is_distinguished_sm(sm)
             && accessible_vabits4_in_vabits8(a, vabits8)) {
            insert_vabits4_into_vabits8( a, VA_BITS4_DEFINED,
                                         &(sm->vabits8[sm_off]) );
            return;
         }
         PROF_EVENT(MCPE_STOREV16_SLOW2);
         mc_STOREVn_slow( a, 16, (ULong)vbits16, isBigEndian );
      }
      if (V_BITS16_UNDEFINED == vbits16) {
         if (vabits8 == VA_BITS8_UNDEFINED) {
            return;
         }
         if (!is_distinguished_sm(sm)
             && accessible_vabits4_in_vabits8(a, vabits8)) {
            insert_vabits4_into_vabits8( a, VA_BITS4_UNDEFINED,
                                         &(sm->vabits8[sm_off]) );
            return;
         }
         PROF_EVENT(MCPE_STOREV16_SLOW3);
         mc_STOREVn_slow( a, 16, (ULong)vbits16, isBigEndian );
         return;
      }

      PROF_EVENT(MCPE_STOREV16_SLOW4);
      mc_STOREVn_slow( a, 16, (ULong)vbits16, isBigEndian );
   }
#endif
}


VG_REGPARM(2) void MC_(helperc_STOREV16be) ( Addr a, UWord vbits16 )
{
   mc_STOREV16(a, vbits16, True);
}
VG_REGPARM(2) void MC_(helperc_STOREV16le) ( Addr a, UWord vbits16 )
{
   mc_STOREV16(a, vbits16, False);
}

/*------------------------------------------------------------*/
/*--- LOADV8                                               ---*/
/*------------------------------------------------------------*/

/* Note: endianness is irrelevant for size == 1 */

// Non-generic assembly for arm32-linux
#if ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
    && defined(VGP_arm_linux)
__asm__( /* Derived from NCode template */
".text                                  \n"
".align 2                               \n"
".global vgMemCheck_helperc_LOADV8      \n"
".type   vgMemCheck_helperc_LOADV8, %function \n"
"vgMemCheck_helperc_LOADV8:             \n" //
"      lsr    r2, r0, #16               \n" // r2 = pri-map-ix
"      movw   r3, #:lower16:primary_map \n" //
"      uxth   r1, r0                    \n" // r1 = sec-map-offB
"      movt   r3, #:upper16:primary_map \n" //
"      ldr    r2, [r3, r2, lsl #2]      \n" // r2 = sec-map
"      ldrb   r1, [r2, r1, lsr #2]      \n" // r1 = sec-map-VABITS8
"      cmp    r1, #0xAA                 \n" // r1 == VA_BITS8_DEFINED?
"      bne    .LLV8c0                   \n" // no, goto .LLV8c0
".LLV8h9:                               \n" //
"      mov    r0, #0xFFFFFF00           \n" // V_BITS8_DEFINED | top24safe
"      bx     lr                        \n" //
".LLV8c0:                               \n" //
"      cmp    r1, #0x55                 \n" // VA_BITS8_UNDEFINED
"      bne    .LLV8c4                   \n" //
".LLV8c2:                               \n" //
"      mov    r0, #0xFFFFFFFF           \n" // V_BITS8_UNDEFINED | top24safe
"      bx     lr                        \n" //
".LLV8c4:                               \n" //
       // r1 holds sec-map-VABITS8
       // r0 holds the address.  Extract the relevant 2 bits and inspect.
"      and    r2, r0, #3       \n" // addr & 3
"      add    r2, r2, r2       \n" // 2 * (addr & 3)
"      lsr    r1, r1, r2       \n" // sec-map-VABITS8 >> (2 * (addr & 3))
"      and    r1, r1, #3       \n" // (sec-map-VABITS8 >> (2 * (addr & 3))) & 3

"      cmp    r1, #2                    \n" // VA_BITS2_DEFINED
"      beq    .LLV8h9                   \n" //

"      cmp    r1, #1                    \n" // VA_BITS2_UNDEFINED
"      beq    .LLV8c2                   \n" //

"      push   {r4, lr}                  \n" //
"      mov    r2, #0                    \n" //
"      mov    r1, #8                    \n" //
"      bl     mc_LOADVn_slow            \n" //
"      pop    {r4, pc}                  \n" //
".size vgMemCheck_helperc_LOADV8, .-vgMemCheck_helperc_LOADV8 \n"
".previous\n"
);

/* Non-generic assembly for x86-linux */
#elif ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
      && (defined(VGP_x86_linux) || defined(VGP_x86_solaris))
__asm__(
".text\n"
".align 16\n"
".global vgMemCheck_helperc_LOADV8\n"
".type   vgMemCheck_helperc_LOADV8, @function\n"
"vgMemCheck_helperc_LOADV8:\n"
"      mov    %eax,  %edx\n"
"      shr    $0x10, %edx\n"
"      mov    primary_map(,%edx,4), %ecx\n"
"      movzwl %ax,   %edx\n"
"      shr    $0x2,  %edx\n"
"      movzbl (%ecx,%edx,1), %edx\n"/* edx = VA bits for 32bit */
"      cmp    $0xaa, %edx\n"        /* compare to VA_BITS8_DEFINED? */
"      jne    .LLV8LE2\n"           /* jump if not defined */
".LLV8LE1:\n"
"      mov    $0xffffff00, %eax\n"  /* V_BITS8_DEFINED | top24safe */
"      ret\n"
".LLV8LE2:\n"
"      cmp    $0x55, %edx\n"        /* compare to VA_BITS8_UNDEFINED */
"      jne    .LLV8LE4\n"           /* jump if not all 32bits are undefined */
".LLV8LE3:\n"
"      or     $0xffffffff, %eax\n"  /* V_BITS8_UNDEFINED | top24safe */
"      ret\n"
".LLV8LE4:\n"
"      mov    %eax,  %ecx\n"
"      and    $0x3,  %ecx\n"
"      add    %ecx,  %ecx\n"
"      sar    %cl,   %edx\n"
"      and    $0x3,  %edx\n"
"      cmp    $0x2,  %edx\n"
"      je     .LLV8LE1\n"           /* jump if all 8bits are defined */
"      cmp    $0x1,  %edx\n"
"      je     .LLV8LE3\n"           /* jump if all 8bits are undefined */
"      xor    %ecx,  %ecx\n"        /* tail call to mc_LOADVn_slow(a, 8, 0) */
"      mov    $0x8,  %edx\n"
"      jmp    mc_LOADVn_slow\n"
".size vgMemCheck_helperc_LOADV8, .-vgMemCheck_helperc_LOADV8\n"
".previous\n"
);

#else
// Generic for all platforms except {arm32,x86}-linux and x86-solaris
VG_REGPARM(1)
UWord MC_(helperc_LOADV8) ( Addr a )
{
   PROF_EVENT(MCPE_LOADV8);

#ifndef PERF_FAST_LOADV
   return (UWord)mc_LOADVn_slow( a, 8, False/*irrelevant*/ );
#else
   {
      UWord   sm_off, vabits8;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,8) )) {
         PROF_EVENT(MCPE_LOADV8_SLOW1);
         return (UWord)mc_LOADVn_slow( a, 8, False/*irrelevant*/ );
      }

      sm      = get_secmap_for_reading_low(a);
      sm_off  = SM_OFF(a);
      vabits8 = sm->vabits8[sm_off];
      // Convert V bits from compact memory form to expanded register form
      // Handle common case quickly: a is mapped, and the entire
      // word32 it lives in is addressible.
      if      (LIKELY(vabits8 == VA_BITS8_DEFINED  )) { return V_BITS8_DEFINED;   }
      else if (LIKELY(vabits8 == VA_BITS8_UNDEFINED)) { return V_BITS8_UNDEFINED; }
      else {
         // The 4 (yes, 4) bytes are not all-defined or all-undefined, check
         // the single byte.
         UChar vabits2 = extract_vabits2_from_vabits8(a, vabits8);
         if      (vabits2 == VA_BITS2_DEFINED  ) { return V_BITS8_DEFINED;   }
         else if (vabits2 == VA_BITS2_UNDEFINED) { return V_BITS8_UNDEFINED; }
         else {
            /* Slow case: the byte is not all-defined or all-undefined. */
            PROF_EVENT(MCPE_LOADV8_SLOW2);
            return (UWord)mc_LOADVn_slow( a, 8, False/*irrelevant*/ );
         }
      }
   }
#endif
}
#endif

/*------------------------------------------------------------*/
/*--- STOREV8                                              ---*/
/*------------------------------------------------------------*/

VG_REGPARM(2)
void MC_(helperc_STOREV8) ( Addr a, UWord vbits8 )
{
   PROF_EVENT(MCPE_STOREV8);

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
#else
   {
      UWord   sm_off, vabits8;
      SecMap* sm;

      if (UNLIKELY( UNALIGNED_OR_HIGH(a,8) )) {
         PROF_EVENT(MCPE_STOREV8_SLOW1);
         mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
         return;
      }

      sm      = get_secmap_for_reading_low(a);
      sm_off  = SM_OFF(a);
      vabits8 = sm->vabits8[sm_off];

      // Clevernesses to speed up storing V bits.
      // The 64/32/16 bit cases also have similar clevernesses, but it
      // works a little differently to the code below.
      //
      // Cleverness 1:  sometimes we don't have to write the shadow memory at
      // all, if we can tell that what we want to write is the same as what is
      // already there. These cases are marked below as "defined on defined" and
      // "undefined on undefined".
      //
      // Cleverness 2:
      // We also avoid to call mc_STOREVn_slow if the V bits can directly
      // be written in the secondary map. V bits can be directly written
      // if 4 conditions are respected:
      //   * The address for which V bits are written is naturally aligned
      //        on 1 byte  for STOREV8 (this is always true)
      //        on 2 bytes for STOREV16
      //        on 4 bytes for STOREV32
      //        on 8 bytes for STOREV64.
      //   * V bits being written are either fully defined or fully undefined.
      //     (for partially defined V bits, V bits cannot be directly written,
      //      as the secondary vbits table must be maintained).
      //   * the secmap is not distinguished (distinguished maps cannot be
      //     modified).
      //   * the memory corresponding to the V bits being written is
      //     accessible (if one or more bytes are not accessible,
      //     we must call mc_STOREVn_slow in order to report accessibility
      //     errors).
      //     Note that for STOREV32 and STOREV64, it is too expensive
      //     to verify the accessibility of each byte for the benefit it
      //     brings. Instead, a quicker check is done by comparing to
      //     VA_BITS(8|16)_(UN)DEFINED. This guarantees accessibility,
      //     but misses some opportunity of direct modifications.
      //     Checking each byte accessibility was measured for
      //     STOREV32+perf tests and was slowing down all perf tests.
      // The cases corresponding to cleverness 2 are marked below as
      // "direct mod".
      if (LIKELY(V_BITS8_DEFINED == vbits8)) {
         if (LIKELY(vabits8 == VA_BITS8_DEFINED)) {
            return; // defined on defined
         }
         if (!is_distinguished_sm(sm)
             && VA_BITS2_NOACCESS != extract_vabits2_from_vabits8(a, vabits8)) {
            // direct mod
            insert_vabits2_into_vabits8( a, VA_BITS2_DEFINED,
                                         &(sm->vabits8[sm_off]) );
            return;
         }
         PROF_EVENT(MCPE_STOREV8_SLOW2);
         mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
         return;
      }
      if (V_BITS8_UNDEFINED == vbits8) {
         if (vabits8 == VA_BITS8_UNDEFINED) {
            return; // undefined on undefined
         }
         if (!is_distinguished_sm(sm)
             && (VA_BITS2_NOACCESS
                 != extract_vabits2_from_vabits8(a, vabits8))) {
            // direct mod
            insert_vabits2_into_vabits8( a, VA_BITS2_UNDEFINED,
                                         &(sm->vabits8[sm_off]) );
            return;
         }
         PROF_EVENT(MCPE_STOREV8_SLOW3);
         mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
         return;
      }

      // Partially defined word
      PROF_EVENT(MCPE_STOREV8_SLOW4);
      mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
   }
#endif
}


/*------------------------------------------------------------*/
/*--- Functions called directly from generated code:       ---*/
/*--- Value-check failure handlers.                        ---*/
/*------------------------------------------------------------*/

/* Call these ones when an origin is available ... */
VG_REGPARM(1)
void MC_(helperc_value_check0_fail_w_o) ( UWord origin ) {
   MC_(record_cond_error) ( VG_(get_running_tid)(), (UInt)origin );
}

VG_REGPARM(1)
void MC_(helperc_value_check1_fail_w_o) ( UWord origin ) {
   MC_(record_value_error) ( VG_(get_running_tid)(), 1, (UInt)origin );
}

VG_REGPARM(1)
void MC_(helperc_value_check4_fail_w_o) ( UWord origin ) {
   MC_(record_value_error) ( VG_(get_running_tid)(), 4, (UInt)origin );
}

VG_REGPARM(1)
void MC_(helperc_value_check8_fail_w_o) ( UWord origin ) {
   MC_(record_value_error) ( VG_(get_running_tid)(), 8, (UInt)origin );
}

VG_REGPARM(2)
void MC_(helperc_value_checkN_fail_w_o) ( HWord sz, UWord origin ) {
   MC_(record_value_error) ( VG_(get_running_tid)(), (Int)sz, (UInt)origin );
}

/* ... and these when an origin isn't available. */

VG_REGPARM(0)
void MC_(helperc_value_check0_fail_no_o) ( void ) {
   MC_(record_cond_error) ( VG_(get_running_tid)(), 0/*origin*/ );
}

VG_REGPARM(0)
void MC_(helperc_value_check1_fail_no_o) ( void ) {
   MC_(record_value_error) ( VG_(get_running_tid)(), 1, 0/*origin*/ );
}

VG_REGPARM(0)
void MC_(helperc_value_check4_fail_no_o) ( void ) {
   MC_(record_value_error) ( VG_(get_running_tid)(), 4, 0/*origin*/ );
}

VG_REGPARM(0)
void MC_(helperc_value_check8_fail_no_o) ( void ) {
   MC_(record_value_error) ( VG_(get_running_tid)(), 8, 0/*origin*/ );
}

VG_REGPARM(1)
void MC_(helperc_value_checkN_fail_no_o) ( HWord sz ) {
   MC_(record_value_error) ( VG_(get_running_tid)(), (Int)sz, 0/*origin*/ );
}


/*------------------------------------------------------------*/
/*--- Metadata get/set functions, for client requests.     ---*/
/*------------------------------------------------------------*/

// Nb: this expands the V+A bits out into register-form V bits, even though
// they're in memory.  This is for backward compatibility, and because it's
// probably what the user wants.

/* Copy Vbits from/to address 'a'. Returns: 1 == OK, 2 == alignment
   error [no longer used], 3 == addressing error. */
/* Nb: We used to issue various definedness/addressability errors from here,
   but we took them out because they ranged from not-very-helpful to
   downright annoying, and they complicated the error data structures. */
static Int mc_get_or_set_vbits_for_client (
   Addr a,
   Addr vbits,
   SizeT szB,
   Bool setting, /* True <=> set vbits,  False <=> get vbits */
   Bool is_client_request /* True <=> real user request
                             False <=> internal call from gdbserver */
)
{
   SizeT i;
   Bool  ok;
   UChar vbits8;

   /* Check that arrays are addressible before doing any getting/setting.
      vbits to be checked only for real user request. */
   for (i = 0; i < szB; i++) {
      if (VA_BITS2_NOACCESS == get_vabits2(a + i) ||
          (is_client_request && VA_BITS2_NOACCESS == get_vabits2(vbits + i))) {
         return 3;
      }
   }

   /* Do the copy */
   if (setting) {
      /* setting */
      for (i = 0; i < szB; i++) {
         ok = set_vbits8(a + i, ((UChar*)vbits)[i]);
         tl_assert(ok);
      }
   } else {
      /* getting */
      for (i = 0; i < szB; i++) {
         ok = get_vbits8(a + i, &vbits8);
         tl_assert(ok);
         ((UChar*)vbits)[i] = vbits8;
      }
      if (is_client_request)
        // The bytes in vbits[] have now been set, so mark them as such.
        MC_(make_mem_defined)(vbits, szB);
   }

   return 1;
}


/*------------------------------------------------------------*/
/*--- Detecting leaked (unreachable) malloc'd blocks.      ---*/
/*------------------------------------------------------------*/

/* For the memory leak detector, say whether an entire 64k chunk of
   address space is possibly in use, or not.  If in doubt return
   True.
*/
Bool MC_(is_within_valid_secondary) ( Addr a )
{
   SecMap* sm = maybe_get_secmap_for ( a );
   if (sm == NULL || sm == &sm_distinguished[SM_DIST_NOACCESS]) {
      /* Definitely not in use. */
      return False;
   } else {
      return True;
   }
}


/* For the memory leak detector, say whether or not a given word
   address is to be regarded as valid. */
Bool MC_(is_valid_aligned_word) ( Addr a )
{
   tl_assert(sizeof(UWord) == 4 || sizeof(UWord) == 8);
   tl_assert(VG_IS_WORD_ALIGNED(a));
   if (get_vabits8_for_aligned_word32 (a) != VA_BITS8_DEFINED)
      return False;
   if (sizeof(UWord) == 8) {
      if (get_vabits8_for_aligned_word32 (a + 4) != VA_BITS8_DEFINED)
         return False;
   }
   if (UNLIKELY(MC_(in_ignored_range)(a)))
      return False;
   else
      return True;
}


/*------------------------------------------------------------*/
/*--- Initialisation                                       ---*/
/*------------------------------------------------------------*/

static void init_shadow_memory ( void )
{
   Int     i;
   SecMap* sm;

   tl_assert(V_BIT_UNDEFINED   == 1);
   tl_assert(V_BIT_DEFINED     == 0);
   tl_assert(V_BITS8_UNDEFINED == 0xFF);
   tl_assert(V_BITS8_DEFINED   == 0);

   /* Build the 3 distinguished secondaries */
   sm = &sm_distinguished[SM_DIST_NOACCESS];
   for (i = 0; i < SM_CHUNKS; i++) sm->vabits8[i] = VA_BITS8_NOACCESS;

   sm = &sm_distinguished[SM_DIST_UNDEFINED];
   for (i = 0; i < SM_CHUNKS; i++) sm->vabits8[i] = VA_BITS8_UNDEFINED;

   sm = &sm_distinguished[SM_DIST_DEFINED];
   for (i = 0; i < SM_CHUNKS; i++) sm->vabits8[i] = VA_BITS8_DEFINED;

   /* Set up the primary map. */
   /* These entries gradually get overwritten as the used address
      space expands. */
   for (i = 0; i < N_PRIMARY_MAP; i++)
      primary_map[i] = &sm_distinguished[SM_DIST_NOACCESS];

   /* Auxiliary primary maps */
   init_auxmap_L1_L2();

   /* auxmap_size = auxmap_used = 0;
      no ... these are statically initialised */

   /* Secondary V bit table */
   secVBitTable = createSecVBitTable();
}


/*------------------------------------------------------------*/
/*--- Sanity check machinery (permanently engaged)         ---*/
/*------------------------------------------------------------*/

static Bool mc_cheap_sanity_check ( void )
{
   n_sanity_cheap++;
   PROF_EVENT(MCPE_CHEAP_SANITY_CHECK);
   /* Check for sane operating level */
   if (MC_(clo_mc_level) < 1 || MC_(clo_mc_level) > 3)
      return False;
   /* nothing else useful we can rapidly check */
   return True;
}

static Bool mc_expensive_sanity_check ( void )
{
   Int     i;
   Word    n_secmaps_found;
   SecMap* sm;
   const HChar*  errmsg;
   Bool    bad = False;

   if (0) VG_(printf)("expensive sanity check\n");
   if (0) return True;

   n_sanity_expensive++;
   PROF_EVENT(MCPE_EXPENSIVE_SANITY_CHECK);

   /* Check for sane operating level */
   if (MC_(clo_mc_level) < 1 || MC_(clo_mc_level) > 3)
      return False;

   /* Check that the 3 distinguished SMs are still as they should be. */

   /* Check noaccess DSM. */
   sm = &sm_distinguished[SM_DIST_NOACCESS];
   for (i = 0; i < SM_CHUNKS; i++)
      if (sm->vabits8[i] != VA_BITS8_NOACCESS)
         bad = True;

   /* Check undefined DSM. */
   sm = &sm_distinguished[SM_DIST_UNDEFINED];
   for (i = 0; i < SM_CHUNKS; i++)
      if (sm->vabits8[i] != VA_BITS8_UNDEFINED)
         bad = True;

   /* Check defined DSM. */
   sm = &sm_distinguished[SM_DIST_DEFINED];
   for (i = 0; i < SM_CHUNKS; i++)
      if (sm->vabits8[i] != VA_BITS8_DEFINED)
         bad = True;

   if (bad) {
      VG_(printf)("memcheck expensive sanity: "
                  "distinguished_secondaries have changed\n");
      return False;
   }

   /* If we're not checking for undefined value errors, the secondary V bit
    * table should be empty. */
   if (MC_(clo_mc_level) == 1) {
      if (0 != VG_(OSetGen_Size)(secVBitTable))
         return False;
   }

   /* check the auxiliary maps, very thoroughly */
   n_secmaps_found = 0;
   errmsg = check_auxmap_L1_L2_sanity( &n_secmaps_found );
   if (errmsg) {
      VG_(printf)("memcheck expensive sanity, auxmaps:\n\t%s", errmsg);
      return False;
   }

   /* n_secmaps_found is now the number referred to by the auxiliary
      primary map.  Now add on the ones referred to by the main
      primary map. */
   for (i = 0; i < N_PRIMARY_MAP; i++) {
      if (primary_map[i] == NULL) {
         bad = True;
      } else {
         if (!is_distinguished_sm(primary_map[i]))
            n_secmaps_found++;
      }
   }

   /* check that the number of secmaps issued matches the number that
      are reachable (iow, no secmap leaks) */
   if (n_secmaps_found != (n_issued_SMs - n_deissued_SMs))
      bad = True;

   if (bad) {
      VG_(printf)("memcheck expensive sanity: "
                  "apparent secmap leakage\n");
      return False;
   }

   if (bad) {
      VG_(printf)("memcheck expensive sanity: "
                  "auxmap covers wrong address space\n");
      return False;
   }

   /* there is only one pointer to each secmap (expensive) */

   return True;
}

/*------------------------------------------------------------*/
/*--- Command line args                                    ---*/
/*------------------------------------------------------------*/

/* 31 Aug 2015: Vectorised code is now so widespread that
   --partial-loads-ok needs to be enabled by default on all platforms.
   Not doing so causes lots of false errors. */
Bool          MC_(clo_partial_loads_ok)       = True;
Long          MC_(clo_freelist_vol)           = 20LL*1000LL*1000LL;
Long          MC_(clo_freelist_big_blocks)    =  1LL*1000LL*1000LL;
LeakCheckMode MC_(clo_leak_check)             = LC_Summary;
VgRes         MC_(clo_leak_resolution)        = Vg_HighRes;
UInt          MC_(clo_show_leak_kinds)        = R2S(Possible) | R2S(Unreached);
UInt          MC_(clo_error_for_leak_kinds)   = R2S(Possible) | R2S(Unreached);
UInt          MC_(clo_leak_check_heuristics)  =   H2S(LchStdString)
                                                | H2S( LchLength64)
                                                | H2S( LchNewArray)
                                                | H2S( LchMultipleInheritance);
Bool          MC_(clo_xtree_leak)             = False;
const HChar*  MC_(clo_xtree_leak_file) = "xtleak.kcg.%p";
Bool          MC_(clo_workaround_gcc296_bugs) = False;
Int           MC_(clo_malloc_fill)            = -1;
Int           MC_(clo_free_fill)              = -1;
KeepStacktraces MC_(clo_keep_stacktraces)     = KS_alloc_and_free;
Int           MC_(clo_mc_level)               = 2;
Bool          MC_(clo_show_mismatched_frees)  = True;
Bool          MC_(clo_show_realloc_size_zero) = True;

ExpensiveDefinednessChecks
              MC_(clo_expensive_definedness_checks) = EdcAUTO;

Bool          MC_(clo_ignore_range_below_sp)               = False;
UInt          MC_(clo_ignore_range_below_sp__first_offset) = 0;
UInt          MC_(clo_ignore_range_below_sp__last_offset)  = 0;

static const HChar * MC_(parse_leak_heuristics_tokens) =
   "-,stdstring,length64,newarray,multipleinheritance";
/* The first heuristic value (LchNone) has no keyword, as this is
   a fake heuristic used to collect the blocks found without any
   heuristic. */

static Bool mc_process_cmd_line_options(const HChar* arg)
{
   const HChar* tmp_str;
   Bool   tmp_show;

   tl_assert( MC_(clo_mc_level) >= 1 && MC_(clo_mc_level) <= 3 );

   /* Set MC_(clo_mc_level):
         1 = A bit tracking only
         2 = A and V bit tracking, but no V bit origins
         3 = A and V bit tracking, and V bit origins

      Do this by inspecting --undef-value-errors= and
      --track-origins=.  Reject the case --undef-value-errors=no
      --track-origins=yes as meaningless.
   */
   if VG_BOOL_CLO(arg, "--undef-value-errors", tmp_show) {
      if (tmp_show) {
         if (MC_(clo_mc_level) == 1)
            MC_(clo_mc_level) = 2;
      } else {
         if (MC_(clo_mc_level) == 3) {
            goto bad_level;
         } else {
            MC_(clo_mc_level) = 1;
         }
      }
   }
   else if VG_BOOL_CLO(arg, "--track-origins", tmp_show) {
      if (tmp_show)  {
         if (MC_(clo_mc_level) == 1) {
            goto bad_level;
         } else {
            MC_(clo_mc_level) = 3;
         }
      } else {
      if (MC_(clo_mc_level) == 3)
         MC_(clo_mc_level) = 2;
      }
   }
   else if VG_BOOL_CLO(arg, "--partial-loads-ok", MC_(clo_partial_loads_ok)) {}
   else if VG_USET_CLOM(cloPD, arg, "--errors-for-leak-kinds",
                        MC_(parse_leak_kinds_tokens),
                        MC_(clo_error_for_leak_kinds)) {}
   else if VG_USET_CLOM(cloPD, arg, "--show-leak-kinds",
                        MC_(parse_leak_kinds_tokens),
                        MC_(clo_show_leak_kinds)) {}
   else if VG_USET_CLOM(cloPD, arg, "--leak-check-heuristics",
                        MC_(parse_leak_heuristics_tokens),
                        MC_(clo_leak_check_heuristics)) {}
   else if (VG_BOOL_CLOM(cloPD, arg, "--show-reachable", tmp_show)) {
      if (tmp_show) {
         MC_(clo_show_leak_kinds) = MC_(all_Reachedness)();
      } else {
         MC_(clo_show_leak_kinds) &= ~R2S(Reachable);
      }
   }
   else if VG_BOOL_CLOM(cloPD, arg, "--show-possibly-lost", tmp_show) {
      if (tmp_show) {
         MC_(clo_show_leak_kinds) |= R2S(Possible);
      } else {
         MC_(clo_show_leak_kinds) &= ~R2S(Possible);
      }
   }
   else if VG_BOOL_CLO(arg, "--workaround-gcc296-bugs",
                       MC_(clo_workaround_gcc296_bugs)) {}

   else if VG_BINT_CLOM(cloPD, arg, "--freelist-vol",  MC_(clo_freelist_vol),
                        0, 10*1000*1000*1000LL) {}

   else if VG_BINT_CLOM(cloPD, arg, "--freelist-big-blocks",
                        MC_(clo_freelist_big_blocks),
                        0, 10*1000*1000*1000LL) {}

   else if VG_XACT_CLOM(cloPD, arg, "--leak-check=no",
                       MC_(clo_leak_check), LC_Off) {}
   else if VG_XACT_CLOM(cloPD, arg, "--leak-check=summary",
                       MC_(clo_leak_check), LC_Summary) {}
   else if VG_XACT_CLOM(cloPD, arg, "--leak-check=yes",
                       MC_(clo_leak_check), LC_Full) {}
   else if VG_XACT_CLOM(cloPD, arg, "--leak-check=full",
                       MC_(clo_leak_check), LC_Full) {}

   else if VG_XACT_CLO(arg, "--leak-resolution=low",
                       MC_(clo_leak_resolution), Vg_LowRes) {}
   else if VG_XACT_CLO(arg, "--leak-resolution=med",
                       MC_(clo_leak_resolution), Vg_MedRes) {}
   else if VG_XACT_CLO(arg, "--leak-resolution=high",
                       MC_(clo_leak_resolution), Vg_HighRes) {}

   else if VG_STR_CLOM(cloPD, arg, "--ignore-ranges", tmp_str) {
      Bool ok = parse_ignore_ranges(tmp_str);
      if (!ok) {
         VG_(message)(Vg_DebugMsg,
            "ERROR: --ignore-ranges: "
            "invalid syntax, or end <= start in range\n");
         return False;
      }
      if (gIgnoredAddressRanges) {
         UInt i;
         for (i = 0; i < VG_(sizeRangeMap)(gIgnoredAddressRanges); i++) {
            UWord val     = IAR_INVALID;
            UWord key_min = ~(UWord)0;
            UWord key_max = (UWord)0;
            VG_(indexRangeMap)( &key_min, &key_max, &val,
                                gIgnoredAddressRanges, i );
            tl_assert(key_min <= key_max);
            UWord limit = 0x4000000; /* 64M - entirely arbitrary limit */
            if (key_max - key_min > limit && val == IAR_CommandLine) {
               VG_(message)(Vg_DebugMsg,
                  "ERROR: --ignore-ranges: suspiciously large range:\n");
               VG_(message)(Vg_DebugMsg,
                   "       0x%lx-0x%lx (size %lu)\n", key_min, key_max,
                   key_max - key_min + 1);
               return False;
            }
         }
      }
   }

   else if VG_STR_CLOM(cloPD, arg, "--ignore-range-below-sp", tmp_str) {
      /* This seems at first a bit weird, but: in order to imply
         a non-wrapped-around address range, the first offset needs to be
         larger than the second one.  For example
            --ignore-range-below-sp=8192,8189
         would cause accesses to in the range [SP-8192, SP-8189] to be
         ignored. */
      UInt offs1 = 0, offs2 = 0;
      Bool ok = parse_UInt_pair(&tmp_str, &offs1, &offs2);
      // Ensure we used all the text after the '=' sign.
      if (ok && *tmp_str != 0) ok = False;
      if (!ok) {
         VG_(message)(Vg_DebugMsg,
                      "ERROR: --ignore-range-below-sp: invalid syntax. "
                      " Expected \"...=decimalnumber-decimalnumber\".\n");
         return False;
      }
      if (offs1 > 1000*1000 /*arbitrary*/ || offs2 > 1000*1000 /*ditto*/) {
         VG_(message)(Vg_DebugMsg,
                      "ERROR: --ignore-range-below-sp: suspiciously large "
                      "offset(s): %u and %u\n", offs1, offs2);
         return False;
      }
      if (offs1 <= offs2) {
         VG_(message)(Vg_DebugMsg,
                      "ERROR: --ignore-range-below-sp: invalid offsets "
                      "(the first must be larger): %u and %u\n", offs1, offs2);
         return False;
      }
      tl_assert(offs1 > offs2);
      if (offs1 - offs2 > 4096 /*arbitrary*/) {
         VG_(message)(Vg_DebugMsg,
                      "ERROR: --ignore-range-below-sp: suspiciously large "
                      "range: %u-%u (size %u)\n", offs1, offs2, offs1 - offs2);
         return False;
      }
      MC_(clo_ignore_range_below_sp) = True;
      MC_(clo_ignore_range_below_sp__first_offset) = offs1;
      MC_(clo_ignore_range_below_sp__last_offset)  = offs2;
      return True;
   }

   else if VG_BHEX_CLO(arg, "--malloc-fill", MC_(clo_malloc_fill), 0x00,0xFF) {}
   else if VG_BHEX_CLO(arg, "--free-fill",   MC_(clo_free_fill),   0x00,0xFF) {}

   else if VG_XACT_CLO(arg, "--keep-stacktraces=alloc",
                       MC_(clo_keep_stacktraces), KS_alloc) {}
   else if VG_XACT_CLO(arg, "--keep-stacktraces=free",
                       MC_(clo_keep_stacktraces), KS_free) {}
   else if VG_XACT_CLO(arg, "--keep-stacktraces=alloc-and-free",
                       MC_(clo_keep_stacktraces), KS_alloc_and_free) {}
   else if VG_XACT_CLO(arg, "--keep-stacktraces=alloc-then-free",
                       MC_(clo_keep_stacktraces), KS_alloc_then_free) {}
   else if VG_XACT_CLO(arg, "--keep-stacktraces=none",
                       MC_(clo_keep_stacktraces), KS_none) {}

   else if VG_BOOL_CLOM(cloPD, arg, "--show-mismatched-frees",
                        MC_(clo_show_mismatched_frees)) {}
   else if VG_BOOL_CLOM(cloPD, arg, "--show-realloc-size-zero",
                        MC_(clo_show_realloc_size_zero)) {}

   else if VG_XACT_CLO(arg, "--expensive-definedness-checks=no",
                            MC_(clo_expensive_definedness_checks), EdcNO) {}
   else if VG_XACT_CLO(arg, "--expensive-definedness-checks=auto",
                            MC_(clo_expensive_definedness_checks), EdcAUTO) {}
   else if VG_XACT_CLO(arg, "--expensive-definedness-checks=yes",
                            MC_(clo_expensive_definedness_checks), EdcYES) {}

   else if VG_BOOL_CLO(arg, "--xtree-leak",
                       MC_(clo_xtree_leak)) {}
   else if VG_STR_CLO (arg, "--xtree-leak-file",
                       MC_(clo_xtree_leak_file)) {}

   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;


  bad_level:
   VG_(fmsg_bad_option)(arg,
      "--track-origins=yes has no effect when --undef-value-errors=no.\n");
   return False;
}

static void mc_print_usage(void)
{
   VG_(printf)(
"    --leak-check=no|summary|full     search for memory leaks at exit?  [summary]\n"
"    --leak-resolution=low|med|high   differentiation of leak stack traces [high]\n"
"    --show-leak-kinds=kind1,kind2,.. which leak kinds to show?\n"
"                                            [definite,possible]\n"
"    --errors-for-leak-kinds=kind1,kind2,..  which leak kinds are errors?\n"
"                                            [definite,possible]\n"
"        where kind is one of:\n"
"          definite indirect possible reachable all none\n"
"    --leak-check-heuristics=heur1,heur2,... which heuristics to use for\n"
"        improving leak search false positive [all]\n"
"        where heur is one of:\n"
"          stdstring length64 newarray multipleinheritance all none\n"
"    --show-reachable=yes             same as --show-leak-kinds=all\n"
"    --show-reachable=no --show-possibly-lost=yes\n"
"                                     same as --show-leak-kinds=definite,possible\n"
"    --show-reachable=no --show-possibly-lost=no\n"
"                                     same as --show-leak-kinds=definite\n"
"    --xtree-leak=no|yes              output leak result in xtree format? [no]\n"
"    --xtree-leak-file=<file>         xtree leak report file [xtleak.kcg.%%p]\n"
"    --undef-value-errors=no|yes      check for undefined value errors [yes]\n"
"    --track-origins=no|yes           show origins of undefined values? [no]\n"
"    --partial-loads-ok=no|yes        too hard to explain here; see manual [yes]\n"
"    --expensive-definedness-checks=no|auto|yes\n"
"                                     Use extra-precise definedness tracking [auto]\n"
"    --freelist-vol=<number>          volume of freed blocks queue     [20000000]\n"
"    --freelist-big-blocks=<number>   releases first blocks with size>= [1000000]\n"
"    --workaround-gcc296-bugs=no|yes  self explanatory [no].  Deprecated.\n"
"                                     Use --ignore-range-below-sp instead.\n"
"    --ignore-ranges=0xPP-0xQQ[,0xRR-0xSS]   assume given addresses are OK\n"
"    --ignore-range-below-sp=<number>-<number>  do not report errors for\n"
"                                     accesses at the given offsets below SP\n"
"    --malloc-fill=<hexnumber>        fill malloc'd areas with given value\n"
"    --free-fill=<hexnumber>          fill free'd areas with given value\n"
"    --keep-stacktraces=alloc|free|alloc-and-free|alloc-then-free|none\n"
"        stack trace(s) to keep for malloc'd/free'd areas       [alloc-and-free]\n"
"    --show-mismatched-frees=no|yes   show frees that don't match the allocator? [yes]\n"
"    --show-realloc-size-zero=no|yes  show realocs with a size of zero? [yes]\n"
   );
}

static void mc_print_debug_usage(void)
{
   VG_(printf)(
"    (none)\n"
   );
}


/*------------------------------------------------------------*/
/*--- Client blocks                                        ---*/
/*------------------------------------------------------------*/

/* Client block management:

   This is managed as an expanding array of client block descriptors.
   Indices of live descriptors are issued to the client, so it can ask
   to free them later.  Therefore we cannot slide live entries down
   over dead ones.  Instead we must use free/inuse flags and scan for
   an empty slot at allocation time.  This in turn means allocation is
   relatively expensive, so we hope this does not happen too often.

   An unused block has start == size == 0
*/

/* type CGenBlock is defined in mc_include.h */

/* This subsystem is self-initialising. */
static UWord      cgb_size = 0;
static UWord      cgb_used = 0;
static CGenBlock* cgbs     = NULL;

/* Stats for this subsystem. */
static ULong cgb_used_MAX = 0;   /* Max in use. */
static ULong cgb_allocs   = 0;   /* Number of allocs. */
static ULong cgb_discards = 0;   /* Number of discards. */
static ULong cgb_search   = 0;   /* Number of searches. */


/* Get access to the client block array. */
void MC_(get_ClientBlock_array)( /*OUT*/CGenBlock** blocks,
                                 /*OUT*/UWord* nBlocks )
{
   *blocks  = cgbs;
   *nBlocks = cgb_used;
}


static
Int alloc_client_block ( void )
{
   UWord      i, sz_new;
   CGenBlock* cgbs_new;

   cgb_allocs++;

   for (i = 0; i < cgb_used; i++) {
      cgb_search++;
      if (cgbs[i].start == 0 && cgbs[i].size == 0)
         return i;
   }

   /* Not found.  Try to allocate one at the end. */
   if (cgb_used < cgb_size) {
      cgb_used++;
      return cgb_used-1;
   }

   /* Ok, we have to allocate a new one. */
   tl_assert(cgb_used == cgb_size);
   sz_new = (cgbs == NULL) ? 10 : (2 * cgb_size);

   cgbs_new = VG_(malloc)( "mc.acb.1", sz_new * sizeof(CGenBlock) );
   for (i = 0; i < cgb_used; i++)
      cgbs_new[i] = cgbs[i];

   if (cgbs != NULL)
      VG_(free)( cgbs );
   cgbs = cgbs_new;

   cgb_size = sz_new;
   cgb_used++;
   if (cgb_used > cgb_used_MAX)
      cgb_used_MAX = cgb_used;
   return cgb_used-1;
}


static void show_client_block_stats ( void )
{
   VG_(message)(Vg_DebugMsg,
      "general CBs: %llu allocs, %llu discards, %llu maxinuse, %llu search\n",
      cgb_allocs, cgb_discards, cgb_used_MAX, cgb_search
   );
}
static void print_monitor_help ( void )
{
   VG_(gdb_printf)
      (
"\n"
"memcheck monitor commands:\n"
"  xb <addr> [<len>]\n"
"        prints validity bits for <len> (or 1) bytes at <addr>\n"
"            bit values 0 = valid, 1 = invalid, __ = unaddressable byte\n"
"        Then prints the bytes values below the corresponding validity bits\n"
"        in a layout similar to the gdb command 'x /<len>xb <addr>'\n"
"        Example: xb 0x8049c78 10\n"
"  get_vbits <addr> [<len>]\n"
"        Similar to xb, but only prints the validity bytes by group of 4.\n"
"  make_memory [noaccess|undefined\n"
"                     |defined|Definedifaddressable] <addr> [<len>]\n"
"        mark <len> (or 1) bytes at <addr> with the given accessibility\n"
"  check_memory [addressable|defined] <addr> [<len>]\n"
"        check that <len> (or 1) bytes at <addr> have the given accessibility\n"
"            and outputs a description of <addr>\n"
"  leak_check [full*|summary|xtleak]\n"
"                [kinds kind1,kind2,...|reachable|possibleleak*|definiteleak]\n"
"                [heuristics heur1,heur2,...]\n"
"                [new|increased*|changed|any]\n"
"                [unlimited*|limited <max_loss_records_output>]\n"
"            * = defaults\n"
"         xtleak produces an xtree full leak result in xtleak.kcg.%%p.%%n\n"
"       where kind is one of:\n"
"         definite indirect possible reachable all none\n"
"       where heur is one of:\n"
"         stdstring length64 newarray multipleinheritance all none*\n"
"       Examples: leak_check\n"
"                 leak_check summary any\n"
"                 leak_check full kinds indirect,possible\n"
"                 leak_check full reachable any limited 100\n"
"  block_list <loss_record_nr>|<loss_record_nr_from>..<loss_record_nr_to>\n"
"                [unlimited*|limited <max_blocks>]\n"
"                [heuristics heur1,heur2,...]\n"
"        after a leak search, shows the list of blocks of <loss_record_nr>\n"
"        (or of the range <loss_record_nr_from>..<loss_record_nr_to>).\n"
"        With heuristics, only shows the blocks found via heur1,heur2,...\n"
"            * = defaults\n"
"  who_points_at <addr> [<len>]\n"
"        shows places pointing inside <len> (default 1) bytes at <addr>\n"
"        (with len 1, only shows \"start pointers\" pointing exactly to <addr>,\n"
"         with len > 1, will also show \"interior pointers\")\n"
"  xtmemory [<filename>]\n"
"        dump xtree memory profile in <filename> (default xtmemory.kcg.%%p.%%n)\n"
"\n");
}

/* Print szB bytes at address, with a format similar to the gdb command
   x /<szB>xb address.
   res[i] == 1 indicates the corresponding byte is addressable. */
static void gdb_xb (Addr address, SizeT szB, Int res[])
{
   UInt i;

   for (i = 0; i < szB; i++) {
      UInt bnr = i % 8;
      if (bnr == 0) {
         if (i != 0)
            VG_(printf) ("\n"); // Terminate previous line
         VG_(printf) ("%p:", (void*)(address+i));
      }
      if (res[i] == 1)
         VG_(printf) ("\t0x%02x", *(UChar*)(address+i));
      else
         VG_(printf) ("\t0x??");
   }
   VG_(printf) ("\n"); // Terminate previous line
}


/* Returns the address of the next non space character,
   or address of the string terminator. */
static HChar* next_non_space (HChar *s)
{
   while (*s && *s == ' ')
      s++;
   return s;
}

/* Parse an integer slice, i.e. a single integer or a range of integer.
   Syntax is:
       <integer>[..<integer> ]
   (spaces are allowed before and/or after ..).
   Return True if range correctly parsed, False otherwise. */
static Bool VG_(parse_slice) (HChar* s, HChar** saveptr,
                              UInt *from, UInt *to)
{
   HChar* wl;
   HChar *endptr;
   endptr = NULL;////
   wl = VG_(strtok_r) (s, " ", saveptr);

   /* slice must start with an integer. */
   if (wl == NULL) {
      VG_(gdb_printf) ("expecting integer or slice <from>..<to>\n");
      return False;
   }
   *from = VG_(strtoull10) (wl, &endptr);
   if (endptr == wl) {
      VG_(gdb_printf) ("invalid integer or slice <from>..<to>\n");
      return False;
   }

   if (*endptr == '\0' && *next_non_space(*saveptr) != '.') {
      /* wl token is an integer terminating the string
         or else next token does not start with .
         In both cases, the slice is a single integer. */
      *to = *from;
      return True;
   }

   if (*endptr == '\0') {
      // iii ..    => get the next token
      wl =  VG_(strtok_r) (NULL, " .", saveptr);
   } else {
      // It must be iii..
      if (*endptr != '.' && *(endptr+1) != '.') {
         VG_(gdb_printf) ("expecting slice <from>..<to>\n");
         return False;
      }
      if ( *(endptr+2) == ' ') {
         // It must be iii.. jjj  => get the next token
         wl =  VG_(strtok_r) (NULL, " .", saveptr);
      } else {
         // It must be iii..jjj
         wl = endptr+2;
      }
   }

   *to = VG_(strtoull10) (wl, &endptr);
   if (*endptr != '\0') {
      VG_(gdb_printf) ("missing/wrong 'to' of slice <from>..<to>\n");
      return False;
   }

   if (*from > *to) {
      VG_(gdb_printf) ("<from> cannot be bigger than <to> "
                       "in slice <from>..<to>\n");
      return False;
   }

   return True;
}

/* return True if request recognised, False otherwise */
static Bool handle_gdb_monitor_command (ThreadId tid, HChar *req)
{
   HChar* wcmd;
   HChar s[VG_(strlen)(req) + 1]; /* copy for strtok_r */
   HChar *ssaveptr;

   VG_(strcpy) (s, req);

   wcmd = VG_(strtok_r) (s, " ", &ssaveptr);
   /* NB: if possible, avoid introducing a new command below which
      starts with the same first letter(s) as an already existing
      command. This ensures a shorter abbreviation for the user. */
   switch (VG_(keyword_id)
           ("help get_vbits leak_check make_memory check_memory "
            "block_list who_points_at xb xtmemory",
            wcmd, kwd_report_duplicated_matches)) {
   case -2: /* multiple matches */
      return True;
   case -1: /* not found */
      return False;
   case  0: /* help */
      print_monitor_help();
      return True;
   case  1: { /* get_vbits */
      Addr address;
      SizeT szB = 1;
      if (VG_(strtok_get_address_and_size) (&address, &szB, &ssaveptr)) {
         UChar vbits;
         Int i;
         Int unaddressable = 0;
         for (i = 0; i < szB; i++) {
            Int res = mc_get_or_set_vbits_for_client
               (address+i, (Addr) &vbits, 1,
                False, /* get them */
                False  /* is client request */ );
            /* we are before the first character on next line, print a \n. */
            if ((i % 32) == 0 && i != 0)
               VG_(printf) ("\n");
            /* we are before the next block of 4 starts, print a space. */
            else if ((i % 4) == 0 && i != 0)
               VG_(printf) (" ");
            if (res == 1) {
               VG_(printf) ("%02x", vbits);
            } else {
               tl_assert(3 == res);
               unaddressable++;
               VG_(printf) ("__");
            }
         }
         VG_(printf) ("\n");
         if (unaddressable) {
            VG_(printf)
               ("Address %p len %lu has %d bytes unaddressable\n",
                (void *)address, szB, unaddressable);
         }
      }
      return True;
   }
   case  2: { /* leak_check */
      Int err = 0;
      LeakCheckParams lcp;
      HChar* xt_filename = NULL;
      HChar* kw;

      lcp.mode               = LC_Full;
      lcp.show_leak_kinds    = R2S(Possible) | R2S(Unreached);
      lcp.errors_for_leak_kinds = 0; // no errors for interactive leak search.
      lcp.heuristics         = 0;
      lcp.deltamode          = LCD_Increased;
      lcp.max_loss_records_output = 999999999;
      lcp.requested_by_monitor_command = True;
      lcp.xt_filename = NULL;

      for (kw = VG_(strtok_r) (NULL, " ", &ssaveptr);
           kw != NULL;
           kw = VG_(strtok_r) (NULL, " ", &ssaveptr)) {
         switch (VG_(keyword_id)
                 ("full summary xtleak "
                  "kinds reachable possibleleak definiteleak "
                  "heuristics "
                  "new increased changed any "
                  "unlimited limited ",
                  kw, kwd_report_all)) {
         case -2: err++; break;
         case -1: err++; break;
         case  0: /* full */
            lcp.mode = LC_Full; break;
         case  1: /* summary */
            lcp.mode = LC_Summary; break;
         case  2: /* xtleak */
            lcp.mode = LC_Full;
            xt_filename
               = VG_(expand_file_name)("--xtleak-mc_main.c",
                                       "xtleak.kcg.%p.%n");
            lcp.xt_filename = xt_filename;
            break;
         case  3: { /* kinds */
            wcmd = VG_(strtok_r) (NULL, " ", &ssaveptr);
            if (wcmd == NULL
                || !VG_(parse_enum_set)(MC_(parse_leak_kinds_tokens),
                                        True/*allow_all*/,
                                        wcmd,
                                        &lcp.show_leak_kinds)) {
               VG_(gdb_printf) ("missing or malformed leak kinds set\n");
               err++;
            }
            break;
         }
         case  4: /* reachable */
            lcp.show_leak_kinds = MC_(all_Reachedness)();
            break;
         case  5: /* possibleleak */
            lcp.show_leak_kinds
               = R2S(Possible) | R2S(IndirectLeak) | R2S(Unreached);
            break;
         case  6: /* definiteleak */
            lcp.show_leak_kinds = R2S(Unreached);
            break;
         case  7: { /* heuristics */
            wcmd = VG_(strtok_r) (NULL, " ", &ssaveptr);
            if (wcmd == NULL
                || !VG_(parse_enum_set)(MC_(parse_leak_heuristics_tokens),
                                        True,/*allow_all*/
                                        wcmd,
                                        &lcp.heuristics)) {
               VG_(gdb_printf) ("missing or malformed heuristics set\n");
               err++;
            }
            break;
         }
         case  8: /* new */
            lcp.deltamode = LCD_New; break;
         case  9: /* increased */
            lcp.deltamode = LCD_Increased; break;
         case 10: /* changed */
            lcp.deltamode = LCD_Changed; break;
         case 11: /* any */
            lcp.deltamode = LCD_Any; break;
         case 12: /* unlimited */
            lcp.max_loss_records_output = 999999999; break;
         case 13: { /* limited */
            Int int_value;
            const HChar* endptr;

            wcmd = VG_(strtok_r) (NULL, " ", &ssaveptr);
            if (wcmd == NULL) {
               int_value = 0;
               endptr = "empty"; /* to report an error below */
            } else {
               HChar *the_end;
               int_value = VG_(strtoll10) (wcmd, &the_end);
               endptr = the_end;
            }
            if (*endptr != '\0')
               VG_(gdb_printf) ("missing or malformed integer value\n");
            else if (int_value > 0)
               lcp.max_loss_records_output = (UInt) int_value;
            else
               VG_(gdb_printf) ("max_loss_records_output must be >= 1,"
                                " got %d\n", int_value);
            break;
         }
         default:
            tl_assert (0);
         }
      }
      if (!err)
         MC_(detect_memory_leaks)(tid, &lcp);
      if (xt_filename != NULL)
         VG_(free)(xt_filename);
      return True;
   }

   case  3: { /* make_memory */
      Addr address;
      SizeT szB = 1;
      Int kwdid = VG_(keyword_id)
         ("noaccess undefined defined Definedifaddressable",
          VG_(strtok_r) (NULL, " ", &ssaveptr), kwd_report_all);
      if (!VG_(strtok_get_address_and_size) (&address, &szB, &ssaveptr))
         return True;
      switch (kwdid) {
      case -2: break;
      case -1: break;
      case  0: MC_(make_mem_noaccess) (address, szB); break;
      case  1: make_mem_undefined_w_tid_and_okind ( address, szB, tid,
                                                    MC_OKIND_USER ); break;
      case  2: MC_(make_mem_defined) ( address, szB ); break;
      case  3: make_mem_defined_if_addressable ( address, szB ); break;;
      default: tl_assert(0);
      }
      return True;
   }

   case  4: { /* check_memory */
      Addr address;
      SizeT szB = 1;
      Addr bad_addr;
      UInt okind;
      const HChar* src;
      UInt otag;
      UInt ecu;
      ExeContext* origin_ec;
      MC_ReadResult res;

      Int kwdid = VG_(keyword_id)
         ("addressable defined",
          VG_(strtok_r) (NULL, " ", &ssaveptr), kwd_report_all);
      if (!VG_(strtok_get_address_and_size) (&address, &szB, &ssaveptr))
         return True;
      switch (kwdid) {
      case -2: break;
      case -1: break;
      case  0: /* addressable */
         if (is_mem_addressable ( address, szB, &bad_addr ))
            VG_(printf) ("Address %p len %lu addressable\n",
                             (void *)address, szB);
         else
            VG_(printf)
               ("Address %p len %lu not addressable:\nbad address %p\n",
                (void *)address, szB, (void *) bad_addr);
         // Describe this (probably live) address with current epoch
         MC_(pp_describe_addr) (VG_(current_DiEpoch)(), address);
         break;
      case  1: /* defined */
         res = is_mem_defined ( address, szB, &bad_addr, &otag );
         if (MC_AddrErr == res)
            VG_(printf)
               ("Address %p len %lu not addressable:\nbad address %p\n",
                (void *)address, szB, (void *) bad_addr);
         else if (MC_ValueErr == res) {
            okind = otag & 3;
            switch (okind) {
            case MC_OKIND_STACK:
               src = " was created by a stack allocation"; break;
            case MC_OKIND_HEAP:
               src = " was created by a heap allocation"; break;
            case MC_OKIND_USER:
               src = " was created by a client request"; break;
            case MC_OKIND_UNKNOWN:
               src = ""; break;
            default: tl_assert(0);
            }
            VG_(printf)
               ("Address %p len %lu not defined:\n"
                "Uninitialised value at %p%s\n",
                (void *)address, szB, (void *) bad_addr, src);
            ecu = otag & ~3;
            if (VG_(is_plausible_ECU)(ecu)) {
               origin_ec = VG_(get_ExeContext_from_ECU)( ecu );
               VG_(pp_ExeContext)( origin_ec );
            }
         }
         else
            VG_(printf) ("Address %p len %lu defined\n",
                         (void *)address, szB);
         // Describe this (probably live) address with current epoch
         MC_(pp_describe_addr) (VG_(current_DiEpoch)(), address);
         break;
      default: tl_assert(0);
      }
      return True;
   }

   case  5: { /* block_list */
      HChar* wl;
      HChar *the_end;
      UInt lr_nr_from = 0;
      UInt lr_nr_to = 0;

      if (VG_(parse_slice) (NULL, &ssaveptr, &lr_nr_from, &lr_nr_to)) {
         UInt limit_blocks = 999999999;
         Int int_value;
         UInt heuristics = 0;

         for (wl = VG_(strtok_r) (NULL, " ", &ssaveptr);
              wl != NULL;
              wl = VG_(strtok_r) (NULL, " ", &ssaveptr)) {
            switch (VG_(keyword_id) ("unlimited limited heuristics ",
                                     wl,  kwd_report_all)) {
            case -2: return True;
            case -1: return True;
            case  0: /* unlimited */
               limit_blocks = 999999999; break;
            case  1: /* limited */
               wcmd = VG_(strtok_r) (NULL, " ", &ssaveptr);
               if (wcmd == NULL) {
                  VG_(gdb_printf) ("missing integer value\n");
                  return True;
               }
               int_value = VG_(strtoll10) (wcmd, &the_end);
               if (*the_end != '\0') {
                  VG_(gdb_printf) ("malformed integer value\n");
                  return True;
               }
               if (int_value <= 0) {
                  VG_(gdb_printf) ("max_blocks must be >= 1,"
                                   " got %d\n", int_value);
                  return True;
               }
               limit_blocks = (UInt) int_value;
               break;
            case  2: /* heuristics */
               wcmd = VG_(strtok_r) (NULL, " ", &ssaveptr);
               if (wcmd == NULL
                   || !VG_(parse_enum_set)(MC_(parse_leak_heuristics_tokens),
                                           True,/*allow_all*/
                                           wcmd,
                                           &heuristics)) {
                  VG_(gdb_printf) ("missing or malformed heuristics set\n");
                  return True;
               }
               break;
            default:
               tl_assert (0);
            }
         }
         /* substract 1 from lr_nr_from/lr_nr_to  as what is shown to the user
            is 1 more than the index in lr_array. */
         if (lr_nr_from == 0 || ! MC_(print_block_list) (lr_nr_from-1,
                                                         lr_nr_to-1,
                                                         limit_blocks,
                                                         heuristics))
            VG_(gdb_printf) ("invalid loss record nr\n");
      }
      return True;
   }

   case  6: { /* who_points_at */
      Addr address;
      SizeT szB = 1;

      if (!VG_(strtok_get_address_and_size) (&address, &szB, &ssaveptr))
         return True;
      if (address == (Addr) 0) {
         VG_(gdb_printf) ("Cannot search who points at 0x0\n");
         return True;
      }
      MC_(who_points_at) (address, szB);
      return True;
   }

   case  7: { /* xb */
      Addr address;
      SizeT szB = 1;
      if (VG_(strtok_get_address_and_size) (&address, &szB, &ssaveptr)) {
         UChar vbits[8];
         Int res[8];
         Int i;
         Int unaddressable = 0;
         for (i = 0; i < szB; i++) {
            Int bnr = i % 8;
            /* We going to print the first vabits of a new line.
               Terminate the previous line if needed: prints a line with the
               address and the data. */
            if (bnr == 0) {
               if (i != 0) {
                  VG_(printf) ("\n");
                  gdb_xb (address + i - 8, 8, res);
               }
               VG_(printf) ("\t"); // To align VABITS with gdb_xb layout
            }
            res[bnr] = mc_get_or_set_vbits_for_client
               (address+i, (Addr) &vbits[bnr], 1,
                False, /* get them */
                False  /* is client request */ );
            if (res[bnr] == 1) {
               VG_(printf) ("\t  %02x", vbits[bnr]);
            } else {
               tl_assert(3 == res[bnr]);
               unaddressable++;
               VG_(printf) ("\t  __");
            }
         }
         VG_(printf) ("\n");
         if (szB % 8 == 0 && szB > 0)
            gdb_xb (address + szB - 8, 8, res);
         else
            gdb_xb (address + szB - szB % 8, szB % 8, res);
         if (unaddressable) {
            VG_(printf)
               ("Address %p len %lu has %d bytes unaddressable\n",
                (void *)address, szB, unaddressable);
         }
      }
      return True;
   }

   case  8: { /* xtmemory */
      HChar* filename;
      filename = VG_(strtok_r) (NULL, " ", &ssaveptr);
      MC_(xtmemory_report)(filename, False);
      return True;
   }

   default:
      tl_assert(0);
      return False;
   }
}

/*------------------------------------------------------------*/
/*--- Client requests                                      ---*/
/*------------------------------------------------------------*/

static Bool mc_handle_client_request ( ThreadId tid, UWord* arg, UWord* ret )
{
   Int   i;
   Addr  bad_addr;
   MC_Chunk* mc = NULL;

   if (!VG_IS_TOOL_USERREQ('M','C',arg[0])
       && VG_USERREQ__MALLOCLIKE_BLOCK != arg[0]
       && VG_USERREQ__RESIZEINPLACE_BLOCK != arg[0]
       && VG_USERREQ__FREELIKE_BLOCK   != arg[0]
       && VG_USERREQ__CREATE_MEMPOOL   != arg[0]
       && VG_USERREQ__DESTROY_MEMPOOL  != arg[0]
       && VG_USERREQ__MEMPOOL_ALLOC    != arg[0]
       && VG_USERREQ__MEMPOOL_FREE     != arg[0]
       && VG_USERREQ__MEMPOOL_TRIM     != arg[0]
       && VG_USERREQ__MOVE_MEMPOOL     != arg[0]
       && VG_USERREQ__MEMPOOL_CHANGE   != arg[0]
       && VG_USERREQ__MEMPOOL_EXISTS   != arg[0]
       && VG_USERREQ__GDB_MONITOR_COMMAND   != arg[0]
       && VG_USERREQ__ENABLE_ADDR_ERROR_REPORTING_IN_RANGE != arg[0]
       && VG_USERREQ__DISABLE_ADDR_ERROR_REPORTING_IN_RANGE != arg[0])
      return False;

   switch (arg[0]) {
      case VG_USERREQ__CHECK_MEM_IS_ADDRESSABLE: {
         Bool ok = is_mem_addressable ( arg[1], arg[2], &bad_addr );
         if (!ok)
            MC_(record_user_error) ( tid, bad_addr, /*isAddrErr*/True, 0 );
         *ret = ok ? (UWord)NULL : bad_addr;
         break;
      }

      case VG_USERREQ__CHECK_MEM_IS_DEFINED: {
         Bool errorV    = False;
         Addr bad_addrV = 0;
         UInt otagV     = 0;
         Bool errorA    = False;
         Addr bad_addrA = 0;
         is_mem_defined_comprehensive(
            arg[1], arg[2],
            &errorV, &bad_addrV, &otagV, &errorA, &bad_addrA
         );
         if (errorV) {
            MC_(record_user_error) ( tid, bad_addrV,
                                     /*isAddrErr*/False, otagV );
         }
         if (errorA) {
            MC_(record_user_error) ( tid, bad_addrA,
                                     /*isAddrErr*/True, 0 );
         }
         /* Return the lower of the two erring addresses, if any. */
         *ret = 0;
         if (errorV && !errorA) {
            *ret = bad_addrV;
         }
         if (!errorV && errorA) {
            *ret = bad_addrA;
         }
         if (errorV && errorA) {
            *ret = bad_addrV < bad_addrA ? bad_addrV : bad_addrA;
         }
         break;
      }

      case VG_USERREQ__DO_LEAK_CHECK: {
         LeakCheckParams lcp;

         if (arg[1] == 0)
            lcp.mode = LC_Full;
         else if (arg[1] == 1)
            lcp.mode = LC_Summary;
         else {
            VG_(message)(Vg_UserMsg,
                         "Warning: unknown memcheck leak search mode\n");
            lcp.mode = LC_Full;
         }

         lcp.show_leak_kinds = MC_(clo_show_leak_kinds);
         lcp.errors_for_leak_kinds = MC_(clo_error_for_leak_kinds);
         lcp.heuristics = MC_(clo_leak_check_heuristics);

         if (arg[2] == 0)
            lcp.deltamode = LCD_Any;
         else if (arg[2] == 1)
            lcp.deltamode = LCD_Increased;
         else if (arg[2] == 2)
            lcp.deltamode = LCD_Changed;
         else if (arg[2] == 3)
            lcp.deltamode = LCD_New;
         else {
            VG_(message)
               (Vg_UserMsg,
                "Warning: unknown memcheck leak search deltamode\n");
            lcp.deltamode = LCD_Any;
         }
         lcp.max_loss_records_output = 999999999;
         lcp.requested_by_monitor_command = False;
         lcp.xt_filename = NULL;

         MC_(detect_memory_leaks)(tid, &lcp);
         *ret = 0; /* return value is meaningless */
         break;
      }

      case VG_USERREQ__MAKE_MEM_NOACCESS:
         MC_(make_mem_noaccess) ( arg[1], arg[2] );
         *ret = -1;
         break;

      case VG_USERREQ__MAKE_MEM_UNDEFINED:
         make_mem_undefined_w_tid_and_okind ( arg[1], arg[2], tid,
                                              MC_OKIND_USER );
         *ret = -1;
         break;

      case VG_USERREQ__MAKE_MEM_DEFINED:
         MC_(make_mem_defined) ( arg[1], arg[2] );
         *ret = -1;
         break;

      case VG_USERREQ__MAKE_MEM_DEFINED_IF_ADDRESSABLE:
         make_mem_defined_if_addressable ( arg[1], arg[2] );
         *ret = -1;
         break;

      case VG_USERREQ__CREATE_BLOCK: /* describe a block */
         if (arg[1] != 0 && arg[2] != 0) {
            i = alloc_client_block();
            /* VG_(printf)("allocated %d %p\n", i, cgbs); */
            cgbs[i].start = arg[1];
            cgbs[i].size  = arg[2];
            cgbs[i].desc  = VG_(strdup)("mc.mhcr.1", (HChar *)arg[3]);
            cgbs[i].where = VG_(record_ExeContext) ( tid, 0/*first_ip_delta*/ );
            *ret = i;
         } else
            *ret = -1;
         break;

      case VG_USERREQ__DISCARD: /* discard */
         if (cgbs == NULL
             || arg[2] >= cgb_used ||
             (cgbs[arg[2]].start == 0 && cgbs[arg[2]].size == 0)) {
            *ret = 1;
         } else {
            tl_assert(arg[2] < cgb_used);
            cgbs[arg[2]].start = cgbs[arg[2]].size = 0;
            VG_(free)(cgbs[arg[2]].desc);
            cgb_discards++;
            *ret = 0;
         }
         break;

      case VG_USERREQ__GET_VBITS:
         *ret = mc_get_or_set_vbits_for_client
                   ( arg[1], arg[2], arg[3],
                     False /* get them */,
                     True /* is client request */ );
         break;

      case VG_USERREQ__SET_VBITS:
         *ret = mc_get_or_set_vbits_for_client
                   ( arg[1], arg[2], arg[3],
                     True /* set them */,
                     True /* is client request */ );
         break;

      case VG_USERREQ__COUNT_LEAKS: { /* count leaked bytes */
         UWord** argp = (UWord**)arg;
         // MC_(bytes_leaked) et al were set by the last leak check (or zero
         // if no prior leak checks performed).
         *argp[1] = MC_(bytes_leaked) + MC_(bytes_indirect);
         *argp[2] = MC_(bytes_dubious);
         *argp[3] = MC_(bytes_reachable);
         *argp[4] = MC_(bytes_suppressed);
         // there is no argp[5]
         //*argp[5] = MC_(bytes_indirect);
         // XXX need to make *argp[1-4] defined;  currently done in the
         // VALGRIND_COUNT_LEAKS_MACRO by initialising them to zero.
         *ret = 0;
         return True;
      }
      case VG_USERREQ__COUNT_LEAK_BLOCKS: { /* count leaked blocks */
         UWord** argp = (UWord**)arg;
         // MC_(blocks_leaked) et al were set by the last leak check (or zero
         // if no prior leak checks performed).
         *argp[1] = MC_(blocks_leaked) + MC_(blocks_indirect);
         *argp[2] = MC_(blocks_dubious);
         *argp[3] = MC_(blocks_reachable);
         *argp[4] = MC_(blocks_suppressed);
         // there is no argp[5]
         //*argp[5] = MC_(blocks_indirect);
         // XXX need to make *argp[1-4] defined;  currently done in the
         // VALGRIND_COUNT_LEAK_BLOCKS_MACRO by initialising them to zero.
         *ret = 0;
         return True;
      }
      case VG_USERREQ__MALLOCLIKE_BLOCK: {
         Addr p         = (Addr)arg[1];
         SizeT sizeB    =       arg[2];
         UInt rzB       =       arg[3];
         Bool is_zeroed = (Bool)arg[4];

         MC_(new_block) ( tid, p, sizeB, /*ignored*/0U, 0U, is_zeroed,
                          MC_AllocCustom, MC_(malloc_list) );
         if (rzB > 0) {
            MC_(make_mem_noaccess) ( p - rzB, rzB);
            MC_(make_mem_noaccess) ( p + sizeB, rzB);
         }
         return True;
      }
      case VG_USERREQ__RESIZEINPLACE_BLOCK: {
         Addr p         = (Addr)arg[1];
         SizeT oldSizeB =       arg[2];
         SizeT newSizeB =       arg[3];
         UInt rzB       =       arg[4];

         MC_(handle_resizeInPlace) ( tid, p, oldSizeB, newSizeB, rzB );
         return True;
      }
      case VG_USERREQ__FREELIKE_BLOCK: {
         Addr p         = (Addr)arg[1];
         UInt rzB       =       arg[2];

         MC_(handle_free) ( tid, p, rzB, MC_AllocCustom );
         return True;
      }

      case _VG_USERREQ__MEMCHECK_RECORD_OVERLAP_ERROR: {
         HChar* s  = (HChar*)arg[1];
         Addr  dst = (Addr) arg[2];
         Addr  src = (Addr) arg[3];
         SizeT len = (SizeT)arg[4];
         MC_(record_overlap_error)(tid, s, src, dst, len);
         return True;
      }

   case _VG_USERREQ__MEMCHECK_VERIFY_ALIGNMENT: {
      struct AlignedAllocInfo *aligned_alloc_info  = (struct AlignedAllocInfo *)arg[1];
      tl_assert(aligned_alloc_info);

      switch (aligned_alloc_info->alloc_kind) {
      case AllocKindMemalign:
         // other platforms just ensure it is a power of 2
         // ignore Illumos only enforcing multiple of 4 (probably a bug)
         if (aligned_alloc_info->orig_alignment  == 0U ||
             (aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be power of 2)" );
         }
         // size zero not allowed on all platforms (e.g. Illumos)
         if (aligned_alloc_info->size == 0) {
            MC_(record_bad_size) ( tid, aligned_alloc_info->size, "memalign()" );
         }
         break;
      case AllocKindPosixMemalign:
         // must be power of 2
         // alignment at least sizeof(size_t)
         // size of 0 implementation defined
         if (aligned_alloc_info->orig_alignment < sizeof(SizeT) ||
             (aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be non-zero, a power of 2 and a multiple of sizeof(void*))" );
         }
         if (aligned_alloc_info->size == 0) {
            MC_(record_bad_size) ( tid, aligned_alloc_info->size, "posix_memalign()" );
         }
         break;
      case AllocKindAlignedAlloc:
         // must be power of 2
         if ((aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be a power of 2)" );
         }
         // size should be integral multiple of alignment
         if (aligned_alloc_info->orig_alignment &&
             aligned_alloc_info->size % aligned_alloc_info->orig_alignment != 0U) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , aligned_alloc_info->size, " (size should be a multiple of alignment)" );
         }
         if (aligned_alloc_info->size == 0) {
            MC_(record_bad_size) ( tid, aligned_alloc_info->size, "aligned_alloc()" );
         }
         break;
      case AllocKindDeleteSized:
         mc = VG_(HT_lookup) ( MC_(malloc_list), (UWord)aligned_alloc_info->mem );
         if (mc && mc->szB != aligned_alloc_info->size) {
            MC_(record_size_mismatch_error) ( tid, mc, aligned_alloc_info->size, "new/delete" );
         }
         break;
      case AllocKindVecDeleteSized:
         mc = VG_(HT_lookup) ( MC_(malloc_list), (UWord)aligned_alloc_info->mem );
         if (mc && mc->szB != aligned_alloc_info->size) {
            MC_(record_size_mismatch_error) ( tid, mc, aligned_alloc_info->size, "new[][/delete[]" );
         }
         break;
      case AllocKindNewAligned:
         if (aligned_alloc_info->orig_alignment == 0 ||
             (aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be non-zero and a power of 2)" );
         }
         break;
      case AllocKindVecNewAligned:
         if (aligned_alloc_info->orig_alignment == 0 ||
             (aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be non-zero and a power of 2)" );
         }
         break;
      case AllocKindDeleteDefault:
         mc = VG_(HT_lookup) ( MC_(malloc_list), (UWord)aligned_alloc_info->mem );
         if (mc && mc->alignB) {
            MC_(record_align_mismatch_error) ( tid, mc, 0U, True, "new/delete");
         }
         break;
      case AllocKindDeleteAligned:
         if (aligned_alloc_info->orig_alignment == 0 ||
             (aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be non-zero and a power of 2)" );
         }
         mc = VG_(HT_lookup) ( MC_(malloc_list), (UWord)aligned_alloc_info->mem );
         if (mc && aligned_alloc_info->orig_alignment != mc->alignB) {
            MC_(record_align_mismatch_error) ( tid, mc, aligned_alloc_info->orig_alignment, False, "new/delete");
         }
         break;
      case AllocKindVecDeleteDefault:
         mc = VG_(HT_lookup) ( MC_(malloc_list), (UWord)aligned_alloc_info->mem );
         if (mc && mc->alignB) {
            MC_(record_align_mismatch_error) ( tid, mc, 0U, True, "new[]/delete[]");
         }
         break;
      case AllocKindVecDeleteAligned:
         if (aligned_alloc_info->orig_alignment == 0 ||
             (aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be non-zero and a power of 2)" );
         }
         mc = VG_(HT_lookup) ( MC_(malloc_list), (UWord)aligned_alloc_info->mem );
         if (mc && aligned_alloc_info->orig_alignment != mc->alignB) {
            MC_(record_align_mismatch_error) ( tid, mc, aligned_alloc_info->orig_alignment, False, "new[]/delete[]");
         }
         break;
      case AllocKindDeleteSizedAligned:
         mc = VG_(HT_lookup) ( MC_(malloc_list), (UWord)aligned_alloc_info->mem );
         if (mc && mc->szB != aligned_alloc_info->size) {
            MC_(record_size_mismatch_error) ( tid, mc, aligned_alloc_info->size, "new/delete");
         }
         if (mc && aligned_alloc_info->orig_alignment != mc->alignB) {
            MC_(record_align_mismatch_error) ( tid, mc, aligned_alloc_info->orig_alignment, False, "new/delete");
         }
         if (aligned_alloc_info->orig_alignment == 0 ||
             (aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be non-zero and a power of 2)" );
         }
         break;
      case AllocKindVecDeleteSizedAligned:
         mc = VG_(HT_lookup) ( MC_(malloc_list), (UWord)aligned_alloc_info->mem );
         if (mc && mc->szB != aligned_alloc_info->size) {
            MC_(record_size_mismatch_error) ( tid, mc, aligned_alloc_info->size, "new[]/delete[]" );
         }
         if (mc && aligned_alloc_info->orig_alignment != mc->alignB) {
            MC_(record_align_mismatch_error) ( tid, mc, aligned_alloc_info->orig_alignment, False, "new[]/delete[]");
         }
         if (aligned_alloc_info->orig_alignment == 0 ||
             (aligned_alloc_info->orig_alignment & (aligned_alloc_info->orig_alignment - 1)) != 0) {
            MC_(record_bad_alignment) ( tid, aligned_alloc_info->orig_alignment , 0U, " (should be non-zero and a power of 2)" );
         }
         break;
      default:
         tl_assert (False);
      }

      return True;
   }

      case VG_USERREQ__CREATE_MEMPOOL: {
         Addr pool      = (Addr)arg[1];
         UInt rzB       =       arg[2];
         Bool is_zeroed = (Bool)arg[3];
         UInt flags     =       arg[4];

         // The create_mempool function does not know these mempool flags,
         // pass as booleans.
         MC_(create_mempool) ( pool, rzB, is_zeroed,
                               (flags & VALGRIND_MEMPOOL_AUTO_FREE),
                               (flags & VALGRIND_MEMPOOL_METAPOOL) );
         return True;
      }

      case VG_USERREQ__DESTROY_MEMPOOL: {
         Addr pool      = (Addr)arg[1];

         MC_(destroy_mempool) ( pool );
         return True;
      }

      case VG_USERREQ__MEMPOOL_ALLOC: {
         Addr pool      = (Addr)arg[1];
         Addr addr      = (Addr)arg[2];
         UInt size      =       arg[3];

         MC_(mempool_alloc) ( tid, pool, addr, size );
         return True;
      }

      case VG_USERREQ__MEMPOOL_FREE: {
         Addr pool      = (Addr)arg[1];
         Addr addr      = (Addr)arg[2];

         MC_(mempool_free) ( pool, addr );
         return True;
      }

      case VG_USERREQ__MEMPOOL_TRIM: {
         Addr pool      = (Addr)arg[1];
         Addr addr      = (Addr)arg[2];
         UInt size      =       arg[3];

         MC_(mempool_trim) ( pool, addr, size );
         return True;
      }

      case VG_USERREQ__MOVE_MEMPOOL: {
         Addr poolA     = (Addr)arg[1];
         Addr poolB     = (Addr)arg[2];

         MC_(move_mempool) ( poolA, poolB );
         return True;
      }

      case VG_USERREQ__MEMPOOL_CHANGE: {
         Addr pool      = (Addr)arg[1];
         Addr addrA     = (Addr)arg[2];
         Addr addrB     = (Addr)arg[3];
         UInt size      =       arg[4];

         MC_(mempool_change) ( pool, addrA, addrB, size );
         return True;
      }

      case VG_USERREQ__MEMPOOL_EXISTS: {
         Addr pool      = (Addr)arg[1];

         *ret = (UWord) MC_(mempool_exists) ( pool );
	 return True;
      }

      case VG_USERREQ__GDB_MONITOR_COMMAND: {
         Bool handled = handle_gdb_monitor_command (tid, (HChar*)arg[1]);
         if (handled)
            *ret = 1;
         else
            *ret = 0;
         return handled;
      }

      case VG_USERREQ__DISABLE_ADDR_ERROR_REPORTING_IN_RANGE:
      case VG_USERREQ__ENABLE_ADDR_ERROR_REPORTING_IN_RANGE: {
         Bool addRange
            = arg[0] == VG_USERREQ__DISABLE_ADDR_ERROR_REPORTING_IN_RANGE;
         Bool ok
            = modify_ignore_ranges(addRange, arg[1], arg[2]);
         *ret = ok ? 1 : 0;
         return True;
      }

      default:
         VG_(message)(Vg_UserMsg,
                      "Warning: unknown memcheck client request code %llx\n",
                      (ULong)arg[0]);
         return False;
   }
   return True;
}


/*------------------------------------------------------------*/
/*--- Crude profiling machinery.                           ---*/
/*------------------------------------------------------------*/

// We track a number of interesting events (using PROF_EVENT)
// if MC_PROFILE_MEMORY is defined.

#ifdef MC_PROFILE_MEMORY

ULong  MC_(event_ctr)[MCPE_LAST];

/* Event counter names. Use the name of the function that increases the
   event counter. Drop any MC_() and mc_ prefices. */
static const HChar* MC_(event_ctr_name)[MCPE_LAST] = {
   [MCPE_LOADVN_SLOW] = "LOADVn_slow",
   [MCPE_LOADVN_SLOW_LOOP] = "LOADVn_slow_loop",
   [MCPE_STOREVN_SLOW] = "STOREVn_slow",
   [MCPE_STOREVN_SLOW_LOOP] = "STOREVn_slow(loop)",
   [MCPE_MAKE_ALIGNED_WORD32_UNDEFINED] = "make_aligned_word32_undefined",
   [MCPE_MAKE_ALIGNED_WORD32_UNDEFINED_SLOW] =
        "make_aligned_word32_undefined_slow",
   [MCPE_MAKE_ALIGNED_WORD64_UNDEFINED] = "make_aligned_word64_undefined",
   [MCPE_MAKE_ALIGNED_WORD64_UNDEFINED_SLOW] =
        "make_aligned_word64_undefined_slow",
   [MCPE_MAKE_ALIGNED_WORD32_NOACCESS] = "make_aligned_word32_noaccess",
   [MCPE_MAKE_ALIGNED_WORD32_NOACCESS_SLOW] =
         "make_aligned_word32_noaccess_slow",
   [MCPE_MAKE_ALIGNED_WORD64_NOACCESS] = "make_aligned_word64_noaccess",
   [MCPE_MAKE_ALIGNED_WORD64_NOACCESS_SLOW] =
        "make_aligned_word64_noaccess_slow",
   [MCPE_MAKE_MEM_NOACCESS] = "make_mem_noaccess",
   [MCPE_MAKE_MEM_UNDEFINED] = "make_mem_undefined",
   [MCPE_MAKE_MEM_UNDEFINED_W_OTAG] = "make_mem_undefined_w_otag",
   [MCPE_MAKE_MEM_DEFINED] = "make_mem_defined",
   [MCPE_CHEAP_SANITY_CHECK] = "cheap_sanity_check",
   [MCPE_EXPENSIVE_SANITY_CHECK] = "expensive_sanity_check",
   [MCPE_COPY_ADDRESS_RANGE_STATE] = "copy_address_range_state",
   [MCPE_COPY_ADDRESS_RANGE_STATE_LOOP1] = "copy_address_range_state(loop1)",
   [MCPE_COPY_ADDRESS_RANGE_STATE_LOOP2] = "copy_address_range_state(loop2)",
   [MCPE_CHECK_MEM_IS_NOACCESS] = "check_mem_is_noaccess",
   [MCPE_CHECK_MEM_IS_NOACCESS_LOOP] = "check_mem_is_noaccess(loop)",
   [MCPE_IS_MEM_ADDRESSABLE] = "is_mem_addressable",
   [MCPE_IS_MEM_ADDRESSABLE_LOOP] = "is_mem_addressable(loop)",
   [MCPE_IS_MEM_DEFINED] = "is_mem_defined",
   [MCPE_IS_MEM_DEFINED_LOOP] = "is_mem_defined(loop)",
   [MCPE_IS_MEM_DEFINED_COMPREHENSIVE] = "is_mem_defined_comprehensive",
   [MCPE_IS_MEM_DEFINED_COMPREHENSIVE_LOOP] =
        "is_mem_defined_comprehensive(loop)",
   [MCPE_IS_DEFINED_ASCIIZ] = "is_defined_asciiz",
   [MCPE_IS_DEFINED_ASCIIZ_LOOP] = "is_defined_asciiz(loop)",
   [MCPE_FIND_CHUNK_FOR_OLD] = "find_chunk_for_OLD",
   [MCPE_FIND_CHUNK_FOR_OLD_LOOP] = "find_chunk_for_OLD(loop)",
   [MCPE_SET_ADDRESS_RANGE_PERMS] = "set_address_range_perms",
   [MCPE_SET_ADDRESS_RANGE_PERMS_SINGLE_SECMAP] =
        "set_address_range_perms(single-secmap)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_STARTOF_SECMAP] =
        "set_address_range_perms(startof-secmap)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_MULTIPLE_SECMAPS] =
   "set_address_range_perms(multiple-secmaps)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_DIST_SM1] =
        "set_address_range_perms(dist-sm1)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_DIST_SM2] =
        "set_address_range_perms(dist-sm2)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_DIST_SM1_QUICK] =
        "set_address_range_perms(dist-sm1-quick)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_DIST_SM2_QUICK] =
        "set_address_range_perms(dist-sm2-quick)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_LOOP1A] = "set_address_range_perms(loop1a)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_LOOP1B] = "set_address_range_perms(loop1b)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_LOOP1C] = "set_address_range_perms(loop1c)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_LOOP8A] = "set_address_range_perms(loop8a)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_LOOP8B] = "set_address_range_perms(loop8b)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_LOOP64K] = "set_address_range_perms(loop64K)",
   [MCPE_SET_ADDRESS_RANGE_PERMS_LOOP64K_FREE_DIST_SM] =
        "set_address_range_perms(loop64K-free-dist-sm)",
   [MCPE_LOADV_128_OR_256_SLOW_LOOP] = "LOADV_128_or_256_slow(loop)",
   [MCPE_LOADV_128_OR_256]       = "LOADV_128_or_256",
   [MCPE_LOADV_128_OR_256_SLOW1] = "LOADV_128_or_256-slow1",
   [MCPE_LOADV_128_OR_256_SLOW2] = "LOADV_128_or_256-slow2",
   [MCPE_LOADV64]        = "LOADV64",
   [MCPE_LOADV64_SLOW1]  = "LOADV64-slow1",
   [MCPE_LOADV64_SLOW2]  = "LOADV64-slow2",
   [MCPE_STOREV64]       = "STOREV64",
   [MCPE_STOREV64_SLOW1] = "STOREV64-slow1",
   [MCPE_STOREV64_SLOW2] = "STOREV64-slow2",
   [MCPE_STOREV64_SLOW3] = "STOREV64-slow3",
   [MCPE_STOREV64_SLOW4] = "STOREV64-slow4",
   [MCPE_LOADV32]        = "LOADV32",
   [MCPE_LOADV32_SLOW1]  = "LOADV32-slow1",
   [MCPE_LOADV32_SLOW2]  = "LOADV32-slow2",
   [MCPE_STOREV32]       = "STOREV32",
   [MCPE_STOREV32_SLOW1] = "STOREV32-slow1",
   [MCPE_STOREV32_SLOW2] = "STOREV32-slow2",
   [MCPE_STOREV32_SLOW3] = "STOREV32-slow3",
   [MCPE_STOREV32_SLOW4] = "STOREV32-slow4",
   [MCPE_LOADV16]        = "LOADV16",
   [MCPE_LOADV16_SLOW1]  = "LOADV16-slow1",
   [MCPE_LOADV16_SLOW2]  = "LOADV16-slow2",
   [MCPE_STOREV16]       = "STOREV16",
   [MCPE_STOREV16_SLOW1] = "STOREV16-slow1",
   [MCPE_STOREV16_SLOW2] = "STOREV16-slow2",
   [MCPE_STOREV16_SLOW3] = "STOREV16-slow3",
   [MCPE_STOREV16_SLOW4] = "STOREV16-slow4",
   [MCPE_LOADV8]         = "LOADV8",
   [MCPE_LOADV8_SLOW1]   = "LOADV8-slow1",
   [MCPE_LOADV8_SLOW2]   = "LOADV8-slow2",
   [MCPE_STOREV8]        = "STOREV8",
   [MCPE_STOREV8_SLOW1]  = "STOREV8-slow1",
   [MCPE_STOREV8_SLOW2]  = "STOREV8-slow2",
   [MCPE_STOREV8_SLOW3]  = "STOREV8-slow3",
   [MCPE_STOREV8_SLOW4]  = "STOREV8-slow4",
   [MCPE_NEW_MEM_STACK_4]   = "new_mem_stack_4",
   [MCPE_NEW_MEM_STACK_8]   = "new_mem_stack_8",
   [MCPE_NEW_MEM_STACK_12]  = "new_mem_stack_12",
   [MCPE_NEW_MEM_STACK_16]  = "new_mem_stack_16",
   [MCPE_NEW_MEM_STACK_32]  = "new_mem_stack_32",
   [MCPE_NEW_MEM_STACK_112] = "new_mem_stack_112",
   [MCPE_NEW_MEM_STACK_128] = "new_mem_stack_128",
   [MCPE_NEW_MEM_STACK_144] = "new_mem_stack_144",
   [MCPE_NEW_MEM_STACK_160] = "new_mem_stack_160",
   [MCPE_DIE_MEM_STACK_4]   = "die_mem_stack_4",
   [MCPE_DIE_MEM_STACK_8]   = "die_mem_stack_8",
   [MCPE_DIE_MEM_STACK_12]  = "die_mem_stack_12",
   [MCPE_DIE_MEM_STACK_16]  = "die_mem_stack_16",
   [MCPE_DIE_MEM_STACK_32]  = "die_mem_stack_32",
   [MCPE_DIE_MEM_STACK_112] = "die_mem_stack_112",
   [MCPE_DIE_MEM_STACK_128] = "die_mem_stack_128",
   [MCPE_DIE_MEM_STACK_144] = "die_mem_stack_144",
   [MCPE_DIE_MEM_STACK_160] = "die_mem_stack_160",
   [MCPE_NEW_MEM_STACK]     = "new_mem_stack",
   [MCPE_DIE_MEM_STACK]     = "die_mem_stack",
   [MCPE_MAKE_STACK_UNINIT_W_O]      = "MAKE_STACK_UNINIT_w_o",
   [MCPE_MAKE_STACK_UNINIT_NO_O]     = "MAKE_STACK_UNINIT_no_o",
   [MCPE_MAKE_STACK_UNINIT_128_NO_O] = "MAKE_STACK_UNINIT_128_no_o",
   [MCPE_MAKE_STACK_UNINIT_128_NO_O_ALIGNED_16]
                                     = "MAKE_STACK_UNINIT_128_no_o_aligned_16",
   [MCPE_MAKE_STACK_UNINIT_128_NO_O_ALIGNED_8]
                                     = "MAKE_STACK_UNINIT_128_no_o_aligned_8",
   [MCPE_MAKE_STACK_UNINIT_128_NO_O_SLOWCASE]
                                     = "MAKE_STACK_UNINIT_128_no_o_slowcase",
};

static void init_prof_mem ( void )
{
   Int i, name_count = 0;

   for (i = 0; i < MCPE_LAST; i++) {
      MC_(event_ctr)[i] = 0;
      if (MC_(event_ctr_name)[i] != NULL)
         ++name_count;
   }

   /* Make sure every profiling event has a name */
   tl_assert(name_count == MCPE_LAST);
}

static void done_prof_mem ( void )
{
   Int  i, n;
   Bool spaced = False;
   for (i = n = 0; i < MCPE_LAST; i++) {
      if (!spaced && (n % 10) == 0) {
         VG_(printf)("\n");
         spaced = True;
      }
      if (MC_(event_ctr)[i] > 0) {
         spaced = False;
         ++n;
         VG_(printf)( "prof mem event %3d: %11llu   %s\n",
                      i, MC_(event_ctr)[i],
                      MC_(event_ctr_name)[i]);
      }
   }
}

#else

static void init_prof_mem ( void ) { }
static void done_prof_mem ( void ) { }

#endif


/*------------------------------------------------------------*/
/*--- Origin tracking stuff                                ---*/
/*------------------------------------------------------------*/

/*--------------------------------------------*/
/*--- Origin tracking: load handlers       ---*/
/*--------------------------------------------*/

static INLINE UInt merge_origins ( UInt or1, UInt or2 ) {
   return or1 > or2 ? or1 : or2;
}

UWord VG_REGPARM(1) MC_(helperc_b_load1)( Addr a ) {
   OCacheLine* line;
   UChar descr;
   UWord lineoff = oc_line_offset(a);
   UWord byteoff = a & 3; /* 0, 1, 2 or 3 */

   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff >= 0 && lineoff < OC_W32S_PER_LINE);
   }

   line = find_OCacheLine( a );

   descr = line->u.main.descr[lineoff];
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(descr < 0x10);
   }

   if (LIKELY(0 == (descr & (1 << byteoff))))  {
      return 0;
   } else {
      return line->u.main.w32[lineoff];
   }
}

UWord VG_REGPARM(1) MC_(helperc_b_load2)( Addr a ) {
   OCacheLine* line;
   UChar descr;
   UWord lineoff, byteoff;

   if (UNLIKELY(a & 1)) {
      /* Handle misaligned case, slowly. */
      UInt oLo   = (UInt)MC_(helperc_b_load1)( a + 0 );
      UInt oHi   = (UInt)MC_(helperc_b_load1)( a + 1 );
      return merge_origins(oLo, oHi);
   }

   lineoff = oc_line_offset(a);
   byteoff = a & 3; /* 0 or 2 */

   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff >= 0 && lineoff < OC_W32S_PER_LINE);
   }
   line = find_OCacheLine( a );

   descr = line->u.main.descr[lineoff];
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(descr < 0x10);
   }

   if (LIKELY(0 == (descr & (3 << byteoff)))) {
      return 0;
   } else {
      return line->u.main.w32[lineoff];
   }
}

UWord VG_REGPARM(1) MC_(helperc_b_load4)( Addr a ) {
   OCacheLine* line;
   UChar descr;
   UWord lineoff;

   if (UNLIKELY(a & 3)) {
      /* Handle misaligned case, slowly. */
      UInt oLo   = (UInt)MC_(helperc_b_load2)( a + 0 );
      UInt oHi   = (UInt)MC_(helperc_b_load2)( a + 2 );
      return merge_origins(oLo, oHi);
   }

   lineoff = oc_line_offset(a);
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff >= 0 && lineoff < OC_W32S_PER_LINE);
   }

   line = find_OCacheLine( a );

   descr = line->u.main.descr[lineoff];
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(descr < 0x10);
   }

   if (LIKELY(0 == descr)) {
      return 0;
   } else {
      return line->u.main.w32[lineoff];
   }
}

UWord VG_REGPARM(1) MC_(helperc_b_load8)( Addr a ) {
   OCacheLine* line;
   UChar descrLo, descrHi, descr;
   UWord lineoff;

   if (UNLIKELY(a & 7)) {
      /* Handle misaligned case, slowly. */
      UInt oLo   = (UInt)MC_(helperc_b_load4)( a + 0 );
      UInt oHi   = (UInt)MC_(helperc_b_load4)( a + 4 );
      return merge_origins(oLo, oHi);
   }

   lineoff = oc_line_offset(a);
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff == (lineoff & 6)); /*0,2,4,6*//*since 8-aligned*/
   }

   line = find_OCacheLine( a );

   descrLo = line->u.main.descr[lineoff + 0];
   descrHi = line->u.main.descr[lineoff + 1];
   descr   = descrLo | descrHi;
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(descr < 0x10);
   }

   if (LIKELY(0 == descr)) {
      return 0; /* both 32-bit chunks are defined */
   } else {
      UInt oLo = descrLo == 0 ? 0 : line->u.main.w32[lineoff + 0];
      UInt oHi = descrHi == 0 ? 0 : line->u.main.w32[lineoff + 1];
      return merge_origins(oLo, oHi);
   }
}

UWord VG_REGPARM(1) MC_(helperc_b_load16)( Addr a ) {
   UInt oLo   = (UInt)MC_(helperc_b_load8)( a + 0 );
   UInt oHi   = (UInt)MC_(helperc_b_load8)( a + 8 );
   UInt oBoth = merge_origins(oLo, oHi);
   return (UWord)oBoth;
}

UWord VG_REGPARM(1) MC_(helperc_b_load32)( Addr a ) {
   UInt oQ0   = (UInt)MC_(helperc_b_load8)( a + 0 );
   UInt oQ1   = (UInt)MC_(helperc_b_load8)( a + 8 );
   UInt oQ2   = (UInt)MC_(helperc_b_load8)( a + 16 );
   UInt oQ3   = (UInt)MC_(helperc_b_load8)( a + 24 );
   UInt oAll  = merge_origins(merge_origins(oQ0, oQ1),
                              merge_origins(oQ2, oQ3));
   return (UWord)oAll;
}


/*--------------------------------------------*/
/*--- Origin tracking: store handlers      ---*/
/*--------------------------------------------*/

void VG_REGPARM(2) MC_(helperc_b_store1)( Addr a, UWord d32 ) {
   OCacheLine* line;
   UWord lineoff = oc_line_offset(a);
   UWord byteoff = a & 3; /* 0, 1, 2 or 3 */

   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff >= 0 && lineoff < OC_W32S_PER_LINE);
   }

   line = find_OCacheLine( a );

#if OC_PRECISION_STORE
   if (LIKELY(d32 == 0)) {
      // The byte is defined.  Just mark it as so in the descr and leave the w32
      // unchanged.  This may make the descr become zero, so the line no longer
      // contains useful info, but that's OK.  No loss of information.
      line->u.main.descr[lineoff] &= ~(1 << byteoff);
   } else if (d32 == line->u.main.w32[lineoff]) {
      // At least one of the four bytes in the w32 is undefined with the same
      // origin.  Just extend the mask.  No loss of information.
      line->u.main.descr[lineoff] |= (1 << byteoff);
   } else {
      // Here, we have a conflict: at least one byte in the group is undefined
      // but with some other origin.  We can't represent both origins, so we
      // forget about the previous origin and install this one instead.
      line->u.main.descr[lineoff] = (1 << byteoff);
      line->u.main.w32[lineoff] = d32;
   }
#else
   if (d32 == 0) {
      line->u.main.descr[lineoff] &= ~(1 << byteoff);
   } else {
      line->u.main.descr[lineoff] |= (1 << byteoff);
      line->u.main.w32[lineoff] = d32;
   }
#endif
}

void VG_REGPARM(2) MC_(helperc_b_store2)( Addr a, UWord d32 ) {
   OCacheLine* line;
   UWord lineoff, byteoff;

   if (UNLIKELY(a & 1)) {
      /* Handle misaligned case, slowly. */
      MC_(helperc_b_store1)( a + 0, d32 );
      MC_(helperc_b_store1)( a + 1, d32 );
      return;
   }

   lineoff = oc_line_offset(a);
   byteoff = a & 3; /* 0 or 2 */

   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff >= 0 && lineoff < OC_W32S_PER_LINE);
   }

   line = find_OCacheLine( a );

#if OC_PRECISION_STORE
   // Same logic as in the store1 case above.
   if (LIKELY(d32 == 0)) {
      line->u.main.descr[lineoff] &= ~(3 << byteoff);
   } else if (d32 == line->u.main.w32[lineoff]) {
      line->u.main.descr[lineoff] |= (3 << byteoff);
      line->u.main.w32[lineoff] = d32;
   } else {
      line->u.main.descr[lineoff] = (3 << byteoff);
      line->u.main.w32[lineoff] = d32;
   }
#else
   if (d32 == 0) {
      line->u.main.descr[lineoff] &= ~(3 << byteoff);
   } else {
      line->u.main.descr[lineoff] |= (3 << byteoff);
      line->u.main.w32[lineoff] = d32;
   }
#endif
}

void VG_REGPARM(2) MC_(helperc_b_store4)( Addr a, UWord d32 ) {
   OCacheLine* line;
   UWord lineoff;

   if (UNLIKELY(a & 3)) {
      /* Handle misaligned case, slowly. */
      MC_(helperc_b_store2)( a + 0, d32 );
      MC_(helperc_b_store2)( a + 2, d32 );
      return;
   }

   lineoff = oc_line_offset(a);
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff >= 0 && lineoff < OC_W32S_PER_LINE);
   }

   line = find_OCacheLine( a );

   if (d32 == 0) {
      line->u.main.descr[lineoff] = 0;
   } else {
      line->u.main.descr[lineoff] = 0xF;
      line->u.main.w32[lineoff] = d32;
   }
}

void VG_REGPARM(2) MC_(helperc_b_store8)( Addr a, UWord d32 ) {
   STATIC_ASSERT(OC_W32S_PER_LINE == 8);
   OCacheLine* line;
   UWord lineoff;

   if (UNLIKELY(a & 7)) {
      /* Handle misaligned case, slowly. */
      MC_(helperc_b_store4)( a + 0, d32 );
      MC_(helperc_b_store4)( a + 4, d32 );
      return;
   }

   lineoff = oc_line_offset(a);
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff == (lineoff & 6)); /*0,2,4,6*//*since 8-aligned*/
   }

   line = find_OCacheLine( a );

   if (d32 == 0) {
      line->u.main.descr[lineoff + 0] = 0;
      line->u.main.descr[lineoff + 1] = 0;
   } else {
      line->u.main.descr[lineoff + 0] = 0xF;
      line->u.main.descr[lineoff + 1] = 0xF;
      line->u.main.w32[lineoff + 0] = d32;
      line->u.main.w32[lineoff + 1] = d32;
   }
}

void VG_REGPARM(2) MC_(helperc_b_store16)( Addr a, UWord d32 ) {
   STATIC_ASSERT(OC_W32S_PER_LINE == 8);
   OCacheLine* line;
   UWord lineoff;

   if (UNLIKELY(a & 15)) {
      /* Handle misaligned case, slowly. */
      MC_(helperc_b_store8)( a + 0, d32 );
      MC_(helperc_b_store8)( a + 8, d32 );
      return;
   }

   lineoff = oc_line_offset(a);
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff == (lineoff & 4)); /*0,4*//*since 16-aligned*/
   }

   line = find_OCacheLine( a );

   if (d32 == 0) {
      line->u.main.descr[lineoff + 0] = 0;
      line->u.main.descr[lineoff + 1] = 0;
      line->u.main.descr[lineoff + 2] = 0;
      line->u.main.descr[lineoff + 3] = 0;
   } else {
      line->u.main.descr[lineoff + 0] = 0xF;
      line->u.main.descr[lineoff + 1] = 0xF;
      line->u.main.descr[lineoff + 2] = 0xF;
      line->u.main.descr[lineoff + 3] = 0xF;
      line->u.main.w32[lineoff + 0] = d32;
      line->u.main.w32[lineoff + 1] = d32;
      line->u.main.w32[lineoff + 2] = d32;
      line->u.main.w32[lineoff + 3] = d32;
   }
}

void VG_REGPARM(2) MC_(helperc_b_store32)( Addr a, UWord d32 ) {
   STATIC_ASSERT(OC_W32S_PER_LINE == 8);
   OCacheLine* line;
   UWord lineoff;

   if (UNLIKELY(a & 31)) {
      /* Handle misaligned case, slowly. */
      MC_(helperc_b_store16)( a + 0, d32 );
      MC_(helperc_b_store16)( a + 16, d32 );
      return;
   }

   lineoff = oc_line_offset(a);
   if (OC_ENABLE_ASSERTIONS) {
      tl_assert(lineoff == 0);
   }

   line = find_OCacheLine( a );

   if (d32 == 0) {
      line->u.main.descr[0] = 0;
      line->u.main.descr[1] = 0;
      line->u.main.descr[2] = 0;
      line->u.main.descr[3] = 0;
      line->u.main.descr[4] = 0;
      line->u.main.descr[5] = 0;
      line->u.main.descr[6] = 0;
      line->u.main.descr[7] = 0;
   } else {
      line->u.main.descr[0] = 0xF;
      line->u.main.descr[1] = 0xF;
      line->u.main.descr[2] = 0xF;
      line->u.main.descr[3] = 0xF;
      line->u.main.descr[4] = 0xF;
      line->u.main.descr[5] = 0xF;
      line->u.main.descr[6] = 0xF;
      line->u.main.descr[7] = 0xF;
      line->u.main.w32[0] = d32;
      line->u.main.w32[1] = d32;
      line->u.main.w32[2] = d32;
      line->u.main.w32[3] = d32;
      line->u.main.w32[4] = d32;
      line->u.main.w32[5] = d32;
      line->u.main.w32[6] = d32;
      line->u.main.w32[7] = d32;
   }
}


/*--------------------------------------------*/
/*--- Origin tracking: sarp handlers       ---*/
/*--------------------------------------------*/

// We may get asked to do very large SARPs (bug 446103), hence it is important
// to process 32-byte chunks at a time when possible.

__attribute__((noinline))
static void ocache_sarp_Set_Origins ( Addr a, UWord len, UInt otag ) {
   if ((a & 1) && len >= 1) {
      MC_(helperc_b_store1)( a, otag );
      a++;
      len--;
   }
   if ((a & 2) && len >= 2) {
      MC_(helperc_b_store2)( a, otag );
      a += 2;
      len -= 2;
   }
   if ((a & 4) && len >= 4) {
      MC_(helperc_b_store4)( a, otag );
      a += 4;
      len -= 4;
   }
   if ((a & 8) && len >= 8) {
      MC_(helperc_b_store8)( a, otag );
      a += 8;
      len -= 8;
   }
   if ((a & 16) && len >= 16) {
      MC_(helperc_b_store16)( a, otag );
      a += 16;
      len -= 16;
   }
   if (len >= 32) {
      tl_assert(0 == (a & 31));
      while (len >= 32) {
         MC_(helperc_b_store32)( a, otag );
         a += 32;
         len -= 32;
      }
   }
   if (len >= 16) {
      MC_(helperc_b_store16)( a, otag );
      a += 16;
      len -= 16;
   }
   if (len >= 8) {
      MC_(helperc_b_store8)( a, otag );
      a += 8;
      len -= 8;
   }
   if (len >= 4) {
      MC_(helperc_b_store4)( a, otag );
      a += 4;
      len -= 4;
   }
   if (len >= 2) {
      MC_(helperc_b_store2)( a, otag );
      a += 2;
      len -= 2;
   }
   if (len >= 1) {
      MC_(helperc_b_store1)( a, otag );
      //a++;
      len--;
   }
   tl_assert(len == 0);
}

__attribute__((noinline))
static void ocache_sarp_Clear_Origins ( Addr a, UWord len ) {
   if ((a & 1) && len >= 1) {
      MC_(helperc_b_store1)( a, 0 );
      a++;
      len--;
   }
   if ((a & 2) && len >= 2) {
      MC_(helperc_b_store2)( a, 0 );
      a += 2;
      len -= 2;
   }
   if ((a & 4) && len >= 4) {
      MC_(helperc_b_store4)( a, 0 );
      a += 4;
      len -= 4;
   }
   if ((a & 8) && len >= 8) {
      MC_(helperc_b_store8)( a, 0 );
      a += 8;
      len -= 8;
   }
   if ((a & 16) && len >= 16) {
      MC_(helperc_b_store16)( a, 0 );
      a += 16;
      len -= 16;
   }
   if (len >= 32) {
      tl_assert(0 == (a & 31));
      while (len >= 32) {
         MC_(helperc_b_store32)( a, 0 );
         a += 32;
         len -= 32;
      }
   }
   if (len >= 16) {
      MC_(helperc_b_store16)( a, 0 );
      a += 16;
      len -= 16;
   }
   if (len >= 8) {
      MC_(helperc_b_store8)( a, 0 );
      a += 8;
      len -= 8;
   }
   if (len >= 4) {
      MC_(helperc_b_store4)( a, 0 );
      a += 4;
      len -= 4;
   }
   if (len >= 2) {
      MC_(helperc_b_store2)( a, 0 );
      a += 2;
      len -= 2;
   }
   if (len >= 1) {
      MC_(helperc_b_store1)( a, 0 );
      //a++;
      len--;
   }
   tl_assert(len == 0);
}


/*------------------------------------------------------------*/
/*--- Setup and finalisation                               ---*/
/*------------------------------------------------------------*/

static void mc_post_clo_init ( void )
{
   /* If we've been asked to emit XML, mash around various other
      options so as to constrain the output somewhat. */
   if (VG_(clo_xml)) {
      /* Extract as much info as possible from the leak checker. */
      MC_(clo_leak_check) = LC_Full;
   }

   if (MC_(clo_freelist_big_blocks) >= MC_(clo_freelist_vol)
       && VG_(clo_verbosity) == 1 && !VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg,
                   "Warning: --freelist-big-blocks value %lld has no effect\n"
                   "as it is >= to --freelist-vol value %lld\n",
                   MC_(clo_freelist_big_blocks),
                   MC_(clo_freelist_vol));
   }

   if (MC_(clo_workaround_gcc296_bugs)
       && VG_(clo_verbosity) == 1 && !VG_(clo_xml)) {
      VG_(umsg)(
         "Warning: --workaround-gcc296-bugs=yes is deprecated.\n"
         "Warning: Instead use: --ignore-range-below-sp=1024-1\n"
         "\n"
      );
   }

   tl_assert( MC_(clo_mc_level) >= 1 && MC_(clo_mc_level) <= 3 );

   if (MC_(clo_mc_level) == 3) {
      /* We're doing origin tracking. */
#     ifdef PERF_FAST_STACK
      VG_(track_new_mem_stack_4_w_ECU)   ( mc_new_mem_stack_4_w_ECU   );
      VG_(track_new_mem_stack_8_w_ECU)   ( mc_new_mem_stack_8_w_ECU   );
      VG_(track_new_mem_stack_12_w_ECU)  ( mc_new_mem_stack_12_w_ECU  );
      VG_(track_new_mem_stack_16_w_ECU)  ( mc_new_mem_stack_16_w_ECU  );
      VG_(track_new_mem_stack_32_w_ECU)  ( mc_new_mem_stack_32_w_ECU  );
      VG_(track_new_mem_stack_112_w_ECU) ( mc_new_mem_stack_112_w_ECU );
      VG_(track_new_mem_stack_128_w_ECU) ( mc_new_mem_stack_128_w_ECU );
      VG_(track_new_mem_stack_144_w_ECU) ( mc_new_mem_stack_144_w_ECU );
      VG_(track_new_mem_stack_160_w_ECU) ( mc_new_mem_stack_160_w_ECU );
#     endif
      VG_(track_new_mem_stack_w_ECU)     ( mc_new_mem_stack_w_ECU     );
      VG_(track_new_mem_stack_signal)    ( mc_new_mem_w_tid_make_ECU );
   } else {
      /* Not doing origin tracking */
#     ifdef PERF_FAST_STACK
      VG_(track_new_mem_stack_4)   ( mc_new_mem_stack_4   );
      VG_(track_new_mem_stack_8)   ( mc_new_mem_stack_8   );
      VG_(track_new_mem_stack_12)  ( mc_new_mem_stack_12  );
      VG_(track_new_mem_stack_16)  ( mc_new_mem_stack_16  );
      VG_(track_new_mem_stack_32)  ( mc_new_mem_stack_32  );
      VG_(track_new_mem_stack_112) ( mc_new_mem_stack_112 );
      VG_(track_new_mem_stack_128) ( mc_new_mem_stack_128 );
      VG_(track_new_mem_stack_144) ( mc_new_mem_stack_144 );
      VG_(track_new_mem_stack_160) ( mc_new_mem_stack_160 );
#     endif
      VG_(track_new_mem_stack)     ( mc_new_mem_stack     );
      VG_(track_new_mem_stack_signal) ( mc_new_mem_w_tid_no_ECU );
   }

   // We assume that brk()/sbrk() does not initialise new memory.  Is this
   // accurate?  John Reiser says:
   //
   //   0) sbrk() can *decrease* process address space.  No zero fill is done
   //   for a decrease, not even the fragment on the high end of the last page
   //   that is beyond the new highest address.  For maximum safety and
   //   portability, then the bytes in the last page that reside above [the
   //   new] sbrk(0) should be considered to be uninitialized, but in practice
   //   it is exceedingly likely that they will retain their previous
   //   contents.
   //
   //   1) If an increase is large enough to require new whole pages, then
   //   those new whole pages (like all new pages) are zero-filled by the
   //   operating system.  So if sbrk(0) already is page aligned, then
   //   sbrk(PAGE_SIZE) *does* zero-fill the new memory.
   //
   //   2) Any increase that lies within an existing allocated page is not
   //   changed.  So if (x = sbrk(0)) is not page aligned, then
   //   sbrk(PAGE_SIZE) yields ((PAGE_SIZE -1) & -x) bytes which keep their
   //   existing contents, and an additional PAGE_SIZE bytes which are zeroed.
   //   ((PAGE_SIZE -1) & x) of them are "covered" by the sbrk(), and the rest
   //   of them come along for the ride because the operating system deals
   //   only in whole pages.  Again, for maximum safety and portability, then
   //   anything that lives above [the new] sbrk(0) should be considered
   //   uninitialized, but in practice will retain previous contents [zero in
   //   this case.]"
   //
   // In short:
   //
   //   A key property of sbrk/brk is that new whole pages that are supplied
   //   by the operating system *do* get initialized to zero.
   //
   // As for the portability of all this:
   //
   //   sbrk and brk are not POSIX.  However, any system that is a derivative
   //   of *nix has sbrk and brk because there are too many software (such as
   //   the Bourne shell) which rely on the traditional memory map (.text,
   //   .data+.bss, stack) and the existence of sbrk/brk.
   //
   // So we should arguably observe all this.  However:
   // - The current inaccuracy has caused maybe one complaint in seven years(?)
   // - Relying on the zeroed-ness of whole brk'd pages is pretty grotty... I
   //   doubt most programmers know the above information.
   // So I'm not terribly unhappy with marking it as undefined. --njn.
   //
   // [More:  I think most of what John said only applies to sbrk().  It seems
   // that brk() always deals in whole pages.  And since this event deals
   // directly with brk(), not with sbrk(), perhaps it would be reasonable to
   // just mark all memory it allocates as defined.]
   //
#  if !defined(VGO_solaris)
   if (MC_(clo_mc_level) == 3)
      VG_(track_new_mem_brk)         ( mc_new_mem_w_tid_make_ECU );
   else
      VG_(track_new_mem_brk)         ( mc_new_mem_w_tid_no_ECU );
#  else
   // On Solaris, brk memory has to be marked as defined, otherwise we get
   // many false positives.
   VG_(track_new_mem_brk)         ( make_mem_defined_w_tid );
#  endif

   /* This origin tracking cache is huge (~100M), so only initialise
      if we need it. */
   if (MC_(clo_mc_level) >= 3) {
      init_OCache();
      tl_assert(ocacheL1 != NULL);
      for (UInt i = 0; i < 4096; i++ ) {
         tl_assert(ocachesL2[i] != NULL);
      }
   } else {
      tl_assert(ocacheL1 == NULL);
      for (UInt i = 0; i < 4096; i++ ) {
         tl_assert(ocachesL2[i] == NULL);
      }
   }

   MC_(chunk_poolalloc) = VG_(newPA)
      (sizeof(MC_Chunk) + MC_(n_where_pointers)() * sizeof(ExeContext*),
       1000,
       VG_(malloc),
       "mc.cMC.1 (MC_Chunk pools)",
       VG_(free));

   /* Do not check definedness of guest state if --undef-value-errors=no */
   if (MC_(clo_mc_level) >= 2)
      VG_(track_pre_reg_read) ( mc_pre_reg_read );

   if (VG_(clo_xtree_memory) == Vg_XTMemory_Full) {
      if (MC_(clo_keep_stacktraces) == KS_none
          || MC_(clo_keep_stacktraces) == KS_free)
         VG_(fmsg_bad_option)("--keep-stacktraces",
                              "To use --xtree-memory=full, you must"
                              " keep at least the alloc stacktrace\n");
      // Activate full xtree memory profiling.
      VG_(XTMemory_Full_init)(VG_(XT_filter_1top_and_maybe_below_main));
   }

}

static void print_SM_info(const HChar* type, Int n_SMs)
{
   VG_(message)(Vg_DebugMsg,
      " memcheck: SMs: %s = %d (%luk, %luM)\n",
      type,
      n_SMs,
      n_SMs * sizeof(SecMap) / 1024UL,
      n_SMs * sizeof(SecMap) / (1024 * 1024UL) );
}

static void mc_print_stats (void)
{
   SizeT max_secVBit_szB, max_SMs_szB, max_shmem_szB;

   VG_(message)(Vg_DebugMsg, " memcheck: freelist: vol %lld length %lld\n",
                VG_(free_queue_volume), VG_(free_queue_length));
   VG_(message)(Vg_DebugMsg,
      " memcheck: sanity checks: %d cheap, %d expensive\n",
      n_sanity_cheap, n_sanity_expensive );
   VG_(message)(Vg_DebugMsg,
      " memcheck: auxmaps: %llu auxmap entries (%lluk, %lluM) in use\n",
      n_auxmap_L2_nodes,
      n_auxmap_L2_nodes * 64,
      n_auxmap_L2_nodes / 16 );
   VG_(message)(Vg_DebugMsg,
      " memcheck: auxmaps_L1: %llu searches, %llu cmps, ratio %llu:10\n",
      n_auxmap_L1_searches, n_auxmap_L1_cmps,
      (10ULL * n_auxmap_L1_cmps)
         / (n_auxmap_L1_searches ? n_auxmap_L1_searches : 1)
   );
   VG_(message)(Vg_DebugMsg,
      " memcheck: auxmaps_L2: %llu searches, %llu nodes\n",
      n_auxmap_L2_searches, n_auxmap_L2_nodes
   );

   print_SM_info("n_issued     ", n_issued_SMs);
   print_SM_info("n_deissued   ", n_deissued_SMs);
   print_SM_info("max_noaccess ", max_noaccess_SMs);
   print_SM_info("max_undefined", max_undefined_SMs);
   print_SM_info("max_defined  ", max_defined_SMs);
   print_SM_info("max_non_DSM  ", max_non_DSM_SMs);

   // Three DSMs, plus the non-DSM ones
   max_SMs_szB = (3 + max_non_DSM_SMs) * sizeof(SecMap);
   // The 3*sizeof(Word) bytes is the AVL node metadata size.
   // The VG_ROUNDUP is because the OSet pool allocator will/must align
   // the elements on pointer size.
   // Note that the pool allocator has some additional small overhead
   // which is not counted in the below.
   // Hardwiring this logic sucks, but I don't see how else to do it.
   max_secVBit_szB = max_secVBit_nodes *
         (3*sizeof(Word) + VG_ROUNDUP(sizeof(SecVBitNode), sizeof(void*)));
   max_shmem_szB   = sizeof(primary_map) + max_SMs_szB + max_secVBit_szB;

   VG_(message)(Vg_DebugMsg,
      " memcheck: max sec V bit nodes:    %d (%luk, %luM)\n",
      max_secVBit_nodes, max_secVBit_szB / 1024,
                         max_secVBit_szB / (1024 * 1024));
   VG_(message)(Vg_DebugMsg,
      " memcheck: set_sec_vbits8 calls: %llu (new: %llu, updates: %llu)\n",
      sec_vbits_new_nodes + sec_vbits_updates,
      sec_vbits_new_nodes, sec_vbits_updates );
   VG_(message)(Vg_DebugMsg,
      " memcheck: max shadow mem size:   %luk, %luM\n",
      max_shmem_szB / 1024, max_shmem_szB / (1024 * 1024));

   if (MC_(clo_mc_level) >= 3) {
      VG_(message)(Vg_DebugMsg,
                   " ocacheL1: %'14lu refs   %'14lu misses (%'lu lossage)\n",
                   stats_ocacheL1_find,
                   stats_ocacheL1_misses,
                   stats_ocacheL1_lossage );
      VG_(message)(Vg_DebugMsg,
                   " ocacheL1: %'14lu at 0   %'14lu at 1\n",
                   stats_ocacheL1_find - stats_ocacheL1_misses
                      - stats_ocacheL1_found_at_1
                      - stats_ocacheL1_found_at_N,
                   stats_ocacheL1_found_at_1 );
      VG_(message)(Vg_DebugMsg,
                   " ocacheL1: %'14lu at 2+  %'14lu move-fwds\n",
                   stats_ocacheL1_found_at_N,
                   stats_ocacheL1_movefwds );
      VG_(message)(Vg_DebugMsg,
                   " ocacheL1: %'14lu sizeB  %'14d useful\n",
                   (SizeT)sizeof(OCache),
                   4 * OC_W32S_PER_LINE * OC_LINES_PER_SET * OC_N_SETS );
      VG_(message)(Vg_DebugMsg,
                   " ocacheL2: %'14lu finds  %'14lu misses\n",
                   stats__ocacheL2_finds,
                   stats__ocacheL2_misses );
      VG_(message)(Vg_DebugMsg,
                   " ocacheL2: %'14lu adds   %'14lu dels\n",
                   stats__ocacheL2_adds,
                   stats__ocacheL2_dels );
      VG_(message)(Vg_DebugMsg,
                   " ocacheL2:    %'9lu max nodes %'9lu curr nodes\n",
                   stats__ocacheL2_n_nodes_max,
                   stats__ocacheL2_n_nodes );
      VG_(message)(Vg_DebugMsg,
                   " niacache: %'12lu refs   %'12lu misses\n",
                   stats__nia_cache_queries, stats__nia_cache_misses);
   } else {
      tl_assert(ocacheL1 == NULL);
      for (UInt i = 0; i < 4096; i++ ) {
         tl_assert(ocachesL2[1] == NULL);
      }
   }
}


static void mc_fini ( Int exitcode )
{
   MC_(xtmemory_report) (VG_(clo_xtree_memory_file), True);
   MC_(print_malloc_stats)();

   if (MC_(clo_leak_check) != LC_Off) {
      LeakCheckParams lcp;
      HChar* xt_filename = NULL;
      lcp.mode = MC_(clo_leak_check);
      lcp.show_leak_kinds = MC_(clo_show_leak_kinds);
      lcp.heuristics = MC_(clo_leak_check_heuristics);
      lcp.errors_for_leak_kinds = MC_(clo_error_for_leak_kinds);
      lcp.deltamode = LCD_Any;
      lcp.max_loss_records_output = 999999999;
      lcp.requested_by_monitor_command = False;
      if (MC_(clo_xtree_leak)) {
         xt_filename = VG_(expand_file_name)("--xtree-leak-file",
                                             MC_(clo_xtree_leak_file));
         lcp.xt_filename = xt_filename;
         lcp.mode = LC_Full;
         lcp.show_leak_kinds = MC_(all_Reachedness)();
      }
      else
         lcp.xt_filename = NULL;
      MC_(detect_memory_leaks)(1/*bogus ThreadId*/, &lcp);
      if (MC_(clo_xtree_leak))
         VG_(free)(xt_filename);
   } else {
      if (VG_(clo_verbosity) == 1 && !VG_(clo_xml)) {
         VG_(umsg)(
            "For a detailed leak analysis, rerun with: --leak-check=full\n"
            "\n"
         );
      }
   }

   if (MC_(any_value_errors) && !VG_(clo_xml) && VG_(clo_verbosity) >= 1
       && MC_(clo_mc_level) == 2) {
      VG_(message)(Vg_UserMsg,
                   "Use --track-origins=yes to see where "
                   "uninitialised values come from\n");
   }

   /* Print a warning if any client-request generated ignore-ranges
      still exist.  It would be reasonable to expect that a properly
      written program would remove any such ranges before exiting, and
      since they are a bit on the dangerous side, let's comment.  By
      contrast ranges which are specified on the command line normally
      pertain to hardware mapped into the address space, and so we
      can't expect the client to have got rid of them. */
   if (gIgnoredAddressRanges) {
      UInt i, nBad = 0;
      for (i = 0; i < VG_(sizeRangeMap)(gIgnoredAddressRanges); i++) {
         UWord val     = IAR_INVALID;
         UWord key_min = ~(UWord)0;
         UWord key_max = (UWord)0;
         VG_(indexRangeMap)( &key_min, &key_max, &val,
                             gIgnoredAddressRanges, i );
         if (val != IAR_ClientReq)
           continue;
         /* Print the offending range.  Also, if it is the first,
            print a banner before it. */
         nBad++;
         if (nBad == 1) {
            VG_(umsg)(
              "WARNING: exiting program has the following client-requested\n"
              "WARNING: address error disablement range(s) still in force,\n"
              "WARNING: "
                 "possibly as a result of some mistake in the use of the\n"
              "WARNING: "
                 "VALGRIND_{DISABLE,ENABLE}_ERROR_REPORTING_IN_RANGE macros.\n"
            );
         }
         VG_(umsg)("   [%u]  0x%016lx-0x%016lx  %s\n",
                   i, key_min, key_max, showIARKind(val));
      }
   }

   done_prof_mem();

   if (VG_(clo_stats))
      mc_print_stats();

   if (0) {
      VG_(message)(Vg_DebugMsg,
        "------ Valgrind's client block stats follow ---------------\n" );
      show_client_block_stats();
   }
}

/* mark the given addr/len unaddressable for watchpoint implementation
   The PointKind will be handled at access time */
static Bool mc_mark_unaddressable_for_watchpoint (PointKind kind, Bool insert,
                                                  Addr addr, SizeT len)
{
   /* GDBTD this is somewhat fishy. We might rather have to save the previous
      accessibility and definedness in gdbserver so as to allow restoring it
      properly. Currently, we assume that the user only watches things
      which are properly addressable and defined */
   if (insert)
      MC_(make_mem_noaccess) (addr, len);
   else
      MC_(make_mem_defined)  (addr, len);
   return True;
}

static void mc_pre_clo_init(void)
{
   VG_(details_name)            ("Memcheck");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a memory error detector");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2022, and GNU GPL'd, by Julian Seward et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 640 );

   VG_(basic_tool_funcs)          (mc_post_clo_init,
                                   MC_(instrument),
                                   mc_fini);

   VG_(needs_final_IR_tidy_pass)  ( MC_(final_tidy) );


   VG_(needs_core_errors)         ();
   VG_(needs_tool_errors)         (MC_(eq_Error),
                                   MC_(before_pp_Error),
                                   MC_(pp_Error),
                                   True,/*show TIDs for errors*/
                                   MC_(update_Error_extra),
                                   MC_(is_recognised_suppression),
                                   MC_(read_extra_suppression_info),
                                   MC_(error_matches_suppression),
                                   MC_(get_error_name),
                                   MC_(get_extra_suppression_info),
                                   MC_(print_extra_suppression_use),
                                   MC_(update_extra_suppression_use));
   VG_(needs_libc_freeres)        ();
   VG_(needs_cxx_freeres)         ();
   VG_(needs_command_line_options)(mc_process_cmd_line_options,
                                   mc_print_usage,
                                   mc_print_debug_usage);
   VG_(needs_client_requests)     (mc_handle_client_request);
   VG_(needs_sanity_checks)       (mc_cheap_sanity_check,
                                   mc_expensive_sanity_check);
   VG_(needs_print_stats)         (mc_print_stats);
   VG_(needs_info_location)       (MC_(pp_describe_addr));
   VG_(needs_malloc_replacement)  (MC_(malloc),
                                   MC_(__builtin_new),
                                   MC_(__builtin_new_aligned),
                                   MC_(__builtin_vec_new),
                                   MC_(__builtin_vec_new_aligned),
                                   MC_(memalign),
                                   MC_(calloc),
                                   MC_(free),
                                   MC_(__builtin_delete),
                                   MC_(__builtin_delete_aligned),
                                   MC_(__builtin_vec_delete),
                                   MC_(__builtin_vec_delete_aligned),
                                   MC_(realloc),
                                   MC_(malloc_usable_size),
                                   MC_MALLOC_DEFAULT_REDZONE_SZB );
   MC_(Malloc_Redzone_SzB) = VG_(malloc_effective_client_redzone_size)();

   VG_(needs_xml_output)          ();

   VG_(track_new_mem_startup)     ( mc_new_mem_startup );

   // Handling of mmap and mprotect isn't simple (well, it is simple,
   // but the justification isn't.)  See comments above, just prior to
   // mc_new_mem_mmap.
   VG_(track_new_mem_mmap)        ( mc_new_mem_mmap );
   VG_(track_change_mem_mprotect) ( mc_new_mem_mprotect );

   VG_(track_copy_mem_remap)      ( MC_(copy_address_range_state) );

   VG_(track_die_mem_stack_signal)( MC_(make_mem_noaccess) );
   VG_(track_die_mem_brk)         ( MC_(make_mem_noaccess) );
   VG_(track_die_mem_munmap)      ( MC_(make_mem_noaccess) );

   /* Defer the specification of the new_mem_stack functions to the
      post_clo_init function, since we need to first parse the command
      line before deciding which set to use. */

#  ifdef PERF_FAST_STACK
   VG_(track_die_mem_stack_4)     ( mc_die_mem_stack_4   );
   VG_(track_die_mem_stack_8)     ( mc_die_mem_stack_8   );
   VG_(track_die_mem_stack_12)    ( mc_die_mem_stack_12  );
   VG_(track_die_mem_stack_16)    ( mc_die_mem_stack_16  );
   VG_(track_die_mem_stack_32)    ( mc_die_mem_stack_32  );
   VG_(track_die_mem_stack_112)   ( mc_die_mem_stack_112 );
   VG_(track_die_mem_stack_128)   ( mc_die_mem_stack_128 );
   VG_(track_die_mem_stack_144)   ( mc_die_mem_stack_144 );
   VG_(track_die_mem_stack_160)   ( mc_die_mem_stack_160 );
#  endif
   VG_(track_die_mem_stack)       ( mc_die_mem_stack     );

   VG_(track_ban_mem_stack)       ( MC_(make_mem_noaccess) );

   VG_(track_pre_mem_read)        ( check_mem_is_defined );
   VG_(track_pre_mem_read_asciiz) ( check_mem_is_defined_asciiz );
   VG_(track_pre_mem_write)       ( check_mem_is_addressable );
   VG_(track_post_mem_write)      ( mc_post_mem_write );

   VG_(track_post_reg_write)                  ( mc_post_reg_write );
   VG_(track_post_reg_write_clientcall_return)( mc_post_reg_write_clientcall );

   if (MC_(clo_mc_level) >= 2) {
      VG_(track_copy_mem_to_reg)  ( mc_copy_mem_to_reg );
      VG_(track_copy_reg_to_mem)  ( mc_copy_reg_to_mem );
   }

   VG_(needs_watchpoint)          ( mc_mark_unaddressable_for_watchpoint );

   init_shadow_memory();
   // MC_(chunk_poolalloc) must be allocated in post_clo_init
   tl_assert(MC_(chunk_poolalloc) == NULL);
   MC_(malloc_list)  = VG_(HT_construct)( "MC_(malloc_list)" );
   MC_(mempool_list) = VG_(HT_construct)( "MC_(mempool_list)" );
   init_prof_mem();

   tl_assert( mc_expensive_sanity_check() );

   // {LOADV,STOREV}[8421] will all fail horribly if this isn't true.
   tl_assert(sizeof(UWord) == sizeof(Addr));
   // Call me paranoid.  I don't care.
   tl_assert(sizeof(void*) == sizeof(Addr));

   // BYTES_PER_SEC_VBIT_NODE must be a power of two.
   tl_assert(-1 != VG_(log2)(BYTES_PER_SEC_VBIT_NODE));

   /* This is small.  Always initialise it. */
   init_nia_to_ecu_cache();

   /* We can't initialise ocacheL1/ocacheL2 yet, since we don't know
      if we need to, since the command line args haven't been
      processed yet.  Hence defer it to mc_post_clo_init. */
   tl_assert(ocacheL1 == NULL);
   for (UInt i = 0; i < 4096; i++ ) {
      tl_assert(ocachesL2[i] == NULL);
   }

   /* Check some important stuff.  See extensive comments above
      re UNALIGNED_OR_HIGH for background. */
#  if VG_WORDSIZE == 4
   tl_assert(sizeof(void*) == 4);
   tl_assert(sizeof(Addr)  == 4);
   tl_assert(sizeof(UWord) == 4);
   tl_assert(sizeof(Word)  == 4);
   tl_assert(MAX_PRIMARY_ADDRESS == 0xFFFFFFFFUL);
   tl_assert(MASK(1) == 0UL);
   tl_assert(MASK(2) == 1UL);
   tl_assert(MASK(4) == 3UL);
   tl_assert(MASK(8) == 7UL);
#  else
   tl_assert(VG_WORDSIZE == 8);
   tl_assert(sizeof(void*) == 8);
   tl_assert(sizeof(Addr)  == 8);
   tl_assert(sizeof(UWord) == 8);
   tl_assert(sizeof(Word)  == 8);
   tl_assert(MAX_PRIMARY_ADDRESS == 0x1FFFFFFFFFULL);
   tl_assert(MASK(1) == 0xFFFFFFE000000000ULL);
   tl_assert(MASK(2) == 0xFFFFFFE000000001ULL);
   tl_assert(MASK(4) == 0xFFFFFFE000000003ULL);
   tl_assert(MASK(8) == 0xFFFFFFE000000007ULL);
#  endif

   /* Check some assertions to do with the instrumentation machinery. */
   MC_(do_instrumentation_startup_checks)();
}

STATIC_ASSERT(sizeof(UWord) == sizeof(SizeT));

VG_DETERMINE_INTERFACE_VERSION(mc_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                mc_main.c ---*/
/*--------------------------------------------------------------------*/
