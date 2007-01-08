
/*--------------------------------------------------------------------*/
/*--- MemCheck: Maintain bitmaps of memory, tracking the           ---*/
/*--- accessibility (A) and validity (V) status of each byte.      ---*/
/*---                                                    mc_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

#include "pub_tool_basics.h"
#include "pub_tool_aspacemgr.h"
#include "pub_tool_hashtable.h"     // For mc_include.h
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_oset.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_oset.h"

#include "mc_include.h"
#include "memcheck.h"   /* for client requests */

#ifdef HAVE_BUILTIN_EXPECT
#define EXPECTED_TAKEN(cond)     __builtin_expect((cond),1)
#define EXPECTED_NOT_TAKEN(cond) __builtin_expect((cond),0)
#else
#define EXPECTED_TAKEN(cond)     (cond)
#define EXPECTED_NOT_TAKEN(cond) (cond)
#endif

/* Set to 1 to do a little more sanity checking */
#define VG_DEBUG_MEMORY 0

#define DEBUG(fmt, args...) //VG_(printf)(fmt, ## args)


/*------------------------------------------------------------*/
/*--- Fast-case knobs                                      ---*/
/*------------------------------------------------------------*/
 
// Comment these out to disable the fast cases (don't just set them to zero).

#define PERF_FAST_LOADV    1
#define PERF_FAST_STOREV   1

#define PERF_FAST_SARP     1

#define PERF_FAST_STACK    1
#define PERF_FAST_STACK2   1

/*------------------------------------------------------------*/
/*--- V bits and A bits                                    ---*/
/*------------------------------------------------------------*/

/* Conceptually, every byte value has 8 V bits, which track whether Memcheck
   thinks the corresponding value bit is defined.  And every memory byte
   has an A bit, which tracks whether Memcheck thinks the program can access
   it safely.   So every N-bit register is shadowed with N V bits, and every
   memory byte is shadowed with 8 V bits and one A bit.

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
   accesses.  So instead the top-level map table has 2^19 entries (indexed
   using bits 16..34 of the address);  this covers the bottom 32GB.  Any
   accesses above 32GB are handled with a slow, sparse auxiliary table.
   Valgrind's address space manager tries very hard to keep things below
   this 32GB barrier so that performance doesn't suffer too much.

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

/* Just handle the first 32G fast and the rest via auxiliary
   primaries. */
#  define N_PRIMARY_BITS  19

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


#define SM_CHUNKS             16384
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

typedef 
   struct {
      UChar vabits8[SM_CHUNKS];
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

   new_sm = VG_(am_shadow_alloc)(sizeof(SecMap));
   if (new_sm == NULL)
      VG_(out_of_memory_NORETURN)( "memcheck:allocate new SecMap", 
                                   sizeof(SecMap) );
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
static SecMap* primary_map[N_PRIMARY_MAP];


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
   auxmap_L2 = VG_(OSet_Create)( /*keyOff*/  offsetof(AuxMapEnt,base),
                                 /*fastCmp*/ NULL,
                                 VG_(malloc), VG_(free) );
}

/* Check representation invariants; if OK return NULL; else a
   descriptive bit of text.  Also return the number of
   non-distinguished secondary maps referred to from the auxiliary
   primary maps. */

static HChar* check_auxmap_L1_L2_sanity ( Word* n_secmaps_found )
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
      if (VG_(OSet_Size)(auxmap_L2) != 0)
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
      VG_(OSet_ResetIter)(auxmap_L2);
      while ( (elem = VG_(OSet_Next)(auxmap_L2)) ) {
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
         res = VG_(OSet_Lookup)(auxmap_L2, &key);
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

   if (EXPECTED_TAKEN(auxmap_L1[0].base == a))
      return auxmap_L1[0].ent;
   if (EXPECTED_TAKEN(auxmap_L1[1].base == a)) {
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

   res = VG_(OSet_Lookup)(auxmap_L2, &key);
   if (res)
      insert_into_auxmap_L1_at( AUXMAP_L1_INSERT_IX, res );
   return res;
}

static AuxMapEnt* find_or_alloc_in_auxmap ( Addr a )
{
   AuxMapEnt *nyu, *res;

   /* First see if we already have it. */
   res = maybe_find_in_auxmap( a );
   if (EXPECTED_TAKEN(res))
      return res;

   /* Ok, there's no entry in the secondary map, so we'll have
      to allocate one. */
   a &= ~(Addr)0xFFFF;

   nyu = (AuxMapEnt*) VG_(OSet_AllocNode)( auxmap_L2, sizeof(AuxMapEnt) );
   tl_assert(nyu);
   nyu->base = a;
   nyu->sm   = &sm_distinguished[SM_DIST_NOACCESS];
   VG_(OSet_Insert)( auxmap_L2, nyu );
   insert_into_auxmap_L1_at( AUXMAP_L1_INSERT_IX, nyu );
   n_auxmap_L2_nodes++;
   return nyu;
}

/* --------------- SecMap fundamentals --------------- */

// In all these, 'low' means it's definitely in the main primary map,
// 'high' means it's definitely in the auxiliary table.

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

static SecMap** get_secmap_ptr ( Addr a )
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
   if (EXPECTED_NOT_TAKEN(is_distinguished_sm(*p)))
      *p = copy_for_writing(*p);
   return *p;
}

static INLINE SecMap* get_secmap_for_writing_high ( Addr a )
{
   SecMap** p = get_secmap_high_ptr(a);
   if (EXPECTED_NOT_TAKEN(is_distinguished_sm(*p)))
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
static SecMap* get_secmap_for_writing ( Addr a )
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
// traversing it and evicting any "sufficiently stale" nodes, ie. nodes that
// are stale and haven't been touched for a certain number of collections.
// If more than a certain proportion of nodes survived, we increase the
// table size so that GCs occur less often.  
//
// (So this a bit different to a traditional GC, where you definitely want
// to remove any dead nodes.  It's more like we have a resizable cache and
// we're trying to find the right balance how many elements to evict and how
// big to make the cache.)
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

// We make the table bigger if more than this many nodes survive a GC.
#define MAX_SURVIVOR_PROPORTION  0.5

// Each time we make the table bigger, we increase it by this much.
#define TABLE_GROWTH_FACTOR      2

// This defines "sufficiently stale" -- any node that hasn't been touched in
// this many GCs will be removed.
#define MAX_STALE_AGE            2
      
// We GC the table when it gets this many nodes in it, ie. it's effectively
// the table size.  It can change.
static Int  secVBitLimit = 1024;

// The number of GCs done, used to age sec-V-bit nodes for eviction.
// Because it's unsigned, wrapping doesn't matter -- the right answer will
// come out anyway.
static UInt GCs_done = 0;

typedef 
   struct {
      Addr  a;
      UChar vbits8[BYTES_PER_SEC_VBIT_NODE];
      UInt  last_touched;
   } 
   SecVBitNode;

static OSet* createSecVBitTable(void)
{
   return VG_(OSet_Create)( offsetof(SecVBitNode, a), 
                            NULL, // use fast comparisons
                            VG_(malloc), VG_(free) );
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
   VG_(OSet_ResetIter)(secVBitTable);
   while ( (n = VG_(OSet_Next)(secVBitTable)) ) {
      Bool keep = False;
      if ( (GCs_done - n->last_touched) <= MAX_STALE_AGE ) {
         // Keep node if it's been touched recently enough (regardless of
         // freshness/staleness).
         keep = True;
      } else {
         // Keep node if any of its bytes are non-stale.  Using
         // get_vabits2() for the lookup is not very efficient, but I don't
         // think it matters.
         for (i = 0; i < BYTES_PER_SEC_VBIT_NODE; i++) {
            if (VA_BITS2_PARTDEFINED == get_vabits2(n->a + i)) {
               keep = True;      // Found a non-stale byte, so keep
               break;
            }
         }
      }

      if ( keep ) {
         // Insert a copy of the node into the new table.
         SecVBitNode* n2 = 
            VG_(OSet_AllocNode)(secVBitTable2, sizeof(SecVBitNode));
         *n2 = *n;
         VG_(OSet_Insert)(secVBitTable2, n2);
      }
   }

   // Get the before and after sizes.
   n_nodes     = VG_(OSet_Size)(secVBitTable);
   n_survivors = VG_(OSet_Size)(secVBitTable2);

   // Destroy the old table, and put the new one in its place.
   VG_(OSet_Destroy)(secVBitTable, NULL);
   secVBitTable = secVBitTable2;

   if (VG_(clo_verbosity) > 1) {
      Char percbuf[6];
      VG_(percentify)(n_survivors, n_nodes, 1, 6, percbuf);
      VG_(message)(Vg_DebugMsg, "memcheck GC: %d nodes, %d survivors (%s)",
                   n_nodes, n_survivors, percbuf);
   }

   // Increase table size if necessary.
   if (n_survivors > (secVBitLimit * MAX_SURVIVOR_PROPORTION)) {
      secVBitLimit *= TABLE_GROWTH_FACTOR;
      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg, "memcheck GC: increase table size to %d",
                      secVBitLimit);
   }
}

static UWord get_sec_vbits8(Addr a)
{
   Addr         aAligned = VG_ROUNDDN(a, BYTES_PER_SEC_VBIT_NODE);
   Int          amod     = a % BYTES_PER_SEC_VBIT_NODE;
   SecVBitNode* n        = VG_(OSet_Lookup)(secVBitTable, &aAligned);
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
   SecVBitNode* n        = VG_(OSet_Lookup)(secVBitTable, &aAligned);
   // Shouldn't be fully defined or fully undefined -- those cases shouldn't
   // make it to the secondary V bits table.
   tl_assert(V_BITS8_DEFINED != vbits8 && V_BITS8_UNDEFINED != vbits8);
   if (n) {
      n->vbits8[amod] = vbits8;     // update
      n->last_touched = GCs_done;
      sec_vbits_updates++;
   } else {
      // New node:  assign the specific byte, make the rest invalid (they
      // should never be read as-is, but be cautious).
      n = VG_(OSet_AllocNode)(secVBitTable, sizeof(SecVBitNode));
      n->a            = aAligned;
      for (i = 0; i < BYTES_PER_SEC_VBIT_NODE; i++) {
         n->vbits8[i] = V_BITS8_UNDEFINED;
      }
      n->vbits8[amod] = vbits8;
      n->last_touched = GCs_done;

      // Do a table GC if necessary.  Nb: do this before inserting the new
      // node, to avoid erroneously GC'ing the new node.
      if (secVBitLimit == VG_(OSet_Size)(secVBitTable)) {
         gcSecVBitTable();
      }

      // Insert the new node.
      VG_(OSet_Insert)(secVBitTable, n);
      sec_vbits_new_nodes++;

      n_secVBit_nodes = VG_(OSet_Size)(secVBitTable);
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

#define M_IGNORE_RANGES 4

typedef
   struct {
      Int  used;
      Addr start[M_IGNORE_RANGES];
      Addr end[M_IGNORE_RANGES];
   }
   IgnoreRanges;

static IgnoreRanges ignoreRanges;

static INLINE Bool in_ignored_range ( Addr a )
{
   Int i;
   if (EXPECTED_TAKEN(ignoreRanges.used == 0))
      return False;
   for (i = 0; i < ignoreRanges.used; i++) {
      if (a >= ignoreRanges.start[i] && a < ignoreRanges.end[i])
         return True;
   }
   return False;
}


/* Parse a 32- or 64-bit hex number, including leading 0x, from string
   starting at *ppc, putting result in *result, and return True.  Or
   fail, in which case *ppc and *result are undefined, and return
   False. */

static Bool isHex ( UChar c )
{
  return ((c >= '0' && c <= '9')
	  || (c >= 'a' && c <= 'f')
	  || (c >= 'A' && c <= 'F'));
}

static UInt fromHex ( UChar c )
{
   if (c >= '0' && c <= '9')
      return (UInt)c - (UInt)'0';
   if (c >= 'a' && c <= 'f')
      return 10 +  (UInt)c - (UInt)'a';
   if (c >= 'A' && c <= 'F')
      return 10 +  (UInt)c - (UInt)'A';
   /*NOTREACHED*/
   tl_assert(0);
   return 0;
}

static Bool parse_Addr ( UChar** ppc, Addr* result )
{
   Int used, limit = 2 * sizeof(Addr);
   if (**ppc != '0')
      return False;
   (*ppc)++;
   if (**ppc != 'x')
      return False;
   (*ppc)++;
   *result = 0;
   used = 0;
   while (isHex(**ppc)) {
      UInt d = fromHex(**ppc);
      tl_assert(d < 16);
      *result = ((*result) << 4) | fromHex(**ppc);
      (*ppc)++;
      used++;
      if (used > limit) return False;
   }
   if (used == 0)
      return False;
   return True;
}

/* Parse two such numbers separated by a dash, or fail. */

static Bool parse_range ( UChar** ppc, Addr* result1, Addr* result2 )
{
   Bool ok = parse_Addr(ppc, result1);
   if (!ok)
      return False;
   if (**ppc != '-')
      return False;
   (*ppc)++;
   ok = parse_Addr(ppc, result2);
   if (!ok)
      return False;
   return True;
}

/* Parse a set of ranges separated by commas into 'ignoreRanges', or
   fail. */

static Bool parse_ignore_ranges ( UChar* str0 )
{
   Addr start, end;
   Bool ok;
   UChar*  str = str0;
   UChar** ppc = &str;
   ignoreRanges.used = 0;
   while (1) {
      ok = parse_range(ppc, &start, &end);
      if (!ok)
         return False;
      if (ignoreRanges.used >= M_IGNORE_RANGES)
         return False;
      ignoreRanges.start[ignoreRanges.used] = start;
      ignoreRanges.end[ignoreRanges.used] = end;
      ignoreRanges.used++;
      if (**ppc == 0)
         return True;
      if (**ppc != ',')
         return False;
      (*ppc)++;
   }
   /*NOTREACHED*/
   return False;
}


/* --------------- Load/store slow cases. --------------- */

// Forward declarations
static void mc_record_address_error  ( ThreadId tid, Addr a,
                                       Int size, Bool isWrite );
static void mc_record_core_mem_error ( ThreadId tid, Bool isAddrErr, Char* s );
static void mc_record_regparam_error ( ThreadId tid, Char* msg );
static void mc_record_memparam_error ( ThreadId tid, Addr a,
                                       Bool isAddrErr, Char* msg );
static void mc_record_jump_error     ( ThreadId tid, Addr a );

static
#ifndef PERF_FAST_LOADV
INLINE
#endif
ULong mc_LOADVn_slow ( Addr a, SizeT nBits, Bool bigendian )
{
   /* Make up a 64-bit result V word, which contains the loaded data for
      valid addresses and Defined for invalid addresses.  Iterate over
      the bytes in the word, from the most significant down to the
      least. */
   ULong vbits64     = V_BITS64_UNDEFINED;
   SizeT szB         = nBits / 8;
   SSizeT i          = szB-1;    // Must be signed
   SizeT n_addrs_bad = 0;
   Addr  ai;
   Bool  partial_load_exemption_applies;
   UChar vbits8;
   Bool  ok;

   PROF_EVENT(30, "mc_LOADVn_slow");

   /* ------------ BEGIN semi-fast cases ------------ */
   /* These deal quickly-ish with the common auxiliary primary map
      cases on 64-bit platforms.  Are merely a speedup hack; can be
      omitted without loss of correctness/functionality.  Note that in
      both cases the "sizeof(void*) == 8" causes these cases to be
      folded out by compilers on 32-bit platforms.  These are derived
      from LOADV64 and LOADV32.
   */
   if (EXPECTED_TAKEN(sizeof(void*) == 8 
                      && nBits == 64 && VG_IS_8_ALIGNED(a))) {
      SecMap* sm       = get_secmap_for_reading(a);
      UWord   sm_off16 = SM_OFF_16(a);
      UWord   vabits16 = ((UShort*)(sm->vabits8))[sm_off16];
      if (EXPECTED_TAKEN(vabits16 == VA_BITS16_DEFINED))
         return V_BITS64_DEFINED;
      if (EXPECTED_TAKEN(vabits16 == VA_BITS16_UNDEFINED))
         return V_BITS64_UNDEFINED;
      /* else fall into the slow case */
   }
   if (EXPECTED_TAKEN(sizeof(void*) == 8 
                      && nBits == 32 && VG_IS_4_ALIGNED(a))) {
      SecMap* sm = get_secmap_for_reading(a);
      UWord sm_off = SM_OFF(a);
      UWord vabits8 = sm->vabits8[sm_off];
      if (EXPECTED_TAKEN(vabits8 == VA_BITS8_DEFINED))
         return ((UWord)0xFFFFFFFF00000000ULL | (UWord)V_BITS32_DEFINED);
      if (EXPECTED_TAKEN(vabits8 == VA_BITS8_UNDEFINED))
         return ((UWord)0xFFFFFFFF00000000ULL | (UWord)V_BITS32_UNDEFINED);
      /* else fall into slow case */
   }
   /* ------------ END semi-fast cases ------------ */

   tl_assert(nBits == 64 || nBits == 32 || nBits == 16 || nBits == 8);

   for (i = szB-1; i >= 0; i--) {
      PROF_EVENT(31, "mc_LOADVn_slow(loop)");
      ai = a + byte_offset_w(szB, bigendian, i);
      ok = get_vbits8(ai, &vbits8);
      if (!ok) n_addrs_bad++;
      vbits64 <<= 8; 
      vbits64 |= vbits8;
   }

   /* This is a hack which avoids producing errors for code which
      insists in stepping along byte strings in aligned word-sized
      chunks, and there is a partially defined word at the end.  (eg,
      optimised strlen).  Such code is basically broken at least WRT
      semantics of ANSI C, but sometimes users don't have the option
      to fix it, and so this option is provided.  Note it is now
      defaulted to not-engaged.

      A load from a partially-addressible place is allowed if:
      - the command-line flag is set
      - it's a word-sized, word-aligned load
      - at least one of the addresses in the word *is* valid
   */
   partial_load_exemption_applies
      = MC_(clo_partial_loads_ok) && szB == VG_WORDSIZE 
                                   && VG_IS_WORD_ALIGNED(a) 
                                   && n_addrs_bad < VG_WORDSIZE;

   if (n_addrs_bad > 0 && !partial_load_exemption_applies)
      mc_record_address_error( VG_(get_running_tid)(), a, szB, False );

   return vbits64;
}


static
#ifndef PERF_FAST_STOREV
INLINE
#endif
void mc_STOREVn_slow ( Addr a, SizeT nBits, ULong vbytes, Bool bigendian )
{
   SizeT szB = nBits / 8;
   SizeT i, n_addrs_bad = 0;
   UChar vbits8;
   Addr  ai;
   Bool  ok;

   PROF_EVENT(35, "mc_STOREVn_slow");

   /* ------------ BEGIN semi-fast cases ------------ */
   /* These deal quickly-ish with the common auxiliary primary map
      cases on 64-bit platforms.  Are merely a speedup hack; can be
      omitted without loss of correctness/functionality.  Note that in
      both cases the "sizeof(void*) == 8" causes these cases to be
      folded out by compilers on 32-bit platforms.  These are derived
      from STOREV64 and STOREV32.
   */
   if (EXPECTED_TAKEN(sizeof(void*) == 8 
                      && nBits == 64 && VG_IS_8_ALIGNED(a))) {
      SecMap* sm       = get_secmap_for_reading(a);
      UWord   sm_off16 = SM_OFF_16(a);
      UWord   vabits16 = ((UShort*)(sm->vabits8))[sm_off16];
      if (EXPECTED_TAKEN( !is_distinguished_sm(sm) && 
                          (VA_BITS16_DEFINED   == vabits16 ||
                           VA_BITS16_UNDEFINED == vabits16) )) {
         /* Handle common case quickly: a is suitably aligned, */
         /* is mapped, and is addressible. */
         // Convert full V-bits in register to compact 2-bit form.
         if (EXPECTED_TAKEN(V_BITS64_DEFINED == vbytes)) {
            ((UShort*)(sm->vabits8))[sm_off16] = (UShort)VA_BITS16_DEFINED;
            return;
         } else if (V_BITS64_UNDEFINED == vbytes) {
            ((UShort*)(sm->vabits8))[sm_off16] = (UShort)VA_BITS16_UNDEFINED;
            return;
         }
         /* else fall into the slow case */
      }
      /* else fall into the slow case */
   }
   if (EXPECTED_TAKEN(sizeof(void*) == 8
                      && nBits == 32 && VG_IS_4_ALIGNED(a))) {
      SecMap* sm      = get_secmap_for_reading(a);
      UWord   sm_off  = SM_OFF(a);
      UWord   vabits8 = sm->vabits8[sm_off];
      if (EXPECTED_TAKEN( !is_distinguished_sm(sm) && 
                          (VA_BITS8_DEFINED   == vabits8 ||
                           VA_BITS8_UNDEFINED == vabits8) )) {
         /* Handle common case quickly: a is suitably aligned, */
         /* is mapped, and is addressible. */
         // Convert full V-bits in register to compact 2-bit form.
         if (EXPECTED_TAKEN(V_BITS32_DEFINED == (vbytes & 0xFFFFFFFF))) {
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
      PROF_EVENT(36, "mc_STOREVn_slow(loop)");
      ai     = a + byte_offset_w(szB, bigendian, i);
      vbits8 = vbytes & 0xff;
      ok     = set_vbits8(ai, vbits8);
      if (!ok) n_addrs_bad++;
      vbytes >>= 8;
   }

   /* If an address error has happened, report it. */
   if (n_addrs_bad > 0)
      mc_record_address_error( VG_(get_running_tid)(), a, szB, True );
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

   PROF_EVENT(150, "set_address_range_perms");

   /* Check the V+A bits make sense. */
   tl_assert(VA_BITS16_NOACCESS  == vabits16 ||
             VA_BITS16_UNDEFINED == vabits16 ||
             VA_BITS16_DEFINED   == vabits16);

   // This code should never write PDBs;  ensure this.  (See comment above
   // set_vabits2().)
   tl_assert(VA_BITS2_PARTDEFINED != vabits2);

   if (lenT == 0)
      return;

   if (lenT > 100 * 1000 * 1000) {
      if (VG_(clo_verbosity) > 0 && !VG_(clo_xml)) {
         Char* s = "unknown???";
         if (vabits16 == VA_BITS16_NOACCESS ) s = "noaccess";
         if (vabits16 == VA_BITS16_UNDEFINED) s = "undefined";
         if (vabits16 == VA_BITS16_DEFINED  ) s = "defined";
         VG_(message)(Vg_UserMsg, "Warning: set address range perms: "
                                  "large range %lu (%s)", lenT, s);
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
      PROF_EVENT(151, "set_address_range_perms-single-secmap");
      lenA = lenT;
      lenB = 0;
   } else if (is_start_of_sm(a)) {
      // Range spans at least one whole sec-map, and starts at the beginning
      // of a sec-map; skip to Part 2.
      PROF_EVENT(152, "set_address_range_perms-startof-secmap");
      lenA = 0;
      lenB = lenT;
      goto part2;
   } else {
      // Range spans two or more sec-maps, first one is partial.
      PROF_EVENT(153, "set_address_range_perms-multiple-secmaps");
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
         PROF_EVENT(154, "set_address_range_perms-dist-sm1-quick");
         a    = aNext;
         lenA = 0;
      } else {
         PROF_EVENT(155, "set_address_range_perms-dist-sm1");
         *sm_ptr = copy_for_writing(*sm_ptr);
      }
   }
   sm = *sm_ptr;

   // 1 byte steps
   while (True) {
      if (VG_IS_8_ALIGNED(a)) break;
      if (lenA < 1)           break;
      PROF_EVENT(156, "set_address_range_perms-loop1a");
      sm_off = SM_OFF(a);
      insert_vabits2_into_vabits8( a, vabits2, &(sm->vabits8[sm_off]) );
      a    += 1;
      lenA -= 1;
   }
   // 8-aligned, 8 byte steps
   while (True) {
      if (lenA < 8) break;
      PROF_EVENT(157, "set_address_range_perms-loop8a");
      sm_off16 = SM_OFF_16(a);
      ((UShort*)(sm->vabits8))[sm_off16] = vabits16;
      a    += 8;
      lenA -= 8;
   }
   // 1 byte steps
   while (True) {
      if (lenA < 1) break;
      PROF_EVENT(158, "set_address_range_perms-loop1b");
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
   while (True) {
      if (lenB < SM_SIZE) break;
      tl_assert(is_start_of_sm(a));
      PROF_EVENT(159, "set_address_range_perms-loop64K");
      sm_ptr = get_secmap_ptr(a);
      if (!is_distinguished_sm(*sm_ptr)) {
         PROF_EVENT(160, "set_address_range_perms-loop64K-free-dist-sm");
         // Free the non-distinguished sec-map that we're replacing.  This
         // case happens moderately often, enough to be worthwhile.
         VG_(am_munmap_valgrind)((Addr)*sm_ptr, sizeof(SecMap));
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
         PROF_EVENT(161, "set_address_range_perms-dist-sm2-quick");
         return;
      } else {
         PROF_EVENT(162, "set_address_range_perms-dist-sm2");
         *sm_ptr = copy_for_writing(*sm_ptr);
      }
   }
   sm = *sm_ptr;

   // 8-aligned, 8 byte steps
   while (True) {
      if (lenB < 8) break;
      PROF_EVENT(163, "set_address_range_perms-loop8b");
      sm_off16 = SM_OFF_16(a);
      ((UShort*)(sm->vabits8))[sm_off16] = vabits16;
      a    += 8;
      lenB -= 8;
   }
   // 1 byte steps
   while (True) {
      if (lenB < 1) return;
      PROF_EVENT(164, "set_address_range_perms-loop1c");
      sm_off = SM_OFF(a);
      insert_vabits2_into_vabits8( a, vabits2, &(sm->vabits8[sm_off]) );
      a    += 1;
      lenB -= 1;
   }
}


/* --- Set permissions for arbitrary address ranges --- */

void MC_(make_mem_noaccess) ( Addr a, SizeT len )
{
   PROF_EVENT(40, "MC_(make_mem_noaccess)");
   DEBUG("MC_(make_mem_noaccess)(%p, %lu)\n", a, len);
   set_address_range_perms ( a, len, VA_BITS16_NOACCESS, SM_DIST_NOACCESS );
}

void MC_(make_mem_undefined) ( Addr a, SizeT len )
{
   PROF_EVENT(41, "MC_(make_mem_undefined)");
   DEBUG("MC_(make_mem_undefined)(%p, %lu)\n", a, len);
   set_address_range_perms ( a, len, VA_BITS16_UNDEFINED, SM_DIST_UNDEFINED );
}

void MC_(make_mem_defined) ( Addr a, SizeT len )
{
   PROF_EVENT(42, "MC_(make_mem_defined)");
   DEBUG("MC_(make_mem_defined)(%p, %lu)\n", a, len);
   set_address_range_perms ( a, len, VA_BITS16_DEFINED, SM_DIST_DEFINED );
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
      if (EXPECTED_TAKEN(VA_BITS2_NOACCESS != vabits2)) {
         set_vabits2(a+i, VA_BITS2_DEFINED);
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
   PROF_EVENT(50, "MC_(copy_address_range_state)");

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
         if (EXPECTED_TAKEN(VA_BITS8_DEFINED == vabits8 
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
            PROF_EVENT(51, "MC_(copy_address_range_state)(loop)");
            vabits2 = get_vabits2( src+j );
            set_vabits2( dst+j, vabits2 );
            if (VA_BITS2_PARTDEFINED == vabits2) {
               set_sec_vbits8( dst+j, get_sec_vbits8( src+j ) );
            }
         }
      }

      if (src > dst) {
         for (i = 0; i < len; i++) {
            PROF_EVENT(52, "MC_(copy_address_range_state)(loop)");
            vabits2 = get_vabits2( src+i );
            set_vabits2( dst+i, vabits2 );
            if (VA_BITS2_PARTDEFINED == vabits2) {
               set_sec_vbits8( dst+i, get_sec_vbits8( src+i ) );
            }
         }
      }
   }

}


/* --- Fast case permission setters, for dealing with stacks. --- */

static INLINE
void make_aligned_word32_undefined ( Addr a )
{
   UWord   sm_off;
   SecMap* sm;

   PROF_EVENT(300, "make_aligned_word32_undefined");

#ifndef PERF_FAST_STACK2
   MC_(make_mem_undefined)(a, 4);
#else
   if (EXPECTED_NOT_TAKEN(a > MAX_PRIMARY_ADDRESS)) {
      PROF_EVENT(301, "make_aligned_word32_undefined-slow1");
      MC_(make_mem_undefined)(a, 4);
      return;
   }

   sm                  = get_secmap_for_writing_low(a);
   sm_off              = SM_OFF(a);
   sm->vabits8[sm_off] = VA_BITS8_UNDEFINED;
#endif
}


static INLINE
void make_aligned_word32_noaccess ( Addr a )
{
   UWord   sm_off;
   SecMap* sm;

   PROF_EVENT(310, "make_aligned_word32_noaccess");

#ifndef PERF_FAST_STACK2
   MC_(make_mem_noaccess)(a, 4);
#else
   if (EXPECTED_NOT_TAKEN(a > MAX_PRIMARY_ADDRESS)) {
      PROF_EVENT(311, "make_aligned_word32_noaccess-slow1");
      MC_(make_mem_noaccess)(a, 4);
      return;
   }

   sm                  = get_secmap_for_writing_low(a);
   sm_off              = SM_OFF(a);
   sm->vabits8[sm_off] = VA_BITS8_NOACCESS;
#endif
}


/* Nb: by "aligned" here we mean 8-byte aligned */
static INLINE
void make_aligned_word64_undefined ( Addr a )
{
   UWord   sm_off16;
   SecMap* sm;

   PROF_EVENT(320, "make_aligned_word64_undefined");

#ifndef PERF_FAST_STACK2
   MC_(make_mem_undefined)(a, 8);
#else
   if (EXPECTED_NOT_TAKEN(a > MAX_PRIMARY_ADDRESS)) {
      PROF_EVENT(321, "make_aligned_word64_undefined-slow1");
      MC_(make_mem_undefined)(a, 8);
      return;
   }

   sm       = get_secmap_for_writing_low(a);
   sm_off16 = SM_OFF_16(a);
   ((UShort*)(sm->vabits8))[sm_off16] = VA_BITS16_UNDEFINED;
#endif
}


static INLINE
void make_aligned_word64_noaccess ( Addr a )
{
   UWord   sm_off16;
   SecMap* sm;

   PROF_EVENT(330, "make_aligned_word64_noaccess");

#ifndef PERF_FAST_STACK2
   MC_(make_mem_noaccess)(a, 8);
#else
   if (EXPECTED_NOT_TAKEN(a > MAX_PRIMARY_ADDRESS)) {
      PROF_EVENT(331, "make_aligned_word64_noaccess-slow1");
      MC_(make_mem_noaccess)(a, 8);
      return;
   }

   sm       = get_secmap_for_writing_low(a);
   sm_off16 = SM_OFF_16(a);
   ((UShort*)(sm->vabits8))[sm_off16] = VA_BITS16_NOACCESS;
#endif
}


/*------------------------------------------------------------*/
/*--- Stack pointer adjustment                             ---*/
/*------------------------------------------------------------*/

static void VG_REGPARM(1) mc_new_mem_stack_4(Addr new_SP)
{
   PROF_EVENT(110, "new_mem_stack_4");
   if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 4 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_4(Addr new_SP)
{
   PROF_EVENT(120, "die_mem_stack_4");
   if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-4, 4 );
   }
}

static void VG_REGPARM(1) mc_new_mem_stack_8(Addr new_SP)
{
   PROF_EVENT(111, "new_mem_stack_8");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP   );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4 );
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 8 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_8(Addr new_SP)
{
   PROF_EVENT(121, "die_mem_stack_8");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8 );
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-8, 8 );
   }
}

static void VG_REGPARM(1) mc_new_mem_stack_12(Addr new_SP)
{
   PROF_EVENT(112, "new_mem_stack_12");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP   );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* from previous test we don't have 8-alignment at offset +0,
         hence must have 8 alignment at offsets +4/-4.  Hence safe to
         do 4 at +0 and then 8 at +4/. */
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP   );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4 );
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 12 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_12(Addr new_SP)
{
   PROF_EVENT(122, "die_mem_stack_12");
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

static void VG_REGPARM(1) mc_new_mem_stack_16(Addr new_SP)
{
   PROF_EVENT(113, "new_mem_stack_16");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Have 8-alignment at +0, hence do 8 at +0 and 8 at +8. */
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP   );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Have 4 alignment at +0 but not 8; hence 8 must be at +4.
         Hence do 4 at +0, 8 at +4, 4 at +12. */
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP    );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4  );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+12 );
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 16 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_16(Addr new_SP)
{
   PROF_EVENT(123, "die_mem_stack_16");
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

static void VG_REGPARM(1) mc_new_mem_stack_32(Addr new_SP)
{
   PROF_EVENT(114, "new_mem_stack_32");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* Straightforward */
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP    );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8  );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+16 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+24 );
   } else if (VG_IS_4_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      /* 8 alignment must be at +4.  Hence do 8 at +4,+12,+20 and 4 at
         +0,+28. */
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP    );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4  );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+12 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+20 );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+28 );
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 32 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_32(Addr new_SP)
{
   PROF_EVENT(124, "die_mem_stack_32");
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

static void VG_REGPARM(1) mc_new_mem_stack_112(Addr new_SP)
{
   PROF_EVENT(115, "new_mem_stack_112");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP    );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8  );
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
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+104);
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 112 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_112(Addr new_SP)
{
   PROF_EVENT(125, "die_mem_stack_112");
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

static void VG_REGPARM(1) mc_new_mem_stack_128(Addr new_SP)
{
   PROF_EVENT(116, "new_mem_stack_128");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP    );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8  );
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
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+104);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+112);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+120);
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 128 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_128(Addr new_SP)
{
   PROF_EVENT(126, "die_mem_stack_128");
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

static void VG_REGPARM(1) mc_new_mem_stack_144(Addr new_SP)
{
   PROF_EVENT(117, "new_mem_stack_144");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP    );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8  );
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
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+104);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+112);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+120);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+128);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+136);
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 144 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_144(Addr new_SP)
{
   PROF_EVENT(127, "die_mem_stack_144");
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

static void VG_REGPARM(1) mc_new_mem_stack_160(Addr new_SP)
{
   PROF_EVENT(118, "new_mem_stack_160");
   if (VG_IS_8_ALIGNED( -VG_STACK_REDZONE_SZB + new_SP )) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP    );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8  );
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
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+104);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+112);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+120);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+128);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+136);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+144);
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+152);
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 160 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_160(Addr new_SP)
{
   PROF_EVENT(128, "die_mem_stack_160");
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

static void mc_new_mem_stack ( Addr a, SizeT len )
{
   PROF_EVENT(115, "new_mem_stack");
   MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + a, len );
}

static void mc_die_mem_stack ( Addr a, SizeT len )
{
   PROF_EVENT(125, "die_mem_stack");
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
void MC_(helperc_MAKE_STACK_UNINIT) ( Addr base, UWord len )
{
   tl_assert(sizeof(UWord) == sizeof(SizeT));
   if (0)
      VG_(printf)("helperc_MAKE_STACK_UNINIT %p %d\n", base, len );

#  if 0
   /* Really slow version */
   MC_(make_mem_undefined)(base, len);
#  endif

#  if 0
   /* Slow(ish) version, which is fairly easily seen to be correct.
   */
   if (EXPECTED_TAKEN( VG_IS_8_ALIGNED(base) && len==128 )) {
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
      MC_(make_mem_undefined)(base, len);
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
   if (EXPECTED_TAKEN( len == 128 && VG_IS_8_ALIGNED(base) )) {
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 128 - 1);
      tl_assert(a_lo < a_hi);             // paranoia: detect overflow
      if (a_hi < MAX_PRIMARY_ADDRESS) {
         // Now we know the entire range is within the main primary map.
         SecMap* sm    = get_secmap_for_writing_low(a_lo);
         SecMap* sm_hi = get_secmap_for_writing_low(a_hi);
         /* Now we know that the entire address range falls within a
            single secondary map, and that that secondary 'lives' in
            the main primary map. */
         if (EXPECTED_TAKEN(sm == sm_hi)) {
            // Finally, we know that the range is entirely within one secmap.
            UWord   v_off = SM_OFF(a_lo);
            UShort* p     = (UShort*)(&sm->vabits8[v_off]);
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
   if (EXPECTED_TAKEN( len == 288 && VG_IS_8_ALIGNED(base) )) {
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 288 - 1);
      tl_assert(a_lo < a_hi);             // paranoia: detect overflow
      if (a_hi < MAX_PRIMARY_ADDRESS) {
         // Now we know the entire range is within the main primary map.
         SecMap* sm    = get_secmap_for_writing_low(a_lo);
         SecMap* sm_hi = get_secmap_for_writing_low(a_hi);
         /* Now we know that the entire address range falls within a
            single secondary map, and that that secondary 'lives' in
            the main primary map. */
         if (EXPECTED_TAKEN(sm == sm_hi)) {
            // Finally, we know that the range is entirely within one secmap.
            UWord   v_off = SM_OFF(a_lo);
            UShort* p     = (UShort*)(&sm->vabits8[v_off]);
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
   MC_(make_mem_undefined)(base, len);
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

   PROF_EVENT(60, "check_mem_is_noaccess");
   for (i = 0; i < len; i++) {
      PROF_EVENT(61, "check_mem_is_noaccess(loop)");
      vabits2 = get_vabits2(a);
      if (VA_BITS2_NOACCESS != vabits2) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
}

static Bool is_mem_addressable ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UWord vabits2;

   PROF_EVENT(62, "is_mem_addressable");
   for (i = 0; i < len; i++) {
      PROF_EVENT(63, "is_mem_addressable(loop)");
      vabits2 = get_vabits2(a);
      if (VA_BITS2_NOACCESS == vabits2) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
}

static MC_ReadResult is_mem_defined ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UWord vabits2;

   PROF_EVENT(64, "is_mem_defined");
   DEBUG("is_mem_defined\n");
   for (i = 0; i < len; i++) {
      PROF_EVENT(65, "is_mem_defined(loop)");
      vabits2 = get_vabits2(a);
      if (VA_BITS2_DEFINED != vabits2) {
         // Error!  Nb: Report addressability errors in preference to
         // definedness errors.  And don't report definedeness errors unless
         // --undef-value-errors=yes.
         if (bad_addr != NULL) *bad_addr = a;
         if      ( VA_BITS2_NOACCESS == vabits2 ) return MC_AddrErr; 
         else if ( MC_(clo_undef_value_errors)  ) return MC_ValueErr;
      }
      a++;
   }
   return MC_Ok;
}


/* Check a zero-terminated ascii string.  Tricky -- don't want to
   examine the actual bytes, to find the end, until we're sure it is
   safe to do so. */

static Bool mc_is_defined_asciiz ( Addr a, Addr* bad_addr )
{
   UWord vabits2;

   PROF_EVENT(66, "mc_is_defined_asciiz");
   DEBUG("mc_is_defined_asciiz\n");
   while (True) {
      PROF_EVENT(67, "mc_is_defined_asciiz(loop)");
      vabits2 = get_vabits2(a);
      if (VA_BITS2_DEFINED != vabits2) {
         // Error!  Nb: Report addressability errors in preference to
         // definedness errors.  And don't report definedeness errors unless
         // --undef-value-errors=yes.
         if (bad_addr != NULL) *bad_addr = a;
         if      ( VA_BITS2_NOACCESS == vabits2 ) return MC_AddrErr; 
         else if ( MC_(clo_undef_value_errors)  ) return MC_ValueErr;
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
void check_mem_is_addressable ( CorePart part, ThreadId tid, Char* s,
                                Addr base, SizeT size )
{
   Addr bad_addr;
   Bool ok = is_mem_addressable ( base, size, &bad_addr );

   if (!ok) {
      switch (part) {
      case Vg_CoreSysCall:
         mc_record_memparam_error ( tid, bad_addr, /*isAddrErr*/True, s );
         break;

      case Vg_CoreSignal:
         mc_record_core_mem_error( tid, /*isAddrErr*/True, s );
         break;

      default:
         VG_(tool_panic)("check_mem_is_addressable: unexpected CorePart");
      }
   }
}

static
void check_mem_is_defined ( CorePart part, ThreadId tid, Char* s,
                            Addr base, SizeT size )
{     
   Addr bad_addr;
   MC_ReadResult res = is_mem_defined ( base, size, &bad_addr );

   if (MC_Ok != res) {
      Bool isAddrErr = ( MC_AddrErr == res ? True : False );

      switch (part) {
      case Vg_CoreSysCall:
         mc_record_memparam_error ( tid, bad_addr, isAddrErr, s );
         break;
      
      /* If we're being asked to jump to a silly address, record an error 
         message before potentially crashing the entire system. */
      case Vg_CoreTranslate:
         mc_record_jump_error( tid, bad_addr );
         break;

      default:
         VG_(tool_panic)("check_mem_is_defined: unexpected CorePart");
      }
   }
}

static
void check_mem_is_defined_asciiz ( CorePart part, ThreadId tid,
                                   Char* s, Addr str )
{
   MC_ReadResult res;
   Addr bad_addr = 0;   // shut GCC up

   tl_assert(part == Vg_CoreSysCall);
   res = mc_is_defined_asciiz ( (Addr)str, &bad_addr );
   if (MC_Ok != res) {
      Bool isAddrErr = ( MC_AddrErr == res ? True : False );
      mc_record_memparam_error ( tid, bad_addr, isAddrErr, s );
   }
}

static
void mc_new_mem_startup( Addr a, SizeT len, Bool rr, Bool ww, Bool xx )
{
   /* Ignore the permissions, just make it defined.  Seems to work... */
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
   DEBUG("mc_new_mem_startup(%p, %llu, rr=%u, ww=%u, xx=%u)\n",
         a, (ULong)len, rr, ww, xx);
   MC_(make_mem_defined)(a, len);
}

static
void mc_new_mem_mmap ( Addr a, SizeT len, Bool rr, Bool ww, Bool xx )
{
   MC_(make_mem_defined)(a, len);
}

static
void mc_post_mem_write(CorePart part, ThreadId tid, Addr a, SizeT len)
{
   MC_(make_mem_defined)(a, len);
}


/*------------------------------------------------------------*/
/*--- Register event handlers                              ---*/
/*------------------------------------------------------------*/

/* When some chunk of guest state is written, mark the corresponding
   shadow area as valid.  This is used to initialise arbitrarily large
   chunks of guest state, hence the _SIZE value, which has to be as
   big as the biggest guest state.
*/
static void mc_post_reg_write ( CorePart part, ThreadId tid, 
                                OffT offset, SizeT size)
{
#  define MAX_REG_WRITE_SIZE 1408
   UChar area[MAX_REG_WRITE_SIZE];
   tl_assert(size <= MAX_REG_WRITE_SIZE);
   VG_(memset)(area, V_BITS8_DEFINED, size);
   VG_(set_shadow_regs_area)( tid, offset, size, area );
#  undef MAX_REG_WRITE_SIZE
}

static 
void mc_post_reg_write_clientcall ( ThreadId tid, 
                                    OffT offset, SizeT size,
                                    Addr f)
{
   mc_post_reg_write(/*dummy*/0, tid, offset, size);
}

/* Look at the definedness of the guest's shadow state for 
   [offset, offset+len).  If any part of that is undefined, record 
   a parameter error.
*/
static void mc_pre_reg_read ( CorePart part, ThreadId tid, Char* s, 
                              OffT offset, SizeT size)
{
   Int   i;
   Bool  bad;

   UChar area[16];
   tl_assert(size <= 16);

   VG_(get_shadow_regs_area)( tid, offset, size, area );

   bad = False;
   for (i = 0; i < size; i++) {
      if (area[i] != V_BITS8_DEFINED) {
         bad = True;
         break;
      }
   }

   if (bad)
      mc_record_regparam_error ( tid, s );
}


/*------------------------------------------------------------*/
/*--- Error types                                          ---*/
/*------------------------------------------------------------*/

// Different kinds of blocks.
typedef enum {
   Block_Mallocd = 111,
   Block_Freed,
   Block_Mempool,
   Block_MempoolChunk,
   Block_UserG
} BlockKind;

/* ------------------ Addresses -------------------- */

/* The classification of a faulting address. */
typedef 
   enum { 
      Addr_Undescribed,   // as-yet unclassified
      Addr_Unknown,       // classification yielded nothing useful
      Addr_Stack,          
      Addr_Block,
   }
   AddrTag;

typedef
   struct _AddrInfo
   AddrInfo;

struct _AddrInfo {
   AddrTag tag;
   union {
      // As-yet unclassified.
      struct { } Undescribed;

      // On a stack.
      struct {
         ThreadId tid;        // Which thread's stack?
      } Stack;

      // This covers heap blocks (normal and from mempools) and user-defined
      // blocks.
      struct {
         BlockKind   block_kind;
         Char*       block_desc;    // "block", "mempool" or user-defined
         SizeT       block_szB;
         OffT        rwoffset;
         ExeContext* lastchange;
      } Block;

      // Classification yielded nothing useful.
      struct { } Unknown;

   } Addr;
};

/* ------------------ Errors ----------------------- */

/* What kind of error it is. */
typedef 
   enum { 
      Err_Value,
      Err_Cond,
      Err_CoreMem,
      Err_Addr, 
      Err_Jump, 
      Err_RegParam,
      Err_MemParam,
      Err_User,
      Err_Free,
      Err_FreeMismatch,
      Err_Overlap,
      Err_Leak,
      Err_IllegalMempool,
   }
   MC_ErrorTag;


typedef struct _MC_Error MC_Error;

struct _MC_Error {
   // Nb: we don't need the tag here, as it's stored in the Error type! Yuk.
   //MC_ErrorTag tag;

   union {
      // Use of an undefined value:
      // - as a pointer in a load or store
      // - as a jump target
      struct {
         SizeT szB;     // size of value in bytes
      } Value;

      // Use of an undefined value in a conditional branch or move.
      struct {
      } Cond;

      // Addressability error in core (signal-handling) operation.
      // It would be good to get rid of this error kind, merge it with
      // another one somehow.
      struct {
      } CoreMem;

      // Use of an unaddressable memory location in a load or store.
      struct {
         Bool     isWrite;    // read or write?
         SizeT    szB;        // not used for exec (jump) errors
         Bool     maybe_gcc;  // True if just below %esp -- could be a gcc bug
         AddrInfo ai;
      } Addr;

      // Jump to an unaddressable memory location.
      struct {
         AddrInfo ai;
      } Jump;

      // System call register input contains undefined bytes.
      struct {
      } RegParam;

      // System call memory input contains undefined/unaddressable bytes
      struct {
         Bool     isAddrErr;  // Addressability or definedness error?
         AddrInfo ai;
      } MemParam;

      // Problem found from a client request like CHECK_MEM_IS_ADDRESSABLE.
      struct {
         Bool     isAddrErr;  // Addressability or definedness error?
         AddrInfo ai;
      } User;

      // Program tried to free() something that's not a heap block (this
      // covers double-frees). */
      struct {
         AddrInfo ai;
      } Free;

      // Program allocates heap block with one function
      // (malloc/new/new[]/custom) and deallocates with not the matching one.
      struct {
         AddrInfo ai;
      } FreeMismatch;

      // Call to strcpy, memcpy, etc, with overlapping blocks.
      struct {
         Addr src;   // Source block
         Addr dst;   // Destination block
         Int  szB;   // Size in bytes;  0 if unused.
      } Overlap;

      // A memory leak.
      struct {
         UInt        n_this_record;
         UInt        n_total_records;
         LossRecord* lossRecord;
      } Leak;

      // A memory pool error.
      struct {
         AddrInfo ai;
      } IllegalMempool;

   } Err;
};


/*------------------------------------------------------------*/
/*--- Printing errors                                      ---*/
/*------------------------------------------------------------*/

static void mc_pp_AddrInfo ( Addr a, AddrInfo* ai, Bool maybe_gcc )
{
   HChar* xpre  = VG_(clo_xml) ? "  <auxwhat>" : " ";
   HChar* xpost = VG_(clo_xml) ? "</auxwhat>"  : "";

   switch (ai->tag) {
      case Addr_Unknown:
         if (maybe_gcc) {
            VG_(message)(Vg_UserMsg, 
               "%sAddress 0x%llx is just below the stack ptr.  "
               "To suppress, use: --workaround-gcc296-bugs=yes%s",
               xpre, (ULong)a, xpost
            );
	 } else {
            VG_(message)(Vg_UserMsg, 
               "%sAddress 0x%llx "
               "is not stack'd, malloc'd or (recently) free'd%s",
               xpre, (ULong)a, xpost);
         }
         break;

      case Addr_Stack: 
         VG_(message)(Vg_UserMsg, 
                      "%sAddress 0x%llx is on thread %d's stack%s", 
                      xpre, (ULong)a, ai->Addr.Stack.tid, xpost);
         break;

      case Addr_Block: {
         SizeT block_szB  = ai->Addr.Block.block_szB;
         OffT  rwoffset   = ai->Addr.Block.rwoffset;
         SizeT delta;
         const Char* relative;

         if (rwoffset < 0) {
            delta    = (SizeT)(-rwoffset);
            relative = "before";
         } else if (rwoffset >= block_szB) {
            delta    = rwoffset - block_szB;
            relative = "after";
         } else {
            delta    = rwoffset;
            relative = "inside";
         }
         VG_(message)(Vg_UserMsg, 
            "%sAddress 0x%lx is %,lu bytes %s a %s of size %,lu %s%s",
            xpre,
            a, delta, relative, ai->Addr.Block.block_desc,
            block_szB,
            ai->Addr.Block.block_kind==Block_Mallocd ? "alloc'd" 
            : ai->Addr.Block.block_kind==Block_Freed ? "free'd" 
                                                     : "client-defined",
            xpost);
         VG_(pp_ExeContext)(ai->Addr.Block.lastchange);
         break;
      }

      default:
         VG_(tool_panic)("mc_pp_AddrInfo");
   }
}

static const HChar* str_leak_lossmode ( Reachedness lossmode )
{
   const HChar *loss = "?";
   switch (lossmode) {
      case Unreached:    loss = "definitely lost"; break;
      case IndirectLeak: loss = "indirectly lost"; break;
      case Interior:     loss = "possibly lost"; break;
      case Proper:       loss = "still reachable"; break;
   }
   return loss;
}

static const HChar* xml_leak_kind ( Reachedness lossmode )
{
   const HChar *loss = "?";
   switch (lossmode) {
      case Unreached:    loss = "Leak_DefinitelyLost"; break;
      case IndirectLeak: loss = "Leak_IndirectlyLost"; break;
      case Interior:     loss = "Leak_PossiblyLost"; break;
      case Proper:       loss = "Leak_StillReachable"; break;
   }
   return loss;
}

static void mc_pp_msg( Char* xml_name, Error* err, const HChar* format, ... )
{
   HChar* xpre  = VG_(clo_xml) ? "  <what>" : "";
   HChar* xpost = VG_(clo_xml) ? "</what>"  : "";
   Char buf[256];
   va_list vargs;

   if (VG_(clo_xml))
      VG_(message)(Vg_UserMsg, "  <kind>%s</kind>", xml_name);
   // Stick xpre and xpost on the front and back of the format string.
   VG_(snprintf)(buf, 256, "%s%s%s", xpre, format, xpost);
   va_start(vargs, format);
   VG_(vmessage) ( Vg_UserMsg, buf, vargs );
   va_end(vargs);
   VG_(pp_ExeContext)( VG_(get_error_where)(err) );
}

static void mc_pp_Error ( Error* err )
{
   MC_Error* extra = VG_(get_error_extra)(err);

   switch (VG_(get_error_kind)(err)) {
      case Err_CoreMem: {
         /* What the hell *is* a CoreMemError? jrs 2005-May-18 */
         /* As of 2006-Dec-14, it's caused by unaddressable bytes in a
            signal handler frame.  --njn */
         mc_pp_msg("CoreMemError", err,
                   "%s contains unaddressable byte(s)", 
                   VG_(get_error_string)(err));
         break;
      } 
      
      case Err_Value:
         mc_pp_msg("UninitValue", err,
                   "Use of uninitialised value of size %d",
                   extra->Err.Value.szB);
         break;

      case Err_Cond:
         mc_pp_msg("UninitCondition", err,
                   "Conditional jump or move depends"
                   " on uninitialised value(s)");
         break;

      case Err_RegParam:
         mc_pp_msg("SyscallParam", err,
                   "Syscall param %s contains uninitialised byte(s)",
                   VG_(get_error_string)(err));
         break;

      case Err_MemParam:
         mc_pp_msg("SyscallParam", err,
                   "Syscall param %s points to %s byte(s)",
                   VG_(get_error_string)(err),
                   ( extra->Err.MemParam.isAddrErr 
                     ? "unaddressable" : "uninitialised" ));
         mc_pp_AddrInfo(VG_(get_error_address)(err),
                        &extra->Err.MemParam.ai, False);
         break;

      case Err_User:
         mc_pp_msg("ClientCheck", err,
                   "%s byte(s) found during client check request", 
                   ( extra->Err.User.isAddrErr
                     ? "Unaddressable" : "Uninitialised" ));
         mc_pp_AddrInfo(VG_(get_error_address)(err), &extra->Err.User.ai,
                        False);
         break;

      case Err_Free:
         mc_pp_msg("InvalidFree", err,
                   "Invalid free() / delete / delete[]");
         mc_pp_AddrInfo(VG_(get_error_address)(err),
                        &extra->Err.Free.ai, False);
         break;

      case Err_FreeMismatch:
         mc_pp_msg("MismatchedFree", err,
                   "Mismatched free() / delete / delete []");
         mc_pp_AddrInfo(VG_(get_error_address)(err),
                        &extra->Err.FreeMismatch.ai, False);
         break;

      case Err_Addr:
         if (extra->Err.Addr.isWrite) {
            mc_pp_msg("InvalidWrite", err,
                      "Invalid write of size %d", 
                      extra->Err.Addr.szB); 
         } else {
            mc_pp_msg("InvalidRead", err,
                      "Invalid read of size %d", 
                      extra->Err.Addr.szB); 
         }
         mc_pp_AddrInfo(VG_(get_error_address)(err), &extra->Err.Addr.ai,
                        extra->Err.Addr.maybe_gcc);
         break;

      case Err_Jump:
         mc_pp_msg("InvalidJump", err,
                   "Jump to the invalid address stated on the next line");
         mc_pp_AddrInfo(VG_(get_error_address)(err), &extra->Err.Jump.ai,
                        False);
         break;

      case Err_Overlap:
         if (extra->Err.Overlap.szB == 0)
            mc_pp_msg("Overlap", err,
                      "Source and destination overlap in %s(%p, %p)",
                      VG_(get_error_string)(err),
                      extra->Err.Overlap.dst, extra->Err.Overlap.src);
         else
            mc_pp_msg("Overlap", err,
                      "Source and destination overlap in %s(%p, %p, %d)",
                      VG_(get_error_string)(err),
                      extra->Err.Overlap.dst, extra->Err.Overlap.src,
                      extra->Err.Overlap.szB);
         break;

      case Err_IllegalMempool:
         mc_pp_msg("InvalidMemPool", err,
                   "Illegal memory pool address");
         mc_pp_AddrInfo(VG_(get_error_address)(err),
                        &extra->Err.IllegalMempool.ai, False);
         break;

      case Err_Leak: {
         HChar*      xpre  = VG_(clo_xml) ? "  <what>" : "";
         HChar*      xpost = VG_(clo_xml) ? "</what>"  : "";
         UInt        n_this_record   = extra->Err.Leak.n_this_record;
         UInt        n_total_records = extra->Err.Leak.n_total_records;
         LossRecord* l               = extra->Err.Leak.lossRecord;

         if (VG_(clo_xml)) {
            VG_(message)(Vg_UserMsg, "  <kind>%t</kind>",
                         xml_leak_kind(l->loss_mode));
         } else {
            VG_(message)(Vg_UserMsg, "");
         }

         if (l->indirect_bytes) {
            VG_(message)(Vg_UserMsg, 
               "%s%,lu (%,lu direct, %,lu indirect) bytes in %,u blocks"
               " are %s in loss record %,u of %,u%s",
               xpre,
               l->total_bytes + l->indirect_bytes, 
               l->total_bytes, l->indirect_bytes, l->num_blocks,
               str_leak_lossmode(l->loss_mode), n_this_record, n_total_records,
               xpost
            );
            if (VG_(clo_xml)) {
               // Nb: don't put commas in these XML numbers 
               VG_(message)(Vg_UserMsg, "  <leakedbytes>%lu</leakedbytes>", 
                                        l->total_bytes + l->indirect_bytes);
               VG_(message)(Vg_UserMsg, "  <leakedblocks>%u</leakedblocks>", 
                                        l->num_blocks);
            }
         } else {
            VG_(message)(
               Vg_UserMsg, 
               "%s%,lu bytes in %,u blocks are %s in loss record %,u of %,u%s",
               xpre,
               l->total_bytes, l->num_blocks,
               str_leak_lossmode(l->loss_mode), n_this_record, n_total_records,
               xpost
            );
            if (VG_(clo_xml)) {
               VG_(message)(Vg_UserMsg, "  <leakedbytes>%d</leakedbytes>", 
                                        l->total_bytes);
               VG_(message)(Vg_UserMsg, "  <leakedblocks>%d</leakedblocks>", 
                                        l->num_blocks);
            }
         }
         VG_(pp_ExeContext)(l->allocated_at);
         break;
      }

      default: 
         VG_(printf)("Error:\n  unknown Memcheck error code %d\n",
                     VG_(get_error_kind)(err));
         VG_(tool_panic)("unknown error code in mc_pp_Error)");
   }
}

/*------------------------------------------------------------*/
/*--- Recording errors                                     ---*/
/*------------------------------------------------------------*/

/* These many bytes below %ESP are considered addressible if we're
   doing the --workaround-gcc296-bugs hack. */
#define VG_GCC296_BUG_STACK_SLOP 1024

/* Is this address within some small distance below %ESP?  Used only
   for the --workaround-gcc296-bugs kludge. */
static Bool is_just_below_ESP( Addr esp, Addr aa )
{
   if (esp > aa && (esp - aa) <= VG_GCC296_BUG_STACK_SLOP)
      return True;
   else
      return False;
}

/* --- Called from generated and non-generated code --- */

static void mc_record_address_error ( ThreadId tid, Addr a, Int szB,
                                      Bool isWrite )
{
   MC_Error extra;
   Bool     just_below_esp;

   if (in_ignored_range(a)) 
      return;

#  if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   /* AIX zero-page handling.  On AIX, reads from page zero are,
      bizarrely enough, legitimate.  Writes to page zero aren't,
      though.  Since memcheck can't distinguish reads from writes, the
      best we can do is to 'act normal' and mark the A bits in the
      normal way as noaccess, but then hide any reads from that page
      that get reported here. */
   if ((!isWrite) && a >= 0 && a < 4096 && a+szB <= 4096) 
      return;

   /* Appalling AIX hack.  It suppresses reads done by glink
      fragments.  Getting rid of this would require figuring out
      somehow where the referenced data areas are (and their
      sizes). */
   if ((!isWrite) && szB == sizeof(Word)) { 
      UInt i1, i2;
      UInt* pc = (UInt*)VG_(get_IP)(tid);
      if (sizeof(Word) == 4) {
         i1 = 0x800c0000; /* lwz r0,0(r12) */
         i2 = 0x804c0004; /* lwz r2,4(r12) */
      } else {
         i1 = 0xe80c0000; /* ld  r0,0(r12) */
         i2 = 0xe84c0008; /* ld  r2,8(r12) */
      }
      if (pc[0] == i1 && pc[1] == i2) return;
      if (pc[0] == i2 && pc[-1] == i1) return;
   }
#  endif

   just_below_esp = is_just_below_ESP( VG_(get_SP)(tid), a );

   /* If this is caused by an access immediately below %ESP, and the
      user asks nicely, we just ignore it. */
   if (MC_(clo_workaround_gcc296_bugs) && just_below_esp)
      return;

   extra.Err.Addr.isWrite   = isWrite;
   extra.Err.Addr.szB       = szB;
   extra.Err.Addr.maybe_gcc = just_below_esp;
   extra.Err.Addr.ai.tag    = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_Addr, a, /*s*/NULL, &extra );
}

static void mc_record_value_error ( ThreadId tid, Int szB )
{
   MC_Error extra;
   tl_assert(MC_(clo_undef_value_errors));
   extra.Err.Value.szB = szB;
   VG_(maybe_record_error)( tid, Err_Value, /*addr*/0, /*s*/NULL, &extra );
}

static void mc_record_cond_error ( ThreadId tid )
{
   tl_assert(MC_(clo_undef_value_errors));
   VG_(maybe_record_error)( tid, Err_Cond, /*addr*/0, /*s*/NULL, /*extra*/NULL);
}

/* --- Called from non-generated code --- */

/* This is for memory errors in pthread functions, as opposed to pthread API
   errors which are found by the core. */
static void mc_record_core_mem_error ( ThreadId tid, Bool isAddrErr, Char* msg )
{
   VG_(maybe_record_error)( tid, Err_CoreMem, /*addr*/0, msg, /*extra*/NULL );
}

static void mc_record_regparam_error ( ThreadId tid, Char* msg )
{
   tl_assert(VG_INVALID_THREADID != tid);
   VG_(maybe_record_error)( tid, Err_RegParam, /*addr*/0, msg, /*extra*/NULL );
}

static void mc_record_memparam_error ( ThreadId tid, Addr a, 
                                       Bool isAddrErr, Char* msg )
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   if (!isAddrErr) 
      tl_assert(MC_(clo_undef_value_errors));
   extra.Err.MemParam.isAddrErr = isAddrErr;
   extra.Err.MemParam.ai.tag    = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_MemParam, a, msg, &extra );
}

static void mc_record_jump_error ( ThreadId tid, Addr a )
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.Jump.ai.tag = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_Jump, a, /*s*/NULL, &extra );
}

void MC_(record_free_error) ( ThreadId tid, Addr a ) 
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.Free.ai.tag = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_Free, a, /*s*/NULL, &extra );
}

void MC_(record_freemismatch_error) ( ThreadId tid, MC_Chunk* mc )
{
   MC_Error extra;
   AddrInfo* ai = &extra.Err.FreeMismatch.ai;
   tl_assert(VG_INVALID_THREADID != tid);
   ai->tag = Addr_Block;
   ai->Addr.Block.block_kind = Block_Mallocd;  // Nb: Not 'Block_Freed'
   ai->Addr.Block.block_desc = "block";
   ai->Addr.Block.block_szB  = mc->szB;
   ai->Addr.Block.rwoffset   = 0;
   ai->Addr.Block.lastchange = mc->where;
   VG_(maybe_record_error)( tid, Err_FreeMismatch, mc->data, /*s*/NULL,
                            &extra );
}

void MC_(record_illegal_mempool_error) ( ThreadId tid, Addr a ) 
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.IllegalMempool.ai.tag = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_IllegalMempool, a, /*s*/NULL, &extra );
}

static void mc_record_overlap_error ( ThreadId tid, Char* function,
                                      Addr src, Addr dst, SizeT szB )
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.Overlap.src = src;
   extra.Err.Overlap.dst = dst;
   extra.Err.Overlap.szB = szB;
   VG_(maybe_record_error)( 
      tid, Err_Overlap, /*addr*/0, /*s*/function, &extra );
}

Bool MC_(record_leak_error) ( ThreadId tid, UInt n_this_record,
                              UInt n_total_records, LossRecord* lossRecord,
                              Bool print_record )
{
   MC_Error extra;
   extra.Err.Leak.n_this_record   = n_this_record;
   extra.Err.Leak.n_total_records = n_total_records;
   extra.Err.Leak.lossRecord      = lossRecord;
   return
   VG_(unique_error) ( tid, Err_Leak, /*Addr*/0, /*s*/NULL, &extra,
                       lossRecord->allocated_at, print_record,
                       /*allow_GDB_attach*/False, /*count_error*/False );
}

static void mc_record_user_error ( ThreadId tid, Addr a, Bool isAddrErr )
{
   MC_Error extra;

   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.User.isAddrErr = isAddrErr;
   extra.Err.User.ai.tag    = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_User, a, /*s*/NULL, &extra );
}

/*------------------------------------------------------------*/
/*--- Other error operations                               ---*/
/*------------------------------------------------------------*/

/* Compare error contexts, to detect duplicates.  Note that if they
   are otherwise the same, the faulting addrs and associated rwoffsets
   are allowed to be different.  */
static Bool mc_eq_Error ( VgRes res, Error* e1, Error* e2 )
{
   MC_Error* extra1 = VG_(get_error_extra)(e1);
   MC_Error* extra2 = VG_(get_error_extra)(e2);

   /* Guaranteed by calling function */
   tl_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));
   
   switch (VG_(get_error_kind)(e1)) {
      case Err_CoreMem: {
         Char *e1s, *e2s;
         e1s = VG_(get_error_string)(e1);
         e2s = VG_(get_error_string)(e2);
         if (e1s == e2s)                   return True;
         if (VG_STREQ(e1s, e2s))           return True;
         return False;
      }

      case Err_RegParam:
         return VG_STREQ(VG_(get_error_string)(e1), VG_(get_error_string)(e2));

      // Perhaps we should also check the addrinfo.akinds for equality.
      // That would result in more error reports, but only in cases where
      // a register contains uninitialised bytes and points to memory
      // containing uninitialised bytes.  Currently, the 2nd of those to be
      // detected won't be reported.  That is (nearly?) always the memory
      // error, which is good.
      case Err_MemParam:
         if (!VG_STREQ(VG_(get_error_string)(e1),
                       VG_(get_error_string)(e2))) return False;
         // fall through
      case Err_User:
         return ( extra1->Err.User.isAddrErr == extra2->Err.User.isAddrErr
                ? True : False );

      case Err_Free:
      case Err_FreeMismatch:
      case Err_Jump:
      case Err_IllegalMempool:
      case Err_Overlap:
      case Err_Cond:
         return True;

      case Err_Addr:
         return ( extra1->Err.Addr.szB == extra2->Err.Addr.szB
                ? True : False );

      case Err_Value:
         return ( extra1->Err.Value.szB == extra2->Err.Value.szB
                ? True : False );

      case Err_Leak:
         VG_(tool_panic)("Shouldn't get Err_Leak in mc_eq_Error,\n"
                         "since it's handled with VG_(unique_error)()!");

      default: 
         VG_(printf)("Error:\n  unknown error code %d\n",
                     VG_(get_error_kind)(e1));
         VG_(tool_panic)("unknown error code in mc_eq_Error");
   }
}

/* Function used when searching MC_Chunk lists */
static Bool addr_is_in_MC_Chunk(MC_Chunk* mc, Addr a)
{
   // Nb: this is not quite right!  It assumes that the heap block has
   // a redzone of size MC_MALLOC_REDZONE_SZB.  That's true for malloc'd
   // blocks, but not necessarily true for custom-alloc'd blocks.  So
   // in some cases this could result in an incorrect description (eg.
   // saying "12 bytes after block A" when really it's within block B.
   // Fixing would require adding redzone size to MC_Chunks, though.
   return VG_(addr_is_in_block)( a, mc->data, mc->szB,
                                 MC_MALLOC_REDZONE_SZB );
}

// Forward declaration
static Bool client_perm_maybe_describe( Addr a, AddrInfo* ai );


/* Describe an address as best you can, for error messages,
   putting the result in ai. */
static void describe_addr ( Addr a, AddrInfo* ai )
{
   MC_Chunk* mc;
   ThreadId  tid;
   Addr      stack_min, stack_max;

   tl_assert(Addr_Undescribed == ai->tag);

   /* Perhaps it's a user-def'd block? */
   if (client_perm_maybe_describe( a, ai ))
      return;

   /* Perhaps it's on a thread's stack? */
   VG_(thread_stack_reset_iter)();
   while ( VG_(thread_stack_next)(&tid, &stack_min, &stack_max) ) {
      if (stack_min <= a && a <= stack_max) {
         ai->tag            = Addr_Stack;
         ai->Addr.Stack.tid = tid;
         return;
      }
   }
   /* Search for a recently freed block which might bracket it. */
   mc = MC_(get_freed_list_head)();
   while (mc) {
      if (addr_is_in_MC_Chunk(mc, a)) {
         ai->tag = Addr_Block;
         ai->Addr.Block.block_kind = Block_Freed;
         ai->Addr.Block.block_desc = "block";
         ai->Addr.Block.block_szB  = mc->szB;
         ai->Addr.Block.rwoffset   = (Int)a - (Int)mc->data;
         ai->Addr.Block.lastchange = mc->where;
         return;
      }
      mc = mc->next; 
   }
   /* Search for a currently malloc'd block which might bracket it. */
   VG_(HT_ResetIter)(MC_(malloc_list));
   while ( (mc = VG_(HT_Next)(MC_(malloc_list))) ) {
      if (addr_is_in_MC_Chunk(mc, a)) {
         ai->tag = Addr_Block;
         ai->Addr.Block.block_kind = Block_Mallocd;
         ai->Addr.Block.block_desc = "block";
         ai->Addr.Block.block_szB  = mc->szB;
         ai->Addr.Block.rwoffset   = (Int)a - (Int)mc->data;
         ai->Addr.Block.lastchange = mc->where;
         return;
      }
   }
   /* Clueless ... */
   ai->tag = Addr_Unknown;
   return;
}

/* Updates the copy with address info if necessary (but not for all errors). */
static UInt mc_update_extra( Error* err )
{
   MC_Error* extra = VG_(get_error_extra)(err);

   switch (VG_(get_error_kind)(err)) {
   // These ones don't have addresses associated with them, and so don't
   // need any updating.
   case Err_CoreMem:
   case Err_Value:
   case Err_Cond:
   case Err_Overlap:
   case Err_RegParam:
   // For Err_Leaks the returned size does not matter -- they are always
   // shown with VG_(unique_error)() so they 'extra' not copied.  But we make it
   // consistent with the others.
   case Err_Leak:
      return sizeof(MC_Error);

   // These ones always involve a memory address.
   case Err_Addr:
      describe_addr ( VG_(get_error_address)(err), &extra->Err.Addr.ai );
      return sizeof(MC_Error);
   case Err_MemParam:
      describe_addr ( VG_(get_error_address)(err), &extra->Err.MemParam.ai );
      return sizeof(MC_Error);
   case Err_Jump:
      describe_addr ( VG_(get_error_address)(err), &extra->Err.Jump.ai );
      return sizeof(MC_Error);
   case Err_User:
      describe_addr ( VG_(get_error_address)(err), &extra->Err.User.ai );
      return sizeof(MC_Error);
   case Err_Free:
      describe_addr ( VG_(get_error_address)(err), &extra->Err.Free.ai );
      return sizeof(MC_Error);
   case Err_IllegalMempool:
      describe_addr ( VG_(get_error_address)(err),
                      &extra->Err.IllegalMempool.ai );
      return sizeof(MC_Error);

   // Err_FreeMismatches have already had their address described;  this is
   // possible because we have the MC_Chunk on hand when the error is
   // detected.  However, the address may be part of a user block, and if so
   // we override the pre-determined description with a user block one.
   case Err_FreeMismatch: {
      tl_assert(extra && Block_Mallocd ==
                extra->Err.FreeMismatch.ai.Addr.Block.block_kind);
      (void)client_perm_maybe_describe( VG_(get_error_address)(err), 
                                        &extra->Err.FreeMismatch.ai );
      return sizeof(MC_Error);
   }

   default: VG_(tool_panic)("mc_update_extra: bad errkind");
   }
}

/*------------------------------------------------------------*/
/*--- Suppressions                                         ---*/
/*------------------------------------------------------------*/

typedef 
   enum { 
      ParamSupp,     // Bad syscall params
      UserSupp,      // Errors arising from client-request checks
      CoreMemSupp,   // Memory errors in core (pthread ops, signal handling)

      // Undefined value errors of given size
      Value1Supp, Value2Supp, Value4Supp, Value8Supp, Value16Supp,

      // Undefined value error in conditional.
      CondSupp,

      // Unaddressable read/write attempt at given size
      Addr1Supp, Addr2Supp, Addr4Supp, Addr8Supp, Addr16Supp,

      JumpSupp,      // Jump to unaddressable target
      FreeSupp,      // Invalid or mismatching free
      OverlapSupp,   // Overlapping blocks in memcpy(), strcpy(), etc
      LeakSupp,      // Something to be suppressed in a leak check.
      MempoolSupp,   // Memory pool suppression.
   } 
   MC_SuppKind;

static Bool mc_recognised_suppression ( Char* name, Supp* su )
{
   SuppKind skind;

   if      (VG_STREQ(name, "Param"))   skind = ParamSupp;
   else if (VG_STREQ(name, "User"))    skind = UserSupp;
   else if (VG_STREQ(name, "CoreMem")) skind = CoreMemSupp;
   else if (VG_STREQ(name, "Addr1"))   skind = Addr1Supp;
   else if (VG_STREQ(name, "Addr2"))   skind = Addr2Supp;
   else if (VG_STREQ(name, "Addr4"))   skind = Addr4Supp;
   else if (VG_STREQ(name, "Addr8"))   skind = Addr8Supp;
   else if (VG_STREQ(name, "Addr16"))  skind = Addr16Supp;
   else if (VG_STREQ(name, "Jump"))    skind = JumpSupp;
   else if (VG_STREQ(name, "Free"))    skind = FreeSupp;
   else if (VG_STREQ(name, "Leak"))    skind = LeakSupp;
   else if (VG_STREQ(name, "Overlap")) skind = OverlapSupp;
   else if (VG_STREQ(name, "Mempool")) skind = MempoolSupp;
   else if (VG_STREQ(name, "Cond"))    skind = CondSupp;
   else if (VG_STREQ(name, "Value0"))  skind = CondSupp; /* backwards compat */
   else if (VG_STREQ(name, "Value1"))  skind = Value1Supp;
   else if (VG_STREQ(name, "Value2"))  skind = Value2Supp;
   else if (VG_STREQ(name, "Value4"))  skind = Value4Supp;
   else if (VG_STREQ(name, "Value8"))  skind = Value8Supp;
   else if (VG_STREQ(name, "Value16")) skind = Value16Supp;
   else 
      return False;

   VG_(set_supp_kind)(su, skind);
   return True;
}

static 
Bool mc_read_extra_suppression_info ( Int fd, Char* buf, Int nBuf, Supp *su )
{
   Bool eof;

   if (VG_(get_supp_kind)(su) == ParamSupp) {
      eof = VG_(get_line) ( fd, buf, nBuf );
      if (eof) return False;
      VG_(set_supp_string)(su, VG_(strdup)(buf));
   }
   return True;
}

static Bool mc_error_matches_suppression(Error* err, Supp* su)
{
   Int       su_szB;
   MC_Error* extra = VG_(get_error_extra)(err);
   ErrorKind ekind = VG_(get_error_kind )(err);

   switch (VG_(get_supp_kind)(su)) {
      case ParamSupp:
         return ((ekind == Err_RegParam || ekind == Err_MemParam)
              && VG_STREQ(VG_(get_error_string)(err), 
                          VG_(get_supp_string)(su)));

      case UserSupp:
         return (ekind == Err_User);

      case CoreMemSupp:
         return (ekind == Err_CoreMem
              && VG_STREQ(VG_(get_error_string)(err),
                          VG_(get_supp_string)(su)));

      case Value1Supp: su_szB = 1; goto value_case;
      case Value2Supp: su_szB = 2; goto value_case;
      case Value4Supp: su_szB = 4; goto value_case;
      case Value8Supp: su_szB = 8; goto value_case;
      case Value16Supp:su_szB =16; goto value_case;
      value_case:
         return (ekind == Err_Value && extra->Err.Value.szB == su_szB);

      case CondSupp:
         return (ekind == Err_Cond);

      case Addr1Supp: su_szB = 1; goto addr_case;
      case Addr2Supp: su_szB = 2; goto addr_case;
      case Addr4Supp: su_szB = 4; goto addr_case;
      case Addr8Supp: su_szB = 8; goto addr_case;
      case Addr16Supp:su_szB =16; goto addr_case;
      addr_case:
         return (ekind == Err_Addr && extra->Err.Addr.szB == su_szB);

      case JumpSupp:
         return (ekind == Err_Jump);

      case FreeSupp:
         return (ekind == Err_Free || ekind == Err_FreeMismatch);

      case OverlapSupp:
         return (ekind == Err_Overlap);

      case LeakSupp:
         return (ekind == Err_Leak);

      case MempoolSupp:
         return (ekind == Err_IllegalMempool);

      default:
         VG_(printf)("Error:\n"
                     "  unknown suppression type %d\n",
                     VG_(get_supp_kind)(su));
         VG_(tool_panic)("unknown suppression type in "
                         "MC_(error_matches_suppression)");
   }
}

static Char* mc_get_error_name ( Error* err )
{
   switch (VG_(get_error_kind)(err)) {
   case Err_RegParam:       return "Param";
   case Err_MemParam:       return "Param";
   case Err_User:           return "User";
   case Err_FreeMismatch:   return "Free";
   case Err_IllegalMempool: return "Mempool";
   case Err_Free:           return "Free";
   case Err_Jump:           return "Jump";
   case Err_CoreMem:        return "CoreMem";
   case Err_Overlap:        return "Overlap";
   case Err_Leak:           return "Leak";
   case Err_Cond:           return "Cond";
   case Err_Addr: {
      MC_Error* extra = VG_(get_error_extra)(err);
      switch ( extra->Err.Addr.szB ) {
      case 1:               return "Addr1";
      case 2:               return "Addr2";
      case 4:               return "Addr4";
      case 8:               return "Addr8";
      case 16:              return "Addr16";
      default:              VG_(tool_panic)("unexpected size for Addr");
      }
   }
   case Err_Value: {
      MC_Error* extra = VG_(get_error_extra)(err);
      switch ( extra->Err.Value.szB ) {
      case 1:               return "Value1";
      case 2:               return "Value2";
      case 4:               return "Value4";
      case 8:               return "Value8";
      case 16:              return "Value16";
      default:              VG_(tool_panic)("unexpected size for Value");
      }
   }
   default:                 VG_(tool_panic)("get_error_name: unexpected type");
   }
}

static void mc_print_extra_suppression_info ( Error* err )
{
   ErrorKind ekind = VG_(get_error_kind )(err);
   if (Err_RegParam == ekind || Err_MemParam == ekind) {
      VG_(printf)("   %s\n", VG_(get_error_string)(err));
   }
}

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

/* If any part of '_a' indicated by the mask is 1, either
   '_a' is not naturally '_sz/8'-aligned, or it exceeds the range
   covered by the primary map. */
#define UNALIGNED_OR_HIGH(_a,_sz)   ((_a) & MASK((_sz>>3)))
#define MASK(_sz)   ( ~((0x10000-(_sz)) | ((N_PRIMARY_MAP-1) << 16)) )


/* ------------------------ Size = 8 ------------------------ */

static INLINE
ULong mc_LOADV64 ( Addr a, Bool isBigEndian )
{
   UWord   sm_off16, vabits16;
   SecMap* sm;

   PROF_EVENT(200, "mc_LOADV64");

#ifndef PERF_FAST_LOADV
   return mc_LOADVn_slow( a, 64, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,64) )) {
      PROF_EVENT(201, "mc_LOADV64-slow1");
      return (ULong)mc_LOADVn_slow( a, 64, isBigEndian );
   }

   sm       = get_secmap_for_reading_low(a);
   sm_off16 = SM_OFF_16(a);
   vabits16 = ((UShort*)(sm->vabits8))[sm_off16];

   // Handle common case quickly: a is suitably aligned, is mapped, and
   // addressible.
   // Convert V bits from compact memory form to expanded register form.
   if (EXPECTED_TAKEN(vabits16 == VA_BITS16_DEFINED)) {
      return V_BITS64_DEFINED;
   } else if (EXPECTED_TAKEN(vabits16 == VA_BITS16_UNDEFINED)) {
      return V_BITS64_UNDEFINED;
   } else {
      /* Slow case: the 8 bytes are not all-defined or all-undefined. */
      PROF_EVENT(202, "mc_LOADV64-slow2");
      return mc_LOADVn_slow( a, 64, isBigEndian );
   }
#endif
}

VG_REGPARM(1) ULong MC_(helperc_LOADV64be) ( Addr a )
{
   return mc_LOADV64(a, True);
}
VG_REGPARM(1) ULong MC_(helperc_LOADV64le) ( Addr a )
{
   return mc_LOADV64(a, False);
}


static INLINE
void mc_STOREV64 ( Addr a, ULong vbits64, Bool isBigEndian )
{
   UWord   sm_off16, vabits16;
   SecMap* sm;

   PROF_EVENT(210, "mc_STOREV64");

#ifndef PERF_FAST_STOREV
   // XXX: this slow case seems to be marginally faster than the fast case!
   // Investigate further.
   mc_STOREVn_slow( a, 64, vbits64, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,64) )) {
      PROF_EVENT(211, "mc_STOREV64-slow1");
      mc_STOREVn_slow( a, 64, vbits64, isBigEndian );
      return;
   }

   sm       = get_secmap_for_reading_low(a);
   sm_off16 = SM_OFF_16(a);
   vabits16 = ((UShort*)(sm->vabits8))[sm_off16];

   if (EXPECTED_TAKEN( !is_distinguished_sm(sm) && 
                       (VA_BITS16_DEFINED   == vabits16 ||
                        VA_BITS16_UNDEFINED == vabits16) ))
   {
      /* Handle common case quickly: a is suitably aligned, */
      /* is mapped, and is addressible. */
      // Convert full V-bits in register to compact 2-bit form.
      if (V_BITS64_DEFINED == vbits64) {
         ((UShort*)(sm->vabits8))[sm_off16] = (UShort)VA_BITS16_DEFINED;
      } else if (V_BITS64_UNDEFINED == vbits64) {
         ((UShort*)(sm->vabits8))[sm_off16] = (UShort)VA_BITS16_UNDEFINED;
      } else {
         /* Slow but general case -- writing partially defined bytes. */
         PROF_EVENT(212, "mc_STOREV64-slow2");
         mc_STOREVn_slow( a, 64, vbits64, isBigEndian );
      }
   } else {
      /* Slow but general case. */
      PROF_EVENT(213, "mc_STOREV64-slow3");
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


/* ------------------------ Size = 4 ------------------------ */

static INLINE
UWord mc_LOADV32 ( Addr a, Bool isBigEndian )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(220, "mc_LOADV32");

#ifndef PERF_FAST_LOADV
   return (UWord)mc_LOADVn_slow( a, 32, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,32) )) {
      PROF_EVENT(221, "mc_LOADV32-slow1");
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
   if (EXPECTED_TAKEN(vabits8 == VA_BITS8_DEFINED)) {
      return ((UWord)0xFFFFFFFF00000000ULL | (UWord)V_BITS32_DEFINED);
   } else if (EXPECTED_TAKEN(vabits8 == VA_BITS8_UNDEFINED)) {
      return ((UWord)0xFFFFFFFF00000000ULL | (UWord)V_BITS32_UNDEFINED);
   } else {
      /* Slow case: the 4 bytes are not all-defined or all-undefined. */
      PROF_EVENT(222, "mc_LOADV32-slow2");
      return (UWord)mc_LOADVn_slow( a, 32, isBigEndian );
   }
#endif
}

VG_REGPARM(1) UWord MC_(helperc_LOADV32be) ( Addr a )
{
   return mc_LOADV32(a, True);
}
VG_REGPARM(1) UWord MC_(helperc_LOADV32le) ( Addr a )
{
   return mc_LOADV32(a, False);
}


static INLINE
void mc_STOREV32 ( Addr a, UWord vbits32, Bool isBigEndian )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(230, "mc_STOREV32");

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,32) )) {
      PROF_EVENT(231, "mc_STOREV32-slow1");
      mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
      return;
   }

   sm      = get_secmap_for_reading_low(a);
   sm_off  = SM_OFF(a);
   vabits8 = sm->vabits8[sm_off];

//---------------------------------------------------------------------------
#if 1
   // Cleverness:  sometimes we don't have to write the shadow memory at
   // all, if we can tell that what we want to write is the same as what is
   // already there.
   if (V_BITS32_DEFINED == vbits32) {
      if (vabits8 == (UInt)VA_BITS8_DEFINED) {
         return;
      } else if (!is_distinguished_sm(sm) && VA_BITS8_UNDEFINED == vabits8) {
         sm->vabits8[sm_off] = (UInt)VA_BITS8_DEFINED;
      } else {
         // not defined/undefined, or distinguished and changing state
         PROF_EVENT(232, "mc_STOREV32-slow2");
         mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
      }
   } else if (V_BITS32_UNDEFINED == vbits32) {
      if (vabits8 == (UInt)VA_BITS8_UNDEFINED) {
         return;
      } else if (!is_distinguished_sm(sm) && VA_BITS8_DEFINED == vabits8) {
         sm->vabits8[sm_off] = (UInt)VA_BITS8_UNDEFINED;
      } else {
         // not defined/undefined, or distinguished and changing state
         PROF_EVENT(233, "mc_STOREV32-slow3");
         mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
      }
   } else {
      // Partially defined word
      PROF_EVENT(234, "mc_STOREV32-slow4");
      mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
   }
//---------------------------------------------------------------------------
#else
   if (EXPECTED_TAKEN( !is_distinguished_sm(sm) && 
                       (VA_BITS8_DEFINED   == vabits8 ||
                        VA_BITS8_UNDEFINED == vabits8) ))
   {
      /* Handle common case quickly: a is suitably aligned, */
      /* is mapped, and is addressible. */
      // Convert full V-bits in register to compact 2-bit form.
      if (V_BITS32_DEFINED == vbits32) {
         sm->vabits8[sm_off] = VA_BITS8_DEFINED;
      } else if (V_BITS32_UNDEFINED == vbits32) {
         sm->vabits8[sm_off] = VA_BITS8_UNDEFINED;
      } else {
         /* Slow but general case -- writing partially defined bytes. */
         PROF_EVENT(232, "mc_STOREV32-slow2");
         mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
      }
   } else {
      /* Slow but general case. */
      PROF_EVENT(233, "mc_STOREV32-slow3");
      mc_STOREVn_slow( a, 32, (ULong)vbits32, isBigEndian );
   }
#endif
//---------------------------------------------------------------------------
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


/* ------------------------ Size = 2 ------------------------ */

static INLINE
UWord mc_LOADV16 ( Addr a, Bool isBigEndian )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(240, "mc_LOADV16");

#ifndef PERF_FAST_LOADV
   return (UWord)mc_LOADVn_slow( a, 16, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,16) )) {
      PROF_EVENT(241, "mc_LOADV16-slow1");
      return (UWord)mc_LOADVn_slow( a, 16, isBigEndian );
   }

   sm      = get_secmap_for_reading_low(a);
   sm_off  = SM_OFF(a);
   vabits8 = sm->vabits8[sm_off];
   // Handle common case quickly: a is suitably aligned, is mapped, and is
   // addressible.
   // Convert V bits from compact memory form to expanded register form
   if      (vabits8 == VA_BITS8_DEFINED  ) { return V_BITS16_DEFINED;   }
   else if (vabits8 == VA_BITS8_UNDEFINED) { return V_BITS16_UNDEFINED; }
   else {
      // The 4 (yes, 4) bytes are not all-defined or all-undefined, check
      // the two sub-bytes.
      UChar vabits4 = extract_vabits4_from_vabits8(a, vabits8);
      if      (vabits4 == VA_BITS4_DEFINED  ) { return V_BITS16_DEFINED;   }
      else if (vabits4 == VA_BITS4_UNDEFINED) { return V_BITS16_UNDEFINED; }
      else {
         /* Slow case: the two bytes are not all-defined or all-undefined. */
         PROF_EVENT(242, "mc_LOADV16-slow2");
         return (UWord)mc_LOADVn_slow( a, 16, isBigEndian );
      }
   }
#endif
}

VG_REGPARM(1) UWord MC_(helperc_LOADV16be) ( Addr a )
{
   return mc_LOADV16(a, True);
}
VG_REGPARM(1) UWord MC_(helperc_LOADV16le) ( Addr a )
{
   return mc_LOADV16(a, False);
}


static INLINE
void mc_STOREV16 ( Addr a, UWord vbits16, Bool isBigEndian )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(250, "mc_STOREV16");

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 16, (ULong)vbits16, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,16) )) {
      PROF_EVENT(251, "mc_STOREV16-slow1");
      mc_STOREVn_slow( a, 16, (ULong)vbits16, isBigEndian );
      return;
   }

   sm      = get_secmap_for_reading_low(a);
   sm_off  = SM_OFF(a);
   vabits8 = sm->vabits8[sm_off];
   if (EXPECTED_TAKEN( !is_distinguished_sm(sm) && 
                       (VA_BITS8_DEFINED   == vabits8 ||
                        VA_BITS8_UNDEFINED == vabits8) ))
   {
      /* Handle common case quickly: a is suitably aligned, */
      /* is mapped, and is addressible. */
      // Convert full V-bits in register to compact 2-bit form.
      if (V_BITS16_DEFINED == vbits16) {
         insert_vabits4_into_vabits8( a, VA_BITS4_DEFINED ,
                                      &(sm->vabits8[sm_off]) );
      } else if (V_BITS16_UNDEFINED == vbits16) {
         insert_vabits4_into_vabits8( a, VA_BITS4_UNDEFINED,
                                      &(sm->vabits8[sm_off]) );
      } else {
         /* Slow but general case -- writing partially defined bytes. */
         PROF_EVENT(252, "mc_STOREV16-slow2");
         mc_STOREVn_slow( a, 16, (ULong)vbits16, isBigEndian );
      }
   } else {
      /* Slow but general case. */
      PROF_EVENT(253, "mc_STOREV16-slow3");
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


/* ------------------------ Size = 1 ------------------------ */
/* Note: endianness is irrelevant for size == 1 */

VG_REGPARM(1)
UWord MC_(helperc_LOADV8) ( Addr a )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(260, "mc_LOADV8");

#ifndef PERF_FAST_LOADV
   return (UWord)mc_LOADVn_slow( a, 8, False/*irrelevant*/ );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,8) )) {
      PROF_EVENT(261, "mc_LOADV8-slow1");
      return (UWord)mc_LOADVn_slow( a, 8, False/*irrelevant*/ );
   }

   sm      = get_secmap_for_reading_low(a);
   sm_off  = SM_OFF(a);
   vabits8 = sm->vabits8[sm_off];
   // Convert V bits from compact memory form to expanded register form
   // Handle common case quickly: a is mapped, and the entire
   // word32 it lives in is addressible.
   if      (vabits8 == VA_BITS8_DEFINED  ) { return V_BITS8_DEFINED;   }
   else if (vabits8 == VA_BITS8_UNDEFINED) { return V_BITS8_UNDEFINED; }
   else {
      // The 4 (yes, 4) bytes are not all-defined or all-undefined, check
      // the single byte.
      UChar vabits2 = extract_vabits2_from_vabits8(a, vabits8);
      if      (vabits2 == VA_BITS2_DEFINED  ) { return V_BITS8_DEFINED;   }
      else if (vabits2 == VA_BITS2_UNDEFINED) { return V_BITS8_UNDEFINED; }
      else {
         /* Slow case: the byte is not all-defined or all-undefined. */
         PROF_EVENT(262, "mc_LOADV8-slow2");
         return (UWord)mc_LOADVn_slow( a, 8, False/*irrelevant*/ );
      }
   }
#endif
}


VG_REGPARM(2)
void MC_(helperc_STOREV8) ( Addr a, UWord vbits8 )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(270, "mc_STOREV8");

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,8) )) {
      PROF_EVENT(271, "mc_STOREV8-slow1");
      mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
      return;
   }

   sm      = get_secmap_for_reading_low(a);
   sm_off  = SM_OFF(a);
   vabits8 = sm->vabits8[sm_off];
   if (EXPECTED_TAKEN
         ( !is_distinguished_sm(sm) &&
           ( (VA_BITS8_DEFINED == vabits8 || VA_BITS8_UNDEFINED == vabits8)
          || (VA_BITS2_NOACCESS != extract_vabits2_from_vabits8(a, vabits8))
           )
         )
      )
   {
      /* Handle common case quickly: a is mapped, the entire word32 it
         lives in is addressible. */
      // Convert full V-bits in register to compact 2-bit form.
      if (V_BITS8_DEFINED == vbits8) {
         insert_vabits2_into_vabits8( a, VA_BITS2_DEFINED,
                                       &(sm->vabits8[sm_off]) );
      } else if (V_BITS8_UNDEFINED == vbits8) {
         insert_vabits2_into_vabits8( a, VA_BITS2_UNDEFINED,
                                       &(sm->vabits8[sm_off]) );
      } else {
         /* Slow but general case -- writing partially defined bytes. */
         PROF_EVENT(272, "mc_STOREV8-slow2");
         mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
      }
   } else {
      /* Slow but general case. */
      PROF_EVENT(273, "mc_STOREV8-slow3");
      mc_STOREVn_slow( a, 8, (ULong)vbits8, False/*irrelevant*/ );
   }
#endif
}


/*------------------------------------------------------------*/
/*--- Functions called directly from generated code:       ---*/
/*--- Value-check failure handlers.                        ---*/
/*------------------------------------------------------------*/

void MC_(helperc_value_check0_fail) ( void )
{
   mc_record_cond_error ( VG_(get_running_tid)() );
}

void MC_(helperc_value_check1_fail) ( void )
{
   mc_record_value_error ( VG_(get_running_tid)(), 1 );
}

void MC_(helperc_value_check4_fail) ( void )
{
   mc_record_value_error ( VG_(get_running_tid)(), 4 );
}

void MC_(helperc_value_check8_fail) ( void )
{
   mc_record_value_error ( VG_(get_running_tid)(), 8 );
}

VG_REGPARM(1) void MC_(helperc_complain_undef) ( HWord sz )
{
   mc_record_value_error ( VG_(get_running_tid)(), (Int)sz );
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
   ThreadId tid,
   Addr a, 
   Addr vbits, 
   SizeT szB, 
   Bool setting /* True <=> set vbits,  False <=> get vbits */ 
)
{
   SizeT i;
   Bool  ok;
   UChar vbits8;

   /* Check that arrays are addressible before doing any getting/setting. */
   for (i = 0; i < szB; i++) {
      if (VA_BITS2_NOACCESS == get_vabits2(a + i) ||
          VA_BITS2_NOACCESS == get_vabits2(vbits + i)) {
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
static
Bool mc_is_within_valid_secondary ( Addr a )
{
   SecMap* sm = maybe_get_secmap_for ( a );
   if (sm == NULL || sm == &sm_distinguished[SM_DIST_NOACCESS]
       || in_ignored_range(a)) {
      /* Definitely not in use. */
      return False;
   } else {
      return True;
   }
}


/* For the memory leak detector, say whether or not a given word
   address is to be regarded as valid. */
static
Bool mc_is_valid_aligned_word ( Addr a )
{
   tl_assert(sizeof(UWord) == 4 || sizeof(UWord) == 8);
   if (sizeof(UWord) == 4) {
      tl_assert(VG_IS_4_ALIGNED(a));
   } else {
      tl_assert(VG_IS_8_ALIGNED(a));
   }
   if (is_mem_defined( a, sizeof(UWord), NULL ) == MC_Ok
       && !in_ignored_range(a)) {
      return True;
   } else {
      return False;
   }
}


/* Leak detector for this tool.  We don't actually do anything, merely
   run the generic leak detector with suitable parameters for this
   tool. */
static void mc_detect_memory_leaks ( ThreadId tid, LeakCheckMode mode )
{
   MC_(do_detect_memory_leaks) ( 
      tid, 
      mode, 
      mc_is_within_valid_secondary, 
      mc_is_valid_aligned_word 
   );
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
   /* nothing useful we can rapidly check */
   n_sanity_cheap++;
   PROF_EVENT(490, "cheap_sanity_check");
   return True;
}

static Bool mc_expensive_sanity_check ( void )
{
   Int     i;
   Word    n_secmaps_found;
   SecMap* sm;
   HChar*  errmsg;
   Bool    bad = False;

   if (0) VG_(printf)("expensive sanity check\n");
   if (0) return True;

   n_sanity_expensive++;
   PROF_EVENT(491, "expensive_sanity_check");

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
   if (!MC_(clo_undef_value_errors)) {
      if (0 != VG_(OSet_Size)(secVBitTable))
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

Bool          MC_(clo_partial_loads_ok)       = False;
Int           MC_(clo_freelist_vol)           = 5000000;
LeakCheckMode MC_(clo_leak_check)             = LC_Summary;
VgRes         MC_(clo_leak_resolution)        = Vg_LowRes;
Bool          MC_(clo_show_reachable)         = False;
Bool          MC_(clo_workaround_gcc296_bugs) = False;
Bool          MC_(clo_undef_value_errors)     = True;

static Bool mc_process_cmd_line_options(Char* arg)
{
	VG_BOOL_CLO(arg, "--partial-loads-ok",      MC_(clo_partial_loads_ok))
   else VG_BOOL_CLO(arg, "--show-reachable",        MC_(clo_show_reachable))
   else VG_BOOL_CLO(arg, "--workaround-gcc296-bugs",MC_(clo_workaround_gcc296_bugs))

   else VG_BOOL_CLO(arg, "--undef-value-errors",    MC_(clo_undef_value_errors))
   
   else VG_BNUM_CLO(arg, "--freelist-vol",  MC_(clo_freelist_vol), 0, 1000000000)
   
   else if (VG_CLO_STREQ(arg, "--leak-check=no"))
      MC_(clo_leak_check) = LC_Off;
   else if (VG_CLO_STREQ(arg, "--leak-check=summary"))
      MC_(clo_leak_check) = LC_Summary;
   else if (VG_CLO_STREQ(arg, "--leak-check=yes") ||
	    VG_CLO_STREQ(arg, "--leak-check=full"))
      MC_(clo_leak_check) = LC_Full;

   else if (VG_CLO_STREQ(arg, "--leak-resolution=low"))
      MC_(clo_leak_resolution) = Vg_LowRes;
   else if (VG_CLO_STREQ(arg, "--leak-resolution=med"))
      MC_(clo_leak_resolution) = Vg_MedRes;
   else if (VG_CLO_STREQ(arg, "--leak-resolution=high"))
      MC_(clo_leak_resolution) = Vg_HighRes;

   else if (VG_CLO_STREQN(16,arg,"--ignore-ranges=")) {
      Int    i;
      UChar* txt = (UChar*)(arg+16);
      Bool   ok  = parse_ignore_ranges(txt);
      if (!ok)
        return False;
      tl_assert(ignoreRanges.used >= 0);
      tl_assert(ignoreRanges.used < M_IGNORE_RANGES);
      for (i = 0; i < ignoreRanges.used; i++) {
         Addr s = ignoreRanges.start[i];
         Addr e = ignoreRanges.end[i];
         Addr limit = 0x4000000; /* 64M - entirely arbitrary limit */
         if (e <= s) {
            VG_(message)(Vg_DebugMsg, 
               "ERROR: --ignore-ranges: end <= start in range:");
            VG_(message)(Vg_DebugMsg, 
               "       0x%lx-0x%lx", s, e);
            return False;
         }
         if (e - s > limit) {
            VG_(message)(Vg_DebugMsg, 
               "ERROR: --ignore-ranges: suspiciously large range:");
            VG_(message)(Vg_DebugMsg, 
               "       0x%lx-0x%lx (size %ld)", s, e, (UWord)(e-s));
            return False;
	 }
      }
   }

   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;
}

static void mc_print_usage(void)
{  
   VG_(printf)(
"    --leak-check=no|summary|full     search for memory leaks at exit?  [summary]\n"
"    --leak-resolution=low|med|high   how much bt merging in leak check [low]\n"
"    --show-reachable=no|yes          show reachable blocks in leak check? [no]\n"
"    --undef-value-errors=no|yes      check for undefined value errors [yes]\n"
"    --partial-loads-ok=no|yes        too hard to explain here; see manual [no]\n"
"    --freelist-vol=<number>          volume of freed blocks queue [5000000]\n"
"    --workaround-gcc296-bugs=no|yes  self explanatory [no]\n"
"    --ignore-ranges=0xPP-0xQQ[,0xRR-0xSS]   assume given addresses are OK\n"
   );
   VG_(replacement_malloc_print_usage)();
}

static void mc_print_debug_usage(void)
{  
   VG_(replacement_malloc_print_debug_usage)();
}


/*------------------------------------------------------------*/
/*--- Client requests                                      ---*/
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

typedef
   struct {
      Addr          start;
      SizeT         size;
      ExeContext*   where;
      Char*            desc;
   } 
   CGenBlock;

/* This subsystem is self-initialising. */
static UInt       cgb_size = 0;
static UInt       cgb_used = 0;
static CGenBlock* cgbs     = NULL;

/* Stats for this subsystem. */
static UInt cgb_used_MAX = 0;   /* Max in use. */
static UInt cgb_allocs   = 0;   /* Number of allocs. */
static UInt cgb_discards = 0;   /* Number of discards. */
static UInt cgb_search   = 0;   /* Number of searches. */


static
Int alloc_client_block ( void )
{
   UInt       i, sz_new;
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

   cgbs_new = VG_(malloc)( sz_new * sizeof(CGenBlock) );
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
      "general CBs: %d allocs, %d discards, %d maxinuse, %d search",
      cgb_allocs, cgb_discards, cgb_used_MAX, cgb_search 
   );
}

static Bool client_perm_maybe_describe( Addr a, AddrInfo* ai )
{
   UInt i;

   /* Perhaps it's a general block ? */
   for (i = 0; i < cgb_used; i++) {
      if (cgbs[i].start == 0 && cgbs[i].size == 0) 
         continue;
      // Use zero as the redzone for client blocks.
      if (VG_(addr_is_in_block)(a, cgbs[i].start, cgbs[i].size, 0)) {
         /* OK - maybe it's a mempool, too? */
         MC_Mempool* mp = VG_(HT_lookup)(MC_(mempool_list),
                                          (UWord)cgbs[i].start);
         if (mp != NULL) {
            if (mp->chunks != NULL) {
               MC_Chunk* mc;
               VG_(HT_ResetIter)(mp->chunks);
               while ( (mc = VG_(HT_Next)(mp->chunks)) ) {
                  if (addr_is_in_MC_Chunk(mc, a)) {
                     ai->tag = Addr_Block;
                     ai->Addr.Block.block_kind = Block_MempoolChunk;
                     ai->Addr.Block.block_desc = "block";
                     ai->Addr.Block.block_szB  = mc->szB;
                     ai->Addr.Block.rwoffset   = (Int)a - (Int)mc->data;
                     ai->Addr.Block.lastchange = mc->where;
                     return True;
                  }
               }
            }
            ai->tag = Addr_Block;
            ai->Addr.Block.block_kind = Block_Mempool;
            ai->Addr.Block.block_desc = "mempool";
            ai->Addr.Block.block_szB  = cgbs[i].size;
            ai->Addr.Block.rwoffset   = (Int)(a) - (Int)(cgbs[i].start);
            ai->Addr.Block.lastchange = cgbs[i].where;
            return True;
         }
         ai->tag = Addr_Block;
         ai->Addr.Block.block_kind = Block_UserG;
         ai->Addr.Block.block_desc = cgbs[i].desc;
         ai->Addr.Block.block_szB  = cgbs[i].size;
         ai->Addr.Block.rwoffset   = (Int)(a) - (Int)(cgbs[i].start);
         ai->Addr.Block.lastchange = cgbs[i].where;
         return True;
      }
   }
   return False;
}

static Bool mc_handle_client_request ( ThreadId tid, UWord* arg, UWord* ret )
{
   Int   i;
   Bool  ok;
   Addr  bad_addr;

   if (!VG_IS_TOOL_USERREQ('M','C',arg[0])
    && VG_USERREQ__MALLOCLIKE_BLOCK != arg[0]
    && VG_USERREQ__FREELIKE_BLOCK   != arg[0]
    && VG_USERREQ__CREATE_MEMPOOL   != arg[0]
    && VG_USERREQ__DESTROY_MEMPOOL  != arg[0]
    && VG_USERREQ__MEMPOOL_ALLOC    != arg[0]
    && VG_USERREQ__MEMPOOL_FREE     != arg[0]
    && VG_USERREQ__MEMPOOL_TRIM     != arg[0]
    && VG_USERREQ__MOVE_MEMPOOL     != arg[0]
    && VG_USERREQ__MEMPOOL_CHANGE   != arg[0]
    && VG_USERREQ__MEMPOOL_EXISTS   != arg[0])
      return False;

   switch (arg[0]) {
      case VG_USERREQ__CHECK_MEM_IS_ADDRESSABLE:
         ok = is_mem_addressable ( arg[1], arg[2], &bad_addr );
         if (!ok)
            mc_record_user_error ( tid, bad_addr, /*isAddrErr*/True );
         *ret = ok ? (UWord)NULL : bad_addr;
         break;

      case VG_USERREQ__CHECK_MEM_IS_DEFINED: {
         MC_ReadResult res;
         res = is_mem_defined ( arg[1], arg[2], &bad_addr );
         if (MC_AddrErr == res)
            mc_record_user_error ( tid, bad_addr, /*isAddrErr*/True );
         else if (MC_ValueErr == res)
            mc_record_user_error ( tid, bad_addr, /*isAddrErr*/False );
         *ret = ( res==MC_Ok ? (UWord)NULL : bad_addr );
         break;
      }

      case VG_USERREQ__DO_LEAK_CHECK:
         mc_detect_memory_leaks(tid, arg[1] ? LC_Summary : LC_Full);
         *ret = 0; /* return value is meaningless */
         break;

      case VG_USERREQ__MAKE_MEM_NOACCESS:
         MC_(make_mem_noaccess) ( arg[1], arg[2] );
         *ret = -1;
         break;

      case VG_USERREQ__MAKE_MEM_UNDEFINED:
         MC_(make_mem_undefined) ( arg[1], arg[2] );
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
            cgbs[i].desc  = VG_(strdup)((Char *)arg[3]);
            cgbs[i].where = VG_(record_ExeContext) ( tid );

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
            tl_assert(arg[2] >= 0 && arg[2] < cgb_used);
            cgbs[arg[2]].start = cgbs[arg[2]].size = 0;
            VG_(free)(cgbs[arg[2]].desc);
            cgb_discards++;
            *ret = 0;
         }
         break;

      case VG_USERREQ__GET_VBITS:
         *ret = mc_get_or_set_vbits_for_client
                   ( tid, arg[1], arg[2], arg[3], False /* get them */ );
         break;

      case VG_USERREQ__SET_VBITS:
         *ret = mc_get_or_set_vbits_for_client
                   ( tid, arg[1], arg[2], arg[3], True /* set them */ );
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
         // XXX need to make *argp[1-4] defined
         *ret = 0;
         return True;
      }
      case VG_USERREQ__MALLOCLIKE_BLOCK: {
         Addr p         = (Addr)arg[1];
         SizeT sizeB    =       arg[2];
         UInt rzB       =       arg[3];
         Bool is_zeroed = (Bool)arg[4];

         MC_(new_block) ( tid, p, sizeB, /*ignored*/0, rzB, is_zeroed, 
                          MC_AllocCustom, MC_(malloc_list) );
         return True;
      }
      case VG_USERREQ__FREELIKE_BLOCK: {
         Addr p         = (Addr)arg[1];
         UInt rzB       =       arg[2];

         MC_(handle_free) ( tid, p, rzB, MC_AllocCustom );
         return True;
      }

      case _VG_USERREQ__MEMCHECK_RECORD_OVERLAP_ERROR: {
         Char* s   = (Char*)arg[1];
         Addr  dst = (Addr) arg[2];
         Addr  src = (Addr) arg[3];
         SizeT len = (SizeT)arg[4];
         mc_record_overlap_error(tid, s, src, dst, len);
         return True;
      }

      case VG_USERREQ__CREATE_MEMPOOL: {
         Addr pool      = (Addr)arg[1];
         UInt rzB       =       arg[2];
         Bool is_zeroed = (Bool)arg[3];

         MC_(create_mempool) ( pool, rzB, is_zeroed );
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


      default:
         VG_(message)(Vg_UserMsg, 
                      "Warning: unknown memcheck client request code %llx",
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

UInt   MC_(event_ctr)[N_PROF_EVENTS];
HChar* MC_(event_ctr_name)[N_PROF_EVENTS];

static void init_prof_mem ( void )
{
   Int i;
   for (i = 0; i < N_PROF_EVENTS; i++) {
      MC_(event_ctr)[i] = 0;
      MC_(event_ctr_name)[i] = NULL;
   }
}

static void done_prof_mem ( void )
{
   Int  i;
   Bool spaced = False;
   for (i = 0; i < N_PROF_EVENTS; i++) {
      if (!spaced && (i % 10) == 0) {
         VG_(printf)("\n");
         spaced = True;
      }
      if (MC_(event_ctr)[i] > 0) {
         spaced = False;
         VG_(printf)( "prof mem event %3d: %9d   %s\n", 
                      i, MC_(event_ctr)[i],
                      MC_(event_ctr_name)[i] 
                         ? MC_(event_ctr_name)[i] : "unnamed");
      }
   }
}

#else

static void init_prof_mem ( void ) { }
static void done_prof_mem ( void ) { }

#endif

/*------------------------------------------------------------*/
/*--- Setup and finalisation                               ---*/
/*------------------------------------------------------------*/

static void mc_post_clo_init ( void )
{
   /* If we've been asked to emit XML, mash around various other
      options so as to constrain the output somewhat. */
   if (VG_(clo_xml)) {
      /* Extract as much info as possible from the leak checker. */
      /* MC_(clo_show_reachable) = True; */
      MC_(clo_leak_check) = LC_Full;
   }
}

static void print_SM_info(char* type, int n_SMs)
{
   VG_(message)(Vg_DebugMsg,
      " memcheck: SMs: %s = %d (%dk, %dM)",
      type,
      n_SMs,
      n_SMs * sizeof(SecMap) / 1024,
      n_SMs * sizeof(SecMap) / (1024 * 1024) );
}

static void mc_fini ( Int exitcode )
{
   MC_(print_malloc_stats)();

   if (VG_(clo_verbosity) == 1 && !VG_(clo_xml)) {
      if (MC_(clo_leak_check) == LC_Off)
         VG_(message)(Vg_UserMsg, 
             "For a detailed leak analysis,  rerun with: --leak-check=yes");

      VG_(message)(Vg_UserMsg, 
                   "For counts of detected errors, rerun with: -v");
   }
   if (MC_(clo_leak_check) != LC_Off)
      mc_detect_memory_leaks(1/*bogus ThreadId*/, MC_(clo_leak_check));

   done_prof_mem();

   if (VG_(clo_verbosity) > 1) {
      SizeT max_secVBit_szB, max_SMs_szB, max_shmem_szB;
      
      VG_(message)(Vg_DebugMsg,
         " memcheck: sanity checks: %d cheap, %d expensive",
         n_sanity_cheap, n_sanity_expensive );
      VG_(message)(Vg_DebugMsg,
         " memcheck: auxmaps: %d auxmap entries (%dk, %dM) in use",
         n_auxmap_L2_nodes, 
         n_auxmap_L2_nodes * 64, 
         n_auxmap_L2_nodes / 16 );
      VG_(message)(Vg_DebugMsg,
         " memcheck: auxmaps_L1: %lld searches, %lld cmps, ratio %lld:10",
         n_auxmap_L1_searches, n_auxmap_L1_cmps,
         (10ULL * n_auxmap_L1_cmps) 
            / (n_auxmap_L1_searches ? n_auxmap_L1_searches : 1) 
      );   
      VG_(message)(Vg_DebugMsg,
         " memcheck: auxmaps_L2: %lld searches, %lld nodes",
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
      // The 4*sizeof(Word) bytes is the malloc metadata size.
      // Hardwiring these sizes in sucks, but I don't see how else to do it.
      max_secVBit_szB = max_secVBit_nodes * 
            (sizeof(SecVBitNode) + 3*sizeof(Word) + 4*sizeof(Word));
      max_shmem_szB   = sizeof(primary_map) + max_SMs_szB + max_secVBit_szB;

      VG_(message)(Vg_DebugMsg,
         " memcheck: max sec V bit nodes:    %d (%dk, %dM)",
         max_secVBit_nodes, max_secVBit_szB / 1024,
                            max_secVBit_szB / (1024 * 1024));
      VG_(message)(Vg_DebugMsg,
         " memcheck: set_sec_vbits8 calls: %llu (new: %llu, updates: %llu)",
         sec_vbits_new_nodes + sec_vbits_updates,
         sec_vbits_new_nodes, sec_vbits_updates );
      VG_(message)(Vg_DebugMsg,
         " memcheck: max shadow mem size:   %dk, %dM",
         max_shmem_szB / 1024, max_shmem_szB / (1024 * 1024));
   }

   if (0) {
      VG_(message)(Vg_DebugMsg, 
        "------ Valgrind's client block stats follow ---------------" );
      show_client_block_stats();
   }
}

static void mc_pre_clo_init(void)
{
   VG_(details_name)            ("Memcheck");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a memory error detector");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2007, and GNU GPL'd, by Julian Seward et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 556 );

   VG_(basic_tool_funcs)          (mc_post_clo_init,
                                   MC_(instrument),
                                   mc_fini);

   VG_(needs_core_errors)         ();
   VG_(needs_tool_errors)         (mc_eq_Error,
                                   mc_pp_Error,
                                   mc_update_extra,
                                   mc_recognised_suppression,
                                   mc_read_extra_suppression_info,
                                   mc_error_matches_suppression,
                                   mc_get_error_name,
                                   mc_print_extra_suppression_info);
   VG_(needs_libc_freeres)        ();
   VG_(needs_command_line_options)(mc_process_cmd_line_options,
                                   mc_print_usage,
                                   mc_print_debug_usage);
   VG_(needs_client_requests)     (mc_handle_client_request);
   VG_(needs_sanity_checks)       (mc_cheap_sanity_check,
                                   mc_expensive_sanity_check);
   VG_(needs_malloc_replacement)  (MC_(malloc),
                                   MC_(__builtin_new),
                                   MC_(__builtin_vec_new),
                                   MC_(memalign),
                                   MC_(calloc),
                                   MC_(free),
                                   MC_(__builtin_delete),
                                   MC_(__builtin_vec_delete),
                                   MC_(realloc),
                                   MC_MALLOC_REDZONE_SZB );
   VG_(needs_xml_output)          ();

   VG_(track_new_mem_startup)     ( mc_new_mem_startup );
   VG_(track_new_mem_stack_signal)( MC_(make_mem_undefined) );
   VG_(track_new_mem_brk)         ( MC_(make_mem_undefined) );
   VG_(track_new_mem_mmap)        ( mc_new_mem_mmap );
   
   VG_(track_copy_mem_remap)      ( MC_(copy_address_range_state) );

   // Nb: we don't do anything with mprotect.  This means that V bits are
   // preserved if a program, for example, marks some memory as inaccessible
   // and then later marks it as accessible again.
   // 
   // If an access violation occurs (eg. writing to read-only memory) we let
   // it fault and print an informative termination message.  This doesn't
   // happen if the program catches the signal, though, which is bad.  If we
   // had two A bits (for readability and writability) that were completely
   // distinct from V bits, then we could handle all this properly.
   VG_(track_change_mem_mprotect) ( NULL );
      
   VG_(track_die_mem_stack_signal)( MC_(make_mem_noaccess) ); 
   VG_(track_die_mem_brk)         ( MC_(make_mem_noaccess) );
   VG_(track_die_mem_munmap)      ( MC_(make_mem_noaccess) ); 

#ifdef PERF_FAST_STACK
   VG_(track_new_mem_stack_4)     ( mc_new_mem_stack_4   );
   VG_(track_new_mem_stack_8)     ( mc_new_mem_stack_8   );
   VG_(track_new_mem_stack_12)    ( mc_new_mem_stack_12  );
   VG_(track_new_mem_stack_16)    ( mc_new_mem_stack_16  );
   VG_(track_new_mem_stack_32)    ( mc_new_mem_stack_32  );
   VG_(track_new_mem_stack_112)   ( mc_new_mem_stack_112 );
   VG_(track_new_mem_stack_128)   ( mc_new_mem_stack_128 );
   VG_(track_new_mem_stack_144)   ( mc_new_mem_stack_144 );
   VG_(track_new_mem_stack_160)   ( mc_new_mem_stack_160 );
#endif
   VG_(track_new_mem_stack)       ( mc_new_mem_stack     );

#ifdef PERF_FAST_STACK
   VG_(track_die_mem_stack_4)     ( mc_die_mem_stack_4   );
   VG_(track_die_mem_stack_8)     ( mc_die_mem_stack_8   );
   VG_(track_die_mem_stack_12)    ( mc_die_mem_stack_12  );
   VG_(track_die_mem_stack_16)    ( mc_die_mem_stack_16  );
   VG_(track_die_mem_stack_32)    ( mc_die_mem_stack_32  );
   VG_(track_die_mem_stack_112)   ( mc_die_mem_stack_112 );
   VG_(track_die_mem_stack_128)   ( mc_die_mem_stack_128 );
   VG_(track_die_mem_stack_144)   ( mc_die_mem_stack_144 );
   VG_(track_die_mem_stack_160)   ( mc_die_mem_stack_160 );
#endif
   VG_(track_die_mem_stack)       ( mc_die_mem_stack     );
   
   VG_(track_ban_mem_stack)       ( MC_(make_mem_noaccess) );

   VG_(track_pre_mem_read)        ( check_mem_is_defined );
   VG_(track_pre_mem_read_asciiz) ( check_mem_is_defined_asciiz );
   VG_(track_pre_mem_write)       ( check_mem_is_addressable );
   VG_(track_post_mem_write)      ( mc_post_mem_write );

   if (MC_(clo_undef_value_errors))
      VG_(track_pre_reg_read)     ( mc_pre_reg_read );

   VG_(track_post_reg_write)                  ( mc_post_reg_write );
   VG_(track_post_reg_write_clientcall_return)( mc_post_reg_write_clientcall );

   init_shadow_memory();
   MC_(malloc_list)  = VG_(HT_construct)( 80021 );   // prime, big
   MC_(mempool_list) = VG_(HT_construct)( 1009  );   // prime, not so big
   init_prof_mem();

   tl_assert( mc_expensive_sanity_check() );

   // {LOADV,STOREV}[8421] will all fail horribly if this isn't true.
   tl_assert(sizeof(UWord) == sizeof(Addr));
   // Call me paranoid.  I don't care.
   tl_assert(sizeof(void*) == sizeof(Addr));

   // BYTES_PER_SEC_VBIT_NODE must be a power of two.
   tl_assert(-1 != VG_(log2)(BYTES_PER_SEC_VBIT_NODE));
}

VG_DETERMINE_INTERFACE_VERSION(mc_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
