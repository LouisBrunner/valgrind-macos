
/*--------------------------------------------------------------------*/
/*--- MemCheck: Maintain bitmaps of memory, tracking the           ---*/
/*--- accessibility (A) and validity (V) status of each byte.      ---*/
/*---                                                    mc_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

static ULong n_auxmap_searches  = 0;
static ULong n_auxmap_cmps      = 0;
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
   one of the three distinguished secondaries.
*/
typedef
   struct { 
      Addr    base;
      SecMap* sm;
   }
   AuxMapEnt;

/* An expanding array of AuxMapEnts. */
#define N_AUXMAPS 20000 /* HACK */
static AuxMapEnt  hacky_auxmaps[N_AUXMAPS];
static Int        auxmap_size = N_AUXMAPS;
static Int        auxmap_used = 0;
static AuxMapEnt* auxmap      = &hacky_auxmaps[0];


/* Find an entry in the auxiliary map.  If an entry is found, move it
   one step closer to the front of the array, then return its address.
   If an entry is not found, return NULL.  Note carefully that
   because a each call potentially rearranges the entries, each call
   to this function invalidates ALL AuxMapEnt*s previously obtained by
   calling this fn.  
*/
static AuxMapEnt* maybe_find_in_auxmap ( Addr a )
{
   UWord i;
   tl_assert(a > MAX_PRIMARY_ADDRESS);

   a &= ~(Addr)0xFFFF;

   /* Search .. */
   n_auxmap_searches++;
   for (i = 0; i < auxmap_used; i++) {
      if (auxmap[i].base == a)
         break;
   }
   n_auxmap_cmps += (ULong)(i+1);

   if (i < auxmap_used) {
      /* Found it.  Nudge it a bit closer to the front. */
      if (i > 0) {
         AuxMapEnt tmp = auxmap[i-1];
         auxmap[i-1] = auxmap[i];
         auxmap[i] = tmp;
         i--;
      }
      return &auxmap[i];
   }

   return NULL;
}


/* Find an entry in the auxiliary map.  If an entry is found, move it
   one step closer to the front of the array, then return its address.
   If an entry is not found, allocate one.  Note carefully that
   because a each call potentially rearranges the entries, each call
   to this function invalidates ALL AuxMapEnt*s previously obtained by
   calling this fn.  
*/
static AuxMapEnt* find_or_alloc_in_auxmap ( Addr a )
{
   AuxMapEnt* am = maybe_find_in_auxmap(a);
   if (am)
      return am;

   /* We didn't find it.  Hmm.  This is a new piece of address space.
      We'll need to allocate a new AuxMap entry for it. */
   if (auxmap_used >= auxmap_size) {
      tl_assert(auxmap_used == auxmap_size);
      /* Out of auxmap entries. */
      tl_assert2(0, "failed to expand the auxmap table");
   }

   tl_assert(auxmap_used < auxmap_size);

   auxmap[auxmap_used].base = a & ~(Addr)0xFFFF;
   auxmap[auxmap_used].sm   = &sm_distinguished[SM_DIST_NOACCESS];

   if (0)
      VG_(printf)("new auxmap, base = 0x%llx\n", 
                  (ULong)auxmap[auxmap_used].base );

   auxmap_used++;
   return &auxmap[auxmap_used-1];
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
static SecMap* get_secmap_for_reading ( Addr a )
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

/* --------------- Load/store slow cases. --------------- */

// Forward declarations
static void mc_record_address_error  ( ThreadId tid, Addr a,
                                       Int size, Bool isWrite );
static void mc_record_core_mem_error ( ThreadId tid, Bool isUnaddr, Char* s );
static void mc_record_param_error    ( ThreadId tid, Addr a, Bool isReg,
                                       Bool isUnaddr, Char* msg );
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
   tl_assert(nBits == 64 || nBits == 32 || nBits == 16 || nBits == 8);

   /* Dump vbytes in memory, iterating from least to most significant
      byte.  At the same time establish addressibility of the
      location. */
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
   UChar vabits2;

   DEBUG("MC_(copy_address_range_state)\n");
   PROF_EVENT(50, "MC_(copy_address_range_state)");

   if (len == 0)
      return;

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
   if (VG_IS_4_ALIGNED(new_SP)) {
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 4 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_4(Addr new_SP)
{
   PROF_EVENT(120, "die_mem_stack_4");
   if (VG_IS_4_ALIGNED(new_SP)) {
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-4, 4 );
   }
}

static void VG_REGPARM(1) mc_new_mem_stack_8(Addr new_SP)
{
   PROF_EVENT(111, "new_mem_stack_8");
   if (VG_IS_8_ALIGNED(new_SP)) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP );
   } else if (VG_IS_4_ALIGNED(new_SP)) {
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP   );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+4 );
   } else {
      MC_(make_mem_undefined) ( -VG_STACK_REDZONE_SZB + new_SP, 8 );
   }
}

static void VG_REGPARM(1) mc_die_mem_stack_8(Addr new_SP)
{
   PROF_EVENT(121, "die_mem_stack_8");
   if (VG_IS_8_ALIGNED(new_SP)) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8 );
   } else if (VG_IS_4_ALIGNED(new_SP)) {
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8 );
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4 );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-8, 8 );
   }
}

static void VG_REGPARM(1) mc_new_mem_stack_12(Addr new_SP)
{
   PROF_EVENT(112, "new_mem_stack_12");
   if (VG_IS_8_ALIGNED(new_SP)) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP   );
      make_aligned_word32_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
   } else if (VG_IS_4_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP-12)) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-12 );
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-4  );
   } else if (VG_IS_4_ALIGNED(new_SP)) {
      make_aligned_word32_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-12 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8  );
   } else {
      MC_(make_mem_noaccess) ( -VG_STACK_REDZONE_SZB + new_SP-12, 12 );
   }
}

static void VG_REGPARM(1) mc_new_mem_stack_16(Addr new_SP)
{
   PROF_EVENT(113, "new_mem_stack_16");
   if (VG_IS_8_ALIGNED(new_SP)) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP   );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8 );
   } else if (VG_IS_4_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-8  );
   } else if (VG_IS_4_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP    );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+8  );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+16 );
      make_aligned_word64_undefined ( -VG_STACK_REDZONE_SZB + new_SP+24 );
   } else if (VG_IS_4_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-32 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-24 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP-16 );
      make_aligned_word64_noaccess ( -VG_STACK_REDZONE_SZB + new_SP- 8 );
   } else if (VG_IS_4_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
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
   if (VG_IS_8_ALIGNED(new_SP)) {
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
      UWord a_hi = (UWord)(base + 127);
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
   if (EXPECTED_TAKEN( len == 128 && VG_IS_8_ALIGNED(base) )) {
      /* Now we know the address range is suitably sized and aligned. */
      UWord a_lo = (UWord)(base);
      UWord a_hi = (UWord)(base + 127);
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
         mc_record_param_error ( tid, bad_addr, /*isReg*/False,
                                    /*isUnaddr*/True, s );
         break;

      case Vg_CorePThread:
      case Vg_CoreSignal:
         mc_record_core_mem_error( tid, /*isUnaddr*/True, s );
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
      Bool isUnaddr = ( MC_AddrErr == res ? True : False );

      switch (part) {
      case Vg_CoreSysCall:
         mc_record_param_error ( tid, bad_addr, /*isReg*/False,
                                 isUnaddr, s );
         break;
      
      case Vg_CoreClientReq: // Kludge: make this a CoreMemErr
      case Vg_CorePThread:
         mc_record_core_mem_error( tid, isUnaddr, s );
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
      Bool isUnaddr = ( MC_AddrErr == res ? True : False );
      mc_record_param_error ( tid, bad_addr, /*isReg*/False, isUnaddr, s );
   }
}

static
void mc_new_mem_startup( Addr a, SizeT len, Bool rr, Bool ww, Bool xx )
{
   /* Ignore the permissions, just make it defined.  Seems to work... */
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
#  define MAX_REG_WRITE_SIZE 1392
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
      mc_record_param_error ( tid, 0, /*isReg*/True, /*isUnaddr*/False, s );
}


/*------------------------------------------------------------*/
/*--- Error and suppression types                          ---*/
/*------------------------------------------------------------*/

/* The classification of a faulting address. */
typedef 
   enum { 
      Undescribed,   // as-yet unclassified
      Stack, 
      Unknown,       // classification yielded nothing useful
      Freed, Mallocd, 
      UserG,         // in a user-defined block
      Mempool,       // in a mempool
      Register,      // in a register;  for Param errors only
   }
   AddrKind;

/* Records info about a faulting address. */
typedef
   struct {                   // Used by:
      AddrKind akind;         //   ALL
      SizeT blksize;          //   Freed, Mallocd
      OffT rwoffset;          //   Freed, Mallocd
      ExeContext* lastchange; //   Freed, Mallocd
      ThreadId stack_tid;     //   Stack
      const Char *desc;	      //   UserG
      Bool maybe_gcc;         // True if just below %esp -- could be a gcc bug.
   }
   AddrInfo;

typedef 
   enum { 
      ParamSupp,     // Bad syscall params
      CoreMemSupp,   // Memory errors in core (pthread ops, signal handling)

      // Use of invalid values of given size (MemCheck only)
      Value0Supp, Value1Supp, Value2Supp, Value4Supp, Value8Supp, Value16Supp,

      // Invalid read/write attempt at given size
      Addr1Supp, Addr2Supp, Addr4Supp, Addr8Supp, Addr16Supp,

      FreeSupp,      // Invalid or mismatching free
      OverlapSupp,   // Overlapping blocks in memcpy(), strcpy(), etc
      LeakSupp,      // Something to be suppressed in a leak check.
      MempoolSupp,   // Memory pool suppression.
   } 
   MC_SuppKind;

/* What kind of error it is. */
typedef 
   enum { ValueErr,
          CoreMemErr,   // Error in core op (pthread, signals) or client req
          AddrErr, 
          ParamErr, UserErr,  /* behaves like an anonymous ParamErr */
          FreeErr, FreeMismatchErr,
          OverlapErr,
          LeakErr,
          IllegalMempoolErr,
   }
   MC_ErrorKind;

/* What kind of memory access is involved in the error? */
typedef
   enum { ReadAxs, WriteAxs, ExecAxs }
   AxsKind;

/* Extra context for memory errors */
typedef
   struct {                // Used by:
      AxsKind axskind;     //   AddrErr
      Int size;            //   AddrErr, ValueErr
      AddrInfo addrinfo;   //   {Addr,Free,FreeMismatch,Param,User}Err
      Bool isUnaddr;       //   {CoreMem,Param,User}Err
   }
   MC_Error;

/*------------------------------------------------------------*/
/*--- Printing errors                                      ---*/
/*------------------------------------------------------------*/

static void mc_pp_AddrInfo ( Addr a, AddrInfo* ai )
{
   HChar* xpre  = VG_(clo_xml) ? "  <auxwhat>" : " ";
   HChar* xpost = VG_(clo_xml) ? "</auxwhat>"  : "";

   switch (ai->akind) {
      case Stack: 
         VG_(message)(Vg_UserMsg, 
                      "%sAddress 0x%llx is on thread %d's stack%s", 
                      xpre, (ULong)a, ai->stack_tid, xpost);
         break;
      case Unknown:
         if (ai->maybe_gcc) {
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
      case Freed: case Mallocd: case UserG: case Mempool: {
         SizeT delta;
         const Char* relative;
         const Char* kind;
         if (ai->akind == Mempool) {
            kind = "mempool";
         } else {
            kind = "block";
         }
	 if (ai->desc != NULL)
	    kind = ai->desc;

         if (ai->rwoffset < 0) {
            delta    = (SizeT)(- ai->rwoffset);
            relative = "before";
         } else if (ai->rwoffset >= ai->blksize) {
            delta    = ai->rwoffset - ai->blksize;
            relative = "after";
         } else {
            delta    = ai->rwoffset;
            relative = "inside";
         }
         VG_(message)(Vg_UserMsg, 
            "%sAddress 0x%lx is %,lu bytes %s a %s of size %,lu %s%s",
            xpre,
            a, delta, relative, kind,
            ai->blksize,
            ai->akind==Mallocd ? "alloc'd" 
               : ai->akind==Freed ? "free'd" 
                                  : "client-defined",
            xpost);
         VG_(pp_ExeContext)(ai->lastchange);
         break;
      }
      case Register:
         // print nothing
         tl_assert(0 == a);
         break;
      default:
         VG_(tool_panic)("mc_pp_AddrInfo");
   }
}

static void mc_pp_Error ( Error* err )
{
   MC_Error* err_extra = VG_(get_error_extra)(err);

   HChar* xpre  = VG_(clo_xml) ? "  <what>" : "";
   HChar* xpost = VG_(clo_xml) ? "</what>"  : "";

   switch (VG_(get_error_kind)(err)) {
      case CoreMemErr: {
         Char* s = ( err_extra->isUnaddr ? "unaddressable" : "uninitialised" );
         if (VG_(clo_xml))
            VG_(message)(Vg_UserMsg, "  <kind>CoreMemError</kind>");
            /* What the hell *is* a CoreMemError? jrs 2005-May-18 */
         VG_(message)(Vg_UserMsg, "%s%s contains %s byte(s)%s", 
                      xpre, VG_(get_error_string)(err), s, xpost);

         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         break;
      
      } 
      
      case ValueErr:
         if (err_extra->size == 0) {
            if (VG_(clo_xml))
               VG_(message)(Vg_UserMsg, "  <kind>UninitCondition</kind>");
            VG_(message)(Vg_UserMsg, "%sConditional jump or move depends"
                                     " on uninitialised value(s)%s", 
                                     xpre, xpost);
         } else {
            if (VG_(clo_xml))
               VG_(message)(Vg_UserMsg, "  <kind>UninitValue</kind>");
            VG_(message)(Vg_UserMsg,
                         "%sUse of uninitialised value of size %d%s",
                         xpre, err_extra->size, xpost);
         }
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         break;

      case ParamErr: {
         Bool isReg = ( Register == err_extra->addrinfo.akind );
         Char* s1 = ( isReg ? "contains" : "points to" );
         Char* s2 = ( err_extra->isUnaddr ? "unaddressable" : "uninitialised" );
         if (isReg) tl_assert(!err_extra->isUnaddr);

         if (VG_(clo_xml))
            VG_(message)(Vg_UserMsg, "  <kind>SyscallParam</kind>");
         VG_(message)(Vg_UserMsg, "%sSyscall param %s %s %s byte(s)%s",
                      xpre, VG_(get_error_string)(err), s1, s2, xpost);

         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         mc_pp_AddrInfo(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;
      }
      case UserErr: {
         Char* s = ( err_extra->isUnaddr ? "Unaddressable" : "Uninitialised" );

         if (VG_(clo_xml))
            VG_(message)(Vg_UserMsg, "  <kind>ClientCheck</kind>");
         VG_(message)(Vg_UserMsg, 
            "%s%s byte(s) found during client check request%s", 
            xpre, s, xpost);

         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         mc_pp_AddrInfo(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;
      }
      case FreeErr:
         if (VG_(clo_xml))
            VG_(message)(Vg_UserMsg, "  <kind>InvalidFree</kind>");
         VG_(message)(Vg_UserMsg, 
                      "%sInvalid free() / delete / delete[]%s",
                      xpre, xpost);
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         mc_pp_AddrInfo(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      case FreeMismatchErr:
         if (VG_(clo_xml))
            VG_(message)(Vg_UserMsg, "  <kind>MismatchedFree</kind>");
         VG_(message)(Vg_UserMsg, 
                      "%sMismatched free() / delete / delete []%s",
                      xpre, xpost);
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         mc_pp_AddrInfo(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      case AddrErr:
         switch (err_extra->axskind) {
            case ReadAxs:
               if (VG_(clo_xml))
                  VG_(message)(Vg_UserMsg, "  <kind>InvalidRead</kind>");
               VG_(message)(Vg_UserMsg,
                            "%sInvalid read of size %d%s", 
                            xpre, err_extra->size, xpost ); 
               break;
            case WriteAxs:
               if (VG_(clo_xml))
                  VG_(message)(Vg_UserMsg, "  <kind>InvalidWrite</kind>");
               VG_(message)(Vg_UserMsg, 
                           "%sInvalid write of size %d%s", 
                           xpre, err_extra->size, xpost ); 
               break;
            case ExecAxs:
               if (VG_(clo_xml))
                  VG_(message)(Vg_UserMsg, "  <kind>InvalidJump</kind>");
               VG_(message)(Vg_UserMsg, 
                            "%sJump to the invalid address "
                            "stated on the next line%s",
                            xpre, xpost);
               break;
            default: 
               VG_(tool_panic)("mc_pp_Error(axskind)");
         }
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         mc_pp_AddrInfo(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      case OverlapErr: {
         OverlapExtra* ov_extra = (OverlapExtra*)VG_(get_error_extra)(err);
         if (VG_(clo_xml))
            VG_(message)(Vg_UserMsg, "  <kind>Overlap</kind>");
         if (ov_extra->len == -1)
            VG_(message)(Vg_UserMsg,
                         "%sSource and destination overlap in %s(%p, %p)%s",
                         xpre,
                         VG_(get_error_string)(err),
                         ov_extra->dst, ov_extra->src,
                         xpost);
         else
            VG_(message)(Vg_UserMsg,
                         "%sSource and destination overlap in %s(%p, %p, %d)%s",
                         xpre,
                         VG_(get_error_string)(err),
                         ov_extra->dst, ov_extra->src, ov_extra->len,
                         xpost);
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         break;
      }
      case LeakErr: {
         MC_(pp_LeakError)(err_extra);
         break;
      }

      case IllegalMempoolErr:
         if (VG_(clo_xml))
            VG_(message)(Vg_UserMsg, "  <kind>InvalidMemPool</kind>");
         VG_(message)(Vg_UserMsg, "%sIllegal memory pool address%s",
                                  xpre, xpost);
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         mc_pp_AddrInfo(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

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

static void mc_clear_MC_Error ( MC_Error* err_extra )
{
   err_extra->axskind             = ReadAxs;
   err_extra->size                = 0;
   err_extra->isUnaddr            = True;
   err_extra->addrinfo.akind      = Unknown;
   err_extra->addrinfo.blksize    = 0;
   err_extra->addrinfo.rwoffset   = 0;
   err_extra->addrinfo.lastchange = NULL;
   err_extra->addrinfo.stack_tid  = VG_INVALID_THREADID;
   err_extra->addrinfo.maybe_gcc  = False;
   err_extra->addrinfo.desc       = NULL;
}

/* This one called from generated code and non-generated code. */
static void mc_record_address_error ( ThreadId tid, Addr a, Int size,
                                      Bool isWrite )
{
   MC_Error err_extra;
   Bool      just_below_esp;

   just_below_esp = is_just_below_ESP( VG_(get_SP)(tid), a );

   /* If this is caused by an access immediately below %ESP, and the
      user asks nicely, we just ignore it. */
   if (MC_(clo_workaround_gcc296_bugs) && just_below_esp)
      return;

   mc_clear_MC_Error( &err_extra );
   err_extra.axskind = isWrite ? WriteAxs : ReadAxs;
   err_extra.size    = size;
   err_extra.addrinfo.akind     = Undescribed;
   err_extra.addrinfo.maybe_gcc = just_below_esp;
   VG_(maybe_record_error)( tid, AddrErr, a, /*s*/NULL, &err_extra );
}

/* These ones are called from non-generated code */

/* This is for memory errors in pthread functions, as opposed to pthread API
   errors which are found by the core. */
static void mc_record_core_mem_error ( ThreadId tid, Bool isUnaddr, Char* msg )
{
   MC_Error err_extra;

   mc_clear_MC_Error( &err_extra );
   err_extra.isUnaddr = isUnaddr;
   VG_(maybe_record_error)( tid, CoreMemErr, /*addr*/0, msg, &err_extra );
}

// Three kinds of param errors:
// - register arg contains undefined bytes
// - memory arg is unaddressable
// - memory arg contains undefined bytes
// 'isReg' and 'isUnaddr' dictate which of these it is.
static void mc_record_param_error ( ThreadId tid, Addr a, Bool isReg,
                                    Bool isUnaddr, Char* msg )
{
   MC_Error err_extra;

   tl_assert(MC_(clo_undef_value_errors));
   tl_assert(VG_INVALID_THREADID != tid);
   if (isUnaddr) tl_assert(!isReg);    // unaddressable register is impossible
   mc_clear_MC_Error( &err_extra );
   err_extra.addrinfo.akind = ( isReg ? Register : Undescribed );
   err_extra.isUnaddr = isUnaddr;
   VG_(maybe_record_error)( tid, ParamErr, a, msg, &err_extra );
}

static void mc_record_jump_error ( ThreadId tid, Addr a )
{
   MC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   mc_clear_MC_Error( &err_extra );
   err_extra.axskind = ExecAxs;
   err_extra.size    = 1;     // size only used for suppressions
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tid, AddrErr, a, /*s*/NULL, &err_extra );
}

void MC_(record_free_error) ( ThreadId tid, Addr a ) 
{
   MC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   mc_clear_MC_Error( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tid, FreeErr, a, /*s*/NULL, &err_extra );
}

void MC_(record_illegal_mempool_error) ( ThreadId tid, Addr a ) 
{
   MC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   mc_clear_MC_Error( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tid, IllegalMempoolErr, a, /*s*/NULL, &err_extra );
}

void MC_(record_freemismatch_error) ( ThreadId tid, Addr a, MC_Chunk* mc )
{
   MC_Error err_extra;
   AddrInfo* ai;

   tl_assert(VG_INVALID_THREADID != tid);
   mc_clear_MC_Error( &err_extra );
   ai = &err_extra.addrinfo;
   ai->akind      = Mallocd;     // Nb: not 'Freed'
   ai->blksize    = mc->size;
   ai->rwoffset   = (Int)a - (Int)mc->data;
   ai->lastchange = mc->where;
   VG_(maybe_record_error)( tid, FreeMismatchErr, a, /*s*/NULL, &err_extra );
}

static void mc_record_overlap_error ( ThreadId tid, 
                                      Char* function, OverlapExtra* ov_extra )
{
   VG_(maybe_record_error)( 
      tid, OverlapErr, /*addr*/0, /*s*/function, ov_extra );
}

Bool MC_(record_leak_error) ( ThreadId tid, /*LeakExtra*/void* leak_extra,
                              ExeContext* where, Bool print_record )
{
   return
   VG_(unique_error) ( tid, LeakErr, /*Addr*/0, /*s*/NULL,
                       /*extra*/leak_extra, where, print_record,
                       /*allow_GDB_attach*/False, /*count_error*/False );
}


/* Creates a copy of the 'extra' part, updates the copy with address info if
   necessary, and returns the copy. */
/* This one called from generated code and non-generated code. */
static void mc_record_value_error ( ThreadId tid, Int size )
{
   MC_Error err_extra;

   tl_assert(MC_(clo_undef_value_errors));
   mc_clear_MC_Error( &err_extra );
   err_extra.size     = size;
   err_extra.isUnaddr = False;
   VG_(maybe_record_error)( tid, ValueErr, /*addr*/0, /*s*/NULL, &err_extra );
}

/* This called from non-generated code */

static void mc_record_user_error ( ThreadId tid, Addr a, Bool isWrite,
                                   Bool isUnaddr )
{
   MC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   mc_clear_MC_Error( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   err_extra.isUnaddr       = isUnaddr;
   VG_(maybe_record_error)( tid, UserErr, a, /*s*/NULL, &err_extra );
}

__attribute__ ((unused))
static Bool eq_AddrInfo ( VgRes res, AddrInfo* ai1, AddrInfo* ai2 )
{
   if (ai1->akind != Undescribed 
       && ai2->akind != Undescribed
       && ai1->akind != ai2->akind) 
      return False;
   if (ai1->akind == Freed || ai1->akind == Mallocd) {
      if (ai1->blksize != ai2->blksize)
         return False;
      if (!VG_(eq_ExeContext)(res, ai1->lastchange, ai2->lastchange))
         return False;
   }
   return True;
}

/* Compare error contexts, to detect duplicates.  Note that if they
   are otherwise the same, the faulting addrs and associated rwoffsets
   are allowed to be different.  */
static Bool mc_eq_Error ( VgRes res, Error* e1, Error* e2 )
{
   MC_Error* e1_extra = VG_(get_error_extra)(e1);
   MC_Error* e2_extra = VG_(get_error_extra)(e2);

   /* Guaranteed by calling function */
   tl_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));
   
   switch (VG_(get_error_kind)(e1)) {
      case CoreMemErr: {
         Char *e1s, *e2s;
         if (e1_extra->isUnaddr != e2_extra->isUnaddr) return False;
         e1s = VG_(get_error_string)(e1);
         e2s = VG_(get_error_string)(e2);
         if (e1s == e2s)                               return True;
         if (0 == VG_(strcmp)(e1s, e2s))               return True;
         return False;
      }

      // Perhaps we should also check the addrinfo.akinds for equality.
      // That would result in more error reports, but only in cases where
      // a register contains uninitialised bytes and points to memory
      // containing uninitialised bytes.  Currently, the 2nd of those to be
      // detected won't be reported.  That is (nearly?) always the memory
      // error, which is good.
      case ParamErr:
         if (0 != VG_(strcmp)(VG_(get_error_string)(e1),
                              VG_(get_error_string)(e2)))   return False;
         // fall through
      case UserErr:
         if (e1_extra->isUnaddr != e2_extra->isUnaddr)      return False;
         return True;

      case FreeErr:
      case FreeMismatchErr:
         /* JRS 2002-Aug-26: comparing addrs seems overkill and can
            cause excessive duplication of errors.  Not even AddrErr
            below does that.  So don't compare either the .addr field
            or the .addrinfo fields. */
         /* if (e1->addr != e2->addr) return False; */
         /* if (!eq_AddrInfo(res, &e1_extra->addrinfo, &e2_extra->addrinfo)) 
               return False;
         */
         return True;

      case AddrErr:
         /* if (e1_extra->axskind != e2_extra->axskind) return False; */
         if (e1_extra->size != e2_extra->size) return False;
         /*
         if (!eq_AddrInfo(res, &e1_extra->addrinfo, &e2_extra->addrinfo)) 
            return False;
         */
         return True;

      case ValueErr:
         if (e1_extra->size != e2_extra->size) return False;
         return True;

      case OverlapErr:
         return True;

      case LeakErr:
         VG_(tool_panic)("Shouldn't get LeakErr in mc_eq_Error,\n"
                         "since it's handled with VG_(unique_error)()!");

      case IllegalMempoolErr:
         return True;

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
   return VG_(addr_is_in_block)( a, mc->data, mc->size,
                                 MC_MALLOC_REDZONE_SZB );
}

// Forward declaration
static Bool client_perm_maybe_describe( Addr a, AddrInfo* ai );

/* Describe an address as best you can, for error messages,
   putting the result in ai. */
static void describe_addr ( Addr a, AddrInfo* ai )
{
   MC_Chunk* mc;
   ThreadId   tid;
   Addr       stack_min, stack_max;

   /* Perhaps it's a user-def'd block? */
   if (client_perm_maybe_describe( a, ai ))
      return;

   /* Perhaps it's on a thread's stack? */
   VG_(thread_stack_reset_iter)();
   while ( VG_(thread_stack_next)(&tid, &stack_min, &stack_max) ) {
      if (stack_min <= a && a <= stack_max) {
         ai->akind     = Stack;
         ai->stack_tid = tid;
         return;
      }
   }
   /* Search for a recently freed block which might bracket it. */
   mc = MC_(get_freed_list_head)();
   while (mc) {
      if (addr_is_in_MC_Chunk(mc, a)) {
         ai->akind      = Freed;
         ai->blksize    = mc->size;
         ai->rwoffset   = (Int)a - (Int)mc->data;
         ai->lastchange = mc->where;
         return;
      }
      mc = mc->next; 
   }
   /* Search for a currently malloc'd block which might bracket it. */
   VG_(HT_ResetIter)(MC_(malloc_list));
   while ( (mc = VG_(HT_Next)(MC_(malloc_list))) ) {
      if (addr_is_in_MC_Chunk(mc, a)) {
         ai->akind      = Mallocd;
         ai->blksize    = mc->size;
         ai->rwoffset   = (Int)(a) - (Int)mc->data;
         ai->lastchange = mc->where;
         return;
      }
   }
   /* Clueless ... */
   ai->akind = Unknown;
   return;
}

/* Updates the copy with address info if necessary (but not for all errors). */
static UInt mc_update_extra( Error* err )
{
   switch (VG_(get_error_kind)(err)) {
   // These two don't have addresses associated with them, and so don't
   // need any updating.
   case CoreMemErr:
   case ValueErr: {
      MC_Error* extra = VG_(get_error_extra)(err);
      tl_assert(Unknown == extra->addrinfo.akind);
      return sizeof(MC_Error);
   }

   // ParamErrs sometimes involve a memory address; call describe_addr() in
   // this case.
   case ParamErr: {
      MC_Error* extra = VG_(get_error_extra)(err);
      tl_assert(Undescribed == extra->addrinfo.akind ||
                Register    == extra->addrinfo.akind);
      if (Undescribed == extra->addrinfo.akind)
         describe_addr ( VG_(get_error_address)(err), &(extra->addrinfo) );
      return sizeof(MC_Error);
   }

   // These four always involve a memory address.
   case AddrErr: 
   case UserErr:
   case FreeErr:
   case IllegalMempoolErr: {
      MC_Error* extra = VG_(get_error_extra)(err);
      tl_assert(Undescribed == extra->addrinfo.akind);
      describe_addr ( VG_(get_error_address)(err), &(extra->addrinfo) );
      return sizeof(MC_Error);
   }

   // FreeMismatchErrs have already had their address described;  this is
   // possible because we have the MC_Chunk on hand when the error is
   // detected.  However, the address may be part of a user block, and if so
   // we override the pre-determined description with a user block one.
   case FreeMismatchErr: {
      MC_Error* extra = VG_(get_error_extra)(err);
      tl_assert(extra && Mallocd == extra->addrinfo.akind);
      (void)client_perm_maybe_describe( VG_(get_error_address)(err), 
                                        &(extra->addrinfo) );
      return sizeof(MC_Error);
   }

   // No memory address involved with these ones.  Nb:  for LeakErrs the
   // returned size does not matter -- LeakErrs are always shown with
   // VG_(unique_error)() so they're not copied.
   case LeakErr:     return 0;
   case OverlapErr:  return sizeof(OverlapExtra);

   default: VG_(tool_panic)("mc_update_extra: bad errkind");
   }
}

/*------------------------------------------------------------*/
/*--- Suppressions                                         ---*/
/*------------------------------------------------------------*/

static Bool mc_recognised_suppression ( Char* name, Supp* su )
{
   SuppKind skind;

   if      (VG_STREQ(name, "Param"))   skind = ParamSupp;
   else if (VG_STREQ(name, "CoreMem")) skind = CoreMemSupp;
   else if (VG_STREQ(name, "Addr1"))   skind = Addr1Supp;
   else if (VG_STREQ(name, "Addr2"))   skind = Addr2Supp;
   else if (VG_STREQ(name, "Addr4"))   skind = Addr4Supp;
   else if (VG_STREQ(name, "Addr8"))   skind = Addr8Supp;
   else if (VG_STREQ(name, "Addr16"))  skind = Addr16Supp;
   else if (VG_STREQ(name, "Free"))    skind = FreeSupp;
   else if (VG_STREQ(name, "Leak"))    skind = LeakSupp;
   else if (VG_STREQ(name, "Overlap")) skind = OverlapSupp;
   else if (VG_STREQ(name, "Mempool")) skind = MempoolSupp;
   else if (VG_STREQ(name, "Cond"))    skind = Value0Supp;
   else if (VG_STREQ(name, "Value0"))  skind = Value0Supp;/* backwards compat */
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
   Int        su_size;
   MC_Error* err_extra = VG_(get_error_extra)(err);
   ErrorKind  ekind     = VG_(get_error_kind )(err);

   switch (VG_(get_supp_kind)(su)) {
      case ParamSupp:
         return (ekind == ParamErr 
              && VG_STREQ(VG_(get_error_string)(err), 
                          VG_(get_supp_string)(su)));

      case CoreMemSupp:
         return (ekind == CoreMemErr
              && VG_STREQ(VG_(get_error_string)(err),
                          VG_(get_supp_string)(su)));

      case Value0Supp: su_size = 0; goto value_case;
      case Value1Supp: su_size = 1; goto value_case;
      case Value2Supp: su_size = 2; goto value_case;
      case Value4Supp: su_size = 4; goto value_case;
      case Value8Supp: su_size = 8; goto value_case;
      case Value16Supp:su_size =16; goto value_case;
      value_case:
         return (ekind == ValueErr && err_extra->size == su_size);

      case Addr1Supp: su_size = 1; goto addr_case;
      case Addr2Supp: su_size = 2; goto addr_case;
      case Addr4Supp: su_size = 4; goto addr_case;
      case Addr8Supp: su_size = 8; goto addr_case;
      case Addr16Supp:su_size =16; goto addr_case;
      addr_case:
         return (ekind == AddrErr && err_extra->size == su_size);

      case FreeSupp:
         return (ekind == FreeErr || ekind == FreeMismatchErr);

      case OverlapSupp:
         return (ekind = OverlapErr);

      case LeakSupp:
         return (ekind == LeakErr);

      case MempoolSupp:
         return (ekind == IllegalMempoolErr);

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
   Char* s;
   switch (VG_(get_error_kind)(err)) {
   case ParamErr:           return "Param";
   case UserErr:            return NULL;  /* Can't suppress User errors */
   case FreeMismatchErr:    return "Free";
   case IllegalMempoolErr:  return "Mempool";
   case FreeErr:            return "Free";
   case AddrErr:            
      switch ( ((MC_Error*)VG_(get_error_extra)(err))->size ) {
      case 1:               return "Addr1";
      case 2:               return "Addr2";
      case 4:               return "Addr4";
      case 8:               return "Addr8";
      case 16:              return "Addr16";
      default:              VG_(tool_panic)("unexpected size for Addr");
      }
     
   case ValueErr:
      switch ( ((MC_Error*)VG_(get_error_extra)(err))->size ) {
      case 0:               return "Cond";
      case 1:               return "Value1";
      case 2:               return "Value2";
      case 4:               return "Value4";
      case 8:               return "Value8";
      case 16:              return "Value16";
      default:              VG_(tool_panic)("unexpected size for Value");
      }
   case CoreMemErr:         return "CoreMem";
   case OverlapErr:         return "Overlap";
   case LeakErr:            return "Leak";
   default:                 VG_(tool_panic)("get_error_name: unexpected type");
   }
   VG_(printf)(s);
}

static void mc_print_extra_suppression_info ( Error* err )
{
   if (ParamErr == VG_(get_error_kind)(err)) {
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
void mc_STOREV64 ( Addr a, ULong vbytes, Bool isBigEndian )
{
   UWord   sm_off16, vabits16;
   SecMap* sm;

   PROF_EVENT(210, "mc_STOREV64");

#ifndef PERF_FAST_STOREV
   // XXX: this slow case seems to be marginally faster than the fast case!
   // Investigate further.
   mc_STOREVn_slow( a, 64, vbytes, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,64) )) {
      PROF_EVENT(211, "mc_STOREV64-slow1");
      mc_STOREVn_slow( a, 64, vbytes, isBigEndian );
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
      if (V_BITS64_DEFINED == vbytes) {
         ((UShort*)(sm->vabits8))[sm_off16] = (UShort)VA_BITS16_DEFINED;
      } else if (V_BITS64_UNDEFINED == vbytes) {
         ((UShort*)(sm->vabits8))[sm_off16] = (UShort)VA_BITS16_UNDEFINED;
      } else {
         /* Slow but general case -- writing partially defined bytes. */
         PROF_EVENT(212, "mc_STOREV64-slow2");
         mc_STOREVn_slow( a, 64, vbytes, isBigEndian );
      }
   } else {
      /* Slow but general case. */
      PROF_EVENT(213, "mc_STOREV64-slow3");
      mc_STOREVn_slow( a, 64, vbytes, isBigEndian );
   }
#endif
}

VG_REGPARM(1) void MC_(helperc_STOREV64be) ( Addr a, ULong vbytes )
{
   mc_STOREV64(a, vbytes, True);
}
VG_REGPARM(1) void MC_(helperc_STOREV64le) ( Addr a, ULong vbytes )
{
   mc_STOREV64(a, vbytes, False);
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
void mc_STOREV32 ( Addr a, UWord vbytes, Bool isBigEndian )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(230, "mc_STOREV32");

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 32, (ULong)vbytes, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,32) )) {
      PROF_EVENT(231, "mc_STOREV32-slow1");
      mc_STOREVn_slow( a, 32, (ULong)vbytes, isBigEndian );
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
   if (V_BITS32_DEFINED == vbytes) {
      if (vabits8 == (UInt)VA_BITS8_DEFINED) {
         return;
      } else if (!is_distinguished_sm(sm) && VA_BITS8_UNDEFINED == vabits8) {
         sm->vabits8[sm_off] = (UInt)VA_BITS8_DEFINED;
      } else {
         // not defined/undefined, or distinguished and changing state
         PROF_EVENT(232, "mc_STOREV32-slow2");
         mc_STOREVn_slow( a, 32, (ULong)vbytes, isBigEndian );
      }
   } else if (V_BITS32_UNDEFINED == vbytes) {
      if (vabits8 == (UInt)VA_BITS8_UNDEFINED) {
         return;
      } else if (!is_distinguished_sm(sm) && VA_BITS8_DEFINED == vabits8) {
         sm->vabits8[sm_off] = (UInt)VA_BITS8_UNDEFINED;
      } else {
         // not defined/undefined, or distinguished and changing state
         PROF_EVENT(233, "mc_STOREV32-slow3");
         mc_STOREVn_slow( a, 32, (ULong)vbytes, isBigEndian );
      }
   } else {
      // Partially defined word
      PROF_EVENT(234, "mc_STOREV32-slow4");
      mc_STOREVn_slow( a, 32, (ULong)vbytes, isBigEndian );
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
      if (V_BITS32_DEFINED == vbytes) {
         sm->vabits8[sm_off] = VA_BITS8_DEFINED;
      } else if (V_BITS32_UNDEFINED == vbytes) {
         sm->vabits8[sm_off] = VA_BITS8_UNDEFINED;
      } else {
         /* Slow but general case -- writing partially defined bytes. */
         PROF_EVENT(232, "mc_STOREV32-slow2");
         mc_STOREVn_slow( a, 32, (ULong)vbytes, isBigEndian );
      }
   } else {
      /* Slow but general case. */
      PROF_EVENT(233, "mc_STOREV32-slow3");
      mc_STOREVn_slow( a, 32, (ULong)vbytes, isBigEndian );
   }
#endif
//---------------------------------------------------------------------------
#endif
}

VG_REGPARM(2) void MC_(helperc_STOREV32be) ( Addr a, UWord vbytes )
{
   mc_STOREV32(a, vbytes, True);
}
VG_REGPARM(2) void MC_(helperc_STOREV32le) ( Addr a, UWord vbytes )
{
   mc_STOREV32(a, vbytes, False);
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
   // XXX: set the high 16/48 bits of retval to 1 for 64-bit paranoia?
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
void mc_STOREV16 ( Addr a, UWord vbytes, Bool isBigEndian )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(250, "mc_STOREV16");

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 16, (ULong)vbytes, isBigEndian );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,16) )) {
      PROF_EVENT(251, "mc_STOREV16-slow1");
      mc_STOREVn_slow( a, 16, (ULong)vbytes, isBigEndian );
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
      if (V_BITS16_DEFINED == vbytes) {
         insert_vabits4_into_vabits8( a, VA_BITS4_DEFINED ,
                                      &(sm->vabits8[sm_off]) );
      } else if (V_BITS16_UNDEFINED == vbytes) {
         insert_vabits4_into_vabits8( a, VA_BITS4_UNDEFINED,
                                      &(sm->vabits8[sm_off]) );
      } else {
         /* Slow but general case -- writing partially defined bytes. */
         PROF_EVENT(252, "mc_STOREV16-slow2");
         mc_STOREVn_slow( a, 16, (ULong)vbytes, isBigEndian );
      }
   } else {
      /* Slow but general case. */
      PROF_EVENT(253, "mc_STOREV16-slow3");
      mc_STOREVn_slow( a, 16, (ULong)vbytes, isBigEndian );
   }
#endif
}

VG_REGPARM(2) void MC_(helperc_STOREV16be) ( Addr a, UWord vbytes )
{
   mc_STOREV16(a, vbytes, True);
}
VG_REGPARM(2) void MC_(helperc_STOREV16le) ( Addr a, UWord vbytes )
{
   mc_STOREV16(a, vbytes, False);
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
   // XXX: set the high 24/56 bits of retval to 1 for 64-bit paranoia?
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
void MC_(helperc_STOREV8) ( Addr a, UWord vbyte )
{
   UWord   sm_off, vabits8;
   SecMap* sm;

   PROF_EVENT(270, "mc_STOREV8");

#ifndef PERF_FAST_STOREV
   mc_STOREVn_slow( a, 8, (ULong)vbyte, False/*irrelevant*/ );
#else
   if (EXPECTED_NOT_TAKEN( UNALIGNED_OR_HIGH(a,8) )) {
      PROF_EVENT(271, "mc_STOREV8-slow1");
      mc_STOREVn_slow( a, 8, (ULong)vbyte, False/*irrelevant*/ );
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
      if (V_BITS8_DEFINED == vbyte) {
         insert_vabits2_into_vabits8( a, VA_BITS2_DEFINED,
                                       &(sm->vabits8[sm_off]) );
      } else if (V_BITS8_UNDEFINED == vbyte) {
         insert_vabits2_into_vabits8( a, VA_BITS2_UNDEFINED,
                                       &(sm->vabits8[sm_off]) );
      } else {
         /* Slow but general case -- writing partially defined bytes. */
         PROF_EVENT(272, "mc_STOREV8-slow2");
         mc_STOREVn_slow( a, 8, (ULong)vbyte, False/*irrelevant*/ );
      }
   } else {
      /* Slow but general case. */
      PROF_EVENT(273, "mc_STOREV8-slow3");
      mc_STOREVn_slow( a, 8, (ULong)vbyte, False/*irrelevant*/ );
   }
#endif
}


/*------------------------------------------------------------*/
/*--- Functions called directly from generated code:       ---*/
/*--- Value-check failure handlers.                        ---*/
/*------------------------------------------------------------*/

void MC_(helperc_value_check0_fail) ( void )
{
   mc_record_value_error ( VG_(get_running_tid)(), 0 );
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
      if (VA_BITS2_NOACCESS == get_vabits2(a + i)) {
         mc_record_address_error( tid, a + i,     1, setting ? True : False );
         return 3;
      }
      if (VA_BITS2_NOACCESS == get_vabits2(vbits + i)) {
         mc_record_address_error( tid, vbits + i, 1, setting ? False : True );
         return 3;
      }
   }

   /* Do the copy */
   if (setting) {

      // It's actually a tool ClientReq, but Vg_CoreClientReq is the closest
      // thing we have.
      check_mem_is_defined(Vg_CoreClientReq, tid, "SET_VBITS(vbits)",
                       vbits, szB);
      
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
// XXX: used to do this, but it's a pain
//         if (V_BITS8_DEFINED != vbits8)
//            mc_record_value_error(tid, 1);
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
   if (sm == NULL || sm == &sm_distinguished[SM_DIST_NOACCESS]) {
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
   if (is_mem_defined( a, sizeof(UWord), NULL ) == MC_Ok) {
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
   Int     i, n_secmaps_found;
   SecMap* sm;
   Bool    bad = False;

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

   /* check nonsensical auxmap sizing */
   if (auxmap_used > auxmap_size)
       bad = True;

   if (bad) {
      VG_(printf)("memcheck expensive sanity: "
                  "nonsensical auxmap sizing\n");
      return False;
   }

   /* check that the number of secmaps issued matches the number that
      are reachable (iow, no secmap leaks) */
   n_secmaps_found = 0;
   for (i = 0; i < N_PRIMARY_MAP; i++) {
     if (primary_map[i] == NULL) {
       bad = True;
     } else {
     if (!is_distinguished_sm(primary_map[i]))
       n_secmaps_found++;
     }
   }

   for (i = 0; i < auxmap_used; i++) {
      if (auxmap[i].sm == NULL) {
         bad = True;
      } else {
         if (!is_distinguished_sm(auxmap[i].sm))
            n_secmaps_found++;
      }
   }

   if (n_secmaps_found != (n_issued_SMs - n_deissued_SMs))
      bad = True;

   if (bad) {
      VG_(printf)("memcheck expensive sanity: "
                  "apparent secmap leakage\n");
      return False;
   }

   /* check that auxmap only covers address space that the primary doesn't */
   
   for (i = 0; i < auxmap_used; i++)
      if (auxmap[i].base <= MAX_PRIMARY_ADDRESS)
         bad = True;

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
   /* VG_(printf)("try to identify %d\n", a); */

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
                     ai->akind      = UserG;
                     ai->blksize    = mc->size;
                     ai->rwoffset   = (Int)(a) - (Int)mc->data;
                     ai->lastchange = mc->where;
                     return True;
                  }
               }
            }
            ai->akind      = Mempool;
            ai->blksize    = cgbs[i].size;
            ai->rwoffset   = (Int)(a) - (Int)(cgbs[i].start);
            ai->lastchange = cgbs[i].where;
            return True;
         }
         ai->akind      = UserG;
         ai->blksize    = cgbs[i].size;
         ai->rwoffset   = (Int)(a) - (Int)(cgbs[i].start);
         ai->lastchange = cgbs[i].where;
         ai->desc       = cgbs[i].desc;
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
    && VG_USERREQ__MEMPOOL_FREE     != arg[0])
      return False;

   switch (arg[0]) {
      case VG_USERREQ__CHECK_MEM_IS_ADDRESSABLE:
         ok = is_mem_addressable ( arg[1], arg[2], &bad_addr );
         if (!ok)
            mc_record_user_error ( tid, bad_addr, /*isWrite*/True,
                                   /*isUnaddr*/True );
         *ret = ok ? (UWord)NULL : bad_addr;
         break;

      case VG_USERREQ__CHECK_MEM_IS_DEFINED: {
         MC_ReadResult res;
         res = is_mem_defined ( arg[1], arg[2], &bad_addr );
         if (MC_AddrErr == res)
            mc_record_user_error ( tid, bad_addr, /*isWrite*/False,
                                   /*isUnaddr*/True );
         else if (MC_ValueErr == res)
            mc_record_user_error ( tid, bad_addr, /*isWrite*/False,
                                   /*isUnaddr*/False );
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
         /* Returns: 1 == OK, 2 == alignment error, 3 == addressing
            error. */
         /* VG_(printf)("get_vbits %p %p %d\n", arg[1], arg[2], arg[3] ); */
         *ret = mc_get_or_set_vbits_for_client
                   ( tid, arg[1], arg[2], arg[3], False /* get them */ );
         break;

      case VG_USERREQ__SET_VBITS:
         /* Returns: 1 == OK, 2 == alignment error, 3 == addressing
            error. */
         /* VG_(printf)("set_vbits %p %p %d\n", arg[1], arg[2], arg[3] ); */
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
         Char*         s     = (Char*)        arg[1];
         OverlapExtra* extra = (OverlapExtra*)arg[2];
         mc_record_overlap_error(tid, s, extra);
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
         auxmap_used, 
         auxmap_used * 64, 
         auxmap_used / 16 );
      VG_(message)(Vg_DebugMsg,
         " memcheck: auxmaps: %lld searches, %lld comparisons",
         n_auxmap_searches, n_auxmap_cmps );   

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
      "Copyright (C) 2002-2005, and GNU GPL'd, by Julian Seward et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 370 );

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

   // BYTES_PER_SEC_VBIT_NODE must be a power of two.
   tl_assert(-1 != VG_(log2)(BYTES_PER_SEC_VBIT_NODE));
}

VG_DETERMINE_INTERFACE_VERSION(mc_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/



