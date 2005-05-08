
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

/* TODO 22 Apr 05

   test whether it would be faster, for LOADV4, to check
   only for 8-byte validity on the fast path
*/

#include "mc_include.h"
#include "memcheck.h"   /* for client requests */


#define EXPECTED_TAKEN(cond)     __builtin_expect((cond),1)
#define EXPECTED_NOT_TAKEN(cond) __builtin_expect((cond),0)

/* Define to debug the mem audit system.  Set to:
      0  no debugging, fast cases are used
      1  some sanity checking, fast cases are used
      2  max sanity checking, only slow cases are used
*/
#define VG_DEBUG_MEMORY 0

#define DEBUG(fmt, args...) //VG_(printf)(fmt, ## args)


/*------------------------------------------------------------*/
/*--- Basic A/V bitmap representation.                     ---*/
/*------------------------------------------------------------*/

/* TODO: fix this comment */
//zz /* All reads and writes are checked against a memory map, which
//zz    records the state of all memory in the process.  The memory map is
//zz    organised like this:
//zz 
//zz    The top 16 bits of an address are used to index into a top-level
//zz    map table, containing 65536 entries.  Each entry is a pointer to a
//zz    second-level map, which records the accesibililty and validity
//zz    permissions for the 65536 bytes indexed by the lower 16 bits of the
//zz    address.  Each byte is represented by nine bits, one indicating
//zz    accessibility, the other eight validity.  So each second-level map
//zz    contains 73728 bytes.  This two-level arrangement conveniently
//zz    divides the 4G address space into 64k lumps, each size 64k bytes.
//zz 
//zz    All entries in the primary (top-level) map must point to a valid
//zz    secondary (second-level) map.  Since most of the 4G of address
//zz    space will not be in use -- ie, not mapped at all -- there is a
//zz    distinguished secondary map, which indicates `not addressible and
//zz    not valid' writeable for all bytes.  Entries in the primary map for
//zz    which the entire 64k is not in use at all point at this
//zz    distinguished map.
//zz 
//zz    There are actually 4 distinguished secondaries.  These are used to
//zz    represent a memory range which is either not addressable (validity
//zz    doesn't matter), addressable+not valid, addressable+valid.
//zz 
//zz    [...] lots of stuff deleted due to out of date-ness
//zz 
//zz    As a final optimisation, the alignment and address checks for
//zz    4-byte loads and stores are combined in a neat way.  The primary
//zz    map is extended to have 262144 entries (2^18), rather than 2^16.
//zz    The top 3/4 of these entries are permanently set to the
//zz    distinguished secondary map.  For a 4-byte load/store, the
//zz    top-level map is indexed not with (addr >> 16) but instead f(addr),
//zz    where
//zz 
//zz     f( XXXX XXXX XXXX XXXX ____ ____ ____ __YZ )
//zz         = ____ ____ ____ __YZ XXXX XXXX XXXX XXXX  or 
//zz         = ____ ____ ____ __ZY XXXX XXXX XXXX XXXX
//zz 
//zz    ie the lowest two bits are placed above the 16 high address bits.
//zz    If either of these two bits are nonzero, the address is misaligned;
//zz    this will select a secondary map from the upper 3/4 of the primary
//zz    map.  Because this is always the distinguished secondary map, a
//zz    (bogus) address check failure will result.  The failure handling
//zz    code can then figure out whether this is a genuine addr check
//zz    failure or whether it is a possibly-legitimate access at a
//zz    misaligned address.  
//zz */

/* --------------- Basic configuration --------------- */

/* Only change this.  N_PRIMARY_MAP *must* be a power of 2. */

#if VG_WORDSIZE == 4

/* cover the entire address space */
#  define N_PRIMARY_BITS  16

#else

/* Just handle the first 16G fast and the rest via auxiliary
   primaries. */
#  define N_PRIMARY_BITS  18

#endif


/* Do not change this. */
#define N_PRIMARY_MAP  ( ((UWord)1) << N_PRIMARY_BITS)

/* Do not change this. */
#define MAX_PRIMARY_ADDRESS (Addr)((((Addr)65536) * N_PRIMARY_MAP)-1)


/* --------------- Stats maps --------------- */

static Int   n_secmaps_issued   = 0;
static ULong n_auxmap_searches  = 0;
static ULong n_auxmap_cmps      = 0;
static Int   n_sanity_cheap     = 0;
static Int   n_sanity_expensive = 0;


/* --------------- Secondary maps --------------- */

typedef 
   struct {
      UChar abits[8192];
      UChar vbyte[65536];
   }
   SecMap;

/* 3 distinguished secondary maps, one for no-access, one for
   accessible but undefined, and one for accessible and defined.
   Distinguished secondaries may never be modified.
*/
#define SM_DIST_NOACCESS          0
#define SM_DIST_ACCESS_UNDEFINED  1
#define SM_DIST_ACCESS_DEFINED    2

static SecMap sm_distinguished[3];

static inline Bool is_distinguished_sm ( SecMap* sm ) {
   return sm >= &sm_distinguished[0] && sm <= &sm_distinguished[2];
}

/* dist_sm points to one of our three distinguished secondaries.  Make
   a copy of it so that we can write to it.
*/
static SecMap* copy_for_writing ( SecMap* dist_sm )
{
   SecMap* new_sm;
   tl_assert(dist_sm == &sm_distinguished[0]
             || dist_sm == &sm_distinguished[1]
	     || dist_sm == &sm_distinguished[2]);

   new_sm = VG_(shadow_alloc)(sizeof(SecMap));
   VG_(memcpy)(new_sm, dist_sm, sizeof(SecMap));
   n_secmaps_issued++;
   return new_sm;
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
#define N_AUXMAPS 500 /* HACK */
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

/* Produce the secmap for 'a', either from the primary map or by
   ensuring there is an entry for it in the aux primary map.  The
   secmap may be a distinguished one as the caller will only want to
   be able to read it. 
*/
static SecMap* get_secmap_readable ( Addr a )
{
   if (a <= MAX_PRIMARY_ADDRESS) {
      UWord pm_off = a >> 16;
      return primary_map[ pm_off ];
   } else {
      AuxMapEnt* am = find_or_alloc_in_auxmap(a);
      return am->sm;
   }
}

/* If 'a' has a SecMap, produce it.  Else produce NULL.  But don't
   allocate one if one doesn't already exist.  This is used by the
   leak checker.
*/
static SecMap* maybe_get_secmap_for ( Addr a )
{
   if (a <= MAX_PRIMARY_ADDRESS) {
      UWord pm_off = a >> 16;
      return primary_map[ pm_off ];
   } else {
      AuxMapEnt* am = maybe_find_in_auxmap(a);
      return am ? am->sm : NULL;
   }
}



/* Produce the secmap for 'a', either from the primary map or by
   ensuring there is an entry for it in the aux primary map.  The
   secmap may not be a distinguished one, since the caller will want
   to be able to write it.  If it is a distinguished secondary, make a
   writable copy of it, install it, and return the copy instead.  (COW
   semantics).
*/
static SecMap* get_secmap_writable ( Addr a )
{
   if (a <= MAX_PRIMARY_ADDRESS) {
      UWord pm_off = a >> 16;
      if (is_distinguished_sm(primary_map[ pm_off ]))
         primary_map[pm_off] = copy_for_writing(primary_map[pm_off]);
      return primary_map[pm_off];
   } else {
      AuxMapEnt* am = find_or_alloc_in_auxmap(a);
      if (is_distinguished_sm(am->sm))
         am->sm = copy_for_writing(am->sm);
      return am->sm;
   }
}


/* --------------- Endianness helpers --------------- */

/* Returns the offset in memory of the byteno-th most significant byte
   in a wordszB-sized word, given the specified endianness. */
static inline UWord byte_offset_w ( UWord wordszB, Bool bigendian, 
                                    UWord byteno ) {
   return bigendian ? (wordszB-1-byteno) : byteno;
}


/* --------------- Fundamental functions --------------- */

static 
void get_abit_and_vbyte ( /*OUT*/UWord* abit, 
                          /*OUT*/UWord* vbyte,
                          Addr a )
{
   SecMap* sm = get_secmap_readable(a);
   *vbyte = 0xFF & sm->vbyte[a & 0xFFFF];
   *abit  = read_bit_array(sm->abits, a & 0xFFFF);
} 

static 
UWord get_abit ( Addr a )
{
   SecMap* sm = get_secmap_readable(a);
   return read_bit_array(sm->abits, a & 0xFFFF);
}

static
void set_abit_and_vbyte ( Addr a, UWord abit, UWord vbyte )
{
   SecMap* sm = get_secmap_writable(a);
   sm->vbyte[a & 0xFFFF] = 0xFF & vbyte;
   write_bit_array(sm->abits, a & 0xFFFF, abit);   
}

static
void set_vbyte ( Addr a, UWord vbyte )
{
   SecMap* sm = get_secmap_writable(a);
   sm->vbyte[a & 0xFFFF] = 0xFF & vbyte;
}


/* --------------- Load/store slow cases. --------------- */

static
ULong mc_LOADVn_slow ( Addr a, SizeT szB, Bool bigendian )
{
   /* Make up a result V word, which contains the loaded data for
      valid addresses and Defined for invalid addresses.  Iterate over
      the bytes in the word, from the most significant down to the
      least. */
   ULong vw          = VGM_WORD64_INVALID;
   SizeT i           = szB-1;
   SizeT n_addrs_bad = 0;
   Addr  ai;
   Bool  aok;
   UWord abit, vbyte;

   PROF_EVENT(30, "mc_LOADVn_slow");
   tl_assert(szB == 8 || szB == 4 || szB == 2 || szB == 1);

   while (True) {
      PROF_EVENT(31, "mc_LOADVn_slow(loop)");
      ai = a+byte_offset_w(szB,bigendian,i);
      get_abit_and_vbyte(&abit, &vbyte, ai);
      aok = abit == VGM_BIT_VALID;
      if (!aok)
         n_addrs_bad++;
      vw <<= 8; 
      vw |= 0xFF & (aok ? vbyte : VGM_BYTE_VALID);
      if (i == 0) break;
      i--;
   }

   if (n_addrs_bad > 0)
      MAC_(record_address_error)( VG_(get_running_tid)(), a, szB, False );

   return vw;
}


static 
void mc_STOREVn_slow ( Addr a, SizeT szB, UWord vbytes, Bool bigendian )
{
   SizeT i;
   SizeT n_addrs_bad = 0;
   UWord abit;
   Bool  aok;
   Addr  ai;

   PROF_EVENT(35, "mc_STOREVn_slow");
   tl_assert(szB == 8 || szB == 4 || szB == 2 || szB == 1);

   /* Dump vbytes in memory, iterating from least to most significant
      byte.  At the same time establish addressibility of the
      location. */
   for (i = 0; i < szB; i++) {
      PROF_EVENT(36, "mc_STOREVn_slow(loop)");
      ai = a+byte_offset_w(szB,bigendian,i);
      abit = get_abit(ai);
      aok = abit == VGM_BIT_VALID;
      if (!aok)
         n_addrs_bad++;
      set_vbyte(ai, vbytes & 0xFF ); 
      vbytes >>= 8;
   }

   /* If an address error has happened, report it. */
   if (n_addrs_bad > 0)
      MAC_(record_address_error)( VG_(get_running_tid)(), a, szB, True );
}


//zz /* Reading/writing of the bitmaps, for aligned word-sized accesses. */
//zz 
//zz static __inline__ UChar get_abits4_ALIGNED ( Addr a )
//zz {
//zz    SecMap* sm;
//zz    UInt    sm_off;
//zz    UChar   abits8;
//zz    PROF_EVENT(24);
//zz #  ifdef VG_DEBUG_MEMORY
//zz    tl_assert(VG_IS_4_ALIGNED(a));
//zz #  endif
//zz    sm     = primary_map[PM_IDX(a)];
//zz    sm_off = SM_OFF(a);
//zz    abits8 = sm->abits[sm_off >> 3];
//zz    abits8 >>= (a & 4 /* 100b */);   /* a & 4 is either 0 or 4 */
//zz    abits8 &= 0x0F;
//zz    return abits8;
//zz }
//zz 
//zz static UInt __inline__ get_vbytes4_ALIGNED ( Addr a )
//zz {
//zz    SecMap* sm     = primary_map[PM_IDX(a)];
//zz    UInt    sm_off = SM_OFF(a);
//zz    PROF_EVENT(25);
//zz #  ifdef VG_DEBUG_MEMORY
//zz    tl_assert(VG_IS_4_ALIGNED(a));
//zz #  endif
//zz    return ((UInt*)(sm->vbyte))[sm_off >> 2];
//zz }
//zz 
//zz 
//zz static void __inline__ set_vbytes4_ALIGNED ( Addr a, UInt vbytes )
//zz {
//zz    SecMap* sm;
//zz    UInt    sm_off;
//zz    ENSURE_MAPPABLE(a, "set_vbytes4_ALIGNED");
//zz    sm     = primary_map[PM_IDX(a)];
//zz    sm_off = SM_OFF(a);
//zz    PROF_EVENT(23);
//zz #  ifdef VG_DEBUG_MEMORY
//zz    tl_assert(VG_IS_4_ALIGNED(a));
//zz #  endif
//zz    ((UInt*)(sm->vbyte))[sm_off >> 2] = vbytes;
//zz }


/*------------------------------------------------------------*/
/*--- Setting permissions over address ranges.             ---*/
/*------------------------------------------------------------*/

/* Given address 'a', find the place where the pointer to a's
   secondary map lives.  If a falls into the primary map, the returned
   value points to one of the entries in primary_map[].  Otherwise,
   the auxiliary primary map is searched for 'a', or an entry is
   created for it; either way, the returned value points to the
   relevant AuxMapEnt's .sm field.

   The point of this is to enable set_address_range_perms to assign
   secondary maps in a uniform way, without worrying about whether a
   given secondary map is pointed to from the main or auxiliary
   primary map.  
*/

static SecMap** find_secmap_binder_for_addr ( Addr aA )
{
   if (aA > MAX_PRIMARY_ADDRESS) {
      AuxMapEnt* am = find_or_alloc_in_auxmap(aA);
      return &am->sm;
   } else {
      UWord a      = (UWord)aA;
      UWord sec_no = (UWord)(a >> 16);
#     if VG_DEBUG_MEMORY >= 1
      tl_assert(sec_no < N_PRIMARY_MAP);
#     endif
      return &primary_map[sec_no];
   }
}


static void set_address_range_perms ( Addr aA, SizeT len, 
                                      UWord example_a_bit,
                                      UWord example_v_bit )
{
   PROF_EVENT(150, "set_address_range_perms");

   /* Check the permissions make sense. */
   tl_assert(example_a_bit == VGM_BIT_VALID 
             || example_a_bit == VGM_BIT_INVALID);
   tl_assert(example_v_bit == VGM_BIT_VALID 
             || example_v_bit == VGM_BIT_INVALID);
   if (example_a_bit == VGM_BIT_INVALID)
      tl_assert(example_v_bit == VGM_BIT_INVALID);

   if (len == 0)
      return;

   if (VG_(clo_verbosity) > 0) {
      if (len > 100 * 1000 * 1000) {
         VG_(message)(Vg_UserMsg, 
                      "Warning: set address range perms: "
                      "large range %u, a %d, v %d",
                      len, example_a_bit, example_v_bit );
      }
   }

   UWord a = (UWord)aA;

#  if VG_DEBUG_MEMORY >= 2

   /*------------------ debug-only case ------------------ */
   SizeT i;

   UWord example_vbyte = BIT_TO_BYTE(example_v_bit);

   tl_assert(sizeof(SizeT) == sizeof(Addr));

   if (0 && len >= 4096)
      VG_(printf)("s_a_r_p(0x%llx, %d, %d,%d)\n", 
                  (ULong)a, len, example_a_bit, example_v_bit);

   if (len == 0)
      return;

   for (i = 0; i < len; i++) {
      set_abit_and_vbyte(a+i, example_a_bit, example_vbyte);
   }

#  else

   /*------------------ standard handling ------------------ */
   UWord    vbits8, abits8, vbits32, v_off, a_off;
   SecMap*  sm;
   SecMap** binder;
   SecMap*  example_dsm;

   /* Decide on the distinguished secondary that we might want
      to use (part of the space-compression scheme). */
   if (example_a_bit == VGM_BIT_INVALID) {
      example_dsm = &sm_distinguished[SM_DIST_NOACCESS];
   } else {
      if (example_v_bit == VGM_BIT_VALID) {
         example_dsm = &sm_distinguished[SM_DIST_ACCESS_DEFINED];
      } else {
         example_dsm = &sm_distinguished[SM_DIST_ACCESS_UNDEFINED];
      }
   }

   /* Make various wider versions of the A/V values to use. */
   vbits8  = BIT_TO_BYTE(example_v_bit);
   abits8  = BIT_TO_BYTE(example_a_bit);
   vbits32 = (vbits8 << 24) | (vbits8 << 16) | (vbits8 << 8) | vbits8;

   /* Slowly do parts preceding 8-byte alignment. */
   while (True) {
      if (len == 0) break;
      PROF_EVENT(151, "set_address_range_perms-loop1-pre");
      if (VG_IS_8_ALIGNED(a)) break;
      set_abit_and_vbyte( a, example_a_bit, vbits8 );
      a++;
      len--;
   }   

   if (len == 0)
      return;

   tl_assert(VG_IS_8_ALIGNED(a) && len > 0);

   /* Now go in steps of 8 bytes. */
   binder = find_secmap_binder_for_addr(a);

   while (True) {

      if (len < 8) break;

      PROF_EVENT(152, "set_address_range_perms-loop8");

      if ((a & SECONDARY_MASK) == 0) {
         /* we just traversed a primary map boundary, so update the
            binder. */
         binder = find_secmap_binder_for_addr(a);
         PROF_EVENT(153, "set_address_range_perms-update-binder");

	 /* Space-optimisation.  If we are setting the entire
            secondary map, just point this entry at one of our
            distinguished secondaries.  However, only do that if it
            already points at a distinguished secondary, since doing
            otherwise would leak the existing secondary.  We could do
            better and free up any pre-existing non-distinguished
            secondary at this point, since we are guaranteed that each
            non-dist secondary only has one pointer to it, and we have
            that pointer right here. */
         if (len >= SECONDARY_SIZE && is_distinguished_sm(*binder)) {
            PROF_EVENT(154, "set_address_range_perms-entire-secmap");
            *binder = example_dsm;
            len -= SECONDARY_SIZE;
            a += SECONDARY_SIZE;
            continue;
         }
      }

      /* If the primary is already pointing to a distinguished map
         with the same properties as we're trying to set, then leave
         it that way. */
      if (*binder == example_dsm) {
         a += 8;
         len -= 8;
         continue;
      }

      /* Make sure it's OK to write the secondary. */
      if (is_distinguished_sm(*binder))
         *binder = copy_for_writing(*binder);

      sm = *binder;
      v_off = a & 0xFFFF;
      a_off = v_off >> 3;
      sm->abits[a_off] = (UChar)abits8;
      ((UInt*)(sm->vbyte))[(v_off >> 2) + 0] = (UInt)vbits32;
      ((UInt*)(sm->vbyte))[(v_off >> 2) + 1] = (UInt)vbits32;

      a += 8;
      len -= 8;
   }

   if (len == 0)
      return;

   tl_assert(VG_IS_8_ALIGNED(a) && len > 0 && len < 8);

   /* Finish the upper fragment. */
   while (True) {
      if (len == 0) break;
      PROF_EVENT(155, "set_address_range_perms-loop1-post");
      set_abit_and_vbyte ( a, example_a_bit, vbits8 );
      a++;
      len--;
   }   

#  endif
}


/* --- Set permissions for arbitrary address ranges --- */

static void mc_make_noaccess ( Addr a, SizeT len )
{
   PROF_EVENT(40, "mc_make_noaccess");
   DEBUG("mc_make_noaccess(%p, %llu)\n", a, (ULong)len);
   set_address_range_perms ( a, len, VGM_BIT_INVALID, VGM_BIT_INVALID );
}

static void mc_make_writable ( Addr a, SizeT len )
{
   PROF_EVENT(41, "mc_make_writable");
   DEBUG("mc_make_writable(%p, %llu)\n", a, (ULong)len);
   set_address_range_perms ( a, len, VGM_BIT_VALID, VGM_BIT_INVALID );
}

static void mc_make_readable ( Addr a, SizeT len )
{
   PROF_EVENT(42, "mc_make_readable");
   DEBUG("mc_make_readable(%p, %llu)\n", a, (ULong)len);
   set_address_range_perms ( a, len, VGM_BIT_VALID, VGM_BIT_VALID );
}


/* --- Block-copy permissions (needed for implementing realloc()). --- */

static void mc_copy_address_range_state ( Addr src, Addr dst, SizeT len )
{
   SizeT i;
   UWord abit, vbyte;

   DEBUG("mc_copy_address_range_state\n");

   PROF_EVENT(50, "mc_copy_address_range_state");
   for (i = 0; i < len; i++) {
      PROF_EVENT(51, "mc_copy_address_range_state(loop)");
      get_abit_and_vbyte( &abit, &vbyte, src+i );
      set_abit_and_vbyte( dst+i, abit, vbyte );
   }
}


/* --- Fast case permission setters, for dealing with stacks. --- */

static __inline__
void make_aligned_word32_writable ( Addr aA )
{
   PROF_EVENT(300, "make_aligned_word32_writable");

#  if VG_DEBUG_MEMORY >= 2
   mc_make_writable(aA, 4);
#  else

   if (EXPECTED_NOT_TAKEN(aA > MAX_PRIMARY_ADDRESS)) {
      PROF_EVENT(301, "make_aligned_word32_writable-slow1");
      mc_make_writable(aA, 4);
      return;
   }

   UWord a      = (UWord)aA;
   UWord sec_no = (UWord)(a >> 16);
#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   if (EXPECTED_NOT_TAKEN(is_distinguished_sm(primary_map[sec_no])))
      primary_map[sec_no] = copy_for_writing(primary_map[sec_no]);

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;

   /* Paint the new area as uninitialised. */
   ((UInt*)(sm->vbyte))[v_off >> 2] = VGM_WORD32_INVALID;

   UWord mask = 0x0F;
   mask <<= (a & 4 /* 100b */);   /* a & 4 is either 0 or 4 */
   /* mask now contains 1s where we wish to make address bits valid
      (0s). */
   sm->abits[a_off] &= ~mask;
#  endif
}


static __inline__
void make_aligned_word32_noaccess ( Addr aA )
{
   PROF_EVENT(310, "make_aligned_word32_noaccess");

#  if VG_DEBUG_MEMORY >= 2
   mc_make_noaccess(aA, 4);
#  else

   if (EXPECTED_NOT_TAKEN(aA > MAX_PRIMARY_ADDRESS)) {
      PROF_EVENT(311, "make_aligned_word32_noaccess-slow1");
      mc_make_noaccess(aA, 4);
      return;
   }

   UWord a      = (UWord)aA;
   UWord sec_no = (UWord)(a >> 16);
#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   if (EXPECTED_NOT_TAKEN(is_distinguished_sm(primary_map[sec_no])))
      primary_map[sec_no] = copy_for_writing(primary_map[sec_no]);

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;

   /* Paint the abandoned data as uninitialised.  Probably not
      necessary, but still .. */
   ((UInt*)(sm->vbyte))[v_off >> 2] = VGM_WORD32_INVALID;

   UWord mask = 0x0F;
   mask <<= (a & 4 /* 100b */);   /* a & 4 is either 0 or 4 */
   /* mask now contains 1s where we wish to make address bits invalid
      (1s). */
   sm->abits[a_off] |= mask;
#  endif
}


/* Nb: by "aligned" here we mean 8-byte aligned */
static __inline__
void make_aligned_word64_writable ( Addr aA )
{
   PROF_EVENT(320, "make_aligned_word64_writable");

#  if VG_DEBUG_MEMORY >= 2
   mc_make_writable(aA, 8);
#  else

   if (EXPECTED_NOT_TAKEN(aA > MAX_PRIMARY_ADDRESS)) {
      PROF_EVENT(321, "make_aligned_word64_writable-slow1");
      mc_make_writable(aA, 8);
      return;
   }

   UWord a      = (UWord)aA;
   UWord sec_no = (UWord)(a >> 16);
#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   if (EXPECTED_NOT_TAKEN(is_distinguished_sm(primary_map[sec_no])))
      primary_map[sec_no] = copy_for_writing(primary_map[sec_no]);

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;

   /* Paint the new area as uninitialised. */
   ((ULong*)(sm->vbyte))[v_off >> 3] = VGM_WORD64_INVALID;

   /* Make the relevant area accessible. */
   sm->abits[a_off] = VGM_BYTE_VALID;
#  endif
}


static __inline__
void make_aligned_word64_noaccess ( Addr aA )
{
   PROF_EVENT(330, "make_aligned_word64_noaccess");

#  if VG_DEBUG_MEMORY >= 2
   mc_make_noaccess(aA, 8);
#  else

   if (EXPECTED_NOT_TAKEN(aA > MAX_PRIMARY_ADDRESS)) {
      PROF_EVENT(331, "make_aligned_word64_noaccess-slow1");
      mc_make_noaccess(aA, 8);
      return;
   }

   UWord a      = (UWord)aA;
   UWord sec_no = (UWord)(a >> 16);
#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   if (EXPECTED_NOT_TAKEN(is_distinguished_sm(primary_map[sec_no])))
      primary_map[sec_no] = copy_for_writing(primary_map[sec_no]);

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;

   /* Paint the abandoned data as uninitialised.  Probably not
      necessary, but still .. */
   ((ULong*)(sm->vbyte))[v_off >> 3] = VGM_WORD64_INVALID;

   /* Make the abandoned area inaccessible. */
   sm->abits[a_off] = VGM_BYTE_INVALID;
#  endif
}


/* The stack-pointer update handling functions */
SP_UPDATE_HANDLERS ( make_aligned_word32_writable,
                     make_aligned_word32_noaccess,
                     make_aligned_word64_writable,
                     make_aligned_word64_noaccess,
                     mc_make_writable,
                     mc_make_noaccess 
                   );


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
static Bool mc_check_noaccess ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UWord abit;
   PROF_EVENT(60, "mc_check_noaccess");
   for (i = 0; i < len; i++) {
      PROF_EVENT(61, "mc_check_noaccess(loop)");
      abit = get_abit(a);
      if (abit == VGM_BIT_VALID) {
         if (bad_addr != NULL) 
            *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
}

static Bool mc_check_writable ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UWord abit;
   PROF_EVENT(62, "mc_check_writable");
   for (i = 0; i < len; i++) {
      PROF_EVENT(63, "mc_check_writable(loop)");
      abit = get_abit(a);
      if (abit == VGM_BIT_INVALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
}

static MC_ReadResult mc_check_readable ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UWord abit;
   UWord vbyte;

   PROF_EVENT(64, "mc_check_readable");
   DEBUG("mc_check_readable\n");
   for (i = 0; i < len; i++) {
      PROF_EVENT(65, "mc_check_readable(loop)");
      get_abit_and_vbyte(&abit, &vbyte, a);
      // Report addressability errors in preference to definedness errors
      // by checking the A bits first.
      if (abit != VGM_BIT_VALID) {
         if (bad_addr != NULL) 
            *bad_addr = a;
         return MC_AddrErr;
      }
      if (vbyte != VGM_BYTE_VALID) {
         if (bad_addr != NULL) 
            *bad_addr = a;
         return MC_ValueErr;
      }
      a++;
   }
   return MC_Ok;
}


/* Check a zero-terminated ascii string.  Tricky -- don't want to
   examine the actual bytes, to find the end, until we're sure it is
   safe to do so. */

static Bool mc_check_readable_asciiz ( Addr a, Addr* bad_addr )
{
   UWord abit;
   UWord vbyte;
   PROF_EVENT(66, "mc_check_readable_asciiz");
   DEBUG("mc_check_readable_asciiz\n");
   while (True) {
      PROF_EVENT(67, "mc_check_readable_asciiz(loop)");
      get_abit_and_vbyte(&abit, &vbyte, a);
      // As in mc_check_readable(), check A bits first
      if (abit != VGM_BIT_VALID) {
         if (bad_addr != NULL) 
            *bad_addr = a;
         return MC_AddrErr;
      }
      if (vbyte != VGM_BYTE_VALID) {
         if (bad_addr != NULL) 
            *bad_addr = a;
         return MC_ValueErr;
      }
      /* Ok, a is safe to read. */
      if (* ((UChar*)a) == 0) 
         return MC_Ok;
      a++;
   }
}


/*------------------------------------------------------------*/
/*--- Memory event handlers                                ---*/
/*------------------------------------------------------------*/

static
void mc_check_is_writable ( CorePart part, ThreadId tid, Char* s,
                            Addr base, SizeT size )
{
   Bool ok;
   Addr bad_addr;

   VGP_PUSHCC(VgpCheckMem);

   /* VG_(message)(Vg_DebugMsg,"check is writable: %x .. %x",
                               base,base+size-1); */
   ok = mc_check_writable ( base, size, &bad_addr );
   if (!ok) {
      switch (part) {
      case Vg_CoreSysCall:
         MAC_(record_param_error) ( tid, bad_addr, /*isReg*/False,
                                    /*isUnaddr*/True, s );
         break;

      case Vg_CorePThread:
      case Vg_CoreSignal:
         MAC_(record_core_mem_error)( tid, /*isUnaddr*/True, s );
         break;

      default:
         VG_(tool_panic)("mc_check_is_writable: unexpected CorePart");
      }
   }

   VGP_POPCC(VgpCheckMem);
}

static
void mc_check_is_readable ( CorePart part, ThreadId tid, Char* s,
                            Addr base, SizeT size )
{     
   Addr bad_addr;
   MC_ReadResult res;

   VGP_PUSHCC(VgpCheckMem);
   
   /* VG_(message)(Vg_DebugMsg,"check is readable: %x .. %x",
                               base,base+size-1); */
   res = mc_check_readable ( base, size, &bad_addr );
   if (MC_Ok != res) {
      Bool isUnaddr = ( MC_AddrErr == res ? True : False );
      
      switch (part) {
      case Vg_CoreSysCall:
         MAC_(record_param_error) ( tid, bad_addr, /*isReg*/False,
                                    isUnaddr, s );
         break;
      
      case Vg_CorePThread:
         MAC_(record_core_mem_error)( tid, isUnaddr, s );
         break;

      /* If we're being asked to jump to a silly address, record an error 
         message before potentially crashing the entire system. */
      case Vg_CoreTranslate:
         MAC_(record_jump_error)( tid, bad_addr );
         break;

      default:
         VG_(tool_panic)("mc_check_is_readable: unexpected CorePart");
      }
   }
   VGP_POPCC(VgpCheckMem);
}

static
void mc_check_is_readable_asciiz ( CorePart part, ThreadId tid,
                                   Char* s, Addr str )
{
   MC_ReadResult res;
   Addr bad_addr = 0;   // shut GCC up
   /* VG_(message)(Vg_DebugMsg,"check is readable asciiz: 0x%x",str); */

   VGP_PUSHCC(VgpCheckMem);

   tl_assert(part == Vg_CoreSysCall);
   res = mc_check_readable_asciiz ( (Addr)str, &bad_addr );
   if (MC_Ok != res) {
      Bool isUnaddr = ( MC_AddrErr == res ? True : False );
      MAC_(record_param_error) ( tid, bad_addr, /*isReg*/False, isUnaddr, s );
   }

   VGP_POPCC(VgpCheckMem);
}

static
void mc_new_mem_startup( Addr a, SizeT len, Bool rr, Bool ww, Bool xx )
{
   /* Ignore the permissions, just make it readable.  Seems to work... */
   DEBUG("mc_new_mem_startup(%p, %llu, rr=%u, ww=%u, xx=%u)\n",
         a,(ULong)len,rr,ww,xx);
   mc_make_readable(a, len);
}

static
void mc_new_mem_heap ( Addr a, SizeT len, Bool is_inited )
{
   if (is_inited) {
      mc_make_readable(a, len);
   } else {
      mc_make_writable(a, len);
   }
}

static
void mc_new_mem_mmap ( Addr a, SizeT len, Bool rr, Bool ww, Bool xx )
{
   mc_make_readable(a, len);
}

static
void mc_post_mem_write(CorePart part, ThreadId tid, Addr a, SizeT len)
{
   mc_make_readable(a, len);
}


/*------------------------------------------------------------*/
/*--- Register event handlers                              ---*/
/*------------------------------------------------------------*/

/* When some chunk of guest state is written, mark the corresponding
   shadow area as valid.  This is used to initialise arbitrarily large
   chunks of guest state, hence the (somewhat arbitrary) 1024 limit.
*/
static void mc_post_reg_write ( CorePart part, ThreadId tid, 
                                OffT offset, SizeT size)
{
   UChar area[1024];
   tl_assert(size <= 1024);
   VG_(memset)(area, VGM_BYTE_VALID, size);
   VG_(set_shadow_regs_area)( tid, offset, size, area );
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
      if (area[i] != VGM_BYTE_VALID) {
         bad = True;
         break;
      }
   }

   if (bad)
      MAC_(record_param_error) ( tid, 0, /*isReg*/True, /*isUnaddr*/False, s );
}


/*------------------------------------------------------------*/
/*--- Functions called directly from generated code:       ---*/
/*--- Load/store handlers.                                 ---*/
/*------------------------------------------------------------*/

/* Types:  LOADV4, LOADV2, LOADV1 are:
               UWord fn ( Addr a )
   so they return 32-bits on 32-bit machines and 64-bits on
   64-bit machines.  Addr has the same size as a host word.

   LOADV8 is always  ULong fn ( Addr a )

   Similarly for STOREV1, STOREV2, STOREV4, the supplied vbits
   are a UWord, and for STOREV8 they are a ULong.
*/

/* ------------------------ Size = 8 ------------------------ */

VGA_REGPARM(1)
ULong MC_(helperc_LOADV8) ( Addr aA )
{
   PROF_EVENT(200, "helperc_LOADV8");

#  if VG_DEBUG_MEMORY >= 2
   return mc_LOADVn_slow( aA, 8, False/*littleendian*/ );
#  else

   const UWord mask = ~((0x10000-8) | ((N_PRIMARY_MAP-1) << 16));
   UWord a = (UWord)aA;

   /* If any part of 'a' indicated by the mask is 1, either 'a' is not
      naturally aligned, or 'a' exceeds the range covered by the
      primary map.  Either way we defer to the slow-path case. */
   if (EXPECTED_NOT_TAKEN(a & mask)) {
      PROF_EVENT(201, "helperc_LOADV8-slow1");
      return (UWord)mc_LOADVn_slow( aA, 8, False/*littleendian*/ );
   }

   UWord sec_no = (UWord)(a >> 16);

#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;
   UWord   abits = (UWord)(sm->abits[a_off]);

   if (EXPECTED_TAKEN(abits == VGM_BYTE_VALID)) {
      /* Handle common case quickly: a is suitably aligned, is mapped,
         and is addressible. */
      return ((ULong*)(sm->vbyte))[ v_off >> 3 ];
   } else {
      /* Slow but general case. */
      PROF_EVENT(202, "helperc_LOADV8-slow2");
      return mc_LOADVn_slow( a, 8, False/*littleendian*/ );
   }

#  endif
}

VGA_REGPARM(1)
void MC_(helperc_STOREV8) ( Addr aA, ULong vbytes )
{
   PROF_EVENT(210, "helperc_STOREV8");

#  if VG_DEBUG_MEMORY >= 2
   mc_STOREVn_slow( aA, 8, vbytes, False/*littleendian*/ );
#  else

   const UWord mask = ~((0x10000-8) | ((N_PRIMARY_MAP-1) << 16));
   UWord a = (UWord)aA;

   /* If any part of 'a' indicated by the mask is 1, either 'a' is not
      naturally aligned, or 'a' exceeds the range covered by the
      primary map.  Either way we defer to the slow-path case. */
   if (EXPECTED_NOT_TAKEN(a & mask)) {
      PROF_EVENT(211, "helperc_STOREV8-slow1");
      mc_STOREVn_slow( aA, 8, vbytes, False/*littleendian*/ );
      return;
   }

   UWord sec_no = (UWord)(a >> 16);

#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;
   UWord   abits = (UWord)(sm->abits[a_off]);

   if (EXPECTED_TAKEN(!is_distinguished_sm(sm) 
                      && abits == VGM_BYTE_VALID)) {
      /* Handle common case quickly: a is suitably aligned, is mapped,
         and is addressible. */
      ((ULong*)(sm->vbyte))[ v_off >> 3 ] = vbytes;
   } else {
      /* Slow but general case. */
      PROF_EVENT(212, "helperc_STOREV8-slow2");
      mc_STOREVn_slow( aA, 8, vbytes, False/*littleendian*/ );
   }
#  endif
}

/* ------------------------ Size = 4 ------------------------ */

VGA_REGPARM(1)
UWord MC_(helperc_LOADV4) ( Addr aA )
{
   PROF_EVENT(220, "helperc_LOADV4");

#  if VG_DEBUG_MEMORY >= 2
   return (UWord)mc_LOADVn_slow( aA, 4, False/*littleendian*/ );
#  else

   const UWord mask = ~((0x10000-4) | ((N_PRIMARY_MAP-1) << 16));
   UWord a = (UWord)aA;

   /* If any part of 'a' indicated by the mask is 1, either 'a' is not
      naturally aligned, or 'a' exceeds the range covered by the
      primary map.  Either way we defer to the slow-path case. */
   if (EXPECTED_NOT_TAKEN(a & mask)) {
      PROF_EVENT(221, "helperc_LOADV4-slow1");
      return (UWord)mc_LOADVn_slow( aA, 4, False/*littleendian*/ );
   }

   UWord sec_no = (UWord)(a >> 16);

#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;
   UWord   abits = (UWord)(sm->abits[a_off]);
   abits >>= (a & 4);
   abits &= 15;
   if (EXPECTED_TAKEN(abits == VGM_NIBBLE_VALID)) {
      /* Handle common case quickly: a is suitably aligned, is mapped,
         and is addressible. */
      /* On a 32-bit platform, simply hoick the required 32 bits out of
         the vbyte array.  On a 64-bit platform, also set the upper 32
         bits to 1 ("undefined"), just in case.  This almost certainly
         isn't necessary, but be paranoid. */
      UWord ret = (UWord)0xFFFFFFFF00000000ULL;
      ret |= (UWord)( ((UInt*)(sm->vbyte))[ v_off >> 2 ] );
      return ret;
   } else {
      /* Slow but general case. */
      PROF_EVENT(222, "helperc_LOADV4-slow2");
      return (UWord)mc_LOADVn_slow( a, 4, False/*littleendian*/ );
   }

#  endif
}

VGA_REGPARM(2)
void MC_(helperc_STOREV4) ( Addr aA, UWord vbytes )
{
   PROF_EVENT(230, "helperc_STOREV4");

#  if VG_DEBUG_MEMORY >= 2
   mc_STOREVn_slow( aA, 4, (ULong)vbytes, False/*littleendian*/ );
#  else

   const UWord mask = ~((0x10000-4) | ((N_PRIMARY_MAP-1) << 16));
   UWord a = (UWord)aA;

   /* If any part of 'a' indicated by the mask is 1, either 'a' is not
      naturally aligned, or 'a' exceeds the range covered by the
      primary map.  Either way we defer to the slow-path case. */
   if (EXPECTED_NOT_TAKEN(a & mask)) {
      PROF_EVENT(231, "helperc_STOREV4-slow1");
      mc_STOREVn_slow( aA, 4, (ULong)vbytes, False/*littleendian*/ );
      return;
   }

   UWord sec_no = (UWord)(a >> 16);

#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;
   UWord   abits = (UWord)(sm->abits[a_off]);
   abits >>= (a & 4);
   abits &= 15;
   if (EXPECTED_TAKEN(!is_distinguished_sm(sm) 
                      && abits == VGM_NIBBLE_VALID)) {
      /* Handle common case quickly: a is suitably aligned, is mapped,
         and is addressible. */
      ((UInt*)(sm->vbyte))[ v_off >> 2 ] = (UInt)vbytes;
   } else {
      /* Slow but general case. */
      PROF_EVENT(232, "helperc_STOREV4-slow2");
      mc_STOREVn_slow( aA, 4, (ULong)vbytes, False/*littleendian*/ );
   }
#  endif
}

/* ------------------------ Size = 2 ------------------------ */

VGA_REGPARM(1)
UWord MC_(helperc_LOADV2) ( Addr aA )
{
   PROF_EVENT(240, "helperc_LOADV2");

#  if VG_DEBUG_MEMORY >= 2
   return (UWord)mc_LOADVn_slow( aA, 2, False/*littleendian*/ );
#  else

   const UWord mask = ~((0x10000-2) | ((N_PRIMARY_MAP-1) << 16));
   UWord a = (UWord)aA;

   /* If any part of 'a' indicated by the mask is 1, either 'a' is not
      naturally aligned, or 'a' exceeds the range covered by the
      primary map.  Either way we defer to the slow-path case. */
   if (EXPECTED_NOT_TAKEN(a & mask)) {
      PROF_EVENT(241, "helperc_LOADV2-slow1");
      return (UWord)mc_LOADVn_slow( aA, 2, False/*littleendian*/ );
   }

   UWord sec_no = (UWord)(a >> 16);

#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;
   UWord   abits = (UWord)(sm->abits[a_off]);
   if (EXPECTED_TAKEN(abits == VGM_BYTE_VALID)) {
      /* Handle common case quickly: a is mapped, and the entire
         word32 it lives in is addressible. */
      /* Set the upper 16/48 bits of the result to 1 ("undefined"),
         just in case.  This almost certainly isn't necessary, but be
         paranoid. */
      return (~(UWord)0xFFFF)
             |
             (UWord)( ((UShort*)(sm->vbyte))[ v_off >> 1 ] );
   } else {
      /* Slow but general case. */
      PROF_EVENT(242, "helperc_LOADV2-slow2");
      return (UWord)mc_LOADVn_slow( aA, 2, False/*littleendian*/ );
   }

#  endif
}

VGA_REGPARM(2)
void MC_(helperc_STOREV2) ( Addr aA, UWord vbytes )
{
   PROF_EVENT(250, "helperc_STOREV2");

#  if VG_DEBUG_MEMORY >= 2
   mc_STOREVn_slow( aA, 2, (ULong)vbytes, False/*littleendian*/ );
#  else

   const UWord mask = ~((0x10000-2) | ((N_PRIMARY_MAP-1) << 16));
   UWord a = (UWord)aA;

   /* If any part of 'a' indicated by the mask is 1, either 'a' is not
      naturally aligned, or 'a' exceeds the range covered by the
      primary map.  Either way we defer to the slow-path case. */
   if (EXPECTED_NOT_TAKEN(a & mask)) {
      PROF_EVENT(251, "helperc_STOREV2-slow1");
      mc_STOREVn_slow( aA, 2, (ULong)vbytes, False/*littleendian*/ );
      return;
   }

   UWord sec_no = (UWord)(a >> 16);

#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;
   UWord   abits = (UWord)(sm->abits[a_off]);
   if (EXPECTED_TAKEN(!is_distinguished_sm(sm) 
                      && abits == VGM_BYTE_VALID)) {
      /* Handle common case quickly. */
      ((UShort*)(sm->vbyte))[ v_off >> 1 ] = (UShort)vbytes;
   } else {
      /* Slow but general case. */
      PROF_EVENT(252, "helperc_STOREV2-slow2");
      mc_STOREVn_slow( aA, 2, (ULong)vbytes, False/*littleendian*/ );
   }
#  endif
}

/* ------------------------ Size = 1 ------------------------ */

VGA_REGPARM(1)
UWord MC_(helperc_LOADV1) ( Addr aA )
{
   PROF_EVENT(260, "helperc_LOADV1");

#  if VG_DEBUG_MEMORY >= 2
   return (UWord)mc_LOADVn_slow( aA, 1, False/*littleendian*/ );
#  else

   const UWord mask = ~((0x10000-1) | ((N_PRIMARY_MAP-1) << 16));
   UWord a = (UWord)aA;

   /* If any part of 'a' indicated by the mask is 1, it means 'a'
      exceeds the range covered by the primary map.  In which case we
      defer to the slow-path case. */
   if (EXPECTED_NOT_TAKEN(a & mask)) {
      PROF_EVENT(261, "helperc_LOADV1-slow1");
      return (UWord)mc_LOADVn_slow( aA, 1, False/*littleendian*/ );
   }

   UWord sec_no = (UWord)(a >> 16);

#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;
   UWord   abits = (UWord)(sm->abits[a_off]);
   if (EXPECTED_TAKEN(abits == VGM_BYTE_VALID)) {
      /* Handle common case quickly: a is mapped, and the entire
         word32 it lives in is addressible. */
      /* Set the upper 24/56 bits of the result to 1 ("undefined"),
         just in case.  This almost certainly isn't necessary, but be
         paranoid. */
      return (~(UWord)0xFF)
             |
             (UWord)( ((UChar*)(sm->vbyte))[ v_off ] );
   } else {
      /* Slow but general case. */
      PROF_EVENT(262, "helperc_LOADV1-slow2");
      return (UWord)mc_LOADVn_slow( aA, 1, False/*littleendian*/ );
   }
#  endif
}


VGA_REGPARM(2)
void MC_(helperc_STOREV1) ( Addr aA, UWord vbyte )
{
   PROF_EVENT(270, "helperc_STOREV1");

#  if VG_DEBUG_MEMORY >= 2
   mc_STOREVn_slow( aA, 1, (ULong)vbyte, False/*littleendian*/ );
#  else

   const UWord mask = ~((0x10000-1) | ((N_PRIMARY_MAP-1) << 16));
   UWord a = (UWord)aA;
   /* If any part of 'a' indicated by the mask is 1, it means 'a'
      exceeds the range covered by the primary map.  In which case we
      defer to the slow-path case. */
   if (EXPECTED_NOT_TAKEN(a & mask)) {
      PROF_EVENT(271, "helperc_STOREV1-slow1");
      mc_STOREVn_slow( aA, 1, (ULong)vbyte, False/*littleendian*/ );
      return;
   }

   UWord sec_no = (UWord)(a >> 16);

#  if VG_DEBUG_MEMORY >= 1
   tl_assert(sec_no < N_PRIMARY_MAP);
#  endif

   SecMap* sm    = primary_map[sec_no];
   UWord   v_off = a & 0xFFFF;
   UWord   a_off = v_off >> 3;
   UWord   abits = (UWord)(sm->abits[a_off]);
   if (EXPECTED_TAKEN(!is_distinguished_sm(sm) 
                      && abits == VGM_BYTE_VALID)) {
      /* Handle common case quickly: a is mapped, the entire word32 it
         lives in is addressible. */
      ((UChar*)(sm->vbyte))[ v_off ] = (UChar)vbyte;
   } else {
      PROF_EVENT(272, "helperc_STOREV1-slow2");
      mc_STOREVn_slow( aA, 1, (ULong)vbyte, False/*littleendian*/ );
   }

#  endif
}


/*------------------------------------------------------------*/
/*--- Functions called directly from generated code:       ---*/
/*--- Value-check failure handlers.                        ---*/
/*------------------------------------------------------------*/

void MC_(helperc_value_check0_fail) ( void )
{
   MC_(record_value_error) ( VG_(get_running_tid)(), 0 );
}

void MC_(helperc_value_check1_fail) ( void )
{
   MC_(record_value_error) ( VG_(get_running_tid)(), 1 );
}

void MC_(helperc_value_check4_fail) ( void )
{
   MC_(record_value_error) ( VG_(get_running_tid)(), 4 );
}

void MC_(helperc_value_check8_fail) ( void )
{
   MC_(record_value_error) ( VG_(get_running_tid)(), 8 );
}

VGA_REGPARM(1) void MC_(helperc_complain_undef) ( HWord sz )
{
   MC_(record_value_error) ( VG_(get_running_tid)(), (Int)sz );
}


//zz /*------------------------------------------------------------*/
//zz /*--- Metadata get/set functions, for client requests.     ---*/
//zz /*------------------------------------------------------------*/
//zz 
//zz /* Copy Vbits for src into vbits. Returns: 1 == OK, 2 == alignment
//zz    error, 3 == addressing error. */
//zz static Int mc_get_or_set_vbits_for_client ( 
//zz    ThreadId tid,
//zz    Addr dataV, 
//zz    Addr vbitsV, 
//zz    SizeT size, 
//zz    Bool setting /* True <=> set vbits,  False <=> get vbits */ 
//zz )
//zz {
//zz    Bool addressibleD = True;
//zz    Bool addressibleV = True;
//zz    UInt* data  = (UInt*)dataV;
//zz    UInt* vbits = (UInt*)vbitsV;
//zz    SizeT szW   = size / 4; /* sigh */
//zz    SizeT i;
//zz    UInt* dataP  = NULL; /* bogus init to keep gcc happy */
//zz    UInt* vbitsP = NULL; /* ditto */
//zz 
//zz    /* Check alignment of args. */
//zz    if (!(VG_IS_4_ALIGNED(data) && VG_IS_4_ALIGNED(vbits)))
//zz       return 2;
//zz    if ((size & 3) != 0)
//zz       return 2;
//zz   
//zz    /* Check that arrays are addressible. */
//zz    for (i = 0; i < szW; i++) {
//zz       dataP  = &data[i];
//zz       vbitsP = &vbits[i];
//zz       if (get_abits4_ALIGNED((Addr)dataP) != VGM_NIBBLE_VALID) {
//zz          addressibleD = False;
//zz          break;
//zz       }
//zz       if (get_abits4_ALIGNED((Addr)vbitsP) != VGM_NIBBLE_VALID) {
//zz          addressibleV = False;
//zz          break;
//zz       }
//zz    }
//zz    if (!addressibleD) {
//zz       MAC_(record_address_error)( tid, (Addr)dataP, 4, 
//zz                                   setting ? True : False );
//zz       return 3;
//zz    }
//zz    if (!addressibleV) {
//zz       MAC_(record_address_error)( tid, (Addr)vbitsP, 4, 
//zz                                   setting ? False : True );
//zz       return 3;
//zz    }
//zz  
//zz    /* Do the copy */
//zz    if (setting) {
//zz       /* setting */
//zz       for (i = 0; i < szW; i++) {
//zz          if (get_vbytes4_ALIGNED( (Addr)&vbits[i] ) != VGM_WORD_VALID)
//zz             MC_(record_value_error)(tid, 4);
//zz          set_vbytes4_ALIGNED( (Addr)&data[i], vbits[i] );
//zz       }
//zz    } else {
//zz       /* getting */
//zz       for (i = 0; i < szW; i++) {
//zz          vbits[i] = get_vbytes4_ALIGNED( (Addr)&data[i] );
//zz          set_vbytes4_ALIGNED( (Addr)&vbits[i], VGM_WORD_VALID );
//zz       }
//zz    }
//zz 
//zz    return 1;
//zz }


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
   if (mc_check_readable( a, sizeof(UWord), NULL ) == MC_Ok) {
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
   MAC_(do_detect_memory_leaks) ( 
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

   /* Build the 3 distinguished secondaries */
   tl_assert(VGM_BIT_INVALID == 1);
   tl_assert(VGM_BIT_VALID == 0);
   tl_assert(VGM_BYTE_INVALID == 0xFF);
   tl_assert(VGM_BYTE_VALID == 0);

   /* Set A invalid, V invalid. */
   sm = &sm_distinguished[SM_DIST_NOACCESS];
   for (i = 0; i < 65536; i++)
      sm->vbyte[i] = VGM_BYTE_INVALID;
   for (i = 0; i < 8192; i++)
      sm->abits[i] = VGM_BYTE_INVALID;

   /* Set A valid, V invalid. */
   sm = &sm_distinguished[SM_DIST_ACCESS_UNDEFINED];
   for (i = 0; i < 65536; i++)
      sm->vbyte[i] = VGM_BYTE_INVALID;
   for (i = 0; i < 8192; i++)
      sm->abits[i] = VGM_BYTE_VALID;

   /* Set A valid, V valid. */
   sm = &sm_distinguished[SM_DIST_ACCESS_DEFINED];
   for (i = 0; i < 65536; i++)
      sm->vbyte[i] = VGM_BYTE_VALID;
   for (i = 0; i < 8192; i++)
      sm->abits[i] = VGM_BYTE_VALID;

   /* Set up the primary map. */
   /* These entries gradually get overwritten as the used address
      space expands. */
   for (i = 0; i < N_PRIMARY_MAP; i++)
      primary_map[i] = &sm_distinguished[SM_DIST_NOACCESS];

   /* auxmap_size = auxmap_used = 0; 
      no ... these are statically initialised */
}


/*------------------------------------------------------------*/
/*--- Sanity check machinery (permanently engaged)         ---*/
/*------------------------------------------------------------*/

Bool TL_(cheap_sanity_check) ( void )
{
   /* nothing useful we can rapidly check */
   n_sanity_cheap++;
   PROF_EVENT(490, "cheap_sanity_check");
   return True;
}

Bool TL_(expensive_sanity_check) ( void )
{
   Int     i, n_secmaps_found;
   SecMap* sm;
   Bool    bad = False;

   n_sanity_expensive++;
   PROF_EVENT(491, "expensive_sanity_check");

   /* Check that the 3 distinguished SMs are still as they should
      be. */

   /* Check A invalid, V invalid. */
   sm = &sm_distinguished[SM_DIST_NOACCESS];
   for (i = 0; i < 65536; i++)
      if (!(sm->vbyte[i] == VGM_BYTE_INVALID))
         bad = True;
   for (i = 0; i < 8192; i++)
      if (!(sm->abits[i] == VGM_BYTE_INVALID))
         bad = True;

   /* Check A valid, V invalid. */
   sm = &sm_distinguished[SM_DIST_ACCESS_UNDEFINED];
   for (i = 0; i < 65536; i++)
      if (!(sm->vbyte[i] == VGM_BYTE_INVALID))
         bad = True;
   for (i = 0; i < 8192; i++)
      if (!(sm->abits[i] == VGM_BYTE_VALID))
         bad = True;

   /* Check A valid, V valid. */
   sm = &sm_distinguished[SM_DIST_ACCESS_DEFINED];
   for (i = 0; i < 65536; i++)
      if (!(sm->vbyte[i] == VGM_BYTE_VALID))
         bad = True;
   for (i = 0; i < 8192; i++)
      if (!(sm->abits[i] == VGM_BYTE_VALID))
         bad = True;

   if (bad) {
      VG_(printf)("memcheck expensive sanity: "
                  "distinguished_secondaries have changed\n");
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

   if (n_secmaps_found != n_secmaps_issued)
      bad = True;

   if (bad) {
      VG_(printf)("memcheck expensive sanity: "
                  "apparent secmap leakage\n");
      return False;
   }

   /* check that auxmap only covers address space that the primary
      doesn't */
   
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

Bool  MC_(clo_avoid_strlen_errors)    = True;

Bool TL_(process_cmd_line_option)(Char* arg)
{
        VG_BOOL_CLO(arg, "--avoid-strlen-errors", MC_(clo_avoid_strlen_errors))
   else
      return MAC_(process_common_cmd_line_option)(arg);

   return True;
}

void TL_(print_usage)(void)
{  
   MAC_(print_common_usage)();
   VG_(printf)(
"    --avoid-strlen-errors=no|yes  suppress errs from inlined strlen [yes]\n"
   );
}

void TL_(print_debug_usage)(void)
{  
   MAC_(print_common_debug_usage)();
   VG_(printf)(
"    --cleanup=no|yes          improve after instrumentation? [yes]\n"
   );
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
      Char*	    desc;
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

static Bool find_addr(VgHashNode* sh_ch, void* ap)
{
  MAC_Chunk *m = (MAC_Chunk*)sh_ch;
  Addr a = *(Addr*)ap;

  return VG_(addr_is_in_block)(a, m->data, m->size);
}

static Bool client_perm_maybe_describe( Addr a, AddrInfo* ai )
{
   UInt i;
   /* VG_(printf)("try to identify %d\n", a); */

   /* Perhaps it's a general block ? */
   for (i = 0; i < cgb_used; i++) {
      if (cgbs[i].start == 0 && cgbs[i].size == 0) 
         continue;
      if (VG_(addr_is_in_block)(a, cgbs[i].start, cgbs[i].size)) {
         MAC_Mempool **d, *mp;

         /* OK - maybe it's a mempool, too? */
         mp = (MAC_Mempool*)VG_(HT_get_node)(MAC_(mempool_list),
                                             (UWord)cgbs[i].start,
                                             (void*)&d);
         if(mp != NULL) {
            if(mp->chunks != NULL) {
               MAC_Chunk *mc;

               mc = (MAC_Chunk*)VG_(HT_first_match)(mp->chunks, find_addr, &a);
               if(mc != NULL) {
                  ai->akind = UserG;
                  ai->blksize = mc->size;
                  ai->rwoffset = (Int)(a) - (Int)mc->data;
                  ai->lastchange = mc->where;
                  return True;
               }
            }
            ai->akind = Mempool;
            ai->blksize = cgbs[i].size;
            ai->rwoffset  = (Int)(a) - (Int)(cgbs[i].start);
            ai->lastchange = cgbs[i].where;
            return True;
         }
         ai->akind = UserG;
         ai->blksize = cgbs[i].size;
         ai->rwoffset  = (Int)(a) - (Int)(cgbs[i].start);
         ai->lastchange = cgbs[i].where;
	 ai->desc = cgbs[i].desc;
         return True;
      }
   }
   return False;
}

Bool TL_(handle_client_request) ( ThreadId tid, UWord* arg, UWord* ret )
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
      case VG_USERREQ__CHECK_WRITABLE: /* check writable */
         ok = mc_check_writable ( arg[1], arg[2], &bad_addr );
         if (!ok)
            MC_(record_user_error) ( tid, bad_addr, /*isWrite*/True,
                                     /*isUnaddr*/True );
         *ret = ok ? (UWord)NULL : bad_addr;
	 break;

      case VG_USERREQ__CHECK_READABLE: { /* check readable */
         MC_ReadResult res;
         res = mc_check_readable ( arg[1], arg[2], &bad_addr );
         if (MC_AddrErr == res)
            MC_(record_user_error) ( tid, bad_addr, /*isWrite*/False,
                                     /*isUnaddr*/True );
         else if (MC_ValueErr == res)
            MC_(record_user_error) ( tid, bad_addr, /*isWrite*/False,
                                     /*isUnaddr*/False );
         *ret = ( res==MC_Ok ? (UWord)NULL : bad_addr );
	 break;
      }

      case VG_USERREQ__DO_LEAK_CHECK:
         mc_detect_memory_leaks(tid, arg[1] ? LC_Summary : LC_Full);
	 *ret = 0; /* return value is meaningless */
	 break;

      case VG_USERREQ__MAKE_NOACCESS: /* make no access */
         mc_make_noaccess ( arg[1], arg[2] );
	 *ret = -1;
	 break;

      case VG_USERREQ__MAKE_WRITABLE: /* make writable */
         mc_make_writable ( arg[1], arg[2] );
         *ret = -1;
	 break;

      case VG_USERREQ__MAKE_READABLE: /* make readable */
         mc_make_readable ( arg[1], arg[2] );
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

//zz       case VG_USERREQ__GET_VBITS:
//zz          /* Returns: 1 == OK, 2 == alignment error, 3 == addressing
//zz             error. */
//zz          /* VG_(printf)("get_vbits %p %p %d\n", arg[1], arg[2], arg[3] ); */
//zz          *ret = mc_get_or_set_vbits_for_client
//zz                    ( tid, arg[1], arg[2], arg[3], False /* get them */ );
//zz          break;
//zz 
//zz       case VG_USERREQ__SET_VBITS:
//zz          /* Returns: 1 == OK, 2 == alignment error, 3 == addressing
//zz             error. */
//zz          /* VG_(printf)("set_vbits %p %p %d\n", arg[1], arg[2], arg[3] ); */
//zz          *ret = mc_get_or_set_vbits_for_client
//zz                    ( tid, arg[1], arg[2], arg[3], True /* set them */ );
//zz          break;

      default:
         if (MAC_(handle_common_client_requests)(tid, arg, ret )) {
            return True;
         } else {
            VG_(message)(Vg_UserMsg, 
                         "Warning: unknown memcheck client request code %llx",
                         (ULong)arg[0]);
            return False;
         }
   }
   return True;
}

/*------------------------------------------------------------*/
/*--- Setup                                                ---*/
/*------------------------------------------------------------*/

void TL_(pre_clo_init)(void)
{
   VG_(details_name)            ("Memcheck");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a memory error detector");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2005, and GNU GPL'd, by Julian Seward et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 370 );

   VG_(basic_tool_funcs)          (TL_(post_clo_init),
                                   TL_(instrument),
                                   TL_(fini));

   VG_(needs_core_errors)         ();
   VG_(needs_tool_errors)         (TL_(eq_Error),
                                   TL_(pp_Error),
                                   TL_(update_extra),
                                   TL_(recognised_suppression),
                                   TL_(read_extra_suppression_info),
                                   TL_(error_matches_suppression),
                                   TL_(get_error_name),
                                   TL_(print_extra_suppression_info));
   VG_(needs_libc_freeres)        ();
   VG_(needs_command_line_options)(TL_(process_cmd_line_option),
                                   TL_(print_usage),
                                   TL_(print_debug_usage));
   VG_(needs_client_requests)     (TL_(handle_client_request));
   VG_(needs_sanity_checks)       (TL_(cheap_sanity_check),
                                   TL_(expensive_sanity_check));
   VG_(needs_shadow_memory)       ();

   VG_(malloc_funcs)              (TL_(malloc),
                                   TL_(__builtin_new),
                                   TL_(__builtin_vec_new),
                                   TL_(memalign),
                                   TL_(calloc),
                                   TL_(free),
                                   TL_(__builtin_delete),
                                   TL_(__builtin_vec_delete),
                                   TL_(realloc),
                                   MALLOC_REDZONE_SZB );

   MAC_( new_mem_heap)             = & mc_new_mem_heap;
   MAC_( ban_mem_heap)             = & mc_make_noaccess;
   MAC_(copy_mem_heap)             = & mc_copy_address_range_state;
   MAC_( die_mem_heap)             = & mc_make_noaccess;
   MAC_(check_noaccess)            = & mc_check_noaccess;

   VG_(init_new_mem_startup)      ( & mc_new_mem_startup );
   VG_(init_new_mem_stack_signal) ( & mc_make_writable );
   VG_(init_new_mem_brk)          ( & mc_make_writable );
   VG_(init_new_mem_mmap)         ( & mc_new_mem_mmap );
   
   VG_(init_copy_mem_remap)       ( & mc_copy_address_range_state );
      
   VG_(init_die_mem_stack_signal) ( & mc_make_noaccess ); 
   VG_(init_die_mem_brk)          ( & mc_make_noaccess );
   VG_(init_die_mem_munmap)       ( & mc_make_noaccess ); 

   VG_(init_new_mem_stack_4)      ( & MAC_(new_mem_stack_4)  );
   VG_(init_new_mem_stack_8)      ( & MAC_(new_mem_stack_8)  );
   VG_(init_new_mem_stack_12)     ( & MAC_(new_mem_stack_12) );
   VG_(init_new_mem_stack_16)     ( & MAC_(new_mem_stack_16) );
   VG_(init_new_mem_stack_32)     ( & MAC_(new_mem_stack_32) );
   VG_(init_new_mem_stack)        ( & MAC_(new_mem_stack)    );

   VG_(init_die_mem_stack_4)      ( & MAC_(die_mem_stack_4)  );
   VG_(init_die_mem_stack_8)      ( & MAC_(die_mem_stack_8)  );
   VG_(init_die_mem_stack_12)     ( & MAC_(die_mem_stack_12) );
   VG_(init_die_mem_stack_16)     ( & MAC_(die_mem_stack_16) );
   VG_(init_die_mem_stack_32)     ( & MAC_(die_mem_stack_32) );
   VG_(init_die_mem_stack)        ( & MAC_(die_mem_stack)    );
   
   VG_(init_ban_mem_stack)        ( & mc_make_noaccess );

   VG_(init_pre_mem_read)         ( & mc_check_is_readable );
   VG_(init_pre_mem_read_asciiz)  ( & mc_check_is_readable_asciiz );
   VG_(init_pre_mem_write)        ( & mc_check_is_writable );
   VG_(init_post_mem_write)       ( & mc_post_mem_write );

   VG_(init_pre_reg_read)         ( & mc_pre_reg_read );

   VG_(init_post_reg_write)                   ( & mc_post_reg_write );
   VG_(init_post_reg_write_clientcall_return) ( & mc_post_reg_write_clientcall );

   VG_(register_profile_event) ( VgpSetMem,   "set-mem-perms" );
   VG_(register_profile_event) ( VgpCheckMem, "check-mem-perms" );
   VG_(register_profile_event) ( VgpESPAdj,   "adjust-ESP" );

   /* Additional block description for VG_(describe_addr)() */
   MAC_(describe_addr_supp) = client_perm_maybe_describe;

   init_shadow_memory();
   MAC_(common_pre_clo_init)();

   tl_assert( TL_(expensive_sanity_check)() );
}

void TL_(post_clo_init) ( void )
{
}

void TL_(fini) ( Int exitcode )
{
   MAC_(common_fini)( mc_detect_memory_leaks );

   Int     i, n_accessible_dist;
   SecMap* sm;

   if (VG_(clo_verbosity) > 1) {
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
      VG_(message)(Vg_DebugMsg,
         " memcheck: secondaries: %d issued (%dk, %dM)",
         n_secmaps_issued, 
         n_secmaps_issued * 64,
         n_secmaps_issued / 16 );   

      n_accessible_dist = 0;
      for (i = 0; i < N_PRIMARY_MAP; i++) {
         sm = primary_map[i];
         if (is_distinguished_sm(sm)
             && sm != &sm_distinguished[SM_DIST_NOACCESS])
            n_accessible_dist ++;
      }
      for (i = 0; i < auxmap_used; i++) {
         sm = auxmap[i].sm;
         if (is_distinguished_sm(sm)
             && sm != &sm_distinguished[SM_DIST_NOACCESS])
            n_accessible_dist ++;
      }

      VG_(message)(Vg_DebugMsg,
         " memcheck: secondaries: %d accessible and distinguished (%dk, %dM)",
         n_accessible_dist, 
         n_accessible_dist * 64,
         n_accessible_dist / 16 );   

   }

   if (0) {
      VG_(message)(Vg_DebugMsg, 
        "------ Valgrind's client block stats follow ---------------" );
      show_client_block_stats();
   }
}

VG_DETERMINE_INTERFACE_VERSION(TL_(pre_clo_init), 9./8)

/*--------------------------------------------------------------------*/
/*--- end                                                mc_main.c ---*/
/*--------------------------------------------------------------------*/
