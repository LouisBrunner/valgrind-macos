/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2013 Bart Van Assche <bvanassche@acm.org>.

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


#ifndef __DRD_BITMAP_H
#define __DRD_BITMAP_H


#include "pub_drd_bitmap.h"
#include "pub_tool_basics.h"
#include "pub_tool_oset.h"
#include "pub_tool_libcbase.h"
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
#include "pub_tool_libcassert.h"
#endif


/* Bitmap representation. A bitmap is a data structure in which two bits are
 * reserved per 32 bit address: one bit that indicates that the data at the
 * specified address has been read, and one bit that indicates that the data
 * has been written to.
 */

/* Client addresses are split into bitfields as follows:
 * ------------------------------------------------------
 * | Address MSB |      Address LSB      | Ignored bits |
 * ------------------------------------------------------
 * | Address MSB | UWord MSB | UWord LSB | Ignored bits |
 * ------------------------------------------------------
 */



/* Address MSB / LSB split. */


/** Number of least significant address bits that are ignored. */
#define ADDR_IGNORED_BITS 0
#define ADDR_IGNORED_MASK ((1U << ADDR_IGNORED_BITS) - 1U)
#define ADDR_GRANULARITY  (1U << ADDR_IGNORED_BITS)

/**
 * Round argument a up to a multiple of (1 << ADDR_GRANULARITY), and next
 * shift it right ADDR_GRANULARITY bits. The expression below is optimized
 * for the case where a is a constant.
 */
#define SCALED_SIZE(a)                                                  \
   (((((a) - 1U) | ADDR_IGNORED_MASK) + 1U) >> ADDR_IGNORED_BITS)

/**
 * Number of bits assigned to the least significant component of an address.
 */
#define ADDR_LSB_BITS 12

/**
 * Mask that has to be applied to an address of type Addr in order to
 * compute the least significant part of an address split, after having
 * shifted the address bits ADDR_GRANULARITY to the right.
 */
#define ADDR_LSB_MASK (((UWord)1 << ADDR_LSB_BITS) - 1U)

/** Compute least significant bits of an address of type Addr. */
static __inline__
UWord address_lsb(const Addr a)
{ return (a >> ADDR_IGNORED_BITS) & ADDR_LSB_MASK; }

/**
 * Compute the first address for which address_lsb() is equal to
 * address_lsb(a).
 */
static __inline__
Addr first_address_with_same_lsb(const Addr a)
{
   return ((a | ADDR_IGNORED_MASK) ^ ADDR_IGNORED_MASK);
}

/**
 * Compute the first address for which address_lsb() is greater than
 * address_lsb(a).
 */
static __inline__
Addr first_address_with_higher_lsb(const Addr a)
{
   return ((a | ADDR_IGNORED_MASK) + 1U);
}

/** Compute most significant bits of an address of type Addr. */
static __inline__
UWord address_msb(const Addr a)
{ return a >> (ADDR_LSB_BITS + ADDR_IGNORED_BITS); }

static __inline__
Addr first_address_with_higher_msb(const Addr a)
{
   return ((a | ((ADDR_LSB_MASK << ADDR_IGNORED_BITS) | ADDR_IGNORED_MASK))
           + 1U);
}

/**
 * Convert LSB and MSB back into an address.
 *
 * @note It is assumed that sizeof(Addr) == sizeof(UWord).
 */
static __inline__
Addr make_address(const UWord a1, const UWord a0)
{
   return ((a1 << (ADDR_LSB_BITS + ADDR_IGNORED_BITS))
           | (a0 << ADDR_IGNORED_BITS));
}





/** Number of bits that fit in a variable of type UWord. */
#define BITS_PER_UWORD (8U * sizeof(UWord))

/** Log2 of BITS_PER_UWORD. */
#if defined(VGA_x86) || defined(VGA_ppc32) || defined(VGA_arm) \
    || defined(VGA_mips32)
#define BITS_PER_BITS_PER_UWORD 5
#elif defined(VGA_amd64) || defined(VGA_ppc64) || defined(VGA_s390x) \
      || defined(VGA_mips64) || defined(VGA_arm64)
#define BITS_PER_BITS_PER_UWORD 6
#else
#error Unknown platform.
#endif

/** Number of UWord's needed to store one bit per address LSB. */
#define BITMAP1_UWORD_COUNT (1U << (ADDR_LSB_BITS - BITS_PER_BITS_PER_UWORD))

/**
 * Mask that has to be applied to an (Addr >> ADDR_IGNORED_BITS) expression
 * in order to compute the least significant part of an UWord.
 */
#define UWORD_LSB_MASK (((UWord)1 << BITS_PER_BITS_PER_UWORD) - 1)

/**
 * Compute index into bm0[] array.
 *
 * @param a Address shifted right ADDR_IGNORED_BITS bits.
 */
static __inline__
UWord uword_msb(const UWord a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(a < (1U << ADDR_LSB_BITS));
#endif
   return a >> BITS_PER_BITS_PER_UWORD;
}

/**
 * Return the least significant bits.
 *
 * @param a Address shifted right ADDR_IGNORED_BITS bits.
 */
static __inline__
UWord uword_lsb(const UWord a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(a < (1U << ADDR_LSB_BITS));
#endif
   return a & UWORD_LSB_MASK;
}

/**
 * Compute the highest address lower than a for which
 * uword_lsb(address_lsb(a)) == 0.
 *
 * @param a Address.
 */
static __inline__
Addr first_address_with_same_uword_lsb(const Addr a)
{
   return (a & (~UWORD_LSB_MASK << ADDR_IGNORED_BITS));
}

/**
 * First address that will go in the UWord past the one 'a' goes in.
 *
 *  @param a Address.
 */
static __inline__
Addr first_address_with_higher_uword_msb(const Addr a)
{
   return ((a | ((UWORD_LSB_MASK << ADDR_IGNORED_BITS) | ADDR_IGNORED_MASK))
           + 1);
}



/* Local variables. */

static ULong s_bitmap2_creation_count;



/*********************************************************************/
/*           Functions for manipulating a struct bitmap1.            */
/*********************************************************************/


/* Lowest level, corresponding to the lowest ADDR_LSB_BITS of an address. */
struct bitmap1
{
   UWord bm0_r[BITMAP1_UWORD_COUNT];
   UWord bm0_w[BITMAP1_UWORD_COUNT];
};

static __inline__ UWord bm0_mask(const UWord a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(address_msb(make_address(0, a)) == 0);
#endif
   return ((UWord)1 << uword_lsb(a));
}

/** Set the bit corresponding to address a in bitmap bm0. */
static __inline__ void bm0_set(UWord* bm0, const UWord a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(address_msb(make_address(0, a)) == 0);
#endif
   bm0[uword_msb(a)] |= (UWord)1 << uword_lsb(a);
}

/**
 * Set the bits corresponding to all of the addresses in range
 * [ a << ADDR_IGNORED_BITS .. (a + size) << ADDR_IGNORED_BITS [
 * in bitmap bm0.
 */
static __inline__ void bm0_set_range(UWord* bm0,
                                     const UWord a, const SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(size > 0);
   tl_assert(address_msb(make_address(0, a)) == 0);
   tl_assert(address_msb(make_address(0, a + size - 1)) == 0);
   tl_assert(uword_msb(a) == uword_msb(a + size - 1));
#endif
   bm0[uword_msb(a)]
      |= (((UWord)1 << size) - 1) << uword_lsb(a);
}

/** Clear the bit corresponding to address a in bitmap bm0. */
static __inline__ void bm0_clear(UWord* bm0, const UWord a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(address_msb(make_address(0, a)) == 0);
#endif
   bm0[uword_msb(a)] &= ~((UWord)1 << uword_lsb(a));
}

/**
 * Clear all of the addresses in range
 * [ a << ADDR_IGNORED_BITS .. (a + size) << ADDR_IGNORED_BITS [
 * in bitmap bm0.
 */
static __inline__ void bm0_clear_range(UWord* bm0,
                                       const UWord a, const SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(address_msb(make_address(0, a)) == 0);
   tl_assert(size == 0 || address_msb(make_address(0, a + size - 1)) == 0);
   tl_assert(size == 0 || uword_msb(a) == uword_msb(a + size - 1));
#endif
   /*
    * Note: although the expression below yields a correct result even if
    * size == 0, do not touch bm0[] if size == 0 because this might otherwise
    * cause an access of memory just past the end of the bm0[] array.
    */
   if (size > 0)
   {
      bm0[uword_msb(a)]
         &= ~((((UWord)1 << size) - 1) << uword_lsb(a));
   }
}

/** Test whether the bit corresponding to address a is set in bitmap bm0. */
static __inline__ UWord bm0_is_set(const UWord* bm0, const UWord a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(address_msb(make_address(0, a)) == 0);
#endif
   return (bm0[uword_msb(a)] & ((UWord)1 << uword_lsb(a)));
}

/**
 * Return true if a bit corresponding to any of the addresses in range
 * [ a << ADDR_IGNORED_BITS .. (a + size) << ADDR_IGNORED_BITS [
 * is set in bm0.
 */
static __inline__ UWord bm0_is_any_set(const UWord* bm0,
                                       const Addr a, const SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(size > 0);
   tl_assert(address_msb(make_address(0, a)) == 0);
   tl_assert(address_msb(make_address(0, a + size - 1)) == 0);
   tl_assert(uword_msb(a) == uword_msb(a + size - 1));
#endif
   return (bm0[uword_msb(a)] & ((((UWord)1 << size) - 1) << uword_lsb(a)));
}



/*********************************************************************/
/*           Functions for manipulating a struct bitmap.             */
/*********************************************************************/


/* Second level bitmap. */
struct bitmap2
{
   Addr           addr;   ///< address_msb(...)
   Bool           recalc;
   struct bitmap1 bm1;
};


static void bm2_clear(struct bitmap2* const bm2);
static __inline__
struct bitmap2* bm2_insert(struct bitmap* const bm, const UWord a1);



/**
 * Rotate elements cache[0..n-1] such that the element at position n-1 is
 * moved to position 0. This allows to speed up future cache lookups.
 */
static __inline__
void bm_cache_rotate(struct bm_cache_elem cache[], const int n)
{
#if 0
   struct bm_cache_elem t;

   tl_assert(2 <= n && n <= 8);

   t = cache[0];
   if (n > 1)
      cache[0] = cache[1];
   if (n > 2)
      cache[1] = cache[2];
   if (n > 3)
      cache[2] = cache[3];
   if (n > 4)
      cache[3] = cache[4];
   if (n > 5)
      cache[4] = cache[5];
   if (n > 6)
      cache[5] = cache[6];
   if (n > 7)
      cache[6] = cache[7];
   cache[n - 1] = t;
#endif
}

static __inline__
Bool bm_cache_lookup(struct bitmap* const bm, const UWord a1,
                     struct bitmap2** bm2)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
   tl_assert(bm2);
#endif

#if DRD_BITMAP_N_CACHE_ELEM > 8
#error Please update the code below.
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 1
   if (a1 == bm->cache[0].a1)
   {
      *bm2 = bm->cache[0].bm2;
      return True;
   }
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 2
   if (a1 == bm->cache[1].a1)
   {
      *bm2 = bm->cache[1].bm2;
      return True;
   }
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 3
   if (a1 == bm->cache[2].a1)
   {
      *bm2 = bm->cache[2].bm2;
      bm_cache_rotate(bm->cache, 3);
      return True;
   }
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 4
   if (a1 == bm->cache[3].a1)
   {
      *bm2 = bm->cache[3].bm2;
      bm_cache_rotate(bm->cache, 4);
      return True;
   }
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 5
   if (a1 == bm->cache[4].a1)
   {
      *bm2 = bm->cache[4].bm2;
      bm_cache_rotate(bm->cache, 5);
      return True;
   }
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 6
   if (a1 == bm->cache[5].a1)
   {
      *bm2 = bm->cache[5].bm2;
      bm_cache_rotate(bm->cache, 6);
      return True;
   }
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 7
   if (a1 == bm->cache[6].a1)
   {
      *bm2 = bm->cache[6].bm2;
      bm_cache_rotate(bm->cache, 7);
      return True;
   }
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 8
   if (a1 == bm->cache[7].a1)
   {
      *bm2 = bm->cache[7].bm2;
      bm_cache_rotate(bm->cache, 8);
      return True;
   }
#endif
   *bm2 = 0;
   return False;
}

static __inline__
void bm_update_cache(struct bitmap* const bm,
                     const UWord a1,
                     struct bitmap2* const bm2)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

#if DRD_BITMAP_N_CACHE_ELEM > 8
#error Please update the code below.
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 8
   bm->cache[7] = bm->cache[6];
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 7
   bm->cache[6] = bm->cache[5];
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 6
   bm->cache[5] = bm->cache[4];
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 5
   bm->cache[4] = bm->cache[3];
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 4
   bm->cache[3] = bm->cache[2];
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 3
   bm->cache[2] = bm->cache[1];
#endif
#if DRD_BITMAP_N_CACHE_ELEM >= 2
   bm->cache[1] = bm->cache[0];
#endif
   bm->cache[0].a1  = a1;
   bm->cache[0].bm2 = bm2;
}

/**
 * Look up the address a1 in bitmap bm and return a pointer to a potentially
 * shared second level bitmap. The bitmap where the returned pointer points
 * at may not be modified by the caller.
 *
 * @param a1 client address shifted right by ADDR_LSB_BITS.
 * @param bm bitmap pointer.
 */
static __inline__
const struct bitmap2* bm2_lookup(struct bitmap* const bm, const UWord a1)
{
   struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   if (! bm_cache_lookup(bm, a1, &bm2))
   {
      bm2 = VG_(OSetGen_Lookup)(bm->oset, &a1);
      bm_update_cache(bm, a1, bm2);
   }
   return bm2;
}

/**
 * Look up the address a1 in bitmap bm and return a pointer to a second
 * level bitmap that is not shared and hence may be modified.
 *
 * @param a1 client address shifted right by ADDR_LSB_BITS.
 * @param bm bitmap pointer.
 */
static __inline__
struct bitmap2*
bm2_lookup_exclusive(struct bitmap* const bm, const UWord a1)
{
   struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   if (! bm_cache_lookup(bm, a1, &bm2))
   {
      bm2 = VG_(OSetGen_Lookup)(bm->oset, &a1);
   }

   return bm2;
}

/** Clear the content of the second-level bitmap. */
static __inline__
void bm2_clear(struct bitmap2* const bm2)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm2);
#endif
   VG_(memset)(&bm2->bm1, 0, sizeof(bm2->bm1));
}

/**
 * Insert an uninitialized second level bitmap for the address a1.
 *
 * @param bm bitmap pointer.
 * @param a1 client address shifted right by ADDR_LSB_BITS.
 *
 * @note bitmap2::recalc isn't initialized here on purpose.
 */
static __inline__
struct bitmap2* bm2_insert(struct bitmap* const bm, const UWord a1)
{
   struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   s_bitmap2_creation_count++;

   bm2 = VG_(OSetGen_AllocNode)(bm->oset, sizeof(*bm2));
   bm2->addr = a1;
   VG_(OSetGen_Insert)(bm->oset, bm2);

   bm_update_cache(bm, a1, bm2);

   return bm2;
}

static __inline__
struct bitmap2* bm2_insert_copy(struct bitmap* const bm,
                                struct bitmap2* const bm2)
{
   struct bitmap2* bm2_copy;

   bm2_copy = bm2_insert(bm, bm2->addr);
   VG_(memcpy)(&bm2_copy->bm1, &bm2->bm1, sizeof(bm2->bm1));
   return bm2_copy;
}

/**
 * Look up the address a1 in bitmap bm, and insert it if not found.
 * The returned second level bitmap may not be modified.
 *
 * @param bm bitmap pointer.
 * @param a1 client address shifted right by ADDR_LSB_BITS.
 */
static __inline__
struct bitmap2* bm2_lookup_or_insert(struct bitmap* const bm, const UWord a1)
{
   struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   if (bm_cache_lookup(bm, a1, &bm2))
   {
      if (bm2 == 0)
      {
         bm2 = bm2_insert(bm, a1);
         bm2_clear(bm2);
      }
   }
   else
   {
      bm2 = VG_(OSetGen_Lookup)(bm->oset, &a1);
      if (! bm2)
      {
         bm2 = bm2_insert(bm, a1);
         bm2_clear(bm2);
      }
      bm_update_cache(bm, a1, bm2);
   }
   return bm2;
}

/**
 * Look up the address a1 in bitmap bm, and insert it if not found.
 * The returned second level bitmap may be modified.
 *
 * @param a1 client address shifted right by ADDR_LSB_BITS.
 * @param bm bitmap pointer.
 */
static __inline__
struct bitmap2* bm2_lookup_or_insert_exclusive(struct bitmap* const bm,
                                               const UWord a1)
{
   return bm2_lookup_or_insert(bm, a1);
}

static __inline__
void bm2_remove(struct bitmap* const bm, const UWord a1)
{
   struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   bm2 = VG_(OSetGen_Remove)(bm->oset, &a1);
   VG_(OSetGen_FreeNode)(bm->oset, bm2);

   bm_update_cache(bm, a1, NULL);
}

static __inline__
void bm_access_aligned_load(struct bitmap* const bm,
                            const Addr a1, const SizeT size)
{
   struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   bm2 = bm2_lookup_or_insert_exclusive(bm, address_msb(a1));
   bm0_set_range(bm2->bm1.bm0_r,
                 (a1 >> ADDR_IGNORED_BITS) & ADDR_LSB_MASK,
                 SCALED_SIZE(size));
}

static __inline__
void bm_access_aligned_store(struct bitmap* const bm,
                             const Addr a1, const SizeT size)
{
   struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   bm2 = bm2_lookup_or_insert_exclusive(bm, address_msb(a1));
   bm0_set_range(bm2->bm1.bm0_w,
                 (a1 >> ADDR_IGNORED_BITS) & ADDR_LSB_MASK,
                 SCALED_SIZE(size));
}

static __inline__
Bool bm_aligned_load_has_conflict_with(struct bitmap* const bm,
                                       const Addr a, const SizeT size)
{
   const struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   bm2 = bm2_lookup(bm, address_msb(a));
   return (bm2
           && bm0_is_any_set(bm2->bm1.bm0_w,
                             address_lsb(a),
                             SCALED_SIZE(size)));
}

static __inline__
Bool bm_aligned_store_has_conflict_with(struct bitmap* const bm,
                                        const Addr a, const SizeT size)
{
   const struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(bm);
#endif

   bm2 = bm2_lookup(bm, address_msb(a));
   if (bm2)
   {
      if (bm0_is_any_set(bm2->bm1.bm0_r, address_lsb(a), SCALED_SIZE(size))
          | bm0_is_any_set(bm2->bm1.bm0_w, address_lsb(a), SCALED_SIZE(size)))
      {
         return True;
      }
   }
   return False;
}

#endif /* __DRD_BITMAP_H */
