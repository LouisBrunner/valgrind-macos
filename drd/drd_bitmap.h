/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2008 Bart Van Assche
  bart.vanassche@gmail.com

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


#include "pub_tool_oset.h"
#include "pub_tool_libcbase.h"


/*
  Bitmap representation. A bitmap is a data structure in which two bits are
  reserved per 32 bit address: one bit that indicates that the data at the
  specified address has been read, and one bit that indicates that the data has
  been written to.
*/


/* Macro definitions. */

#define ADDR0_BITS 14

#define ADDR0_COUNT ((UWord)1 << ADDR0_BITS)

#define ADDR0_MASK (ADDR0_COUNT - 1)

#define SPLIT_ADDRESS(a)            \
  UWord a##0 = ((a) & ADDR0_MASK);  \
  UWord a##1 = ((a) >> ADDR0_BITS);

// Assumption: sizeof(Addr) == sizeof(UWord).
#define MAKE_ADDRESS(a1, a0)  \
  (Addr)(((UWord)(a1) << (ADDR0_BITS)) | ((UWord)(a0)))

#define BITS_PER_UWORD (8UL*sizeof(UWord))
#if defined(VGA_x86) || defined(VGA_ppc32)
#define BITS_PER_BITS_PER_UWORD 5
#elif defined(VGA_amd64) || defined(VGA_ppc64)
#define BITS_PER_BITS_PER_UWORD 6
#else
#error Unknown platform.
#endif

#define BITMAP1_UWORD_COUNT (ADDR0_COUNT >> BITS_PER_BITS_PER_UWORD)

/* Highest bits of an address that fit into the same UWord of bm0[]. */
#define UWORD_MSB(a) ((a) & ~(BITS_PER_UWORD - 1))

/* Lowest bits of an address that fit into the same UWord of bm0[]. */
#define UWORD_LSB(a) ((a) & (BITS_PER_UWORD - 1))

/* Highest address that fits in the same UWord as a. */
#define UWORD_HIGHEST_ADDRESS(a) ((a) | (BITS_PER_UWORD - 1))


/* Local variables. */

static ULong s_bitmap2_creation_count;
static ULong s_bitmap2_node_creation_count;



/*********************************************************************/
/*           Functions for manipulating a struct bitmap1.            */
/*********************************************************************/


/* Lowest level, corresponding to the lowest ADDR0_BITS of an address. */
struct bitmap1
{
  UWord bm0_r[BITMAP1_UWORD_COUNT];
  UWord bm0_w[BITMAP1_UWORD_COUNT];
};

static __inline__ UWord bm0_mask(const Addr a)
{
  return ((UWord)1 << UWORD_LSB(a));
}

static __inline__ void bm0_set(UWord* bm0, const Addr a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(a < ADDR0_COUNT);
#endif
  bm0[a >> BITS_PER_BITS_PER_UWORD] |= (UWord)1 << UWORD_LSB(a);
}

/** Set all of the addresses in range [ a1 .. a1 + size [ in bitmap bm0. */
static __inline__ void bm0_set_range(UWord* bm0,
                                     const Addr a1, const SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(a1 < ADDR0_COUNT);
  tl_assert(size > 0);
  tl_assert(a1 + size <= ADDR0_COUNT);
  tl_assert(UWORD_MSB(a1) == UWORD_MSB(a1 + size - 1));
#endif
  bm0[a1 >> BITS_PER_BITS_PER_UWORD]
    |= (((UWord)1 << size) - 1) << UWORD_LSB(a1);
}

static __inline__ void bm0_clear(UWord* bm0, const Addr a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(a < ADDR0_COUNT);
#endif
  bm0[a >> BITS_PER_BITS_PER_UWORD] &= ~((UWord)1 << UWORD_LSB(a));
}

/** Clear all of the addresses in range [ a1 .. a1 + size [ in bitmap bm0. */
static __inline__ void bm0_clear_range(UWord* bm0,
                                       const Addr a1, const SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(a1 < ADDR0_COUNT);
  tl_assert(size > 0);
  tl_assert(a1 + size <= ADDR0_COUNT);
  tl_assert(UWORD_MSB(a1) == UWORD_MSB(a1 + size - 1));
#endif
  bm0[a1 >> BITS_PER_BITS_PER_UWORD]
    &= ~(((UWord)1 << size) - 1) << UWORD_LSB(a1);
}

static __inline__ UWord bm0_is_set(const UWord* bm0, const Addr a)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(a < ADDR0_COUNT);
#endif
  return (bm0[a >> BITS_PER_BITS_PER_UWORD] & ((UWord)1 << UWORD_LSB(a)));
}

/** Return true if any of the bits [ a1 .. a1+size [ are set in bm0. */
static __inline__ UWord bm0_is_any_set(const UWord* bm0,
                                       const Addr a1, const SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(a1 < ADDR0_COUNT);
  tl_assert(size > 0);
  tl_assert(a1 + size <= ADDR0_COUNT);
  tl_assert(UWORD_MSB(a1) == UWORD_MSB(a1 + size - 1));
#endif
  return (bm0[a1 >> BITS_PER_BITS_PER_UWORD]
          & ((((UWord)1 << size) - 1) << UWORD_LSB(a1)));
}



/*********************************************************************/
/*           Functions for manipulating a struct bitmap.             */
/*********************************************************************/


/* Second level bitmap. */
struct bitmap2
{
  Addr           addr;   ///< address >> ADDR0_BITS
  int            refcnt;
  struct bitmap1 bm1;
};

/* One node of bitmap::oset. */
struct bitmap2ref
{
  Addr            addr; ///< address >> ADDR0_BITS
  struct bitmap2* bm2;
};

struct bm_cache_elem
{
  Addr            a1;
  struct bitmap2* bm2;
};

#define N_CACHE_ELEM 4

/* Complete bitmap. */
struct bitmap
{
  struct bm_cache_elem cache[N_CACHE_ELEM];
  OSet*                oset;
};


static struct bitmap2* bm2_new(const UWord a1);
static struct bitmap2* bm2_make_exclusive(struct bitmap* const bm,
                                          struct bitmap2ref* const bm2ref);

/** Rotate elements cache[0..n-1] such that the element at position n-1 is
 *  moved to position 0. This allows to speed up future cache lookups.
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

#if N_CACHE_ELEM > 8
#error Please update the code below.
#endif
#if N_CACHE_ELEM >= 1
  if (a1 == bm->cache[0].a1)
  {
    *bm2 = bm->cache[0].bm2;
    return True;
  }
#endif
#if N_CACHE_ELEM >= 2
  if (a1 == bm->cache[1].a1)
  {
    *bm2 = bm->cache[1].bm2;
    return True;
  }
#endif
#if N_CACHE_ELEM >= 3
  if (a1 == bm->cache[2].a1)
  {
    *bm2 = bm->cache[2].bm2;
    bm_cache_rotate(bm->cache, 3);
    return True;
  }
#endif
#if N_CACHE_ELEM >= 4
  if (a1 == bm->cache[3].a1)
  {
    *bm2 = bm->cache[3].bm2;
    bm_cache_rotate(bm->cache, 4);
    return True;
  }
#endif
#if N_CACHE_ELEM >= 5
  if (a1 == bm->cache[4].a1)
  {
    *bm2 = bm->cache[4].bm2;
    bm_cache_rotate(bm->cache, 5);
    return True;
  }
#endif
#if N_CACHE_ELEM >= 6
  if (a1 == bm->cache[5].a1)
  {
    *bm2 = bm->cache[5].bm2;
    bm_cache_rotate(bm->cache, 6);
    return True;
  }
#endif
#if N_CACHE_ELEM >= 7
  if (a1 == bm->cache[6].a1)
  {
    *bm2 = bm->cache[6].bm2;
    bm_cache_rotate(bm->cache, 7);
    return True;
  }
#endif
#if N_CACHE_ELEM >= 8
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

#if N_CACHE_ELEM > 8
#error Please update the code below.
#endif
#if N_CACHE_ELEM >= 8
  bm->cache[7] = bm->cache[6];
#endif
#if N_CACHE_ELEM >= 7
  bm->cache[6] = bm->cache[5];
#endif
#if N_CACHE_ELEM >= 6
  bm->cache[5] = bm->cache[4];
#endif
#if N_CACHE_ELEM >= 5
  bm->cache[4] = bm->cache[3];
#endif
#if N_CACHE_ELEM >= 4
  bm->cache[3] = bm->cache[2];
#endif
#if N_CACHE_ELEM >= 3
  bm->cache[2] = bm->cache[1];
#endif
#if N_CACHE_ELEM >= 2
  bm->cache[1] = bm->cache[0];
#endif
  bm->cache[0].a1  = a1;
  bm->cache[0].bm2 = bm2;
}

/** Look up the address a1 in bitmap bm and return a pointer to a potentially
 *  shared second level bitmap. The bitmap where the returned pointer points
 *  at may not be modified by the caller.
 *
 *  @param a1 client address shifted right by ADDR0_BITS.
 *  @param bm bitmap pointer.
 */
static __inline__
const struct bitmap2* bm2_lookup(struct bitmap* const bm, const UWord a1)
{
  struct bitmap2*    bm2;
  struct bitmap2ref* bm2ref;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(bm);
#endif
  if (! bm_cache_lookup(bm, a1, &bm2))
  {
    bm2ref = VG_(OSetGen_Lookup)(bm->oset, &a1);
    if (bm2ref)
    {
      bm2 = bm2ref->bm2;
    }
    bm_update_cache(*(struct bitmap**)&bm, a1, bm2);
  }
  return bm2;
}

/** Look up the address a1 in bitmap bm and return a pointer to a second
 *  level bitmap that is not shared and hence may be modified.
 *
 *  @param a1 client address shifted right by ADDR0_BITS.
 *  @param bm bitmap pointer.
 */
static __inline__
struct bitmap2*
bm2_lookup_exclusive(struct bitmap* const bm, const UWord a1)
{
  struct bitmap2ref* bm2ref;
  struct bitmap2* bm2;

  bm2ref = 0;
  if (bm_cache_lookup(bm, a1, &bm2))
  {
    if (bm2 == 0)
      return 0;
    if (bm2->refcnt > 1)
    {
      bm2ref = VG_(OSetGen_Lookup)(bm->oset, &a1);
    }
  }
  else
  {
    bm2ref = VG_(OSetGen_Lookup)(bm->oset, &a1);
    if (bm2ref == 0)
      return 0;
    bm2 = bm2ref->bm2;
  }

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(bm2);
#endif

  if (bm2->refcnt > 1)
  {
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
    tl_assert(bm2ref);
#endif
    bm2 = bm2_make_exclusive(*(struct bitmap**)&bm, bm2ref);
  }

  return bm2;
}

/** Look up the address a1 in bitmap bm. The returned second level bitmap has
 *  reference count one and hence may be modified.
 *
 *  @param a1 client address shifted right by ADDR0_BITS.
 *  @param bm bitmap pointer.
 */
static __inline__
struct bitmap2* bm2_insert(struct bitmap* const bm, const UWord a1)
{
  struct bitmap2ref* bm2ref;
  struct bitmap2* bm2;

  s_bitmap2_node_creation_count++;
  bm2ref       = VG_(OSetGen_AllocNode)(bm->oset, sizeof(*bm2ref));
  bm2ref->addr = a1;
  bm2          = bm2_new(a1);
  bm2ref->bm2  = bm2;
  VG_(memset)(&bm2->bm1, 0, sizeof(bm2->bm1));
  VG_(OSetGen_Insert)(bm->oset, bm2ref);
  
  bm_update_cache(*(struct bitmap**)&bm, a1, bm2);

  return bm2;
}

/** Insert a new node in bitmap bm that points to the second level bitmap
 *  *bm2. This means that *bm2 becomes shared over two or more bitmaps.
 */
static __inline__
struct bitmap2* bm2_insert_addref(struct bitmap* const bm,
                                  struct bitmap2* const bm2)
{
  struct bitmap2ref* bm2ref;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(bm);
  tl_assert(VG_(OSetGen_Lookup)(bm->oset, &bm2->addr) == 0);
#endif

  s_bitmap2_node_creation_count++;
  bm2ref       = VG_(OSetGen_AllocNode)(bm->oset, sizeof(*bm2ref));
  bm2ref->addr = bm2->addr;
  bm2ref->bm2  = bm2;
  bm2->refcnt++;
  VG_(OSetGen_Insert)(bm->oset, bm2ref);
  
  bm_update_cache(*(struct bitmap**)&bm, bm2->addr, bm2);

  return bm2;
}

/** Look up the address a1 in bitmap bm, and insert it if not found.
 *  The returned second level bitmap may not be modified.
 *
 *  @param a1 client address shifted right by ADDR0_BITS.
 *  @param bm bitmap pointer.
 */
static __inline__
struct bitmap2* bm2_lookup_or_insert(struct bitmap* const bm, const UWord a1)
{
  struct bitmap2ref* bm2ref;
  struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(bm);
#endif
  if (bm_cache_lookup(bm, a1, &bm2))
  {
    if (bm2 == 0)
    {
      bm2 = bm2_insert(bm, a1);
    }
  }
  else
  {
    bm2ref = VG_(OSetGen_Lookup)(bm->oset, &a1);
    if (bm2ref)
    {
      bm2 = bm2ref->bm2;
    }
    else
    {
      bm2 = bm2_insert(bm, a1);
    }
    bm_update_cache(*(struct bitmap**)&bm, a1, bm2);
  }
  return bm2;
}

/** Look up the address a1 in bitmap bm, and insert it if not found.
 *  The returned second level bitmap may be modified.
 *
 *  @param a1 client address shifted right by ADDR0_BITS.
 *  @param bm bitmap pointer.
 */
static __inline__
struct bitmap2* bm2_lookup_or_insert_exclusive(struct bitmap* const bm,
                                               const UWord a1)
{
  struct bitmap2* bm2;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(bm);
#endif
  bm2 = (struct bitmap2*)bm2_lookup_or_insert(bm, a1);
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(bm2);
#endif
  if (bm2->refcnt > 1)
  {
    struct bitmap2ref* bm2ref;
    bm2ref = VG_(OSetGen_Lookup)(bm->oset, &a1);
    bm2 = bm2_make_exclusive(bm, bm2ref);
  }
  return bm2;
}

static __inline__
void bm_access_aligned_load(struct bitmap* const bm,
                            const Addr a1, const SizeT size)
{
  struct bitmap2* bm2;

  bm2 = bm2_lookup_or_insert_exclusive(bm, a1 >> ADDR0_BITS);
  bm0_set_range(bm2->bm1.bm0_r, a1 & ADDR0_MASK, size);
}

static __inline__
void bm_access_aligned_store(struct bitmap* const bm,
                             const Addr a1, const SizeT size)
{
  struct bitmap2* bm2;

  bm2 = bm2_lookup_or_insert_exclusive(bm, a1 >> ADDR0_BITS);
  bm0_set_range(bm2->bm1.bm0_w, a1 & ADDR0_MASK, size);
}

static __inline__
Bool bm_aligned_load_has_conflict_with(struct bitmap* const bm,
                                       const Addr a1, const SizeT size)
{
  const struct bitmap2* bm2;

  bm2 = bm2_lookup(bm, a1 >> ADDR0_BITS);

  return (bm2 && bm0_is_any_set(bm2->bm1.bm0_w, a1 & ADDR0_MASK, size));
}

static __inline__
Bool bm_aligned_store_has_conflict_with(struct bitmap* const bm,
                                        const Addr a1, const SizeT size)
{
  const struct bitmap2* bm2;

  bm2 = bm2_lookup(bm, a1 >> ADDR0_BITS);

  if (bm2)
  {
    if (bm0_is_any_set(bm2->bm1.bm0_r, a1 & ADDR0_MASK, size)
        | bm0_is_any_set(bm2->bm1.bm0_w, a1 & ADDR0_MASK, size))
    {
      return True;
    }
  }
  return False;
}

#endif /* __DRD_BITMAP_H */
