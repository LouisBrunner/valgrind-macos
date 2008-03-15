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


#include "pub_tool_basics.h"      // Addr, SizeT
#include "pub_tool_debuginfo.h"   // VG_(get_objname)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(memset)
#include "pub_tool_libcprint.h"   // VG_(printf)
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_mallocfree.h"  // VG_(malloc), VG_(free)
#include "pub_drd_bitmap.h"
#include "drd_bitmap.h"
#include "drd_error.h"
#include "drd_suppression.h"


// Local constants.

static ULong s_bitmap_creation_count;


// Local function declarations.

static void bm2_merge(struct bitmap2* const bm2l,
                      const struct bitmap2* const bm2r);


// Function definitions.

struct bitmap* bm_new()
{
  struct bitmap* bm;

  // If this assert fails, fix the definition of BITS_PER_BITS_PER_UWORD
  // in drd_bitmap.h.
  tl_assert((1 << BITS_PER_BITS_PER_UWORD) == BITS_PER_UWORD);

  bm = VG_(malloc)(sizeof(*bm));
  tl_assert(bm);
  bm->oset = VG_(OSetGen_Create)(0, 0, VG_(malloc), VG_(free));

  s_bitmap_creation_count++;

  return bm;
}

void bm_delete(struct bitmap* const bm)
{
  tl_assert(bm);
  VG_(OSetGen_Destroy)(bm->oset);
  VG_(free)(bm);
}

/**
 * Record an access of type access_type at addresses a .. a + size - 1 in
 * bitmap bm.
 */
static
void bm_access_range(struct bitmap* const bm,
                     const Addr a1, const Addr a2,
                     const BmAccessTypeT access_type)
{
  Addr b, b_next;

  tl_assert(bm);
  tl_assert(a1 < a2);

  for (b = a1; b < a2; b = b_next)
  {
    Addr b_start;
    Addr b_end;
    struct bitmap2* bm2;
    SPLIT_ADDRESS(b);

    b_next = (b & ~ADDR0_MASK) + ADDR0_COUNT;
    if (b_next > a2)
    {
      b_next = a2;
    }

    bm2 = bm2_lookup_or_insert(bm, b1);
    tl_assert(bm2);

    if ((bm2->addr << ADDR0_BITS) < a1)
      b_start = a1;
    else
      if ((bm2->addr << ADDR0_BITS) < a2)
        b_start = (bm2->addr << ADDR0_BITS);
      else
        break;
    tl_assert(a1 <= b_start && b_start <= a2);

    if ((bm2->addr << ADDR0_BITS) + ADDR0_COUNT < a2)
      b_end = (bm2->addr << ADDR0_BITS) + ADDR0_COUNT;
    else
      b_end = a2;
    tl_assert(a1 <= b_end && b_end <= a2);
    tl_assert(b_start < b_end);
    tl_assert((b_start & ADDR0_MASK) <= ((b_end - 1) & ADDR0_MASK));
      
    for (b0 = b_start & ADDR0_MASK; b0 <= ((b_end - 1) & ADDR0_MASK); b0++)
    {
      if (access_type == eLoad)
      {
        bm0_set(bm2->bm1.bm0_r, b0);
      }
      else
      {
        bm0_set(bm2->bm1.bm0_w, b0);
      }
    }
  }
}

static inline
void bm_access_aligned_load(struct bitmap* const bm,
                            const Addr a1, const SizeT size)
{
  struct bitmap2* bm2;

  bm2 = bm2_lookup_or_insert(bm, a1 >> ADDR0_BITS);
  bm0_set_range(bm2->bm1.bm0_r, a1 & ADDR0_MASK, size);
}

static inline
void bm_access_aligned_store(struct bitmap* const bm,
                             const Addr a1, const SizeT size)
{
  struct bitmap2* bm2;

  bm2 = bm2_lookup_or_insert(bm, a1 >> ADDR0_BITS);
  bm0_set_range(bm2->bm1.bm0_w, a1 & ADDR0_MASK, size);
}

void bm_access_range_load(struct bitmap* const bm,
                          const Addr a1, const Addr a2)
{
  bm_access_range(bm, a1, a2, eLoad);
}

void bm_access_load_1(struct bitmap* const bm, const Addr a1)
{
  bm_access_aligned_load(bm, a1, 1);
}

void bm_access_load_2(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 1) == 0)
    bm_access_aligned_load(bm, a1, 2);
  else
    bm_access_range(bm, a1, a1 + 2, eLoad);
}

void bm_access_load_4(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 3) == 0)
    bm_access_aligned_load(bm, a1, 4);
  else
    bm_access_range(bm, a1, a1 + 4, eLoad);
}

void bm_access_load_8(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 7) == 0)
    bm_access_aligned_load(bm, a1, 8);
  else if ((a1 & 3) == 0)
  {
    bm_access_aligned_load(bm, a1 + 0, 4);
    bm_access_aligned_load(bm, a1 + 4, 4);
  }
  else
    bm_access_range(bm, a1, a1 + 8, eLoad);
}

void bm_access_store_1(struct bitmap* const bm, const Addr a1)
{
  bm_access_aligned_store(bm, a1, 1);
}

void bm_access_store_2(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 1) == 0)
    bm_access_aligned_store(bm, a1, 2);
  else
    bm_access_range(bm, a1, a1 + 2, eStore);
}

void bm_access_store_4(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 3) == 0)
    bm_access_aligned_store(bm, a1, 4);
  else
    bm_access_range(bm, a1, a1 + 4, eStore);
}

void bm_access_store_8(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 7) == 0)
    bm_access_aligned_store(bm, a1, 8);
  else if ((a1 & 3) == 0)
  {
    bm_access_aligned_store(bm, a1 + 0, 4);
    bm_access_aligned_store(bm, a1 + 4, 4);
  }
  else
    bm_access_range(bm, a1, a1 + 8, eStore);
}

void bm_access_range_store(struct bitmap* const bm,
                           const Addr a1, const Addr a2)
{
  bm_access_range(bm, a1, a2, eStore);
}

Bool bm_has(const struct bitmap* const bm, const Addr a1, const Addr a2,
            const BmAccessTypeT access_type)
{
  Addr b;
  for (b = a1; b < a2; b++)
  {
    if (! bm_has_1(bm, b, access_type))
    {
      return False;
    }
  }
  return True;
}

Bool bm_has_any(const struct bitmap* const bm,
                const Addr a1, const Addr a2,
                const BmAccessTypeT access_type)
{
  Addr b;

  tl_assert(bm);

  for (b = a1; b < a2; b++)
  {
    if (bm_has_1(bm, b, access_type))
    {
      return True;
    }
  }
  return False;
}

/* Return a non-zero value if there is a read access, write access or both */
/* to any of the addresses in the range [ a1, a2 [ in bitmap bm.           */
UWord bm_has_any_access(const struct bitmap* const bm,
                        const Addr a1,
                        const Addr a2)
{
  Addr b, b_next;

  tl_assert(bm);

  for (b = a1; b < a2; b = b_next)
  {
    struct bitmap2* bm2 = bm_lookup(bm, b);

    b_next = (b & ~ADDR0_MASK) + ADDR0_COUNT;
    if (b_next > a2)
    {
      b_next = a2;
    }

    if (bm2)
    {
      Addr b_start;
      Addr b_end;
      UWord b0;
      const struct bitmap1* const p1 = &bm2->bm1;

      if ((bm2->addr << ADDR0_BITS) < a1)
        b_start = a1;
      else
        if ((bm2->addr << ADDR0_BITS) < a2)
          b_start = (bm2->addr << ADDR0_BITS);
        else
          break;
      tl_assert(a1 <= b_start && b_start <= a2);

      if ((bm2->addr << ADDR0_BITS) + ADDR0_COUNT < a2)
        b_end = (bm2->addr << ADDR0_BITS) + ADDR0_COUNT;
      else
        b_end = a2;
      tl_assert(a1 <= b_end && b_end <= a2);
      tl_assert(b_start < b_end);
      tl_assert((b_start & ADDR0_MASK) <= ((b_end - 1) & ADDR0_MASK));
      
      for (b0 = b_start & ADDR0_MASK; b0 <= ((b_end-1) & ADDR0_MASK); b0++)
      {
        const UWord mask
          = bm0_is_set(p1->bm0_r, b0) | bm0_is_set(p1->bm0_w, b0);
        if (mask)
        {
          return mask;
        }
      }
    }
  }
  return 0;
}

/**
 * Report whether an access of type access_type at address a is recorded in
 * bitmap bm.
 * @return != 0 means true, and == 0 means false
 */
UWord bm_has_1(const struct bitmap* const bm,
               const Addr a,
               const BmAccessTypeT access_type)
{
  struct bitmap2* p2;
  struct bitmap1* p1;
  UWord* p0;
  const UWord a0 = a & ADDR0_MASK;

  tl_assert(bm);

  p2 = bm_lookup(bm, a);
  if (p2)
  {
    p1 = &p2->bm1;
    p0 = (access_type == eLoad) ? p1->bm0_r : p1->bm0_w;
    return bm0_is_set(p0, a0);
  }
  return 0;
}

static __inline__
void bm1_clear(struct bitmap1* const bm1, const Addr a1, const Addr a2)
{
  UWord idx;
  UWord mask;

#if 0
  /* Commented out the statements below because of performance reasons. */
  tl_assert(a1);
  tl_assert(a1 <= a2);
  tl_assert(UWORD_MSB(a1) == UWORD_MSB(a2)
            || UWORD_MSB(a1) == UWORD_MSB(a2 - 1));
#endif

  idx = (a1 & ADDR0_MASK) >> BITS_PER_BITS_PER_UWORD;
  /* mask: a contiguous series of one bits. The first bit set is bit */
  /* UWORD_LSB(a2-1), and the last bit set is UWORD_LSB(a1).         */
  mask = UWORD_LSB(a2) ? bm0_mask(a2) - bm0_mask(a1) : - bm0_mask(a1);
  bm1->bm0_r[idx] &= ~mask;
  bm1->bm0_w[idx] &= ~mask;
}

void bm_clear_all(const struct bitmap* const bm)
{
  struct bitmap2* bm2;

  VG_(OSetGen_ResetIter)(bm->oset);

  for ( ; (bm2 = VG_(OSetGen_Next)(bm->oset)) != 0; )
  {
    struct bitmap1* const bm1 = &bm2->bm1;
    tl_assert(bm1);
    VG_(memset)(&bm1->bm0_r[0], 0, sizeof(bm1->bm0_r));
    VG_(memset)(&bm1->bm0_w[0], 0, sizeof(bm1->bm0_w));
  }
}

void bm_clear(const struct bitmap* const bm,
              const Addr a1,
              const Addr a2)
{
  Addr b, b_next;

  tl_assert(bm);
  tl_assert(a1);
  tl_assert(a1 <= a2);

  for (b = a1; b < a2; b = b_next)
  {
    struct bitmap2* const p2 = bm_lookup(bm, b);

    b_next = (b & ~ADDR0_MASK) + ADDR0_COUNT;
    if (b_next > a2)
    {
      b_next = a2;
    }

    if (p2)
    {
      Addr c = b;
      if (UWORD_LSB(c))
      {
        Addr c_next = UWORD_MSB(c) + BITS_PER_UWORD;
        if (c_next > b_next)
          c_next = b_next;
        bm1_clear(&p2->bm1, c, c_next);
        c = c_next;
      }
      if (UWORD_LSB(c) == 0)
      {
        const Addr c_next = UWORD_MSB(b_next);
        tl_assert(UWORD_LSB(c) == 0);
        tl_assert(UWORD_LSB(c_next) == 0);
        tl_assert(c_next <= b_next);
        tl_assert(c <= c_next);
        if (c_next > c)
        {
          UWord idx = (c & ADDR0_MASK) >> BITS_PER_BITS_PER_UWORD;
          VG_(memset)(&p2->bm1.bm0_r[idx], 0, (c_next - c) / 8);
          VG_(memset)(&p2->bm1.bm0_w[idx], 0, (c_next - c) / 8);
          c = c_next;
        }
      }
      if (c != b_next)
      {
        bm1_clear(&p2->bm1, c, b_next);
      }
    }
  }
}

Bool bm_has_conflict_with(const struct bitmap* const bm,
                          const Addr a1, const Addr a2,
                          const BmAccessTypeT access_type)
{
  Addr b, b_next;

  tl_assert(bm);

  for (b = a1; b < a2; b = b_next)
  {
    struct bitmap2* bm2 = bm_lookup(bm, b);

    b_next = (b & ~ADDR0_MASK) + ADDR0_COUNT;
    if (b_next > a2)
    {
      b_next = a2;
    }

    if (bm2)
    {
      Addr b_start;
      Addr b_end;
      UWord b0;
      const struct bitmap1* const p1 = &bm2->bm1;

      if ((bm2->addr << ADDR0_BITS) < a1)
        b_start = a1;
      else
        if ((bm2->addr << ADDR0_BITS) < a2)
          b_start = (bm2->addr << ADDR0_BITS);
        else
          break;
      tl_assert(a1 <= b_start && b_start <= a2);

      if ((bm2->addr << ADDR0_BITS) + ADDR0_COUNT < a2)
        b_end = (bm2->addr << ADDR0_BITS) + ADDR0_COUNT;
      else
        b_end = a2;
      tl_assert(a1 <= b_end && b_end <= a2);
      tl_assert(b_start < b_end);
      tl_assert((b_start & ADDR0_MASK) <= ((b_end - 1) & ADDR0_MASK));
      
      for (b0 = b_start & ADDR0_MASK; b0 <= ((b_end-1) & ADDR0_MASK); b0++)
      {
        if (access_type == eLoad)
        {
          if (bm0_is_set(p1->bm0_w, b0))
          {
            return True;
          }
        }
        else
        {
          tl_assert(access_type == eStore);
          if (bm0_is_set(p1->bm0_r, b0)
              | bm0_is_set(p1->bm0_w, b0))
          {
            return True;
          }
        }
      }
    }
  }
  return False;
}

static inline
Bool bm_aligned_load_has_conflict_with(const struct bitmap* const bm,
                                       const Addr a1, const SizeT size)
{
  struct bitmap2* bm2;

  bm2 = bm_lookup(bm, a1);

  return (bm2 && bm0_is_any_set(bm2->bm1.bm0_w, a1 & ADDR0_MASK, size));
}

static inline
Bool bm_aligned_store_has_conflict_with(const struct bitmap* const bm,
                                        const Addr a1, const SizeT size)
{
  struct bitmap2* bm2;

  bm2 = bm_lookup(bm, a1);

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

Bool bm_load_has_conflict_with(const struct bitmap* const bm,
                               const Addr a1, const Addr a2)
{
  return bm_has_conflict_with(bm, a1, a2, eLoad);
}

Bool bm_load_1_has_conflict_with(const struct bitmap* const bm, const Addr a1)
{
  return bm_aligned_load_has_conflict_with(bm, a1, 1);
}

Bool bm_load_2_has_conflict_with(const struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 1) == 0)
    return bm_aligned_load_has_conflict_with(bm, a1, 2);
  else
    return bm_has_conflict_with(bm, a1, a1 + 2, eLoad);
}

Bool bm_load_4_has_conflict_with(const struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 3) == 0)
    return bm_aligned_load_has_conflict_with(bm, a1, 4);
  else
    return bm_has_conflict_with(bm, a1, a1 + 4, eLoad);
}

Bool bm_load_8_has_conflict_with(const struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 7) == 0)
    return bm_aligned_load_has_conflict_with(bm, a1, 8);
  else
    return bm_has_conflict_with(bm, a1, a1 + 8, eLoad);
}

Bool bm_store_1_has_conflict_with(const struct bitmap* const bm, const Addr a1)
{
  return bm_aligned_store_has_conflict_with(bm, a1, 1);
}

Bool bm_store_2_has_conflict_with(const struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 1) == 0)
    return bm_aligned_store_has_conflict_with(bm, a1, 2);
  else
    return bm_has_conflict_with(bm, a1, a1 + 2, eStore);
}

Bool bm_store_4_has_conflict_with(const struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 3) == 0)
    return bm_aligned_store_has_conflict_with(bm, a1, 4);
  else
    return bm_has_conflict_with(bm, a1, a1 + 4, eStore);
}

Bool bm_store_8_has_conflict_with(const struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 7) == 0)
    return bm_aligned_store_has_conflict_with(bm, a1, 8);
  else
    return bm_has_conflict_with(bm, a1, a1 + 8, eStore);
}

Bool bm_store_has_conflict_with(const struct bitmap* const bm,
                                const Addr a1, const Addr a2)
{
  return bm_has_conflict_with(bm, a1, a2, eStore);
}

void bm_swap(struct bitmap* const bm1, struct bitmap* const bm2)
{
  OSet* const tmp = bm1->oset;
  bm1->oset = bm2->oset;
  bm2->oset = tmp;
}

void bm_merge2(struct bitmap* const lhs,
               const struct bitmap* const rhs)
{
  struct bitmap2* bm2l;
  const struct bitmap2* bm2r;

  // First step: allocate any missing bitmaps in *lhs.
  VG_(OSetGen_ResetIter)(rhs->oset);
  for ( ; (bm2r = VG_(OSetGen_Next)(rhs->oset)) != 0; )
  {
    bm2_lookup_or_insert(lhs, bm2r->addr);
  }

  VG_(OSetGen_ResetIter)(lhs->oset);
  VG_(OSetGen_ResetIter)(rhs->oset);

  for ( ; (bm2r = VG_(OSetGen_Next)(rhs->oset)) != 0; )
  {
    do
    {
      bm2l = VG_(OSetGen_Next)(lhs->oset);
    } while (bm2l->addr < bm2r->addr);

    tl_assert(bm2l->addr == bm2r->addr);

    bm2_merge(bm2l, bm2r);
  }
}

/**
 * Report whether there are any RW / WR / WW patterns in lhs and rhs.
 * @param lhs First bitmap.
 * @param rhs Bitmap to be compared with lhs.
 * @return !=0 if there are data races, == 0 if there are none.
 */
int bm_has_races(const struct bitmap* const lhs,
                 const struct bitmap* const rhs)
{
  VG_(OSetGen_ResetIter)(lhs->oset);
  VG_(OSetGen_ResetIter)(rhs->oset);

  for (;;)
  {
    const struct bitmap2* bm2l = VG_(OSetGen_Next)(lhs->oset);
    const struct bitmap2* bm2r = VG_(OSetGen_Next)(rhs->oset);
    const struct bitmap1* bm1l;
    const struct bitmap1* bm1r;
    unsigned k;

    while (bm2l && bm2r && bm2l->addr != bm2r->addr)
    {
      if (bm2l->addr < bm2r->addr)
        bm2l = VG_(OSetGen_Next)(lhs->oset);
      else
        bm2r = VG_(OSetGen_Next)(rhs->oset);
    }
    if (bm2l == 0 || bm2r == 0)
      break;

    bm1l = &bm2l->bm1;
    bm1r = &bm2r->bm1;

    for (k = 0; k < BITMAP1_UWORD_COUNT; k++)
    {
      unsigned b;
      for (b = 0; b < BITS_PER_UWORD; b++)
      {
        UWord const access
          = ((bm1l->bm0_r[k] & bm0_mask(b)) ? LHS_R : 0)
          | ((bm1l->bm0_w[k] & bm0_mask(b)) ? LHS_W : 0)
          | ((bm1r->bm0_r[k] & bm0_mask(b)) ? RHS_R : 0)
          | ((bm1r->bm0_w[k] & bm0_mask(b)) ? RHS_W : 0);
        Addr const a = MAKE_ADDRESS(bm2l->addr, k * BITS_PER_UWORD | b);
        if (HAS_RACE(access) && ! drd_is_suppressed(a, a + 1))
        {
          return 1;
        }
      }
    }
  }
  return 0;
}

void bm_print(const struct bitmap* const bm)
{
  struct bitmap2* bm2;

  VG_(OSetGen_ResetIter)(bm->oset);

  for ( ; (bm2 = VG_(OSetGen_Next)(bm->oset)) != 0; )
  {
    const struct bitmap1* const bm1 = &bm2->bm1;
    unsigned k;
    for (k = 0; k < BITMAP1_UWORD_COUNT; k++)
    {
      unsigned b;
      for (b = 0; b < BITS_PER_UWORD; b++)
      {
        int const r = bm1->bm0_r[k] & bm0_mask(b);
        int const w = bm1->bm0_w[k] & bm0_mask(b);
        Addr const a = MAKE_ADDRESS(bm2->addr, k * BITS_PER_UWORD | b);
        if (r || w)
        {
          VG_(printf)("0x%08lx %c %c\n",
                      (Addr)(a), 
                      w ? 'W' : ' ', r ? 'R' : ' ');
        }
      }
    }
  }
}

ULong bm_get_bitmap_creation_count(void)
{
  return s_bitmap_creation_count;
}

ULong bm_get_bitmap2_creation_count(void)
{
  return s_bitmap2_creation_count;
}

static void bm2_merge(struct bitmap2* const bm2l,
                      const struct bitmap2* const bm2r)
{
  unsigned k;

  tl_assert(bm2l->addr == bm2r->addr);

  for (k = 0; k < BITMAP1_UWORD_COUNT; k++)
  {
    bm2l->bm1.bm0_r[k] |= bm2r->bm1.bm0_r[k];
  }
  for (k = 0; k < BITMAP1_UWORD_COUNT; k++)
  {
    bm2l->bm1.bm0_w[k] |= bm2r->bm1.bm0_w[k];
  }
}

#if 0

/* Unit test */
static
struct { Addr address; SizeT size; BmAccessTypeT access_type; }
  s_args[] = {
    {          0, 1, eLoad  },
    {        666, 4, eLoad  },
    {        667, 2, eStore },
    {       1024, 1, eStore },
    { 0x0000ffff, 1, eLoad  },
    { 0x0001ffff, 1, eLoad  },
    { 0x00ffffff, 1, eLoad  },
    { 0xffffffff, 1, eStore },
  };

void bm_test(void)
{
  struct bitmap* bm;
  struct bitmap* bm2;
  int i, j;

  VG_(printf)("Start of DRD BM unit test.\n");

  bm = bm_new();

  for (i = 0; i < sizeof(s_args)/sizeof(s_args[0]); i++)
  {
    bm_access_range(bm,
                    s_args[i].address,
                    s_args[i].address + s_args[i].size,
                    s_args[i].access_type);
  }

  VG_(printf)("Map contents -- should contain 10 addresses:\n");
  bm_print(bm);

  for (i = 0; i < sizeof(s_args)/sizeof(s_args[0]); i++)
  {
    for (j = 0; j < s_args[i].size; j++)
    {
      tl_assert(bm_has_1(bm, s_args[i].address + j, s_args[i].access_type));
    }
  }

  VG_(printf)("Merge result:\n");
  bm2 = bm_merge(bm, bm);
  bm_print(bm);

  bm_delete(bm);
  bm_delete(bm2);

  VG_(printf)("End of DRD BM unit test.\n");
}
#endif
