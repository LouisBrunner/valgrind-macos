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


/* Forward declarations. */

struct bitmap2;


/* Local function declarations. */

static void bm2_merge(struct bitmap2* const bm2l,
                      const struct bitmap2* const bm2r);


/* Local constants. */

static ULong s_bitmap_creation_count;


/* Function definitions. */

struct bitmap* bm_new()
{
  unsigned i;
  struct bitmap* bm;

  /* If this assert fails, fix the definition of BITS_PER_BITS_PER_UWORD */
  /* in drd_bitmap.h.                                                    */
  tl_assert((1 << BITS_PER_BITS_PER_UWORD) == BITS_PER_UWORD);

  bm = VG_(malloc)("drd.bitmap.bn.1", sizeof(*bm));
  tl_assert(bm);
  /* Cache initialization. a1 is initialized with a value that never can */
  /* match any valid address: the upper ADDR0_BITS bits of a1 are always */
  /* zero for a valid cache entry.                                       */
  for (i = 0; i < N_CACHE_ELEM; i++)
  {
    bm->cache[i].a1  = ~(UWord)1;
    bm->cache[i].bm2 = 0;
  }
  bm->oset = VG_(OSetGen_Create)(0, 0, VG_(malloc), "drd.bitmap.bn.2",
                                       VG_(free));

  s_bitmap_creation_count++;

  return bm;
}

void bm_delete(struct bitmap* const bm)
{
  struct bitmap2*    bm2;
  struct bitmap2ref* bm2ref;

  tl_assert(bm);

  VG_(OSetGen_ResetIter)(bm->oset);
  for ( ; (bm2ref = VG_(OSetGen_Next)(bm->oset)) != 0; )
  {
    bm2 = bm2ref->bm2;
    tl_assert(bm2->refcnt >= 1);
    if (--bm2->refcnt == 0)
    {
      VG_(free)(bm2);
    }
  }

  VG_(OSetGen_Destroy)(bm->oset);
  VG_(free)(bm);
}

/**
 * Record an access of type access_type at addresses a .. a + size - 1 in
 * bitmap bm.
 */
void bm_access_range(struct bitmap* const bm,
                     const Addr a1, const Addr a2,
                     const BmAccessTypeT access_type)
{
  Addr b, b_next;

  tl_assert(bm);
  tl_assert(a1 < a2);
  /* The current implementation of bm_access_range does not work for the   */
  /* ADDR0_COUNT highest addresses in the address range. At least on Linux */
  /* this is not a problem since the upper part of the address space is    */
  /* reserved for the kernel.                                              */
  tl_assert(a2 + ADDR0_COUNT > a2);

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

    bm2 = bm2_lookup_or_insert_exclusive(bm, b1);
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
      
    if (access_type == eLoad)
    {
      for (b0 = b_start & ADDR0_MASK; b0 <= ((b_end - 1) & ADDR0_MASK); b0++)
      {
        bm0_set(bm2->bm1.bm0_r, b0);
      }
    }
    else
    {
      for (b0 = b_start & ADDR0_MASK; b0 <= ((b_end - 1) & ADDR0_MASK); b0++)
      {
        bm0_set(bm2->bm1.bm0_w, b0);
      }
    }
  }
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

void bm_access_range_store(struct bitmap* const bm,
                           const Addr a1, const Addr a2)
{
  bm_access_range(bm, a1, a2, eStore);
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

Bool bm_has(struct bitmap* const bm, const Addr a1, const Addr a2,
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

Bool bm_has_any_load(struct bitmap* const bm, const Addr a1, const Addr a2)
{
  Addr b, b_next;

  tl_assert(bm);

  for (b = a1; b < a2; b = b_next)
  {
    const struct bitmap2* bm2 = bm2_lookup(bm, b >> ADDR0_BITS);

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
        if (bm0_is_set(p1->bm0_r, b0))
        {
          return True;
        }
      }
    }
  }
  return 0;
}

Bool bm_has_any_store(struct bitmap* const bm,
                      const Addr a1, const Addr a2)
{
  Addr b, b_next;

  tl_assert(bm);

  for (b = a1; b < a2; b = b_next)
  {
    const struct bitmap2* bm2 = bm2_lookup(bm, b >> ADDR0_BITS);

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
        if (bm0_is_set(p1->bm0_w, b0))
        {
          return True;
        }
      }
    }
  }
  return 0;
}

/* Return True if there is a read access, write access or both   */
/* to any of the addresses in the range [ a1, a2 [ in bitmap bm. */
Bool bm_has_any_access(struct bitmap* const bm,
                       const Addr a1, const Addr a2)
{
  Addr b, b_next;

  tl_assert(bm);

  for (b = a1; b < a2; b = b_next)
  {
    const struct bitmap2* bm2 = bm2_lookup(bm, b >> ADDR0_BITS);

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
        if (bm0_is_set(p1->bm0_r, b0) | bm0_is_set(p1->bm0_w, b0))
        {
          return True;
        }
      }
    }
  }
  return False;
}

/** Report whether an access of type access_type at address a is recorded in
 *  bitmap bm.
 */
Bool bm_has_1(struct bitmap* const bm,
              const Addr a, const BmAccessTypeT access_type)
{
  const struct bitmap2* p2;
  const struct bitmap1* p1;
  const UWord* p0;
  const UWord a0 = a & ADDR0_MASK;

  tl_assert(bm);

  p2 = bm2_lookup(bm, a >> ADDR0_BITS);
  if (p2)
  {
    p1 = &p2->bm1;
    p0 = (access_type == eLoad) ? p1->bm0_r : p1->bm0_w;
    return bm0_is_set(p0, a0) ? True : False;
  }
  return False;
}

void bm_clear(struct bitmap* const bm,
              const Addr a1,
              const Addr a2)
{
  Addr b, b_next;

  tl_assert(bm);
  tl_assert(a1);
  tl_assert(a1 <= a2);

  for (b = a1; b < a2; b = b_next)
  {
    struct bitmap2* const p2 = bm2_lookup_exclusive(bm, b >> ADDR0_BITS);

    b_next = (b & ~ADDR0_MASK) + ADDR0_COUNT;
    if (b_next > a2)
    {
      b_next = a2;
    }

    if (p2)
    {
      Addr c = b;
      /* If the first address in the bitmap that must be cleared does not */
      /* start on an UWord boundary, start clearing the first addresses.  */
      if (UWORD_LSB(c))
      {
        Addr c_next = UWORD_MSB(c) + BITS_PER_UWORD;
        if (c_next > b_next)
          c_next = b_next;
        bm0_clear_range(p2->bm1.bm0_r, c & ADDR0_MASK, c_next - c);
        bm0_clear_range(p2->bm1.bm0_w, c & ADDR0_MASK, c_next - c);
        c = c_next;
      }
      /* If some UWords have to be cleared entirely, do this now. */
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
      /* If the last address in the bitmap that must be cleared does not */
      /* fall on an UWord boundary, clear the last addresses.            */
      /* tl_assert(c <= b_next); */
      bm0_clear_range(p2->bm1.bm0_r, c & ADDR0_MASK, b_next - c);
      bm0_clear_range(p2->bm1.bm0_w, c & ADDR0_MASK, b_next - c);
    }
  }
}

/** Clear all references to loads in bitmap bm starting at address a1 and
 *  up to but not including address a2.
 */
void bm_clear_load(struct bitmap* const bm,
                   const Addr a1, const Addr a2)
{
  Addr a;

  for (a = a1; a < a2; a++)
  {
    struct bitmap2* const p2 = bm2_lookup_exclusive(bm, a >> ADDR0_BITS);
    if (p2)
    {
      bm0_clear(p2->bm1.bm0_r, a & ADDR0_MASK);
    }
  }
}

/** Clear all references to stores in bitmap bm starting at address a1 and
 *  up to but not including address a2.
 */
void bm_clear_store(struct bitmap* const bm,
                    const Addr a1, const Addr a2)
{
  Addr a;

  for (a = a1; a < a2; a++)
  {
    struct bitmap2* const p2 = bm2_lookup_exclusive(bm, a >> ADDR0_BITS);
    if (p2)
    {
      bm0_clear(p2->bm1.bm0_w, a & ADDR0_MASK);
    }
  }
}

/** Clear bitmap bm starting at address a1 and up to but not including address
 *  a2. Return True if and only if any of the addresses was set before
 *  clearing.
 */
Bool bm_test_and_clear(struct bitmap* const bm,
                       const Addr a1, const Addr a2)
{
  Bool result;

  result = bm_has_any_access(bm, a1, a2) != 0;
  bm_clear(bm, a1, a2);
  return result;
}

Bool bm_has_conflict_with(struct bitmap* const bm,
                          const Addr a1, const Addr a2,
                          const BmAccessTypeT access_type)
{
  Addr b, b_next;

  tl_assert(bm);

  for (b = a1; b < a2; b = b_next)
  {
    const struct bitmap2* bm2 = bm2_lookup(bm, b >> ADDR0_BITS);

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

Bool bm_load_has_conflict_with(struct bitmap* const bm,
                               const Addr a1, const Addr a2)
{
  return bm_has_conflict_with(bm, a1, a2, eLoad);
}

Bool bm_load_1_has_conflict_with(struct bitmap* const bm, const Addr a1)
{
  return bm_aligned_load_has_conflict_with(bm, a1, 1);
}

Bool bm_load_2_has_conflict_with(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 1) == 0)
    return bm_aligned_load_has_conflict_with(bm, a1, 2);
  else
    return bm_has_conflict_with(bm, a1, a1 + 2, eLoad);
}

Bool bm_load_4_has_conflict_with(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 3) == 0)
    return bm_aligned_load_has_conflict_with(bm, a1, 4);
  else
    return bm_has_conflict_with(bm, a1, a1 + 4, eLoad);
}

Bool bm_load_8_has_conflict_with(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 7) == 0)
    return bm_aligned_load_has_conflict_with(bm, a1, 8);
  else
    return bm_has_conflict_with(bm, a1, a1 + 8, eLoad);
}

Bool bm_store_1_has_conflict_with(struct bitmap* const bm, const Addr a1)
{
  return bm_aligned_store_has_conflict_with(bm, a1, 1);
}

Bool bm_store_2_has_conflict_with(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 1) == 0)
    return bm_aligned_store_has_conflict_with(bm, a1, 2);
  else
    return bm_has_conflict_with(bm, a1, a1 + 2, eStore);
}

Bool bm_store_4_has_conflict_with(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 3) == 0)
    return bm_aligned_store_has_conflict_with(bm, a1, 4);
  else
    return bm_has_conflict_with(bm, a1, a1 + 4, eStore);
}

Bool bm_store_8_has_conflict_with(struct bitmap* const bm, const Addr a1)
{
  if ((a1 & 7) == 0)
    return bm_aligned_store_has_conflict_with(bm, a1, 8);
  else
    return bm_has_conflict_with(bm, a1, a1 + 8, eStore);
}

Bool bm_store_has_conflict_with(struct bitmap* const bm,
                                const Addr a1, const Addr a2)
{
  return bm_has_conflict_with(bm, a1, a2, eStore);
}

/** Return True if the two bitmaps *lhs and *rhs are identical, and false
 *  if not.
 */
Bool bm_equal(struct bitmap* const lhs, struct bitmap* const rhs)
{
  struct bitmap2* bm2l;
  struct bitmap2ref* bm2l_ref;
  struct bitmap2* bm2r;
  const struct bitmap2ref* bm2r_ref;

  /* It's not possible to have two independent iterators over the same OSet, */
  /* so complain if lhs == rhs.                                              */
  tl_assert(lhs != rhs);

  VG_(OSetGen_ResetIter)(lhs->oset);
  VG_(OSetGen_ResetIter)(rhs->oset);

  for ( ; (bm2l_ref = VG_(OSetGen_Next)(lhs->oset)) != 0; )
  {
    while (bm2l_ref
           && (bm2l = bm2l_ref->bm2)
           && bm2l
           && ! bm_has_any_access(lhs,
                                  bm2l->addr << ADDR0_BITS,
                                  (bm2l->addr + 1) << ADDR0_BITS))
    {
      bm2l_ref = VG_(OSetGen_Next)(lhs->oset);
    }
    if (bm2l_ref == 0)
      break;
    tl_assert(bm2l);
#if 0
    VG_(message)(Vg_DebugMsg, "bm_equal: at 0x%lx", bm2l->addr << ADDR0_BITS);
#endif

    bm2r_ref = VG_(OSetGen_Next)(rhs->oset);
    if (bm2r_ref == 0)
    {
#if 0
      VG_(message)(Vg_DebugMsg, "bm_equal: no match found");
#endif
      return False;
    }
    bm2r = bm2r_ref->bm2;
    tl_assert(bm2r);
    tl_assert(bm_has_any_access(rhs,
                                bm2r->addr << ADDR0_BITS,
                                (bm2r->addr + 1) << ADDR0_BITS));

    if (bm2l != bm2r
        && (bm2l->addr != bm2r->addr
            || VG_(memcmp)(&bm2l->bm1, &bm2r->bm1, sizeof(bm2l->bm1)) != 0))
    {
#if 0
      VG_(message)(Vg_DebugMsg, "bm_equal: rhs 0x%lx -- returning false",
                   bm2r->addr << ADDR0_BITS);
#endif
      return False;
    }
  }
  bm2r = VG_(OSetGen_Next)(rhs->oset);
  if (bm2r)
  {
    tl_assert(bm_has_any_access(rhs,
                                bm2r->addr << ADDR0_BITS,
                                (bm2r->addr + 1) << ADDR0_BITS));
#if 0
    VG_(message)(Vg_DebugMsg,
                 "bm_equal: remaining rhs 0x%lx -- returning false",
                 bm2r->addr << ADDR0_BITS);
#endif
    return False;
  }
  return True;
}

void bm_swap(struct bitmap* const bm1, struct bitmap* const bm2)
{
  OSet* const tmp = bm1->oset;
  bm1->oset = bm2->oset;
  bm2->oset = tmp;
}

/** Merge bitmaps *lhs and *rhs into *lhs. */
void bm_merge2(struct bitmap* const lhs,
               struct bitmap* const rhs)
{
  struct bitmap2* bm2l;
  struct bitmap2ref* bm2l_ref;
  struct bitmap2* bm2r;
  const struct bitmap2ref* bm2r_ref;

  VG_(OSetGen_ResetIter)(rhs->oset);

  for ( ; (bm2r_ref = VG_(OSetGen_Next)(rhs->oset)) != 0; )
  {
    bm2r = bm2r_ref->bm2;
    bm2l_ref = VG_(OSetGen_Lookup)(lhs->oset, &bm2r->addr);
    if (bm2l_ref)
    {
      bm2l = bm2l_ref->bm2;
      if (bm2l != bm2r)
      {
        if (bm2l->refcnt > 1)
          bm2l = bm2_make_exclusive(lhs, bm2l_ref);
        bm2_merge(bm2l, bm2r);
      }
    }
    else
    {
      bm2_insert_addref(lhs, bm2r);
    }
  }
}

/**
 * Report whether there are any RW / WR / WW patterns in lhs and rhs.
 * @param lhs First bitmap.
 * @param rhs Bitmap to be compared with lhs.
 * @return !=0 if there are data races, == 0 if there are none.
 */
int bm_has_races(struct bitmap* const lhs,
                 struct bitmap* const rhs)
{
  VG_(OSetGen_ResetIter)(lhs->oset);
  VG_(OSetGen_ResetIter)(rhs->oset);

  for (;;)
  {
    const struct bitmap2ref* bm2l_ref;
    const struct bitmap2ref* bm2r_ref;
    const struct bitmap2* bm2l;
    const struct bitmap2* bm2r;
    const struct bitmap1* bm1l;
    const struct bitmap1* bm1r;
    unsigned k;

    bm2l_ref = VG_(OSetGen_Next)(lhs->oset);
    bm2l = bm2l_ref->bm2;
    bm2r_ref = VG_(OSetGen_Next)(rhs->oset);
    bm2r = bm2r_ref->bm2;
    while (bm2l && bm2r && bm2l->addr != bm2r->addr)
    {
      if (bm2l->addr < bm2r->addr)
        bm2l = (bm2l_ref = VG_(OSetGen_Next)(lhs->oset))->bm2;
      else
        bm2r = (bm2r_ref = VG_(OSetGen_Next)(rhs->oset))->bm2;
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
        UWord const access_mask
          = ((bm1l->bm0_r[k] & bm0_mask(b)) ? LHS_R : 0)
          | ((bm1l->bm0_w[k] & bm0_mask(b)) ? LHS_W : 0)
          | ((bm1r->bm0_r[k] & bm0_mask(b)) ? RHS_R : 0)
          | ((bm1r->bm0_w[k] & bm0_mask(b)) ? RHS_W : 0);
        Addr const a = MAKE_ADDRESS(bm2l->addr, k * BITS_PER_UWORD | b);
        if (HAS_RACE(access_mask) && ! drd_is_suppressed(a, a + 1))
        {
          return 1;
        }
      }
    }
  }
  return 0;
}

void bm_print(struct bitmap* const bm)
{
  struct bitmap2* bm2;
  struct bitmap2ref* bm2ref;

  VG_(OSetGen_ResetIter)(bm->oset);

  for ( ; (bm2ref = VG_(OSetGen_Next)(bm->oset)) != 0; )
  {
    const struct bitmap1* bm1;
    unsigned b;

    bm2 = bm2ref->bm2;
    bm1 = &bm2->bm1;
    for (b = 0; b < ADDR0_COUNT; b++)
    {
      const Addr a = (bm2->addr << ADDR0_BITS) | b;
      const Bool r = bm0_is_set(bm1->bm0_r, b) != 0;
      const Bool w = bm0_is_set(bm1->bm0_w, b) != 0;
      if (r || w)
      {
        VG_(printf)("0x%08lx %c %c\n",
                    a,
                    w ? 'W' : ' ',
                    r ? 'R' : ' ');
      }
    }
  }
}

ULong bm_get_bitmap_creation_count(void)
{
  return s_bitmap_creation_count;
}

ULong bm_get_bitmap2_node_creation_count(void)
{
  return s_bitmap2_node_creation_count;
}

ULong bm_get_bitmap2_creation_count(void)
{
  return s_bitmap2_creation_count;
}

/** Allocate and initialize a second level bitmap. */
static struct bitmap2* bm2_new(const UWord a1)
{
  struct bitmap2* bm2;

  bm2 = VG_(malloc)("drd.bitmap.bm2n.1", sizeof(*bm2));
  bm2->addr   = a1;
  bm2->refcnt = 1;

  s_bitmap2_creation_count++;

  return bm2;
}

/** Make a copy of a shared second level bitmap such that the copy can be
 *  modified.
 *
 *  @param a1 client address shifted right by ADDR0_BITS.
 *  @param bm bitmap pointer.
 */
static
struct bitmap2* bm2_make_exclusive(struct bitmap* const bm,
                                   struct bitmap2ref* const bm2ref)
{
  UWord a1;
  struct bitmap2* bm2;
  struct bitmap2* bm2_copy;

  tl_assert(bm);
  tl_assert(bm2ref);
  bm2 = bm2ref->bm2;
  tl_assert(bm2);
  tl_assert(bm2->refcnt > 1);
  bm2->refcnt--;
  tl_assert(bm2->refcnt >= 1);
  a1 = bm2->addr;
  bm2_copy = bm2_new(a1);
  tl_assert(bm2_copy);
  tl_assert(bm2_copy->addr   == a1);
  tl_assert(bm2_copy->refcnt == 1);
  VG_(memcpy)(&bm2_copy->bm1, &bm2->bm1, sizeof(bm2->bm1));
  bm2ref->bm2 = bm2_copy;

  bm_update_cache(bm, a1, bm2_copy);

  return bm2_copy;
}

static void bm2_merge(struct bitmap2* const bm2l,
                      const struct bitmap2* const bm2r)
{
  unsigned k;

  tl_assert(bm2l);
  tl_assert(bm2r);
  tl_assert(bm2l->addr == bm2r->addr);
  tl_assert(bm2l->refcnt == 1);

  for (k = 0; k < BITMAP1_UWORD_COUNT; k++)
  {
    bm2l->bm1.bm0_r[k] |= bm2r->bm1.bm0_r[k];
  }
  for (k = 0; k < BITMAP1_UWORD_COUNT; k++)
  {
    bm2l->bm1.bm0_w[k] |= bm2r->bm1.bm0_w[k];
  }
}
