/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2007 Bart Van Assche
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
 * Record an access of type access_type at addresses a in bitmap bm.
 */
static
__inline__
void bm_access_1(struct bitmap* const bm,
                 const Addr a,
                 const BmAccessTypeT access_type)
{
   struct bitmap2* p2;
   struct bitmap1* p1;
   UWord* p0;
   SPLIT_ADDRESS(a);

   tl_assert(bm);

   p2 = bm2_lookup_or_insert(bm, a1);
   p1 = &p2->bm1;
   p0 = (access_type == eLoad) ? p1->bm0_r : p1->bm0_w;
   bm0_set(p0, a0);
}

static
void bm_access_4_nonaligned(struct bitmap* const bm,
                            const Addr a,
                            const BmAccessTypeT access_type)
{
   bm_access_1(bm, a + 0, access_type);
   bm_access_1(bm, a + 1, access_type);
   bm_access_1(bm, a + 2, access_type);
   bm_access_1(bm, a + 3, access_type);
}

static
__inline__
void bm_access_4_aligned(struct bitmap* const bm,
                         const Addr a,
                         const BmAccessTypeT access_type)
{
   struct bitmap2* p2;
   struct bitmap1* p1;
   UWord* p0;
   SPLIT_ADDRESS(a);

   tl_assert(bm);

   p2 = bm2_lookup_or_insert(bm, a1);
   p1 = &p2->bm1;
   p0 = (access_type == eLoad) ? p1->bm0_r : p1->bm0_w;
   bm0_set(p0, a0+0);
   bm0_set(p0, a0+1);
   bm0_set(p0, a0+2);
   bm0_set(p0, a0+3);
}

/**
 * Record an access of type access_type at addresses a .. a + 3 in bitmap bm.
 */
void bm_access_4(struct bitmap* const bm,
                 const Addr a,
                 const BmAccessTypeT access_type)
{
   tl_assert(bm);
   if ((a & 3) != 0)
   {
      bm_access_4_nonaligned(bm, a, access_type);
   }
   else
   {
      bm_access_4_aligned(bm, a, access_type);
   }
}

/**
 * Record an access of type access_type at addresses a .. a + size - 1 in
 * bitmap bm.
 */
void bm_access_range(struct bitmap* const bm,
		     const Addr a,
		     const SizeT size,
		     const BmAccessTypeT access_type)
{
   tl_assert(bm);
   tl_assert(size > 0);

   if (size == 4)
      bm_access_4(bm, a, access_type);
   else if (size == 1)
      bm_access_1(bm, a, access_type);
   else
   {
      Addr b;
      for (b = a; b != a + size; b++)
      {
         bm_access_1(bm, b, access_type);
      }
   }
}

Bool bm_has(const struct bitmap* const bm,
            const Addr a1,
            const Addr a2,
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
                const Addr a1,
                const Addr a2,
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
#if 0
         VG_(message)(Vg_DebugMsg,
                      "in 0x%lx 0x%lx / cur 0x%lx 0x%lx / out 0x%lx 0x%lx",
                      a1, a2,
                      (bm2->addr << ADDR0_BITS),
                      (bm2->addr << ADDR0_BITS) + ADDR0_COUNT,
                      b_start, b_end);
#endif
         tl_assert(a1 <= b_end && b_end <= a2);
         tl_assert(b_start < b_end);
         tl_assert((b_start & ADDR0_MASK) <= ((b_end - 1) & ADDR0_MASK));
      
         for (b0 = b_start & ADDR0_MASK; b0 <= ((b_end - 1) & ADDR0_MASK); b0++)
         {
            const struct bitmap1* const p1 = &bm2->bm1;
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
   // Commented out the assert statements below because of performance reasons.
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

#if 1
// New and fast implementation.
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
#else
// Old and slow implementation
void bm_clear(const struct bitmap* const bm,
              const Addr a1,
              const Addr a2)
{
   Addr b, b_next, c;

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
         for (c = b; c < b_next; c++)
         {
            const UWord c0 = c & ADDR0_MASK;

            p2->bm1.bm0_r[c0 / (8*sizeof(UWord))]
               &= ~(1UL << (c0 % (8*sizeof(UWord))));
            p2->bm1.bm0_w[c0 / (8*sizeof(UWord))]
               &= ~(1UL << (c0 % (8*sizeof(UWord))));
         }
      }
   }
}
#endif

static
__inline__
UWord bm_has_conflict_with_1(const struct bitmap* const bm,
                             const Addr a,
                             const BmAccessTypeT access_type)
{
   struct bitmap2* p2;
   const UWord a0 = a & ADDR0_MASK;

   tl_assert(bm);

   p2 = bm_lookup(bm, a);
   if (p2)
   {
      if (access_type == eLoad)
      {
         return bm0_is_set(p2->bm1.bm0_w, a0);
      }
      else
      {
         tl_assert(access_type == eStore);
         return (bm0_is_set(p2->bm1.bm0_r, a0)
                 | bm0_is_set(p2->bm1.bm0_w, a0));
      }
   }
   return False;
}

/**
 * Return true if the access to [a,a+size[ of type access_type conflicts with
 * any access stored in bitmap bm.
 */
Bool bm_has_conflict_with(const struct bitmap* const bm,
                          const Addr a1,
                          const Addr a2,
                          const BmAccessTypeT access_type)
{
   Addr b;
   for (b = a1; b != a2; b++)
   {
      if (bm_has_conflict_with_1(bm, b, access_type))
      {
         return True;
      }
   }
   return False;
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
         //VG_(message)(Vg_DebugMsg, "0x%x 0x%x", bm2l->addr, bm2r->addr);
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

#ifdef OLD_RACE_DETECTION_ALGORITHM
/**
 * Report RW / WR / WW patterns between lhs and rhs.
 * @param tid1 Thread ID of lhs.
 * @param tid2 Thread ID of rhs.
 * @param lhs First bitmap.
 * @param rhs Bitmap to be compared with lhs.
 * @return Number of reported ranges with data races.
 */
void bm_report_races(const ThreadId tid1,
                     const ThreadId tid2,
                     const struct bitmap* const lhs,
                     const struct bitmap* const rhs)
{
   Addr     range_begin  = 0;
   Addr     range_end    = 0;
   UWord range_access = 0;

   VG_(message)(Vg_UserMsg, "Data addresses accessed by both segments:");

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
            if (access == range_access)
               range_end = a + 1;
            else
            {
               tl_assert(range_begin < range_end);
               if (HAS_RACE(range_access)
                   && ! drd_is_suppressed(range_begin, range_end))
               {
                  DataRaceInfo dri;
                  dri.tid1         = tid1;
                  dri.tid2         = tid2;
                  dri.range_begin  = range_begin;
                  dri.range_end    = range_end;
                  dri.range_access = range_access;
                  tl_assert(dri.range_begin < dri.range_end);
#if 0
                  VG_(maybe_record_error)(tid1,
                                          DataRaceErr,
                                          VG_(get_IP)(tid1), // where
                                          "data race",
                                          &dri);
#else
                  drd_report_data_race(&dri);
#endif
               }
               range_access = access;
               range_begin  = a;
               range_end    = a + 1;
            }
         }
      }
   }
}
#endif

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
      bm_access_range(bm, s_args[i].address,
                      s_args[i].size, s_args[i].access_type);
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


/*
 * Local variables:
 * c-basic-offset: 3
 * End:
 */
