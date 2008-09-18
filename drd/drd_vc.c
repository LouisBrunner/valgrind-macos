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


#include "drd_vc.h"
#include "pub_tool_basics.h"      // Addr, SizeT
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(memset), VG_(memmove)
#include "pub_tool_libcprint.h"   // VG_(printf)
#include "pub_tool_mallocfree.h"  // VG_(malloc), VG_(free)
#include "pub_tool_threadstate.h" // VG_(get_running_tid)


static
void vc_reserve(VectorClock* const vc, const unsigned new_capacity);


void vc_init(VectorClock* const vc,
             const VCElem* const vcelem,
             const unsigned size)
{
  tl_assert(vc);
  vc->size = 0;
  vc->capacity = 0;
  vc->vc = 0;
  vc_reserve(vc, size);
  tl_assert(size == 0 || vc->vc != 0);
  if (vcelem)
  {
    VG_(memcpy)(vc->vc, vcelem, size * sizeof(vcelem[0]));
    vc->size = size;
  }
}

void vc_cleanup(VectorClock* const vc)
{
  vc_reserve(vc, 0);
}

/** Copy constructor -- initializes *new. */
void vc_copy(VectorClock* const new,
             const VectorClock* const rhs)
{
  vc_init(new, rhs->vc, rhs->size);
}

/** Assignment operator -- *lhs is already a valid vector clock. */
void vc_assign(VectorClock* const lhs,
               const VectorClock* const rhs)
{
  vc_cleanup(lhs);
  vc_copy(lhs, rhs);
}

void vc_increment(VectorClock* const vc, ThreadId const threadid)
{
  unsigned i;
  for (i = 0; i < vc->size; i++)
  {
    if (vc->vc[i].threadid == threadid)
    {
      typeof(vc->vc[i].count) const oldcount = vc->vc[i].count;
      vc->vc[i].count++;
      // Check for integer overflow.
      tl_assert(oldcount < vc->vc[i].count);
      return;
    }
  }

  // The specified thread ID does not yet exist in the vector clock
  // -- insert it.
  {
    VCElem vcelem = { threadid, 1 };
    VectorClock vc2;
    vc_init(&vc2, &vcelem, 1);
    vc_combine(vc, &vc2);
    vc_cleanup(&vc2);
  }
}

/**
 * @return True if vector clocks vc1 and vc2 are ordered, and false otherwise.
 * Order is as imposed by thread synchronization actions ("happens before").
 */
Bool vc_ordered(const VectorClock* const vc1,
                const VectorClock* const vc2)
{
  return vc_lte(vc1, vc2) || vc_lte(vc2, vc1);
}

/** Compute elementwise minimum. */
void vc_min(VectorClock* const result, const VectorClock* const rhs)
{
  unsigned i;
  unsigned j;

  tl_assert(result);
  tl_assert(rhs);

  vc_check(result);

  /* Next, combine both vector clocks into one. */
  i = 0;
  for (j = 0; j < rhs->size; j++)
  {
    while (i < result->size && result->vc[i].threadid < rhs->vc[j].threadid)
    {
      /* Thread ID is missing in second vector clock. Clear the count. */
      result->vc[i].count = 0;
      i++;
    }
    if (i >= result->size)
    {
      break;
    }
    if (result->vc[i].threadid <= rhs->vc[j].threadid)
    {
      /* The thread ID is present in both vector clocks. Compute the minimum */
      /* of vc[i].count and vc[j].count. */
      tl_assert(result->vc[i].threadid == rhs->vc[j].threadid);
      if (rhs->vc[j].count < result->vc[i].count)
      {
        result->vc[i].count = rhs->vc[j].count;
      }
    }
  }
  vc_check(result);
}

/**
 * Compute elementwise maximum.
 */
void vc_combine(VectorClock* const result,
                const VectorClock* const rhs)
{
  vc_combine2(result, rhs, -1);
}

/** Compute elementwise maximum.
 *
 *  @return True if *result and *rhs are equal, or if *result and *rhs only
 *          differ in the component with thread ID tid.
 */
Bool vc_combine2(VectorClock* const result,
                 const VectorClock* const rhs,
                 const ThreadId tid)
{
  unsigned i;
  unsigned j;
  unsigned shared;
  unsigned new_size;
  Bool     almost_equal = True;

  tl_assert(result);
  tl_assert(rhs);

  // First count the number of shared thread id's.
  j = 0;
  shared = 0;
  for (i = 0; i < result->size; i++)
  {
    while (j < rhs->size && rhs->vc[j].threadid < result->vc[i].threadid)
      j++;
    if (j >= rhs->size)
      break;
    if (result->vc[i].threadid == rhs->vc[j].threadid)
      shared++;
  }

  vc_check(result);

  new_size = result->size + rhs->size - shared;
  if (new_size > result->capacity)
    vc_reserve(result, new_size);

  vc_check(result);

  // Next, combine both vector clocks into one.
  i = 0;
  for (j = 0; j < rhs->size; j++)
  {
    /* First of all, skip those clocks in result->vc[] for which there */
    /* is no corresponding clock in rhs->vc[].                         */
    while (i < result->size && result->vc[i].threadid < rhs->vc[j].threadid)
    {
      if (result->vc[i].threadid != tid)
      {
        almost_equal = False;
      }
      i++;
    }
    /* If the end of *result is met, append rhs->vc[j] to *result. */
    if (i >= result->size)
    {
      result->size++;
      result->vc[i] = rhs->vc[j];
      if (result->vc[i].threadid != tid)
      {
        almost_equal = False;
      }
    }
    /* If clock rhs->vc[j] is not in *result, insert it. */
    else if (result->vc[i].threadid > rhs->vc[j].threadid)
    {
      unsigned k;
      for (k = result->size; k > i; k--)
      {
        result->vc[k] = result->vc[k - 1];
      }
      result->size++;
      result->vc[i] = rhs->vc[j];
      if (result->vc[i].threadid != tid)
      {
        almost_equal = False;
      }
    }
    /* Otherwise, both *result and *rhs have a clock for thread            */
    /* result->vc[i].threadid == rhs->vc[j].threadid. Compute the maximum. */
    else
    {
      tl_assert(result->vc[i].threadid == rhs->vc[j].threadid);
      if (result->vc[i].threadid != tid
          && rhs->vc[j].count != result->vc[i].count)
      {
        almost_equal = False;
      }
      if (rhs->vc[j].count > result->vc[i].count)
      {
        result->vc[i].count = rhs->vc[j].count;
      }
    }
  }
  vc_check(result);
  tl_assert(result->size == new_size);

  return almost_equal;
}

void vc_print(const VectorClock* const vc)
{
  unsigned i;

  tl_assert(vc);
  VG_(printf)("[");
  for (i = 0; i < vc->size; i++)
  {
    tl_assert(vc->vc);
    VG_(printf)("%s %d: %d", i > 0 ? "," : "",
                vc->vc[i].threadid, vc->vc[i].count);
  }
  VG_(printf)(" ]");
}

void vc_snprint(Char* const str, Int const size,
                const VectorClock* const vc)
{
  unsigned i;
  unsigned j = 1;

  tl_assert(vc);
  VG_(snprintf)(str, size, "[");
  for (i = 0; i < vc->size; i++)
  {
    tl_assert(vc->vc);
    for ( ; j <= vc->vc[i].threadid; j++)
    {
      VG_(snprintf)(str + VG_(strlen)(str), size - VG_(strlen)(str),
                    "%s %d",
                    i > 0 ? "," : "",
                    (j == vc->vc[i].threadid) ? vc->vc[i].count : 0);
    }
  }
  VG_(snprintf)(str + VG_(strlen)(str), size - VG_(strlen)(str), " ]");
}

/**
 * Invariant test.
 */
void vc_check(const VectorClock* const vc)
{
  unsigned i;
  tl_assert(vc->size <= vc->capacity);
  for (i = 1; i < vc->size; i++)
  {
    tl_assert(vc->vc[i-1].threadid < vc->vc[i].threadid);
  }
}

/**
 * Change the size of the memory block pointed at by vc->vc.
 * Changes capacity, but does not change size. If the size of the memory
 * block is increased, the newly allocated memory is not initialized.
 */
static
void vc_reserve(VectorClock* const vc, const unsigned new_capacity)
{
  tl_assert(vc);
  if (new_capacity > vc->capacity)
  {
    if (vc->vc)
    {
      vc->vc = VG_(realloc)("drd.vc.vr.1",
                            vc->vc, new_capacity * sizeof(vc->vc[0]));
    }
    else if (new_capacity > 0)
    {
      vc->vc = VG_(malloc)("drd.vc.vr.2",
                           new_capacity * sizeof(vc->vc[0]));
    }
    else
    {
      tl_assert(vc->vc == 0 && new_capacity == 0);
    }
    vc->capacity = new_capacity;
  }
  tl_assert(new_capacity == 0 || vc->vc != 0);
}

/**
 * Unit test.
 */
void vc_test(void)
{
  VectorClock vc1;
  VCElem vc1elem[] = { { 3, 7 }, { 5, 8 }, };
  VectorClock vc2;
  VCElem vc2elem[] = { { 1, 4 }, { 3, 9 }, };
  VectorClock vc3;
  VCElem vc4elem[] = { { 1, 3 }, { 2, 1 }, };
  VectorClock vc4;
  VCElem vc5elem[] = { { 1, 4 }, };
  VectorClock vc5;

  vc_init(&vc1, vc1elem, sizeof(vc1elem)/sizeof(vc1elem[0]));
  vc_init(&vc2, vc2elem, sizeof(vc2elem)/sizeof(vc2elem[0]));
  vc_init(&vc3, 0, 0);
  vc_init(&vc4, vc4elem, sizeof(vc4elem)/sizeof(vc4elem[0]));
  vc_init(&vc5, vc5elem, sizeof(vc5elem)/sizeof(vc5elem[0]));

  vc_combine(&vc3, &vc1);
  vc_combine(&vc3, &vc2);

  VG_(printf)("vc1: ");
  vc_print(&vc1);
  VG_(printf)("\nvc2: ");
  vc_print(&vc2);
  VG_(printf)("\nvc3: ");
  vc_print(&vc3);
  VG_(printf)("\n");
  VG_(printf)("vc_lte(vc1, vc2) = %d, vc_lte(vc1, vc3) = %d, vc_lte(vc2, vc3) = %d, vc_lte(", vc_lte(&vc1, &vc2), vc_lte(&vc1, &vc3), vc_lte(&vc2, &vc3));
  vc_print(&vc4);
  VG_(printf)(", ");
  vc_print(&vc5);
  VG_(printf)(") = %d sw %d\n", vc_lte(&vc4, &vc5), vc_lte(&vc5, &vc4));
              
  vc_cleanup(&vc1);
  vc_cleanup(&vc2);
  vc_cleanup(&vc3);
}
