/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2012 Bart Van Assche <bvanassche@acm.org>.

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
#include "pub_tool_libcbase.h"    // VG_(memcpy)
#include "pub_tool_libcprint.h"   // VG_(printf)
#include "pub_tool_mallocfree.h"  // VG_(malloc), VG_(free)


/* Local function declarations. */

static
void DRD_(vc_reserve)(VectorClock* const vc, const unsigned new_capacity);


/* Function definitions. */

/**
 * Initialize the memory 'vc' points at as a vector clock with size 'size'.
 * If the pointer 'vcelem' is not null, it is assumed to be an array with
 * 'size' elements and it becomes the initial value of the vector clock.
 */
void DRD_(vc_init)(VectorClock* const vc,
                   const VCElem* const vcelem,
                   const unsigned size)
{
   tl_assert(vc);
   vc->size = 0;
   vc->capacity = 0;
   vc->vc = 0;
   DRD_(vc_reserve)(vc, size);
   tl_assert(size == 0 || vc->vc != 0);
   if (vcelem)
   {
      VG_(memcpy)(vc->vc, vcelem, size * sizeof(vcelem[0]));
      vc->size = size;
   }
}

/** Reset vc to the empty vector clock. */
void DRD_(vc_cleanup)(VectorClock* const vc)
{
   DRD_(vc_reserve)(vc, 0);
}

/** Copy constructor -- initializes *new. */
void DRD_(vc_copy)(VectorClock* const new, const VectorClock* const rhs)
{
   DRD_(vc_init)(new, rhs->vc, rhs->size);
}

/** Assignment operator -- *lhs is already a valid vector clock. */
void DRD_(vc_assign)(VectorClock* const lhs, const VectorClock* const rhs)
{
   DRD_(vc_cleanup)(lhs);
   DRD_(vc_copy)(lhs, rhs);
}

/** Increment the clock of thread 'tid' in vector clock 'vc'. */
void DRD_(vc_increment)(VectorClock* const vc, DrdThreadId const tid)
{
   unsigned i;
   for (i = 0; i < vc->size; i++)
   {
      if (vc->vc[i].threadid == tid)
      {
         typeof(vc->vc[i].count) const oldcount = vc->vc[i].count;
         vc->vc[i].count++;
         // Check for integer overflow.
         tl_assert(oldcount < vc->vc[i].count);
         return;
      }
   }

   /*
    * The specified thread ID does not yet exist in the vector clock
    * -- insert it.
    */
   {
      const VCElem vcelem = { tid, 1 };
      VectorClock vc2;
      DRD_(vc_init)(&vc2, &vcelem, 1);
      DRD_(vc_combine)(vc, &vc2);
      DRD_(vc_cleanup)(&vc2);
   }
}

/**
 * @return True if vector clocks vc1 and vc2 are ordered, and false otherwise.
 * Order is as imposed by thread synchronization actions ("happens before").
 */
Bool DRD_(vc_ordered)(const VectorClock* const vc1,
                      const VectorClock* const vc2)
{
   return DRD_(vc_lte)(vc1, vc2) || DRD_(vc_lte)(vc2, vc1);
}

/** Compute elementwise minimum. */
void DRD_(vc_min)(VectorClock* const result, const VectorClock* const rhs)
{
   unsigned i;
   unsigned j;

   tl_assert(result);
   tl_assert(rhs);

   DRD_(vc_check)(result);

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
         /* The thread ID is present in both vector clocks. Compute the */
         /* minimum of vc[i].count and vc[j].count. */
         tl_assert(result->vc[i].threadid == rhs->vc[j].threadid);
         if (rhs->vc[j].count < result->vc[i].count)
         {
            result->vc[i].count = rhs->vc[j].count;
         }
      }
   }
   DRD_(vc_check)(result);
}

/**
 * Compute elementwise maximum.
 */
void DRD_(vc_combine)(VectorClock* const result, const VectorClock* const rhs)
{
   unsigned i;
   unsigned j;
   unsigned shared;
   unsigned new_size;

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

   DRD_(vc_check)(result);

   new_size = result->size + rhs->size - shared;
   if (new_size > result->capacity)
      DRD_(vc_reserve)(result, new_size);

   DRD_(vc_check)(result);

   // Next, combine both vector clocks into one.
   i = 0;
   for (j = 0; j < rhs->size; j++)
   {
      /* First of all, skip those clocks in result->vc[] for which there */
      /* is no corresponding clock in rhs->vc[].                         */
      while (i < result->size && result->vc[i].threadid < rhs->vc[j].threadid)
      {
         i++;
      }
      /* If the end of *result is met, append rhs->vc[j] to *result. */
      if (i >= result->size)
      {
         result->size++;
         result->vc[i] = rhs->vc[j];
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
      }
      /* Otherwise, both *result and *rhs have a clock for thread            */
      /* result->vc[i].threadid == rhs->vc[j].threadid. Compute the maximum. */
      else
      {
         tl_assert(result->vc[i].threadid == rhs->vc[j].threadid);
         if (rhs->vc[j].count > result->vc[i].count)
         {
            result->vc[i].count = rhs->vc[j].count;
         }
      }
   }
   DRD_(vc_check)(result);
   tl_assert(result->size == new_size);
}

/** Print the contents of vector clock 'vc'. */
void DRD_(vc_print)(const VectorClock* const vc)
{
   char* str;

   if ((str = DRD_(vc_aprint)(vc)) != NULL)
   {
      VG_(printf)("%s", str);
      VG_(free)(str);
   }
}

/**
 * Print the contents of vector clock 'vc' to a newly allocated string.
 * The caller must call VG_(free)() on the return value of this function.
 */
char* DRD_(vc_aprint)(const VectorClock* const vc)
{
   unsigned i;
   unsigned reserved;
   unsigned size;
   char* str = 0;

   tl_assert(vc);
   reserved = 64;
   size = 0;
   str = VG_(realloc)("drd.vc.aprint.1", str, reserved);
   if (! str)
      return str;
   size += VG_(snprintf)(str, reserved, "[");
   for (i = 0; i < vc->size; i++)
   {
      tl_assert(vc->vc);
      if (VG_(strlen)(str) + 32 > reserved)
      {
         reserved *= 2;
         str = VG_(realloc)("drd.vc.aprint.2", str, reserved);
         if (! str)
            return str;
      }
      size += VG_(snprintf)(str + size, reserved - size,
                            "%s %d: %d", i > 0 ? "," : "",
                            vc->vc[i].threadid, vc->vc[i].count);
   }
   size += VG_(snprintf)(str + size, reserved - size, " ]");

   return str;
}

/**
 * Invariant test.
 *
 * The function below tests whether the following two conditions are
 * satisfied:
 * - size <= capacity.
 * - Vector clock elements are stored in thread ID order.
 *
 * If one of these conditions is not met, an assertion failure is triggered.
 */
void DRD_(vc_check)(const VectorClock* const vc)
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
void DRD_(vc_reserve)(VectorClock* const vc, const unsigned new_capacity)
{
   tl_assert(vc);
   tl_assert(vc->capacity > VC_PREALLOCATED
             || vc->vc == 0
             || vc->vc == vc->preallocated);

   if (new_capacity > vc->capacity)
   {
      if (vc->vc && vc->capacity > VC_PREALLOCATED)
      {
         tl_assert(vc->vc
                   && vc->vc != vc->preallocated
                   && vc->capacity > VC_PREALLOCATED);
         vc->vc = VG_(realloc)("drd.vc.vr.1",
                               vc->vc, new_capacity * sizeof(vc->vc[0]));
      }
      else if (vc->vc && new_capacity > VC_PREALLOCATED)
      {
         tl_assert((vc->vc == 0 || vc->vc == vc->preallocated)
                   && new_capacity > VC_PREALLOCATED
                   && vc->capacity <= VC_PREALLOCATED);
         vc->vc = VG_(malloc)("drd.vc.vr.2",
                              new_capacity * sizeof(vc->vc[0]));
         VG_(memcpy)(vc->vc, vc->preallocated,
                     vc->capacity * sizeof(vc->vc[0]));
      }
      else if (vc->vc)
      {
         tl_assert(vc->vc == vc->preallocated
                   && new_capacity <= VC_PREALLOCATED
                   && vc->capacity <= VC_PREALLOCATED);
      }
      else if (new_capacity > VC_PREALLOCATED)
      {
         tl_assert(vc->vc == 0
                   && new_capacity > VC_PREALLOCATED
                   && vc->capacity == 0);
         vc->vc = VG_(malloc)("drd.vc.vr.3",
                              new_capacity * sizeof(vc->vc[0]));
      }
      else
      {
         tl_assert(vc->vc == 0
                   && new_capacity <= VC_PREALLOCATED
                   && vc->capacity == 0);
         vc->vc = vc->preallocated;
      }
      vc->capacity = new_capacity;
   }
   else if (new_capacity == 0 && vc->vc)
   {
      if (vc->capacity > VC_PREALLOCATED)
         VG_(free)(vc->vc);
      vc->vc = 0;
      vc->capacity = 0;
   }

   tl_assert(new_capacity == 0 || vc->vc != 0);
   tl_assert(vc->capacity > VC_PREALLOCATED
             || vc->vc == 0
             || vc->vc == vc->preallocated);
}
