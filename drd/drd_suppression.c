/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2020 Bart Van Assche <bvanassche@acm.org>.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, see <http://www.gnu.org/licenses/>.

  The GNU General Public License is contained in the file COPYING.
*/


#include "drd_suppression.h"
#include "pub_drd_bitmap.h"
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_stacktrace.h"  // VG_(get_and_pp_StackTrace)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()
#include "pub_tool_libcprint.h"   // Vg_DebugMsg


/* Global variables. */

Bool DRD_(g_any_address_traced) = False;


/* Local variables. */

static struct bitmap* s_suppressed;
static struct bitmap* s_traced;
static Bool s_trace_suppression;


/* Function definitions. */

void DRD_(suppression_set_trace)(const Bool trace_suppression)
{
   s_trace_suppression = trace_suppression;
}

void DRD_(suppression_init)(void)
{
   tl_assert(s_suppressed == 0);
   tl_assert(s_traced     == 0);
   s_suppressed = DRD_(bm_new)();
   s_traced     = DRD_(bm_new)();
   tl_assert(s_suppressed);
   tl_assert(s_traced);
}

void DRD_(start_suppression)(const Addr a1, const Addr a2,
                             const HChar* const reason)
{
   if (s_trace_suppression)
      VG_(message)(Vg_DebugMsg, "start suppression of 0x%lx sz %lu (%s)\n",
                   a1, a2 - a1, reason);

   tl_assert(a1 <= a2);
   DRD_(bm_access_range_store)(s_suppressed, a1, a2);
}

void DRD_(finish_suppression)(const Addr a1, const Addr a2)
{
   if (s_trace_suppression) {
      VG_(message)(Vg_DebugMsg, "finish suppression of 0x%lx sz %lu\n",
                   a1, a2 - a1);
      VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), 12);
   }

   tl_assert(a1 <= a2);
   DRD_(bm_clear_store)(s_suppressed, a1, a2);
}

/**
 * Return true if data race detection suppression has been requested for all
 * bytes in the range a1 .. a2 - 1 inclusive. Return false in case the range
 * is only partially suppressed or not suppressed at all.
 */
Bool DRD_(is_suppressed)(const Addr a1, const Addr a2)
{
   return DRD_(bm_has)(s_suppressed, a1, a2, eStore);
}

/**
 * Return true if data race detection suppression has been requested for any
 * of the bytes in the range a1 .. a2 - 1 inclusive. Return false in case none
 * of the bytes in the specified range is suppressed.
 */
Bool DRD_(is_any_suppressed)(const Addr a1, const Addr a2)
{
   return DRD_(bm_has_any_store)(s_suppressed, a1, a2);
}

void DRD_(mark_hbvar)(const Addr a1)
{
   DRD_(bm_access_range_load)(s_suppressed, a1, a1 + 1);
}

Bool DRD_(range_contains_suppression_or_hbvar)(const Addr a1, const Addr a2)
{
   return DRD_(bm_has_any_access)(s_suppressed, a1, a2);
}

/**
 * Start tracing memory accesses in the range [a1,a2). If persistent == True,
 * keep tracing even after memory deallocation and reallocation.
 */
void DRD_(start_tracing_address_range)(const Addr a1, const Addr a2,
                                       const Bool persistent)
{
   tl_assert(a1 <= a2);

   if (s_trace_suppression)
      VG_(message)(Vg_DebugMsg, "start_tracing(0x%lx, %lu) %s\n",
                   a1, a2 - a1, persistent ? "persistent" : "non-persistent");

   DRD_(bm_access_range_load)(s_traced, a1, a2);
   if (persistent)
      DRD_(bm_access_range_store)(s_traced, a1, a2);
   if (!DRD_(g_any_address_traced) && a1 < a2)
      DRD_(g_any_address_traced) = True;
}

/**
 * Stop tracing memory accesses in the range [a1,a2).
 */
void DRD_(stop_tracing_address_range)(const Addr a1, const Addr a2)
{
   tl_assert(a1 <= a2);

   if (s_trace_suppression)
      VG_(message)(Vg_DebugMsg, "stop_tracing(0x%lx, %lu)\n",
                   a1, a2 - a1);

   if (DRD_(g_any_address_traced)) {
      DRD_(bm_clear)(s_traced, a1, a2);
      DRD_(g_any_address_traced) = DRD_(bm_has_any_load_g)(s_traced);
   }
}

Bool DRD_(is_any_traced)(const Addr a1, const Addr a2)
{
   return DRD_(bm_has_any_access)(s_traced, a1, a2);
}

/**
 * Stop using the memory range [a1,a2). Stop tracing memory accesses to
 * non-persistent address ranges.
 */
void DRD_(suppression_stop_using_mem)(const Addr a1, const Addr a2)
{
   if (s_trace_suppression) {
      Addr b;
      for (b = a1; b < a2; b++) {
         if (DRD_(bm_has_1)(s_suppressed, b, eStore)) {
            VG_(message)(Vg_DebugMsg,
                         "stop_using_mem(0x%lx, %lu) finish suppression of"
                         " 0x%lx\n", a1, a2 - a1, b);
         }
      }
   }
   tl_assert(a1);
   tl_assert(a1 <= a2);
   DRD_(bm_clear)(s_suppressed, a1, a2);
   DRD_(bm_clear_load)(s_traced, a1, a2);
}
