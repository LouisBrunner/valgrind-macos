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


#include "drd_suppression.h"
#include "pub_drd_bitmap.h"
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_stacktrace.h"  // VG_(get_and_pp_StackTrace)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()
#include "pub_tool_libcprint.h"   // Vg_DebugMsg


// Local variables.

static struct bitmap* s_suppressed;
static Bool s_trace_suppression;
Bool g_any_address_traced = False;


// Function definitions.

void suppression_set_trace(const Bool trace_suppression)
{
   s_trace_suppression = trace_suppression;
}

void drd_suppression_init(void)
{
  tl_assert(s_suppressed == 0);
  s_suppressed = bm_new();
  tl_assert(s_suppressed);
}

void drd_start_suppression(const Addr a1, const Addr a2,
                           const char* const reason)
{
  if (s_trace_suppression)
  {
    VG_(message)(Vg_DebugMsg, "start suppression of 0x%lx sz %ld (%s)",
                 a1, a2 - a1, reason);
  }

  tl_assert(a1 < a2);
  // tl_assert(! drd_is_any_suppressed(a1, a2));
  bm_access_range_store(s_suppressed, a1, a2);
}

void drd_finish_suppression(const Addr a1, const Addr a2)
{
  if (s_trace_suppression)
  {
    VG_(message)(Vg_DebugMsg, "finish suppression of 0x%lx sz %ld",
                 a1, a2 - a1);
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), 12);   
  }

  tl_assert(a1 < a2);
  if (! drd_is_suppressed(a1, a2))
  {
     VG_(message)(Vg_DebugMsg, "?? [0x%lx,0x%lx[ not suppressed ??", a1, a2);
     VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), 12);
     tl_assert(False);
  }
  bm_clear_store(s_suppressed, a1, a2);
}

/**
 * Return true if data race detection suppression has been requested for all
 * bytes in the range a1 .. a2 - 1 inclusive. Return false in case the range
 * is only partially suppressed or not suppressed at all.
 */
Bool drd_is_suppressed(const Addr a1, const Addr a2)
{
  return bm_has(s_suppressed, a1, a2, eStore);
}

/**
 * Return true if data race detection suppression has been requested for any
 * of the bytes in the range a1 .. a2 - 1 inclusive. Return false in case none
 * of the bytes in the specified range is suppressed.
 */
Bool drd_is_any_suppressed(const Addr a1, const Addr a2)
{
  return bm_has_any_store(s_suppressed, a1, a2);
}

void drd_start_tracing_address_range(const Addr a1, const Addr a2)
{
  tl_assert(a1 < a2);

  bm_access_range_load(s_suppressed, a1, a2);
  if (! g_any_address_traced)
  {
    g_any_address_traced = True;
  }
}

void drd_stop_tracing_address_range(const Addr a1, const Addr a2)
{
  tl_assert(a1 < a2);

  bm_clear_load(s_suppressed, a1, a2);
  if (g_any_address_traced)
  {
    g_any_address_traced = bm_has_any_load(s_suppressed, 0, ~(Addr)0);
  }
}

Bool drd_is_any_traced(const Addr a1, const Addr a2)
{
  return bm_has_any_load(s_suppressed, a1, a2);
}

void drd_suppression_stop_using_mem(const Addr a1, const Addr a2)
{
  if (s_trace_suppression)
  {
    Addr b;
    for (b = a1; b < a2; b++)
    {
      if (bm_has_1(s_suppressed, b, eStore))
      {
        VG_(message)(Vg_DebugMsg,
                     "stop_using_mem(0x%lx, %ld) finish suppression of 0x%lx",
                     a1, a2 - a1, b);
      }
    }
  }
  tl_assert(a1);
  tl_assert(a1 < a2);
  bm_clear(s_suppressed, a1, a2);
}
