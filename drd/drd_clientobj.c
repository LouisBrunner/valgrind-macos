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


#include "drd_clientobj.h"
#include "drd_suppression.h"
#include "pub_tool_basics.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcprint.h"   // VG_(message)()
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"     // VG_(clo_backtrace_size)
#include "pub_tool_oset.h"
#include "pub_tool_stacktrace.h"
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


// Local variables.

static OSet* s_clientobj;
static Bool s_trace_clientobj;


// Function definitions.

void clientobj_set_trace(const Bool trace)
{
  s_trace_clientobj = trace;
}

/** Initialize the client object set. */
void clientobj_init(void)
{
  tl_assert(s_clientobj == 0);
  s_clientobj = VG_(OSetGen_Create)(0, 0, VG_(malloc), "drd.clientobj.ci.1",
                                          VG_(free));
  tl_assert(s_clientobj);
}

/** Free the memory allocated for the client object set.
 *  @pre Client object set is empty.
 */
void clientobj_cleanup(void)
{
  tl_assert(s_clientobj);
  tl_assert(VG_(OSetGen_Size)(s_clientobj) == 0);
  VG_(OSetGen_Destroy)(s_clientobj);
  s_clientobj = 0;
}

/** Return the data associated with the client object at client address addr.
 *  Return 0 if there is no client object in the set with the specified start
 *  address.
 */
DrdClientobj* clientobj_get_any(const Addr addr)
{
  return VG_(OSetGen_Lookup)(s_clientobj, &addr);
}

/** Return the data associated with the client object at client address addr
 *  and that has object type t. Return 0 if there is no client object in the
 *  set with the specified start address.
 */
DrdClientobj* clientobj_get(const Addr addr, const ObjType t)
{
  DrdClientobj* p;
  p = VG_(OSetGen_Lookup)(s_clientobj, &addr);
  if (p && p->any.type == t)
    return p;
  return 0;
}

/** Return true if and only if the address range of any client object overlaps
 *  with the specified address range.
 */
Bool clientobj_present(const Addr a1, const Addr a2)
{
  DrdClientobj *p;

  tl_assert(a1 < a2);
  VG_(OSetGen_ResetIter)(s_clientobj);
  for ( ; (p = VG_(OSetGen_Next)(s_clientobj)) != 0; )
  {
    if (a1 <= p->any.a1 && p->any.a1 < a2)
    {
      return True;  
    }
  }
  return False;
}

/** Add state information for the client object at client address addr and
 *  of type t. Suppress data race reports on the address range [addr,addr+size[.
 *  @pre No other client object is present in the address range [addr,addr+size[.
 */
DrdClientobj*
clientobj_add(const Addr a1, const ObjType t)
{
  DrdClientobj* p;

  tl_assert(! clientobj_present(a1, a1 + 1));
  tl_assert(VG_(OSetGen_Lookup)(s_clientobj, &a1) == 0);

  if (s_trace_clientobj)
  {
    VG_(message)(Vg_UserMsg, "Adding client object 0x%lx of type %d", a1, t);
  }

  p = VG_(OSetGen_AllocNode)(s_clientobj, sizeof(*p));
  VG_(memset)(p, 0, sizeof(*p));
  p->any.a1   = a1;
  p->any.type = t;
  p->any.first_observed_at = VG_(record_ExeContext)(VG_(get_running_tid)(), 0);
  VG_(OSetGen_Insert)(s_clientobj, p);
  tl_assert(VG_(OSetGen_Lookup)(s_clientobj, &a1) == p);
  drd_start_suppression(a1, a1 + 1, "clientobj");
  return p;
}

Bool clientobj_remove(const Addr addr, const ObjType t)
{
  DrdClientobj* p;

  if (s_trace_clientobj)
  {
    VG_(message)(Vg_UserMsg, "Removing client object 0x%lx of type %d",
                 addr, t);
#if 0
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(),
                               VG_(clo_backtrace_size));
#endif
  }

  p = VG_(OSetGen_Lookup)(s_clientobj, &addr);
  tl_assert(p->any.type == t);
  p = VG_(OSetGen_Remove)(s_clientobj, &addr);
  if (p)
  {
    tl_assert(VG_(OSetGen_Lookup)(s_clientobj, &addr) == 0);
    tl_assert(p->any.cleanup);
    (*p->any.cleanup)(p);
    VG_(OSetGen_FreeNode)(s_clientobj, p);
    return True;
  }
  return False;
}

void clientobj_stop_using_mem(const Addr a1, const Addr a2)
{
  Addr removed_at;
  DrdClientobj* p;

  tl_assert(s_clientobj);

  if (! drd_is_any_suppressed(a1, a2))
    return;

  VG_(OSetGen_ResetIter)(s_clientobj);
  p = VG_(OSetGen_Next)(s_clientobj);
  for ( ; p != 0; )
  {
    if (a1 <= p->any.a1 && p->any.a1 < a2)
    {
      removed_at = p->any.a1;
      clientobj_remove(p->any.a1, p->any.type);
      /* The above call removes an element from the oset and hence */
      /* invalidates the iterator. Set the iterator back.          */
      VG_(OSetGen_ResetIter)(s_clientobj);
      while ((p = VG_(OSetGen_Next)(s_clientobj)) != 0
             && p->any.a1 <= removed_at)
      { }
    }
    else
    {
      p = VG_(OSetGen_Next)(s_clientobj);
    }
  }
}

void clientobj_resetiter(void)
{
  VG_(OSetGen_ResetIter)(s_clientobj);
}

DrdClientobj* clientobj_next(const ObjType t)
{
  DrdClientobj* p;
  while ((p = VG_(OSetGen_Next)(s_clientobj)) != 0 && p->any.type != t)
    ;
  return p;
}

const char* clientobj_type_name(const ObjType t)
{
  switch (t)
  {
  case ClientMutex:     return "mutex";
  case ClientCondvar:   return "cond";
  case ClientSemaphore: return "semaphore";
  case ClientBarrier:   return "barrier";
  case ClientRwlock:    return "rwlock";
  }
  return "(unknown)";
}
