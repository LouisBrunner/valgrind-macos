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
#include "pub_tool_libcprint.h"  // VG_(message)()
#include "pub_tool_mallocfree.h"
#include "pub_tool_oset.h"


// Local variables.

static OSet* s_clientobj;


// Function definitions.

/** Initialize the client object set. */
void drd_clientobj_init(void)
{
  tl_assert(s_clientobj == 0);
  s_clientobj = VG_(OSetGen_Create)(0, 0, VG_(malloc), VG_(free));
  tl_assert(s_clientobj);
}

/** Free the memory allocated for the client object set.
 *  @pre Client object set is empty.
 */
void drd_clientobj_cleanup(void)
{
  tl_assert(s_clientobj);
  tl_assert(VG_(OSetGen_Size)(s_clientobj) == 0);
  VG_(OSetGen_Destroy)(s_clientobj);
  s_clientobj = 0;
}

/** Return the data associated with the client object at client address addr
 *  and that has object type t. Return 0 if there is no client object in the
 *  set with the specified start address.
 */
DrdClientobj* drd_clientobj_get(const Addr addr, const ObjType t)
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
Bool drd_clientobj_present(const Addr a1, const Addr a2)
{
  DrdClientobj *p;

  tl_assert(a1 < a2);
  VG_(OSetGen_ResetIter)(s_clientobj);
  for ( ; (p = VG_(OSetGen_Next)(s_clientobj)) != 0; )
  {
    if ((a1 <= p->any.a1 && p->any.a1 < a2)
        || (a1 < p->any.a2 && p->any.a2 <= a2))
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
drd_clientobj_add(const Addr a1, const Addr a2, const ObjType t)
{
  DrdClientobj* p;

  tl_assert(a1 < a2 && a1 + 4096 > a2);
  tl_assert(! drd_clientobj_present(a1, a2));
  tl_assert(VG_(OSetGen_Lookup)(s_clientobj, &a1) == 0);
#if 0
    VG_(message)(Vg_DebugMsg,
                 "registering client obj [0x%lx,0x%lx[ of type %d",
                 a1, a2, t);
#endif
  p = VG_(OSetGen_AllocNode)(s_clientobj, sizeof(*p));
  VG_(memset)(p, 0, sizeof(*p));
  p->any.a1   = a1;
  p->any.a2   = a2;
  p->any.type = t;
  VG_(OSetGen_Insert)(s_clientobj, p);
  tl_assert(VG_(OSetGen_Lookup)(s_clientobj, &a1) == p);
  drd_start_suppression(p->any.a1, p->any.a2, "client object");
  return p;
}

Bool drd_clientobj_remove(const Addr addr, const ObjType t)
{
  DrdClientobj* p;

  p = VG_(OSetGen_Lookup)(s_clientobj, &addr);
  tl_assert(p->any.type == t);
  p = VG_(OSetGen_Remove)(s_clientobj, &addr);
  if (p)
  {
#if 0
    VG_(message)(Vg_DebugMsg, "removing client obj [0x%lx,0x%lx[ of type %d",
                 p->any.a1, p->any.a2, p->any.type);
#endif
    tl_assert(VG_(OSetGen_Lookup)(s_clientobj, &addr) == 0);
    drd_finish_suppression(p->any.a1, p->any.a2);
    tl_assert(p->any.cleanup);
    (*p->any.cleanup)(p);
    VG_(OSetGen_FreeNode)(s_clientobj, p);
    return True;
  }
  return False;
}

void drd_clientobj_stop_using_mem(const Addr a1, const Addr a2)
{
  Addr removed_at;
  DrdClientobj* p;

#if 0
    VG_(message)(Vg_DebugMsg, "drd_clientobj_stop_using_mem [0x%lx,0x%lx[",
                 a1, a2);
#endif
  tl_assert(s_clientobj);
  VG_(OSetGen_ResetIter)(s_clientobj);
  p = VG_(OSetGen_Next)(s_clientobj);
  for ( ; p != 0; )
  {
    if ((a1 <= p->any.a1 && p->any.a1 < a2)
        || (a1 < p->any.a2 && p->any.a2 <= a2))
    {
#if 0
      VG_(message)(Vg_DebugMsg, "drd_clientobj_stop_using_mem [0x%lx,0x%lx[",
                   a1, a2);
#endif
      removed_at = p->any.a1;
      drd_clientobj_remove(p->any.a1, p->any.type);
      /* The above call removes an element from the oset and hence invalidates */
      /* the iterator. Set the iterator back.                                  */
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

void drd_clientobj_resetiter(void)
{
  VG_(OSetGen_ResetIter)(s_clientobj);
}

DrdClientobj* drd_clientobj_next(const ObjType t)
{
  DrdClientobj* p;
  while ((p = VG_(OSetGen_Next)(s_clientobj)) != 0 && p->any.type != t)
    ;
  return p;
}

