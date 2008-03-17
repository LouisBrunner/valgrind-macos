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


#include "drd_error.h"
#include "drd_malloc_wrappers.h"
#include "drd_mutex.h"            // struct mutex_info
#include "drd_suppression.h"      // drd_start_suppression()
#include "pub_drd_bitmap.h"       // LHS_W, ...
#include "pub_tool_vki.h"
#include "pub_tool_basics.h"
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // strlen()
#include "pub_tool_libcfile.h"    // VG_(get_startup_wd)()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"
#include "pub_tool_threadstate.h" // VG_(get_pthread_id)()
#include "pub_tool_tooliface.h"   // VG_(needs_tool_errors)()


typedef enum {
  ConflictingAccessSupp
} DRD_SuppKind;


/* Describe a data address range [a,a+len[ as good as possible, for error */
/* messages, putting the result in ai. */
static
void describe_malloced_addr(Addr const a, SizeT const len, AddrInfo* const ai)
{
  Addr data;

  if (drd_heap_addrinfo(a, &data, &ai->size, &ai->lastchange))
  {
    ai->akind = eMallocd;
    ai->rwoffset = a - data;
  }
  else
  {
    ai->akind = eUnknown;
  }
}

static
void drd_report_data_race2(Error* const err, const DataRaceErrInfo* const dri)
{
  AddrInfo ai;
  Char descr1[256];
  Char descr2[256];

  tl_assert(dri);
  tl_assert(dri->addr);
  tl_assert(dri->size > 0);

  descr1[0] = 0;
  descr2[0] = 0;
  VG_(get_data_description)(descr1, descr2, sizeof(descr1), dri->addr);
  if (descr1[0] == 0)
  {
    describe_malloced_addr(dri->addr, dri->size, &ai);
  }
  VG_(message)(Vg_UserMsg,
               "Conflicting %s by thread %d/%d at 0x%08lx size %ld",
               dri->access_type == eStore ? "store" : "load",
               DrdThreadIdToVgThreadId(dri->tid),
               dri->tid,
               dri->addr,
               dri->size);
  VG_(pp_ExeContext)(VG_(get_error_where)(err));
  if (descr1[0])
  {
    VG_(message)(Vg_UserMsg, "%s", descr1);
    VG_(message)(Vg_UserMsg, "%s", descr2);
  }
  else if (ai.akind == eMallocd && ai.lastchange)
  {
    VG_(message)(Vg_UserMsg,
                 "Address 0x%lx is at offset %ld from 0x%lx."
                 " Allocation context:",
                 dri->addr, ai.rwoffset, dri->addr - ai.rwoffset);
    VG_(pp_ExeContext)(ai.lastchange);
  }
  else
  {
    VG_(message)(Vg_UserMsg, "Allocation context: unknown.");
  }
  thread_report_conflicting_segments(dri->tid,
                                     dri->addr, dri->size, dri->access_type);
}

static Bool drd_tool_error_eq(VgRes res, Error* e1, Error* e2)
{
  return False;
}

static void drd_tool_error_pp(Error* const e)
{
  switch (VG_(get_error_kind)(e))
  {
  case DataRaceErr: {
    drd_report_data_race2(e, VG_(get_error_extra)(e));
    break;
  }
  case MutexErr: {
    MutexErrInfo* p = (MutexErrInfo*)(VG_(get_error_extra)(e));
    tl_assert(p);
    VG_(message)(Vg_UserMsg,
                 "%s: mutex 0x%lx, recursion count %d, owner %d.",
                 VG_(get_error_string)(e),
                 p->mutex,
                 p->recursion_count,
                 p->owner);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
  case CondErr: {
    CondErrInfo* cdei =(CondErrInfo*)(VG_(get_error_extra)(e));
    VG_(message)(Vg_UserMsg,
                 "%s: cond 0x%lx",
                 VG_(get_error_string)(e),
                 cdei->cond);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
  case CondRaceErr: {
    CondRaceErrInfo* cei = (CondRaceErrInfo*)(VG_(get_error_extra)(e));
    VG_(message)(Vg_UserMsg,
                 "Race condition: condition variable 0x%lx has been"
                 " signalled but the associated mutex 0x%lx is not locked"
                 " by the signalling thread",
                 cei->cond, cei->mutex);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
  case CondDestrErr: {
    CondDestrErrInfo* cdi = (CondDestrErrInfo*)(VG_(get_error_extra)(e));
    VG_(message)(Vg_UserMsg,
                 "%s: cond 0x%lx, mutex 0x%lx locked by thread %d/%d",
                 VG_(get_error_string)(e),
                 cdi->cond, cdi->mutex,
                 DrdThreadIdToVgThreadId(cdi->tid), cdi->tid);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
  case SemaphoreErr: {
    SemaphoreErrInfo* sei =(SemaphoreErrInfo*)(VG_(get_error_extra)(e));
    tl_assert(sei);
    VG_(message)(Vg_UserMsg,
                 "%s: semaphore 0x%lx",
                 VG_(get_error_string)(e),
                 sei->semaphore);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
  case BarrierErr: {
    BarrierErrInfo* sei =(BarrierErrInfo*)(VG_(get_error_extra)(e));
    tl_assert(sei);
    VG_(message)(Vg_UserMsg,
                 "%s: barrier 0x%lx",
                 VG_(get_error_string)(e),
                 sei->barrier);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
  case RwlockErr: {
    RwlockErrInfo* p = (RwlockErrInfo*)(VG_(get_error_extra)(e));
    tl_assert(p);
    VG_(message)(Vg_UserMsg,
                 "%s: rwlock 0x%lx.",
                 VG_(get_error_string)(e),
                 p->rwlock);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
  case GenericErr: {
    //GenericErrInfo* gei =(GenericErrInfo*)(VG_(get_error_extra)(e));
    VG_(message)(Vg_UserMsg, "%s", VG_(get_error_string)(e));
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
  default:
    VG_(message)(Vg_UserMsg,
                 "%s",
                 VG_(get_error_string)(e));
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    break;
  }
}

static UInt drd_tool_error_update_extra(Error* e)
{
  switch (VG_(get_error_kind)(e))
  {
  case DataRaceErr:
    return sizeof(DataRaceErrInfo);
  case MutexErr:
    return sizeof(MutexErrInfo);
  case CondErr:
    return sizeof(CondErrInfo);
  case CondRaceErr:
    return sizeof(CondRaceErrInfo);
  case CondDestrErr:
    return sizeof(CondDestrErrInfo);
  case SemaphoreErr:
    return sizeof(SemaphoreErrInfo);
  case BarrierErr:
    return sizeof(BarrierErrInfo);
  case RwlockErr:
    return sizeof(RwlockErrInfo);
  case GenericErr:
    return sizeof(GenericErrInfo);
  default:
    tl_assert(False);
    break;
  }
}

static Bool drd_tool_error_recog(Char* const name, Supp* const supp)
{
  SuppKind skind;

  if (VG_(strcmp)(name, "ConflictingAccess") == 0)
    skind = ConflictingAccessSupp;
  else
    return False;

  VG_(set_supp_kind)(supp, skind);
  return True;
}

static Bool drd_tool_error_read_extra(Int fd, Char* buf, Int nBuf, Supp* supp)
{
  return True;
}

static Bool drd_tool_error_matches(Error* const e, Supp* const supp)
{
  switch (VG_(get_supp_kind)(supp))
  {
  }
  return True;
}

static Char* drd_tool_error_name(Error* e)
{
  switch (VG_(get_error_kind)(e))
  {
  case DataRaceErr:  return "ConflictingAccess";
  case MutexErr:     return "MutexErr";
  case CondErr:      return "CondErr";
  case CondRaceErr:  return "CondRaceErr";
  case CondDestrErr: return "CondDestrErr";
  case SemaphoreErr: return "SemaphoreErr";
  case BarrierErr:   return "BarrierErr";
  case RwlockErr:    return "RwlockErr";
  case GenericErr:   return "GenericErr";
  default:
    tl_assert(0);
  }
  return 0;
}

static void drd_tool_error_print_extra(Error* e)
{
  switch (VG_(get_error_kind)(e))
  {
    // VG_(printf)("   %s\n", VG_(get_error_string)(err));
  }
}

void drd_register_error_handlers(void)
{
  // Tool error reporting.
  VG_(needs_tool_errors)(drd_tool_error_eq,
                         drd_tool_error_pp,
                         True,
                         drd_tool_error_update_extra,
                         drd_tool_error_recog,
                         drd_tool_error_read_extra,
                         drd_tool_error_matches,
                         drd_tool_error_name,
                         drd_tool_error_print_extra);
}
