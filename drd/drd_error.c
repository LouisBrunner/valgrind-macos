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


#include "drd_clientobj.h"        /* struct mutex_info        */
#include "drd_error.h"
#include "drd_malloc_wrappers.h"
#include "drd_mutex.h"
#include "drd_suppression.h"      /* drd_start_suppression()  */
#include "pub_drd_bitmap.h"       /* LHS_W, ...               */
#include "pub_tool_vki.h"
#include "pub_tool_basics.h"
#include "pub_tool_libcassert.h"  /* tl_assert()              */
#include "pub_tool_libcbase.h"    /* strlen()                 */
#include "pub_tool_libcfile.h"    /* VG_(get_startup_wd)()    */
#include "pub_tool_libcprint.h"   /* VG_(printf)()            */
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"  /* VG_(malloc), VG_(free)   */
#include "pub_tool_threadstate.h" /* VG_(get_pthread_id)()    */
#include "pub_tool_tooliface.h"   /* VG_(needs_tool_errors)() */


/* Local variables. */

static Bool s_drd_show_conflicting_segments = True;


void set_show_conflicting_segments(const Bool scs)
{
  s_drd_show_conflicting_segments = scs;
}

/** Describe a data address range [a,a+len[ as good as possible, for error
 *  messages, putting the result in ai.
 */
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

/** Report where an object has been observed for the first time. The printed
 *  call stack will either refer to a pthread_*_init() or a pthread_*lock()
 *  call.
 */
static void first_observed(const Addr obj)
{
  DrdClientobj* cl;

  cl = clientobj_get_any(obj);
  if (cl)
  {
    tl_assert(cl->any.first_observed_at);
    VG_(message)(Vg_UserMsg,
                 "%s 0x%lx was first observed at:",
                 clientobj_type_name(cl->any.type),
                 obj);
    VG_(pp_ExeContext)(cl->any.first_observed_at);
  }
}

static
void drd_report_data_race2(Error* const err, const DataRaceErrInfo* const dri)
{
  AddrInfo ai;
  const unsigned descr_size = 256;
  Char* descr1 = VG_(malloc)("drd.error.drdr2.1", descr_size);
  Char* descr2 = VG_(malloc)("drd.error.drdr2.2", descr_size);

  tl_assert(dri);
  tl_assert(dri->addr);
  tl_assert(dri->size > 0);
  tl_assert(descr1);
  tl_assert(descr2);

  descr1[0] = 0;
  descr2[0] = 0;
  VG_(get_data_description)(descr1, descr2, descr_size, dri->addr);
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
    char sect_name[64];
    VgSectKind sect_kind;

    sect_kind = VG_(seginfo_sect_kind)(sect_name, sizeof(sect_name), dri->addr);
    if (sect_kind != Vg_SectUnknown)
    {
      VG_(message)(Vg_UserMsg,
                   "Allocation context: %s section of %s",
                   VG_(pp_SectKind)(sect_kind),
                   sect_name);
    }
    else
    {
      VG_(message)(Vg_UserMsg, "Allocation context: unknown.");
    }
  }
  if (s_drd_show_conflicting_segments)
  {
    thread_report_conflicting_segments(dri->tid,
                                       dri->addr, dri->size, dri->access_type);
  }

  VG_(free)(descr2);
  VG_(free)(descr1);
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
    if (p->recursion_count >= 0)
    {
      VG_(message)(Vg_UserMsg,
                   "%s: mutex 0x%lx, recursion count %d, owner %d.",
                   VG_(get_error_string)(e),
                   p->mutex,
                   p->recursion_count,
                   p->owner);
    }
    else
    {
      VG_(message)(Vg_UserMsg,
                   "The object at address 0x%lx is not a mutex.",
                   p->mutex);
    }
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    first_observed(p->mutex);
    break;
  }
  case CondErr: {
    CondErrInfo* cdei =(CondErrInfo*)(VG_(get_error_extra)(e));
    VG_(message)(Vg_UserMsg,
                 "%s: cond 0x%lx",
                 VG_(get_error_string)(e),
                 cdei->cond);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    first_observed(cdei->cond);
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
    first_observed(cdi->mutex);
    break;
  }
  case CondRaceErr: {
    CondRaceErrInfo* cei = (CondRaceErrInfo*)(VG_(get_error_extra)(e));
    VG_(message)(Vg_UserMsg,
                 "Probably a race condition: condition variable 0x%lx has been"
                 " signaled but the associated mutex 0x%lx is not locked"
                 " by the signalling thread.",
                 cei->cond, cei->mutex);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    first_observed(cei->cond);
    first_observed(cei->mutex);
    break;
  }
  case CondWaitErr: {
    CondWaitErrInfo* cwei = (CondWaitErrInfo*)(VG_(get_error_extra)(e));
    VG_(message)(Vg_UserMsg,
                 "%s: condition variable 0x%lx, mutexes 0x%lx and 0x%lx",
                 VG_(get_error_string)(e),
                 cwei->cond,
                 cwei->mutex1,
                 cwei->mutex2);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    first_observed(cwei->cond);
    first_observed(cwei->mutex1);
    first_observed(cwei->mutex2);
    break;
  }
  case SemaphoreErr: {
    SemaphoreErrInfo* sei = (SemaphoreErrInfo*)(VG_(get_error_extra)(e));
    tl_assert(sei);
    VG_(message)(Vg_UserMsg,
                 "%s: semaphore 0x%lx",
                 VG_(get_error_string)(e),
                 sei->semaphore);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    first_observed(sei->semaphore);
    break;
  }
  case BarrierErr: {
    BarrierErrInfo* bei =(BarrierErrInfo*)(VG_(get_error_extra)(e));
    tl_assert(bei);
    VG_(message)(Vg_UserMsg,
                 "%s: barrier 0x%lx",
                 VG_(get_error_string)(e),
                 bei->barrier);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    first_observed(bei->barrier);
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
    first_observed(p->rwlock);
    break;
  }
  case HoldtimeErr: {
    HoldtimeErrInfo* p =(HoldtimeErrInfo*)(VG_(get_error_extra)(e));
    tl_assert(p);
    tl_assert(p->acquired_at);
    VG_(message)(Vg_UserMsg, "Acquired at:");
    VG_(pp_ExeContext)(p->acquired_at);
    VG_(message)(Vg_UserMsg,
                 "Lock on %s 0x%lx was held during %d ms (threshold: %d ms).",
                 VG_(get_error_string)(e),
                 p->synchronization_object,
                 p->hold_time_ms,
                 p->threshold_ms);
    VG_(pp_ExeContext)(VG_(get_error_where)(e));
    first_observed(p->synchronization_object);
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
  case CondDestrErr:
    return sizeof(CondDestrErrInfo);
  case CondRaceErr:
    return sizeof(CondRaceErrInfo);
  case CondWaitErr:
    return sizeof(CondWaitErrInfo);
  case SemaphoreErr:
    return sizeof(SemaphoreErrInfo);
  case BarrierErr:
    return sizeof(BarrierErrInfo);
  case RwlockErr:
    return sizeof(RwlockErrInfo);
  case HoldtimeErr:
    return sizeof(HoldtimeErrInfo);
  case GenericErr:
    return sizeof(GenericErrInfo);
  default:
    tl_assert(False);
    break;
  }
}

static Bool drd_tool_error_recog(Char* const name, Supp* const supp)
{
  SuppKind skind = 0;

  if (VG_(strcmp)(name, STR_DataRaceErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_MutexErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_CondErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_CondDestrErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_CondRaceErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_CondWaitErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_SemaphoreErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_BarrierErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_RwlockErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_HoldtimeErr) == 0)
    ;
  else if (VG_(strcmp)(name, STR_GenericErr) == 0)
    ;
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
  case DataRaceErr:  return VGAPPEND(STR_, DataRaceErr);
  case MutexErr:     return VGAPPEND(STR_, MutexErr);
  case CondErr:      return VGAPPEND(STR_, CondErr);
  case CondDestrErr: return VGAPPEND(STR_, CondDestrErr);
  case CondRaceErr:  return VGAPPEND(STR_, CondRaceErr);
  case CondWaitErr:  return VGAPPEND(STR_, CondWaitErr);
  case SemaphoreErr: return VGAPPEND(STR_, SemaphoreErr);
  case BarrierErr:   return VGAPPEND(STR_, BarrierErr);
  case RwlockErr:    return VGAPPEND(STR_, RwlockErr);
  case HoldtimeErr:  return VGAPPEND(STR_, HoldtimeErr);
  case GenericErr:   return VGAPPEND(STR_, GenericErr);
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
