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
#include "pub_tool_options.h"     /* VG_(clo_xml)             */
#include "pub_tool_threadstate.h" /* VG_(get_pthread_id)()    */
#include "pub_tool_tooliface.h"   /* VG_(needs_tool_errors)() */


/* Local function declarations. */

static Char* drd_get_error_name(Error* e);


/* Local variables. */

static Bool s_show_conflicting_segments = True;


void DRD_(set_show_conflicting_segments)(const Bool scs)
{
   s_show_conflicting_segments = scs;
}

void DRD_(trace_msg)(const char* format, ...)
{
   va_list vargs;
   va_start(vargs, format);
   if (VG_(clo_xml)) {
      VG_(printf_xml)("  <trace><text>");
      VG_(vprintf_xml)(format, vargs);
      VG_(printf_xml)("</text></trace>\n");
   } else {
      VG_(vmessage)(Vg_UserMsg, format, vargs);
      VG_(message)(Vg_UserMsg, "\n");
   }
   va_end(vargs);
}

void DRD_(trace_msg_w_bt)(const char* format, ...)
{
   va_list vargs;
   va_start(vargs, format);
   if (VG_(clo_xml)) {
      VG_(printf_xml)("  <trace><text>");
      VG_(vprintf_xml)(format, vargs);
      VG_(printf_xml)("</text>\n");
   } else {
      VG_(vmessage)(Vg_UserMsg, format, vargs);
      VG_(message)(Vg_UserMsg, "\n");
   }
   VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), VG_(clo_backtrace_size));
   va_end(vargs);
   if (VG_(clo_xml))
      VG_(printf_xml)("  </trace>\n");
}

/**
 * Emit error message detail in the format requested by the user.
 */
static void print_err_detail(const char* format, ...) PRINTF_CHECK(1, 2);
static void print_err_detail(const char* format, ...)
{
   va_list vargs;
   va_start(vargs, format);
   if (VG_(clo_xml))
      VG_(vprintf_xml)(format, vargs);
   else
      VG_(vmessage)(Vg_UserMsg, format, vargs);
   va_end(vargs);
}

/**
 * Describe the client address a as good as possible, putting the result in ai.
 */
static
void describe_malloced_addr(Addr const a, AddrInfo* const ai)
{
   Addr heap_block_start;

   if (DRD_(heap_addrinfo)(a, &heap_block_start, &ai->size, &ai->lastchange))
   {
      ai->akind = eMallocd;
      ai->rwoffset = a - heap_block_start;
   }
   else
   {
      ai->akind = eUnknown;
   }
}

/**
 * Report where a client synchronization object has been observed for the first
 * time. The printed call stack will either refer to a pthread_*_init() or a
 * pthread_*lock() call.
 */
static void first_observed(const Addr obj)
{
   DrdClientobj* cl;

   cl = DRD_(clientobj_get_any)(obj);
   if (cl) {
      tl_assert(cl->any.first_observed_at);
      if (VG_(clo_xml)) {
         print_err_detail("  <first_observed_at>\n"
                          "    <what>%pS</what>\n"
                          "    <address>0x%lx</address>\n",
                          DRD_(clientobj_type_name)(cl->any.type), obj);
         VG_(pp_ExeContext)(cl->any.first_observed_at);
         print_err_detail("  </first_observed_at>\n");
      } else {
         print_err_detail("%s 0x%lx was first observed at:\n",
                          DRD_(clientobj_type_name)(cl->any.type), obj);
         VG_(pp_ExeContext)(cl->any.first_observed_at);
      }
   }
}

static
void drd_report_data_race(Error* const err, const DataRaceErrInfo* const dri)
{
   const Bool xml = VG_(clo_xml);
   const char* const what_prefix = xml ? "  <what>" : "";
   const char* const what_suffix = xml ? "</what>" : "";
   const char* const auxwhat_prefix = xml ? "  <auxwhat>" : "";
   const char* const auxwhat_suffix = xml ? "</auxwhat>" : "";
   const char* const indent = xml ? "  " : "";
   AddrInfo ai;

   XArray* /* of HChar */ descr1
      = VG_(newXA)( VG_(malloc), "drd.error.drdr2.1",
                    VG_(free), sizeof(HChar) );
   XArray* /* of HChar */ descr2
      = VG_(newXA)( VG_(malloc), "drd.error.drdr2.2",
                    VG_(free), sizeof(HChar) );

   tl_assert(dri);
   tl_assert(dri->addr);
   tl_assert(dri->size > 0);
   tl_assert(descr1);
   tl_assert(descr2);

   (void) VG_(get_data_description)(descr1, descr2, dri->addr);
   /* If there's nothing in descr1/2, free them.  Why is it safe to to
      VG_(indexXA) at zero here?  Because VG_(get_data_description)
      guarantees to zero terminate descr1/2 regardless of the outcome
      of the call.  So there's always at least one element in each XA
      after the call.
   */
   if (0 == VG_(strlen)( VG_(indexXA)( descr1, 0 ))) {
      VG_(deleteXA)( descr1 );
      descr1 = NULL;
   }
   if (0 == VG_(strlen)( VG_(indexXA)( descr2, 0 ))) {
      VG_(deleteXA)( descr2 );
      descr2 = NULL;
   }
   /* Assume (assert) that VG_(get_data_description) fills in descr1
      before it fills in descr2 */
   if (descr1 == NULL)
      tl_assert(descr2 == NULL);
   /* So anyway.  Do we have something useful? */
   if (descr1 == NULL)
   {
      /* No.  Do Plan B. */
      describe_malloced_addr(dri->addr, &ai);
   }

   print_err_detail("%sConflicting %s by thread %d at 0x%08lx size %ld%s\n",
                    what_prefix, dri->access_type == eStore ? "store" : "load",
                    dri->tid, dri->addr, dri->size, what_suffix);

   VG_(pp_ExeContext)(VG_(get_error_where)(err));
   if (descr1 != NULL) {
      print_err_detail("%s%s\n", indent, (HChar*)VG_(indexXA)(descr1, 0));
      if (descr2 != NULL)
         print_err_detail("%s%s\n", indent, (HChar*)VG_(indexXA)(descr2, 0));
   } else if (ai.akind == eMallocd && ai.lastchange) {
      print_err_detail("%sAddress 0x%lx is at offset %ld from 0x%lx.%s%s",
                       auxwhat_prefix, dri->addr, ai.rwoffset,
                       dri->addr - ai.rwoffset, auxwhat_suffix,
                       xml ? "\n" : "");
      if (xml)
         print_err_detail("  <allocation_context>\n");
      else
         print_err_detail(" Allocation context:\n");
      VG_(pp_ExeContext)(ai.lastchange);
      if (xml)
         print_err_detail("  </allocation_context>\n");
   } else {
      char sect_name[64];
      VgSectKind sect_kind;

      sect_kind = VG_(DebugInfo_sect_kind)(sect_name, sizeof(sect_name),
                                           dri->addr);
      if (sect_kind != Vg_SectUnknown) {
         print_err_detail("%sAllocation context: %ps section of %ps%s\n",
                          auxwhat_prefix, VG_(pp_SectKind)(sect_kind),
                          sect_name, auxwhat_suffix);
      } else {
         print_err_detail("%sAllocation context: unknown.%s\n",
                          auxwhat_prefix, auxwhat_suffix);
      }
   }
   if (s_show_conflicting_segments)
   {
      DRD_(thread_report_conflicting_segments)(dri->tid,
                                               dri->addr, dri->size,
                                               dri->access_type);
   }

   if (descr2)
      VG_(deleteXA)(descr2);
   if (descr1)
      VG_(deleteXA)(descr1);
}

/**
 * Compare two error contexts. The core function VG_(maybe_record_error)()
 * calls this function to compare error contexts such that errors that occur
 * repeatedly are only printed once. This function is only called by the core
 * if the error kind of e1 and e2 matches and if the ExeContext's of e1 and
 * e2 also match.
 */
static Bool drd_compare_error_contexts(VgRes res, Error* e1, Error* e2)
{
   tl_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));

   switch (VG_(get_error_kind)(e1))
   {
   case DataRaceErr:
   {
      const DataRaceErrInfo* const dri1 = VG_(get_error_extra)(e1);
      const DataRaceErrInfo* const dri2 = VG_(get_error_extra)(e2);
      return dri1->access_type == dri2->access_type
	     && dri1->size == dri2->size;
   }
   case MutexErr:
   {
      const MutexErrInfo* const mei1 = VG_(get_error_extra)(e1);
      const MutexErrInfo* const mei2 = VG_(get_error_extra)(e2);
      return mei1->mutex == mei2->mutex;
   }
   default:
      return True;
   }
}

/**
 * Called by the core just before an error message will be printed. Used by
 * DRD to print the thread number as a preamble.
 */
static void drd_tool_error_before_pp(Error* const e)
{
   static DrdThreadId s_last_tid_printed = 1;
   DrdThreadId* err_extra;

   err_extra = VG_(get_error_extra)(e);

   if (err_extra && *err_extra != s_last_tid_printed && !VG_(clo_xml)) {
      VG_(umsg)("%s:\n", DRD_(thread_get_name)(*err_extra));
      s_last_tid_printed = *err_extra;
   }
}

/** Report an error to the user. */
static void drd_tool_error_pp(Error* const e)
{
   const Bool xml = VG_(clo_xml);
   const char* const what_prefix = xml ? "  <what>" : "";
   const char* const what_suffix = xml ? "</what>" : "";

   if (xml)
      VG_(printf_xml)( "  <kind>%pS</kind>\n", drd_get_error_name(e));

   switch (VG_(get_error_kind)(e))
   {
   case DataRaceErr: {
      drd_report_data_race(e, VG_(get_error_extra)(e));
      break;
   }
   case MutexErr: {
      MutexErrInfo* p = (MutexErrInfo*)(VG_(get_error_extra)(e));
      tl_assert(p);
      if (p->recursion_count >= 0) {
         print_err_detail("%s%s: mutex 0x%lx, recursion count %d, owner %d."
                          "%s\n", what_prefix, VG_(get_error_string)(e),
                          p->mutex, p->recursion_count, p->owner, what_suffix);
      } else {
         print_err_detail("%sThe object at address 0x%lx is not a mutex.%s\n",
                          what_prefix, p->mutex, what_suffix);
      }
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      first_observed(p->mutex);
      break;
   }
   case CondErr: {
      CondErrInfo* cdei =(CondErrInfo*)(VG_(get_error_extra)(e));
      print_err_detail("%s%s: cond 0x%lx%s\n", what_prefix,
                       VG_(get_error_string)(e), cdei->cond, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      first_observed(cdei->cond);
      break;
   }
   case CondDestrErr: {
      CondDestrErrInfo* cdi = (CondDestrErrInfo*)(VG_(get_error_extra)(e));
      print_err_detail("%s%s: cond 0x%lx, mutex 0x%lx locked by thread %d%s\n",
                       what_prefix, VG_(get_error_string)(e), cdi->cond,
                       cdi->mutex, cdi->owner, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      first_observed(cdi->mutex);
      break;
   }
   case CondRaceErr: {
      CondRaceErrInfo* cei = (CondRaceErrInfo*)(VG_(get_error_extra)(e));
      print_err_detail("%sProbably a race condition: condition variable 0x%lx"
                       " has been signaled but the associated mutex 0x%lx is"
                       " not locked by the signalling thread.%s\n",
                       what_prefix, cei->cond, cei->mutex, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      first_observed(cei->cond);
      first_observed(cei->mutex);
      break;
   }
   case CondWaitErr: {
      CondWaitErrInfo* cwei = (CondWaitErrInfo*)(VG_(get_error_extra)(e));
      print_err_detail("%s%s: condition variable 0x%lx, mutexes 0x%lx and"
                       " 0x%lx%s\n", what_prefix, VG_(get_error_string)(e),
                       cwei->cond, cwei->mutex1, cwei->mutex2, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      first_observed(cwei->cond);
      first_observed(cwei->mutex1);
      first_observed(cwei->mutex2);
      break;
   }
   case SemaphoreErr: {
      SemaphoreErrInfo* sei = (SemaphoreErrInfo*)(VG_(get_error_extra)(e));
      tl_assert(sei);
      print_err_detail("%s%s: semaphore 0x%lx%s\n", what_prefix,
                       VG_(get_error_string)(e), sei->semaphore, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      first_observed(sei->semaphore);
      break;
   }
   case BarrierErr: {
      BarrierErrInfo* bei = (BarrierErrInfo*)(VG_(get_error_extra)(e));
      tl_assert(bei);
      print_err_detail("%s%s: barrier 0x%lx%s\n", what_prefix,
                       VG_(get_error_string)(e), bei->barrier, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      if (bei->other_context) {
         if (xml)
            print_err_detail("  <confl_wait_call>\n");
         print_err_detail("%sConflicting wait call by thread %d:%s\n",
                          what_prefix, bei->other_tid, what_suffix);
         VG_(pp_ExeContext)(bei->other_context);
         if (xml)
            print_err_detail("  </confl_wait_call>\n");
      }
      first_observed(bei->barrier);
      break;
   }
   case RwlockErr: {
      RwlockErrInfo* p = (RwlockErrInfo*)(VG_(get_error_extra)(e));
      tl_assert(p);
      print_err_detail("%s%s: rwlock 0x%lx.%s\n", what_prefix,
                       VG_(get_error_string)(e), p->rwlock, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      first_observed(p->rwlock);
      break;
   }
   case HoldtimeErr: {
      HoldtimeErrInfo* p =(HoldtimeErrInfo*)(VG_(get_error_extra)(e));
      tl_assert(p);
      tl_assert(p->acquired_at);
      if (xml)
         print_err_detail("  <acquired_at>\n");
      else
         print_err_detail("Acquired at:\n");
      VG_(pp_ExeContext)(p->acquired_at);
      if (xml)
         print_err_detail("  </acquired_at>\n");
      print_err_detail("%sLock on %s 0x%lx was held during %d ms"
                       " (threshold: %d ms).%s\n", what_prefix,
                       VG_(get_error_string)(e), p->synchronization_object,
                       p->hold_time_ms, p->threshold_ms, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      first_observed(p->synchronization_object);
      break;
   }
   case GenericErr: {
      GenericErrInfo* gei = (GenericErrInfo*)(VG_(get_error_extra)(e));
      print_err_detail("%s%s%s\n", what_prefix, VG_(get_error_string)(e),
                       what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      if (gei->addr)
	 first_observed(gei->addr);
      break;
   }
   case InvalidThreadId: {
      InvalidThreadIdInfo* iti =(InvalidThreadIdInfo*)(VG_(get_error_extra)(e));
      print_err_detail("%s%s 0x%llx%s\n", what_prefix, VG_(get_error_string)(e),
                       iti->ptid, what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      break;
   }
   case UnimpHgClReq: {
      UnimpClReqInfo* uicr =(UnimpClReqInfo*)(VG_(get_error_extra)(e));
      print_err_detail("%sThe annotation macro %s has not yet been implemented"
                       " in %ps%s\n", what_prefix, uicr->descr,
                       "<valgrind/helgrind.h>", what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      break;
   }
   case UnimpDrdClReq: {
      UnimpClReqInfo* uicr =(UnimpClReqInfo*)(VG_(get_error_extra)(e));
      print_err_detail("%sThe annotation macro %s has not yet been implemented"
                       " in %ps%s\n", what_prefix, uicr->descr,
                       "<valgrind/drd.h>", what_suffix);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      break;
   }
   default:
      print_err_detail("%s%s%s\n", what_prefix, VG_(get_error_string)(e),
                       what_suffix);
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
   case InvalidThreadId:
      return sizeof(InvalidThreadIdInfo);
   case UnimpHgClReq:
      return sizeof(UnimpClReqInfo);
   case UnimpDrdClReq:
      return sizeof(UnimpClReqInfo);
   default:
      tl_assert(False);
      break;
   }
}

/**
 * Parse suppression name.
 *
 * The suppression types recognized by DRD are the same types as the error
 * types supported by DRD. So try to match the suppression name against the
 * names of DRD error types.
 */
static Bool drd_is_recognized_suppression(Char* const name, Supp* const supp)
{
   DrdErrorKind skind = 0;

   if (VG_(strcmp)(name, STR_DataRaceErr) == 0)
      skind = DataRaceErr;
   else if (VG_(strcmp)(name, STR_MutexErr) == 0)
      skind = MutexErr;
   else if (VG_(strcmp)(name, STR_CondErr) == 0)
      skind = CondErr;
   else if (VG_(strcmp)(name, STR_CondDestrErr) == 0)
      skind = CondDestrErr;
   else if (VG_(strcmp)(name, STR_CondRaceErr) == 0)
      skind = CondRaceErr;
   else if (VG_(strcmp)(name, STR_CondWaitErr) == 0)
      skind = CondWaitErr;
   else if (VG_(strcmp)(name, STR_SemaphoreErr) == 0)
      skind = SemaphoreErr;
   else if (VG_(strcmp)(name, STR_BarrierErr) == 0)
      skind = BarrierErr;
   else if (VG_(strcmp)(name, STR_RwlockErr) == 0)
      skind = RwlockErr;
   else if (VG_(strcmp)(name, STR_HoldtimeErr) == 0)
      skind = HoldtimeErr;
   else if (VG_(strcmp)(name, STR_GenericErr) == 0)
      skind = GenericErr;
   else if (VG_(strcmp)(name, STR_InvalidThreadId) == 0)
      skind = InvalidThreadId;
   else if (VG_(strcmp)(name, STR_UnimpHgClReq) == 0)
      skind = UnimpHgClReq;
   else if (VG_(strcmp)(name, STR_UnimpDrdClReq) == 0)
      skind = UnimpDrdClReq;
   else
      return False;

   VG_(set_supp_kind)(supp, skind);
   return True;
}

/**
 * Read additional suppression information from the suppression file.
 *
 * None of the suppression patterns recognized by DRD has 'extra' lines
 * of information in the suppression file, so just return True to indicate
 * that reading the 'extra' lines succeeded.
 */
static
Bool drd_read_extra_suppression_info(Int fd, Char** bufpp,
                                     SizeT* nBufp, Supp* supp)
{
   return True;
}

/**
 * Determine whether or not the types of the given error message and the
 * given suppression match.
 */
static Bool drd_error_matches_suppression(Error* const e, Supp* const supp)
{
   return VG_(get_supp_kind)(supp) == VG_(get_error_kind)(e);
}

static Char* drd_get_error_name(Error* e)
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
   case InvalidThreadId: return VGAPPEND(STR_, InvalidThreadId);
   case UnimpHgClReq:  return VGAPPEND(STR_, UnimpHgClReq);
   case UnimpDrdClReq: return VGAPPEND(STR_, UnimpDrdClReq);
   default:
      tl_assert(0);
   }
   return 0;
}

/**
 * Return extra suppression information.
 *
 * Invoked while printing a suppression pattern because the user
 * specified --gen-suppressions=yes or all on the command line. DRD does not
 * define any 'extra' suppression information.
 */
static
Bool drd_get_extra_suppression_info(Error* e,
                                    /*OUT*/Char* buf, Int nBuf)
{
   return False;
}

/** Tell the Valgrind core about DRD's error handlers. */
void DRD_(register_error_handlers)(void)
{
   VG_(needs_tool_errors)(drd_compare_error_contexts,
                          drd_tool_error_before_pp,
                          drd_tool_error_pp,
                          False,
                          drd_tool_error_update_extra,
                          drd_is_recognized_suppression,
                          drd_read_extra_suppression_info,
                          drd_error_matches_suppression,
                          drd_get_error_name,
                          drd_get_extra_suppression_info);
}
