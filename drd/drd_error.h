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


#ifndef __DRD_ERROR_H
#define __DRD_ERROR_H


#include "pub_drd_bitmap.h"     // BmAccessTypeT
#include "drd_thread.h"         // DrdThreadId
#include "pub_tool_basics.h"    // SizeT
#include "pub_tool_debuginfo.h" // SegInfo
#include "pub_tool_errormgr.h"  // ExeContext


/* DRD error types. */

typedef enum {
#define STR_DataRaceErr  "ConflictingAccess"
   DataRaceErr    = 1,
#define STR_MutexErr     "MutexErr"
   MutexErr       = 2,
#define STR_CondErr      "CondErr"
   CondErr        = 3,
#define STR_CondDestrErr "CondDestrErr"
   CondDestrErr   = 4,
#define STR_CondRaceErr  "CondRaceErr"
   CondRaceErr    = 5,
#define STR_CondWaitErr  "CondWaitErr"
   CondWaitErr    = 6,
#define STR_SemaphoreErr "SemaphoreErr"
   SemaphoreErr   = 7,
#define STR_BarrierErr   "BarrierErr"
   BarrierErr     = 8,
#define STR_RwlockErr    "RwlockErr"
   RwlockErr      = 9,
#define STR_HoldtimeErr  "HoldtimeErr"
   HoldtimeErr    = 10,
#define STR_GenericErr   "GenericErr"
   GenericErr     = 11,
#define STR_InvalidThreadId "InvalidThreadId"
   InvalidThreadId = 12,
#define STR_UnimpHgClReq  "UnimpHgClReq"
   UnimpHgClReq   = 13,
#define STR_UnimpDrdClReq "UnimpDrdClReq"
   UnimpDrdClReq  = 14,
} DrdErrorKind;

/* The classification of a faulting address. */
typedef
enum {
   //Undescribed,   // as-yet unclassified
   eStack,
   eUnknown,       // classification yielded nothing useful
   //Freed,
   eMallocd,
   eSegment,       // in a segment (as defined in pub_tool_debuginfo.h)
   //UserG,         // in a user-defined block
   //Mempool,       // in a mempool
   //Register,      // in a register;  for Param errors only
}
   AddrKind;

/* Records info about a faulting address. */
typedef
struct {                      // Used by:
   AddrKind    akind;         //   ALL
   SizeT       size;          //   ALL
   PtrdiffT    rwoffset;      //   ALL
   ExeContext* lastchange;    //   Mallocd
   DrdThreadId stack_tid;     //   Stack
   DebugInfo*  debuginfo;     //   Segment
   Char        name[256];     //   Segment
   Char        descr[256];    //   Segment
} AddrInfo;

/*
 * NOTE: the first member of each error info structure MUST be the thread ID
 * in which the error has been observed.
 */
typedef struct {
   DrdThreadId   tid;         // Thread ID of the running thread.
   Addr          addr;        // Conflicting address in current thread.
   SizeT         size;        // Size in bytes of conflicting operation.
   BmAccessTypeT access_type; // Access type: load or store.
} DataRaceErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        mutex;
   Int         recursion_count;
   DrdThreadId owner;
} MutexErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        cond;
} CondErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        cond;
   Addr        mutex;
   DrdThreadId owner;
} CondDestrErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        cond;
   Addr        mutex;
} CondRaceErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        cond;
   Addr        mutex1;
   Addr        mutex2;
} CondWaitErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        semaphore;
} SemaphoreErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        barrier;
   DrdThreadId other_tid;
   ExeContext* other_context;
} BarrierErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        rwlock;
} RwlockErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        synchronization_object;
   ExeContext* acquired_at;
   UInt        hold_time_ms;
   UInt        threshold_ms;
} HoldtimeErrInfo;

typedef struct {
   DrdThreadId tid;
   Addr        addr;
} GenericErrInfo;

typedef struct {
   DrdThreadId tid;
   ULong       ptid;
} InvalidThreadIdInfo;

typedef struct {
   DrdThreadId tid;
   Char*       descr;
} UnimpClReqInfo;

void DRD_(set_show_conflicting_segments)(const Bool scs);
void DRD_(register_error_handlers)(void);
void DRD_(trace_msg)(const char* format, ...) PRINTF_CHECK(1, 2);
void DRD_(trace_msg_w_bt)(const char* format, ...) PRINTF_CHECK(1, 2);


#endif /* __DRD_ERROR_H */
