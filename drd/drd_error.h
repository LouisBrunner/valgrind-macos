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
   OffT        rwoffset;      //   ALL
   ExeContext* lastchange;    //   Mallocd
   DrdThreadId stack_tid;     //   Stack
   DebugInfo*  debuginfo;     //   Segment
   Char        name[256];     //   Segment
   Char        descr[256];    //   Segment
}
   AddrInfo;

typedef struct {
   DrdThreadId   tid;         // Thread ID of the running thread.
   Addr          addr;        // Conflicting address in current thread.
   SizeT         size;        // Size in bytes of conflicting operation.
   BmAccessTypeT access_type; // Access type: load or store.
} DataRaceErrInfo;

typedef struct {
   Addr mutex;
   Int recursion_count;
   DrdThreadId owner;
} MutexErrInfo;

typedef struct {
   Addr cond;
} CondErrInfo;

typedef struct {
   Addr        cond;
   Addr        mutex;
   DrdThreadId tid;
} CondDestrErrInfo;

typedef struct {
   Addr cond;
   Addr mutex;
} CondRaceErrInfo;

typedef struct {
   Addr cond;
   Addr mutex1;
   Addr mutex2;
} CondWaitErrInfo;

typedef struct {
   Addr semaphore;
} SemaphoreErrInfo;

typedef struct {
   Addr barrier;
} BarrierErrInfo;

typedef struct {
   Addr rwlock;
} RwlockErrInfo;

typedef struct {
  Addr        synchronization_object;
  ExeContext* acquired_at;
  UInt        hold_time_ms;
  UInt        threshold_ms;
} HoldtimeErrInfo;

typedef struct {
} GenericErrInfo;


void set_show_conflicting_segments(const Bool scs);
void drd_register_error_handlers(void);


#endif /* __DRD_ERROR_H */
