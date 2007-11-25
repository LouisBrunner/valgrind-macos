/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2007 Bart Van Assche
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


#include "pub_drd_bitmap.h"         // BmAccessTypeT
#include "drd_thread.h"         // DrdThreadId
#include "pub_tool_basics.h"    // SizeT
#include "pub_tool_debuginfo.h" // SegInfo
#include "pub_tool_errormgr.h"  // ExeContext


/* DRD error types. */

typedef enum {
   DataRaceErr    = 1,
   MutexErr       = 2,
   CondRaceErr    = 3,
   CondErr = 4,
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
   SegInfo*    seginfo;       //   Segment
   Char        name[256];     //   Segment
   Char        descr[256];    //   Segment
}
   AddrInfo;

#ifdef OLD_RACE_DETECTION_ALGORITHM
/* Records info about a data race. */
typedef struct {
   ThreadId tid1;      // Thread ID of first thread involved in the data race.
   ThreadId tid2;      // Thread ID of second thread involved in the data race.
   Addr     range_begin;  // Start address of range involved.
   Addr     range_end;    // Last address (exclusive) of range involved.
   UInt     range_access; // How the range was accessed (LHS_[RW] | RHS_[RW]).
} DataRaceInfo;
#endif

typedef struct {
   ThreadId      tid;         // Thread ID of the running thread.
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
   Addr mutex;
} CondRaceErrInfo;

typedef struct {
   Addr cond;
} CondErrInfo;

void describe_addr(Addr const a, SizeT const len, AddrInfo* const ai);
Char* describe_addr_text(Addr const a, SizeT const len, AddrInfo* const ai,
                         Char* const buf, UInt const n_buf);
#ifdef OLD_RACE_DETECTION_ALGORITHM
void drd_report_data_race(const DataRaceInfo* const dri);
#endif
//void drd_report_data_race2(const DataRaceErrInfo* const dri);
void drd_register_error_handlers(void);


#endif /* __DRD_ERROR_H */


/*
 * Local variables:
 * c-basic-offset: 3
 * End:
 */
