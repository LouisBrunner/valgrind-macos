
/*
   ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (drd.h) only.  The rest of Valgrind is licensed under the
   terms of the GNU General Public License, version 2, unless
   otherwise indicated.  See the COPYING file in the source
   distribution for details.

   ----------------------------------------------------------------

   This file is part of drd, a Valgrind tool for verification of
   multithreaded programs.

   Copyright (C) 2006-2008 Bart Van Assche.  All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. The origin of this software must not be misrepresented; you must
      not claim that you wrote the original software.  If you use this
      software in a product, an acknowledgment in the product
      documentation would be appreciated but is not required.

   3. Altered source versions must be plainly marked as such, and must
      not be misrepresented as being the original software.

   4. The name of the author may not be used to endorse or promote
      products derived from this software without specific prior written
      permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   ----------------------------------------------------------------

   Notice that the above BSD-style license applies to this one file
   (drd.h) only.  The entire rest of Valgrind is licensed under
   the terms of the GNU General Public License, version 2.  See the
   COPYING file in the source distribution for details.

   ----------------------------------------------------------------
*/

#ifndef __VALGRIND_DRD_H
#define __VALGRIND_DRD_H


#include "valgrind.h"


/* !! ABIWARNING !! ABIWARNING !! ABIWARNING !! ABIWARNING !!
   This enum comprises an ABI exported by Valgrind to programs
   which use client requests.  DO NOT CHANGE THE ORDER OF THESE
   ENTRIES, NOR DELETE ANY -- add new ones at the end.
 */
enum
{
  /* Ask the core the thread ID assigned by Valgrind. */
  VG_USERREQ__DRD_GET_VALGRIND_THREAD_ID = VG_USERREQ_TOOL_BASE('D','R'),
  /* args: none. */
  /* Ask the core the thread ID assigned by DRD. */
  VG_USERREQ__DRD_GET_DRD_THREAD_ID,
  /* args: none. */

  /* To tell the drd tool to suppress data race detection on the specified */
  /* address range. */
  VG_USERREQ__DRD_START_SUPPRESSION,
  /* args: start address, size in bytes */
  /* To tell the drd tool no longer to suppress data race detection on the */
  /* specified address range. */
  VG_USERREQ__DRD_FINISH_SUPPRESSION,
  /* args: start address, size in bytes */

  /* To ask the drd tool to trace all accesses to the specified range. */
  VG_USERREQ__DRD_START_TRACE_ADDR,
  /* args: Addr, SizeT. */
  /* To ask the drd tool to stop tracing accesses to the specified range. */
  VG_USERREQ__DRD_STOP_TRACE_ADDR,
  /* args: Addr, SizeT. */
};


/** Tell DRD to suppress data race detection on the specified variable. */
#define DRD_IGNORE_VAR(x) vg_drd_ignore_range(&(x), sizeof(x))

/** Tell DRD to trace all memory accesses on the specified variable. 
 *  until the memory that was allocated for the variable is freed.
 */
#define DRD_TRACE_VAR(x) vg_drd_trace_range(&(x), sizeof(x))


static __inline__
int vg_get_valgrind_threadid(void)
{
  int res;
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__DRD_GET_VALGRIND_THREAD_ID,
                             0, 0, 0, 0, 0);
  return res;
}

static __inline__
int vg_get_drd_threadid(void)
{
  int res;
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__DRD_GET_DRD_THREAD_ID,
                             0, 0, 0, 0, 0);
  return res;
}

static __inline__
void vg_drd_ignore_range(const void* const p, const int size)
{
  int res;
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__DRD_START_SUPPRESSION,
                             p, size, 0, 0, 0);
}

static __inline__
void vg_drd_trace_range(const void* const p, const int size)
{
  int res;
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__DRD_START_TRACE_ADDR,
                             p, size, 0, 0, 0);
}


#endif /* __VALGRIND_DRD_H */
