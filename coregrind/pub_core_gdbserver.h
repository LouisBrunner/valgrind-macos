
/*--------------------------------------------------------------------*/
/*--- Handle remote gdb protocol.             pub_core_gdbserver.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011 Philippe Waroquiers

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

#ifndef __PUB_CORE_GDBSERVER_H
#define __PUB_CORE_GDBSERVER_H

#include "pub_tool_gdbserver.h"


// After a fork or after an exec, call the below to (possibly) terminate
// the previous gdbserver and then activate a new gdbserver
// before any guest code execution, to e.g. allow the user to set
// breakpoints before execution.
// If VG_(clo_vgdb) == No, the below has no effect.
void VG_(gdbserver_prerun_action) (ThreadId tid);

// True if there is some activity from vgdb
// If it returns True, then extern void VG_(gdbserver) can be called
// to handle this incoming vgdb request.                                
extern Bool VG_(gdbserver_activity) (ThreadId tid);


/* Called by low level to insert or remove a break or watch point.
   Break or watch point implementation is done using help from the tool.
   break point support implies some (small) specific instrumentation
   taken in charge for all tools by m_translate.c.
   
   Write/read/access watchpoint can only be provided by tools which are
   tracking addressability and/or accessibility of memory 
   (so typically memcheck can provide it). Note that memcheck addressability
   bits do not differentiate between read and write accessibility.
   However, when accessing unaddressable byte, memcheck can differentiate
   reads from write, thereby providing read/write or access watchpoints.

   Note that gdbserver assumes that software breakpoint is supported
   (as this will be done by re-instrumenting the code).
   Note that len is ignored for sofware breakpoints. hardware_breakpoint
   are not supported.

   Returns True if the point has properly been inserted or removed
   Returns False otherwise. */
Bool VG_(gdbserver_point) (PointKind kind, Bool insert, 
                           Addr addr, int len);

/* Entry point invoked by vgdb when it uses ptrace to cause a gdbserver
   invocation. A magic value is passed by vgdb in check as a verification
   that the call has been properly pushed by vgdb. */
extern void VG_(invoke_gdbserver) ( int check );

// To be called before delivering a signal.
// Returns True if gdb user asks to pass the signal to the client.
// Note that if the below returns True, the signal might
// still be ignored if this is the action desired by the
// guest program.
extern Bool VG_(gdbserver_report_signal) (Int signo, ThreadId tid);

/* software_breakpoint, single step and jump support ------------------------*/
/* VG_(instrument_for_gdbserver_if_needed) allows to do "standard and easy"
   instrumentation for gdbserver.
   VG_(instrument_for_gdbserver_if_needed) does the following:
      * checks if gdbserver instrumentation is needed for vge.
      * if no gdbserver instrumentation needed,
           returns sb_in
      * otherwise 
        It will instrument sb_in to allow gdbserver to properly
        handle breakpoints and single_stepping in sb_in.
        All the target jumps of sb_in will also be invalidated
        if these are not yet instrumented for gdbserver.
        This allows to have single_step working, using a lazily
        translation of the blocks which are being single stepped
        in. 

   The typical usage of this function is to call it on the block
   instrumented by the tool instrument function i.e. :
     return VG_(instrument_for_gdbserver_if_needed) (sb_out, 
                                                     layout, 
                                                     vge, 
                                                     gWordTy, 
                                                     hWordTy);
   where sb_out is the block instrumented by the tool.

   If the block contains a call to a dirty helper that indirectly
   calls gdbserver, then this dirty helper can (indirectly) change
   the IP. This implies to jump to this IP after the call to
   gdbserver. */
extern IRSB* VG_(instrument_for_gdbserver_if_needed)
     (IRSB* sb_in,                   /* block to be instrumented */
      VexGuestLayout* layout,
      VexGuestExtents* vge,
      IRType gWordTy, IRType hWordTy);

/* reason for which gdbserver connection must be finished */
typedef
   enum {
      orderly_finish,
      reset_after_error,
      reset_after_fork} FinishReason;

/* output various gdbserver statistics and status. */
extern void VG_(gdbserver_status_output)(void);

/* Shared structure between vgdb and the process running 
   under valgrind.
   We define two variants: a 32 bit and a 64 bit.
   The valgrind process will use the appropriate size,
   according to the architecture.
   vgdb will use what the valgrind process is using. */
/* The below takes care that sizes will be 32 or 64 bits,
   whatever the architecture. A.o., vgdb.c cannot use directly
   the types from pub_core_threadstate.h as we want vgdb.c to
   be independent of the arch it is debugging in case of bi-arch
   Valgrind (e.g. x86 and amd64). So, the valgrind process must
   give all the needed info/offset to vgdb in the below structure. */

typedef 
   struct {
      // PID of the vgdb that last connected to the Valgrind gdbserver.
      // It will be set by vgdb after connecting.
      int vgdb_pid;

      // nr of bytes vgdb has written to valgrind
      volatile int written_by_vgdb;
      // nr of bytes seen by valgrind
      volatile int seen_by_valgrind;
      
      // address at which gdbserver can be invoked
      Addr32 invoke_gdbserver;

      // address of VG_(threads) and various sizes
      // and offset needed by vgdb.
      Addr32 threads;
      int sizeof_ThreadState;
      int offset_status;
      int offset_lwpid;
   } VgdbShared32;

/* Same as VgdbShared32 but for 64 bits arch. */
typedef 
   struct {
      int vgdb_pid;

      volatile int written_by_vgdb;
      volatile int seen_by_valgrind;
      
      Addr64 invoke_gdbserver;

      Addr64 threads;
      int sizeof_ThreadState;
      int offset_status;
      int offset_lwpid;
   } VgdbShared64;

// The below typedef makes the life of valgrind easier.
// vgdb must however work explicitely with the specific 32 or 64 bits version.

#if VEX_HOST_WORDSIZE == 8
typedef VgdbShared64 VgdbShared;
#elif VEX_HOST_WORDSIZE == 4
typedef VgdbShared32 VgdbShared;
#else
# error "unexpected wordsize"
#endif


#endif   // __PUB_CORE_GDBSERVER_H
/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
