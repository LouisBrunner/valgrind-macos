
/*--------------------------------------------------------------------*/
/*--- Handle remote gdb protocol.             pub_tool_gdbserver.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2011 Philippe Waroquiers

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

#ifndef __PUB_TOOL_GDBSERVER_H
#define __PUB_TOOL_GDBSERVER_H

#include "libvex.h"
#include "libvex_ir.h"

//--------------------------------------------------------------------
// PURPOSE: This module provides the support to have a gdb
// connecting to a valgrind process using remote gdb protocol. It provides
//  * A function to allow a tool (or the valgrind core) to
//    wait for a gdb to connect and then handle gdb commands.
//    Typically, this can be used to let the user debug the process
//    when valgrind reports an error.
//  * A function allowing to instrument the code to support gdb breakpoints.
//  * A function allowing the tool to support watchpoints.
//  * A utility function to help implementing the processing of the
//    gdb_monitor_command strings.


// Function to be used by tool or coregrind to allow a gdb to connect
// to this process. 
// Calling VG_(gdbserver) with tid > 0 means to let a debugger attach
// to the valgrind process. gdbserver will report to gdb that the
// process stopped in thread tid.
// tid == 0 indicates to stop gdbserver and report to gdb
// that the valgrind-ified process has exited.
//--------------------------------------------------------------------
extern void VG_(gdbserver) ( ThreadId tid );

/* VG_(dyn_vgdb_error) gets its initial value from
   VG_(clo_vgdb_error).  It can be changed after initial command
   processing in order to enable/disable the call to VG_(gdbserver) in
   m_errormgr.c.  The main reasons to change the below is either
   because the user updates it via a monitor command or to
   (temporarily) avoid calling gdbserver for error reporting during
   monitor command handling.
*/
extern Int VG_(dyn_vgdb_error);

/* defines the various kinds of breakpoints that gdbserver
   might ask to insert/remove. Note that the below matches
   the gdbserver protocol definition. The level of support
   of the various breakpoint kinds depends on the tool.

   For the moment, it is unclear how a tool would implement
   hardware_breakpoint in valgrind  :).

   software_breakpoint implies some (small) specific
   instrumentation to be done for gdbserver. This instrumentation
   is implemented for all tools in m_translate.c.

   write/read/access watchpoints can only be done by tools
   which are maintaining some notion of address accessibility
   as part of their instrumentation. watchpoints can then
   be done by marking the watched address(es) as not accessible.
   But instead of giving back an error (or whatever the tool
   wants to do with unaccessible mechanism), the tool must then
   just call gdbserver. See memcheck for an example of reusing
   accessibility for watchpoint support.
*/
typedef
   enum {
      software_breakpoint,
      hardware_breakpoint,
      write_watchpoint,
      read_watchpoint,
      access_watchpoint } PointKind;
extern char* VG_(ppPointKind) (PointKind kind);


/* watchpoint support --------------------------------------*/
/* True if one or more bytes in [addr, addr+len[ are being watched by
   gdbserver for write or read or access.
   In addition, VG_(is_watched) will invoke gdbserver if
   the access provided by the tool matches the watchpoint kind.
   For this, the tool must pass the kind of access it has detected:
      write_watchpoint indicates the tool has detected a write
      read_watchpoint indicates the tool has detected a read
      access_watchpoint indicates the tool has detected an access but does
      not know if this is a read or a write
*/
extern Bool VG_(is_watched)(PointKind kind, Addr addr, Int szB);

extern void VG_(needs_watchpoint) (
   // indicates the given Addr/len is being watched (insert)
   // or not watched anymore (! insert).
   // gdbserver will maintain the list of watched addresses.
   // The tool can use VG_(is_watched) to verify if an
   // access to an Addr is in one of the watched intervals.
   // Must return True if the watchpoint has been properly inserted or
   // removed. False if not supported.
   // Note that an address can only be watched for a single kind.
   // The tool must be ready to be called successively with
   // multiple kinds for the same addr and len and with
   // different kinds. The last kind must replace the previous values.
   // Behaviour with multiple watches having overlapping addr+len
   // is undefined.
   Bool (*watchpoint) (PointKind kind, Bool insert, Addr addr, SizeT len)
);


// can be used during the processing of the VG_USERREQ__GDB_MONITOR_COMMAND 
// tool client request to output information to gdb or vgdb.
extern UInt VG_(gdb_printf) ( const HChar *format, ... ) PRINTF_CHECK(1, 2);

/* Utility functions to (e.g.) parse gdb monitor commands.

   keywords is a set of keywords separated by a space
   keyword_id will search for the keyword starting with the string input_word
   and return its position.
   It returns -1 if no keyword matches.
   It returns -2 if two or more keywords are starting with input_word
                 and none of these matches exactly input_word
   Example with keywords = "hello world here is hell" :
   input_word    result
   ----------    ------
   paradise   => -1
   i          =>  3
   hell       =>  4
   hel        => -2
   ishtar     => -1

   report indicates when to output an error msg with VG_(gdb_printf).
   kwd_report_none : no error is reported.
   kwd_report_all : the error msg will show all possible keywords
   kwd_report_duplicated_matches : the error msg will show only the
     ambiguous matches.
*/
typedef
   enum {
      kwd_report_none,
      kwd_report_all,
      kwd_report_duplicated_matches } kwd_report_error;
extern Int VG_(keyword_id) (Char* keywords, Char* input_word, 
                            kwd_report_error report);

/* Extract an address and (optionally) a size from the string
   currently being parsed by strtok_r (see pub_tool_libcbase.h).
   If no size in the string, keeps the current value of szB.
   Returns address 0 and szB 0 if there is an error.  Reports to the
   user problems via VG_(gdb_printf).  */
extern void VG_(strtok_get_address_and_size) (Addr* address, 
                                              SizeT* szB, 
                                              Char **ssaveptr);

#endif   // __PUB_TOOL_GDBSERVER_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
