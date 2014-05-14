
/*--------------------------------------------------------------------*/
/*--- Handle remote gdb protocol.                    m_gdbserver.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2013 Philippe Waroquiers

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

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_libcsetjmp.h"
#include "pub_core_threadstate.h"
#include "pub_core_gdbserver.h"
#include "pub_core_options.h"
#include "pub_core_libcsetjmp.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_hashtable.h"
#include "pub_core_xarray.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcsignal.h"
#include "pub_core_signals.h"
#include "pub_core_machine.h"     // VG_(fnptr_to_fnentry)
#include "pub_core_debuginfo.h"
#include "pub_core_scheduler.h"
#include "pub_core_syswrap.h"

#include "server.h"

Int VG_(dyn_vgdb_error);

/* forward declarations */
VG_REGPARM(1)
void VG_(helperc_CallDebugger) ( HWord iaddr );
VG_REGPARM(1)
void VG_(helperc_invalidate_if_not_gdbserved) ( Addr addr );
static void invalidate_current_ip (ThreadId tid, const HChar *who);

/* reasons of call to call_gdbserver. */
typedef
   enum {
      init_reason,    // initialises gdbserver resources
      vgdb_reason,    // gdbserver invocation by vgdb doing ptrace
      core_reason,    // gdbserver invocation by core (e.g. error encountered)
      break_reason,   // break encountered
      watch_reason,   // watchpoint detected by tool
      signal_reason,  // signal encountered
      exit_reason}    // process terminated
    CallReason;

static const HChar* ppCallReason(CallReason reason)
{
   switch (reason) {
   case init_reason:    return "init_reason";
   case vgdb_reason:    return "vgdb_reason";
   case core_reason:    return "core_reason";
   case break_reason:   return "break_reason";
   case watch_reason:   return "watch_reason";
   case signal_reason:  return "signal_reason";
   case exit_reason:    return "exit_reason";
   default: vg_assert (0);
   }
}

/* An instruction instrumented for gdbserver looks like this:
    1. Ist_Mark (0x1234)
    2. Put (IP, 0x1234)
    3. helperc_CallDebugger (0x1234)   
         This will give control to gdb if there is a break at 0x1234
         or if we are single stepping
    4. ... here the real IR for the instruction at 0x1234

    When there is a break at 0x1234:
      if user does "continue" or "step" or similar, 
        then - the call to debugger returns
             - valgrind executes at 3. the real IR(s) for 0x1234

      if as part of helperc_CallDebugger, the user calls 
      some code in gdb e.g print hello_world()
        then - gdb prepares a dummy stack frame with a specific 
               return address (typically it uses _start) and
               inserts a break at this address
             - gdb then puts in EIP the address of hello_world()
             - gdb then continues (so the helperc_CallDebugger
               returns)
             - call_gdbserver() function will then return the
               control to the scheduler (using VG_MINIMAL_LONGJMP)
               to allow the block of the new EIP
               to be executed.
             - hello_world code is executed.
             - when hello_world() returns, it returns to
               _start and encounters the break at _start.
             - gdb then removes this break, put 0x1234 in EIP
               and does a "step". This causes to jump from
               _start to 0x1234, where the call to 
                helperc_CallDebugger is redone.
             - This is all ok, the user can then give new gdb 
               commands. 

    However, when continue is given, address 0x1234 is to
    be executed: gdb gives a single step, which must not 
    report again the break at 0x1234. To avoid a 2nd report
    of the same break, the below tells that the next 
    helperc_CallDebugger call must ignore a break/stop at
    this address.
*/
static Addr ignore_this_break_once = 0;


static void call_gdbserver ( ThreadId tid , CallReason reason);

/* Describes the address addr (for debugging/printing purposes).
   Last two results are kept. A third call will replace the
   oldest result. */
static HChar* sym (Addr addr, Bool is_code)
{
   static HChar buf[2][200];
   static int w = 0;
   PtrdiffT offset;
   if (w == 2) w = 0;
   buf[w][0] = '\0';
   if (is_code) {
      VG_(describe_IP) (addr, buf[w], 200);
   } else {
      VG_(get_datasym_and_offset) (addr, buf[w], 200, &offset);
   }
   return buf[w++];
}

/* Each time gdbserver is called, gdbserver_called is incremented
   gdbserver_exited is incremented when gdbserver is asked to exit */
static int gdbserver_called = 0;
static int gdbserver_exited = 0;

/* alloc and free functions for xarray and similar. */
static void* gs_alloc (const HChar* cc, SizeT sz)
{
   void* res = VG_(arena_malloc)(VG_AR_CORE, cc, sz);
   vg_assert (res);
   return res;
}
static void gs_free (void* ptr)
{
   VG_(arena_free)(VG_AR_CORE, ptr);
}

typedef
   enum {
     GS_break,
     GS_jump
   }
   GS_Kind;

typedef
   struct _GS_Address {
      struct _GS_Address* next;
      Addr    addr;
      GS_Kind kind;
   }
   GS_Address;

/* gs_addresses contains a list of all addresses that have been invalidated
   because they have been (or must be) instrumented for gdbserver. 
   An entry is added in this table when there is a break at this
   address (kind == GS_break) or if this address is the jump target of an
   exit of a block that has been instrumented for gdbserver while
   single stepping (kind == GS_jump).
   When gdbserver is not single stepping anymore, all GS_jump entries
   are removed, their translations are invalidated.

   Note for ARM: addr in GS_Address is the value without the thumb bit set.
*/
static VgHashTable gs_addresses = NULL;

// Transform addr in the form stored in the list of addresses.
// For the ARM architecture, we store it with the thumb bit set to 0.
static Addr HT_addr ( Addr addr )
{
#if defined(VGA_arm)
  return addr & ~(Addr)1;
#else
  return addr;
#endif
}

static void add_gs_address (Addr addr, GS_Kind kind, const HChar* from)
{
   GS_Address *p;

   p = VG_(arena_malloc)(VG_AR_CORE, from, sizeof(GS_Address));
   p->addr = HT_addr (addr);
   p->kind = kind;
   VG_(HT_add_node)(gs_addresses, p);
   /* It should be sufficient to discard a range of 1.
      We use 2 to ensure the below is not sensitive to the presence
      of thumb bit in the range of addresses to discard. 
      No need to discard translations for Vg_VgdbFull as all
      instructions are in any case vgdb-instrumented. */
   if (VG_(clo_vgdb) != Vg_VgdbFull)
      VG_(discard_translations) (addr, 2, from);
}

static void remove_gs_address (GS_Address* g, const HChar* from)
{
   VG_(HT_remove) (gs_addresses, g->addr);
   // See add_gs_address for the explanation for condition and the range 2 below.
   if (VG_(clo_vgdb) != Vg_VgdbFull)
      VG_(discard_translations) (g->addr, 2, from);
   VG_(arena_free) (VG_AR_CORE, g);
}

const HChar* VG_(ppPointKind) (PointKind kind)
{
   switch(kind) {
   case software_breakpoint: return "software_breakpoint";
   case hardware_breakpoint: return "hardware_breakpoint";
   case write_watchpoint:    return "write_watchpoint";
   case read_watchpoint:     return "read_watchpoint";
   case access_watchpoint:   return "access_watchpoint";
   default:                  return "???wrong PointKind";
   }
}

typedef
   struct _GS_Watch {
      Addr    addr;
      SizeT   len;
      PointKind kind;
   }
   GS_Watch;

/* gs_watches contains a list of all addresses+len+kind that are being
   watched. */
static XArray* gs_watches = NULL;

static inline GS_Watch* index_gs_watches(Word i)
{
   return *(GS_Watch **) VG_(indexXA) (gs_watches, i);
}

/* Returns the GS_Watch matching addr/len/kind and sets *g_ix to its
   position in gs_watches.
   If no matching GS_Watch is found, returns NULL and sets g_ix to -1. */
static GS_Watch* lookup_gs_watch (Addr addr, SizeT len, PointKind kind,
                                  Word* g_ix)
{
   const Word n_elems = VG_(sizeXA) (gs_watches);
   Word i;
   GS_Watch *g;

   /* Linear search. If we have many watches, this might be optimised
      by having the array sorted and using VG_(lookupXA) */
   for (i = 0; i < n_elems; i++) {
      g = index_gs_watches(i);
      if (g->addr == addr && g->len == len && g->kind == kind) {
         // Found.
         *g_ix = i;
         return g;
      }
   }

   // Not found.
   *g_ix = -1;
   return NULL;
}


/* protocol spec tells the below must be idempotent. */
static void breakpoint (Bool insert, CORE_ADDR addr)
{
   GS_Address *g;

   g = VG_(HT_lookup) (gs_addresses, (UWord)HT_addr(addr));
   if (insert) {
      /* insert a breakpoint at addr or upgrade its kind */
      if (g == NULL) {
         add_gs_address (addr, GS_break, "m_gdbserver breakpoint insert");
      } else {
         /* already gdbserved. Normally, it must be because of a jump.
            However, due to idempotent or if connection with gdb was
            lost (kept breaks from the previous gdb), if already existing,
            we just upgrade its kind. */
         g->kind = GS_break;
      }
   } else {
      /* delete a breakpoint at addr or downgrade its kind */
      if (g != NULL && g->kind == GS_break) {
         if (valgrind_single_stepping()) {
            /* keep gdbserved instrumentation while single stepping */
            g->kind = GS_jump;
         } else {
            remove_gs_address (g, "m_gdbserver breakpoint remove");
         }
      } else {
         dlog (1, "remove break addr %p %s\n",
               C2v(addr), (g == NULL ? 
                           "NULL" : 
                           (g->kind == GS_jump ? "GS_jump" : "GS_break")));
      }
   }
}

static Bool (*tool_watchpoint) (PointKind kind, 
                                Bool insert, 
                                Addr addr,
                                SizeT len) = NULL;
void VG_(needs_watchpoint) (Bool (*watchpoint) (PointKind kind, 
                                                Bool insert, 
                                                Addr addr,
                                                SizeT len))
{
   tool_watchpoint = watchpoint;
}
     
Bool VG_(gdbserver_point) (PointKind kind, Bool insert,
                           CORE_ADDR addr, int len)
{
   Bool res;
   GS_Watch *g;
   Word g_ix;
   Bool is_code = kind == software_breakpoint || kind == hardware_breakpoint;

   dlog(1, "%s %s at addr %p %s\n",
        (insert ? "insert" : "remove"), 
        VG_(ppPointKind) (kind),
        C2v(addr), 
        sym(addr, is_code));

   if (is_code) {
      breakpoint (insert, addr);
      return True;
   }

   vg_assert (kind == access_watchpoint 
              || kind == read_watchpoint 
              || kind == write_watchpoint);

   if (tool_watchpoint == NULL)
      return False;

   res = (*tool_watchpoint) (kind, insert, addr, len);
   if (!res) 
      return False; /* error or unsupported */

   // Protocol says insert/remove must be idempotent.
   // So, we just ignore double insert or (supposed) double delete.

   g = lookup_gs_watch (addr, len, kind, &g_ix);
   if (insert) {
      if (g == NULL) {
         g = VG_(arena_malloc)(VG_AR_CORE, "gdbserver_point watchpoint",
                               sizeof(GS_Watch));
         g->addr = addr;
         g->len  = len;
         g->kind = kind;
         VG_(addToXA)(gs_watches, &g);
      } else {
         dlog(1, 
              "VG_(gdbserver_point) addr %p len %d kind %s already inserted\n",
               C2v(addr), len, VG_(ppPointKind) (kind));
      }
   } else {
      if (g != NULL) {
         VG_(removeIndexXA) (gs_watches, g_ix);
         VG_(arena_free) (VG_AR_CORE, g);
      } else {
         dlog(1, 
              "VG_(gdbserver_point) addr %p len %d kind %s already deleted?\n",
              C2v(addr), len, VG_(ppPointKind) (kind));
      }
   }  
   return True;
}

Bool VG_(has_gdbserver_breakpoint) (Addr addr)
{
   GS_Address *g;
   if (!gdbserver_called)
      return False;
   g = VG_(HT_lookup) (gs_addresses, (UWord)HT_addr(addr));
   return (g != NULL && g->kind == GS_break);
}

Bool VG_(is_watched)(PointKind kind, Addr addr, Int szB)
{
   Word n_elems;
   GS_Watch* g;
   Word i;
   Bool watched = False;
   const ThreadId tid = VG_(running_tid);

   if (!gdbserver_called)
      return False;

   n_elems = VG_(sizeXA) (gs_watches);

   Addr to = addr + szB; // semi-open interval [addr, to[

   vg_assert (kind == access_watchpoint 
              || kind == read_watchpoint 
              || kind == write_watchpoint);
   dlog(1, "tid %d VG_(is_watched) %s addr %p szB %d\n",
        tid, VG_(ppPointKind) (kind), C2v(addr), szB);

   for (i = 0; i < n_elems; i++) {
      g = index_gs_watches(i);
      switch (g->kind) {
      case software_breakpoint:
      case hardware_breakpoint:
         break;
      case access_watchpoint:
      case read_watchpoint:
      case write_watchpoint:
         if (to <= g->addr || addr >= (g->addr + g->len))
            /* If no overlap, examine next watchpoint: */
            continue;

         watched = True; /* We have an overlap */

         /* call gdbserver if access kind reported by the tool
            matches the watchpoint kind. */
         if (kind == access_watchpoint
             || g->kind == access_watchpoint
             || g->kind == kind) {
            /* Watchpoint encountered.
               If this is a read watchpoint, we directly call gdbserver
               to report it to gdb.
               Otherwise, for a write watchpoint, we have to finish
               the instruction so as to modify the value.
               If we do not finish the instruction, then gdb sees no
               value change and continues.
               For a read watchpoint, we better call gdbserver directly:
               in case the current block is not gdbserved, Valgrind
               will execute instructions till the next block. */

            /* set the watchpoint stop address to the first read or written. */
            if (g->addr <= addr) {
               VG_(set_watchpoint_stop_address) (addr);
            } else {
               VG_(set_watchpoint_stop_address) (g->addr);
            }

            if (kind == write_watchpoint) {
               /* Let Valgrind stop as early as possible after this instruction
                  by switching to Single Stepping mode. */
               valgrind_set_single_stepping (True);
               invalidate_current_ip (tid, "m_gdbserver write watchpoint");
            } else {
               call_gdbserver (tid, watch_reason);
               VG_(set_watchpoint_stop_address) ((Addr) 0);
            }
            return True; // we are watched here.
         }
         break;
      default:
         vg_assert (0);
      }
   }
   return watched;
}

/* Returns the reason for which gdbserver instrumentation is needed */
static VgVgdb VG_(gdbserver_instrumentation_needed) (VexGuestExtents* vge)
{
   GS_Address* g;
   int e;

   if (!gdbserver_called)
      return Vg_VgdbNo;

   if (valgrind_single_stepping()) {
      dlog(2, "gdbserver_instrumentation_needed due to single stepping\n");
      return Vg_VgdbYes;
   }

   if (VG_(clo_vgdb) == Vg_VgdbYes && VG_(HT_count_nodes) (gs_addresses) == 0)
      return Vg_VgdbNo;

   /* We assume we do not have a huge nr of breakpoints.
      Otherwise, we need something more efficient e.g.
      a sorted list of breakpoints or associate extents to it or ...
   */
   VG_(HT_ResetIter) (gs_addresses);
   while ((g = VG_(HT_Next) (gs_addresses))) {
      for (e = 0; e < vge->n_used; e++) {
         if (g->addr >= HT_addr(vge->base[e]) 
             && g->addr < HT_addr(vge->base[e]) + vge->len[e]) {
            dlog(2,
                 "gdbserver_instrumentation_needed %p %s reason %s\n",
                 C2v(g->addr), sym(g->addr, /* is_code */ True),
                 (g->kind == GS_jump ? "GS_jump" : "GS_break"));
            return Vg_VgdbYes;
         }
      }
   }

   if (VG_(clo_vgdb) == Vg_VgdbFull) {
      dlog(4, "gdbserver_instrumentation_needed"
           " due to VG_(clo_vgdb) == Vg_VgdbFull\n");
      return Vg_VgdbFull;
   }


   return Vg_VgdbNo;
}

// Clear gdbserved_addresses in gs_addresses.
// If clear_only_jumps, clears only the addresses that are served
// for jump reasons.
// Otherwise, clear all the addresses.
// Cleared addresses are invalidated so as to have them re-translated.
static void clear_gdbserved_addresses(Bool clear_only_jumps)
{
   GS_Address** ag;
   UInt n_elems;
   int i;

   dlog(1,
        "clear_gdbserved_addresses: scanning hash table nodes %d\n", 
        VG_(HT_count_nodes) (gs_addresses));
   ag = (GS_Address**) VG_(HT_to_array) (gs_addresses, &n_elems);
   for (i = 0; i < n_elems; i++)
      if (!clear_only_jumps || ag[i]->kind == GS_jump)
         remove_gs_address (ag[i], "clear_gdbserved_addresses");
   VG_(free) (ag);
}

// Clear watched addressed in gs_watches, delete gs_watches.
static void clear_watched_addresses(void)
{
   GS_Watch* g;
   const Word n_elems = VG_(sizeXA) (gs_watches);
   Word i;

   dlog(1,
        "clear_watched_addresses: %ld elements\n", 
        n_elems);
   
   for (i = 0; i < n_elems; i++) {
      g = index_gs_watches(i);
      if (!VG_(gdbserver_point) (g->kind,
                                 /* insert */ False,
                                 g->addr,
                                 g->len)) {
         vg_assert (0);
      }
   }

   VG_(deleteXA) (gs_watches);
   gs_watches = NULL;
}

static void invalidate_if_jump_not_yet_gdbserved (Addr addr, const HChar* from)
{
   if (VG_(HT_lookup) (gs_addresses, (UWord)HT_addr(addr)))
      return;
   add_gs_address (addr, GS_jump, from);
}

static void invalidate_current_ip (ThreadId tid, const HChar *who)
{
   invalidate_if_jump_not_yet_gdbserved (VG_(get_IP) (tid), who);
}

Bool VG_(gdbserver_init_done) (void)
{
   return gdbserver_called > 0;
}

Bool VG_(gdbserver_stop_at) (VgdbStopAt stopat)
{
   return gdbserver_called > 0 && VgdbStopAtiS(stopat, VG_(clo_vgdb_stop_at));
}

void VG_(gdbserver_prerun_action) (ThreadId tid)
{
   // Using VG_(dyn_vgdb_error) allows the user to control if gdbserver
   // stops after a fork.
   if (VG_(dyn_vgdb_error) == 0 
       || VgdbStopAtiS(VgdbStopAt_Startup, VG_(clo_vgdb_stop_at))) {
      /* The below call allows gdb to attach at startup
         before the first guest instruction is executed. */
      VG_(umsg)("(action at startup) vgdb me ... \n");
      VG_(gdbserver)(tid); 
   } else {
      /* User has activated gdbserver => initialize now the FIFOs
         to let vgdb/gdb contact us either via the scheduler poll
         mechanism or via vgdb ptrace-ing valgrind. */
      if (VG_(gdbserver_activity) (tid))
         VG_(gdbserver) (tid);
   }
}

/* when fork is done, various cleanup is needed in the child process.
   In particular, child must have its own connection to avoid stealing 
   data from its parent */
static void gdbserver_cleanup_in_child_after_fork(ThreadId me)
{
   dlog(1, "thread %d gdbserver_cleanup_in_child_after_fork pid %d\n",
        me, VG_(getpid) ());

   /* finish connection inheritated from parent */
   remote_finish(reset_after_fork);

   /* ensure next call to gdbserver will be considered as a brand
      new call that will initialize a fresh gdbserver. */
   if (gdbserver_called) {
      gdbserver_called = 0;
      vg_assert (gs_addresses != NULL);
      vg_assert (gs_watches != NULL);
      clear_gdbserved_addresses(/* clear only jumps */ False);
      VG_(HT_destruct) (gs_addresses, VG_(free));
      gs_addresses = NULL;
      clear_watched_addresses();
   } else {
      vg_assert (gs_addresses == NULL);
      vg_assert (gs_watches == NULL);
   }

   
   if (VG_(clo_trace_children)) {
      VG_(gdbserver_prerun_action) (me);
   }
}

/* If reason is init_reason, creates the connection resources (e.g.
      the FIFOs) to allow a gdb connection to be detected by polling
      using remote_desc_activity.
   Otherwise (other reasons):
       If connection with gdb not yet opened, opens the connection with gdb.
       reads gdb remote protocol packets and executes the requested commands.
*/
static void call_gdbserver ( ThreadId tid , CallReason reason)
{
   ThreadState*     tst = VG_(get_ThreadState)(tid);
   int stepping;
   Addr saved_pc;

   dlog(1, 
        "entering call_gdbserver %s ... pid %d tid %d status %s "
        "sched_jmpbuf_valid %d\n",
        ppCallReason (reason),
        VG_(getpid) (), tid, VG_(name_of_ThreadStatus)(tst->status),
        tst->sched_jmpbuf_valid);

   /* If we are about to die, then just run server_main() once to get
      the resume reply out and return immediately because most of the state
      of this tid and process is about to be torn down. */
   if (reason == exit_reason) {
      server_main();
      return;
   }

   vg_assert(VG_(is_valid_tid)(tid));
   saved_pc = VG_(get_IP) (tid);

   if (gdbserver_exited) {
      dlog(0, "call_gdbserver called when gdbserver_exited %d\n",
           gdbserver_exited);
      return;
   }

   if (gdbserver_called == 0) {
      vg_assert (gs_addresses == NULL);
      vg_assert (gs_watches == NULL);
      gs_addresses = VG_(HT_construct)( "gdbserved_addresses" );
      gs_watches = VG_(newXA)(gs_alloc,
                              "gdbserved_watches",
                              gs_free,
                              sizeof(GS_Watch*));
      VG_(atfork)(NULL, NULL, gdbserver_cleanup_in_child_after_fork);
   }
   vg_assert (gs_addresses != NULL);
   vg_assert (gs_watches != NULL);
   
   gdbserver_called++;

   /* call gdbserver_init if this is the first call to gdbserver. */
   if (gdbserver_called == 1)
      gdbserver_init();

   if (reason == init_reason || gdbserver_called == 1)
      remote_open(VG_(clo_vgdb_prefix));

   /* if the call reason is to initialize, then return control to
      valgrind. After this initialization, gdbserver will be called
      again either if there is an error detected by valgrind or
      if vgdb sends data to the valgrind process. */
   if (reason == init_reason) {
      return;
   }

   stepping = valgrind_single_stepping();

   server_main();

   ignore_this_break_once = valgrind_get_ignore_break_once();
   if (ignore_this_break_once)
      dlog(1, "!!! will ignore_this_break_once %s\n", 
           sym(ignore_this_break_once, /* is_code */ True));
      

   if (valgrind_single_stepping()) {
      /* we are single stepping. If we were not stepping on entry,
         then invalidate the current program counter so as to properly
         do single step. In case the program counter was changed by
         gdb, this will also invalidate the target address we will
         jump to. */
      if (!stepping && tid != 0) {
         invalidate_current_ip (tid, "m_gdbserver single step");
      }
   } else {
      /* We are not single stepping.  If we were stepping on entry,
         then clear the gdbserved addresses.  This will cause all
         these gdbserved blocks to be invalidated so that they can be
         re-translated without being gdbserved. */
      if (stepping)
         clear_gdbserved_addresses(/* clear only jumps */ True);
   }
   
   /* can't do sanity check at beginning. At least the stack
      check is not yet possible. */
   if (gdbserver_called > 1)
      VG_(sanity_check_general) (/* force_expensive */ False);

   /* If the PC has been changed by gdb, then we VG_MINIMAL_LONGJMP to
      the scheduler to execute the block of the new PC.
      Otherwise we just return to continue executing the
      current block. */
   if (VG_(get_IP) (tid) != saved_pc) {
      dlog(1, "tid %d %s PC changed from %s to %s\n",
           tid, VG_(name_of_ThreadStatus) (tst->status),
           sym(saved_pc, /* is_code */ True),
           sym(VG_(get_IP) (tid), /* is_code */ True));
      if (tst->status == VgTs_Yielding) {
         SysRes sres;
         VG_(memset)(&sres, 0, sizeof(SysRes));
         VG_(acquire_BigLock)(tid, "gdbsrv VG_MINIMAL_LONGJMP");
      }
      if (tst->sched_jmpbuf_valid) {
         /* resume scheduler */
         VG_MINIMAL_LONGJMP(tst->sched_jmpbuf);
      }
      /* else continue to run */
   }
   /* continue to run */
}

/* busy > 0 when gdbserver is currently being called.
   busy is used to to avoid vgdb invoking gdbserver
   while gdbserver by Valgrind. */
static volatile int busy = 0;

void VG_(gdbserver) ( ThreadId tid )
{
   busy++;
   /* called by the rest of valgrind for 
         --vgdb-error=0 reason
      or by scheduler "poll/debug/interrupt" reason
      or to terminate. */
   if (tid != 0) {
      call_gdbserver (tid, core_reason);
   } else {
      if (gdbserver_called == 0) {
         dlog(1, "VG_(gdbserver) called to terminate, nothing to terminate\n");
      } else if (gdbserver_exited) {
         dlog(0, "VG_(gdbserver) called to terminate again %d\n",
              gdbserver_exited);
      } else {
         gdbserver_terminate();
         gdbserver_exited++;
      }
   }
   busy--;
}

// nr of invoke_gdbserver while gdbserver is already executing.
static int interrupts_while_busy = 0;

// nr of invoke_gdbserver while gdbserver is not executing.
static int interrupts_non_busy = 0;

// nr of invoke_gdbserver when some threads are not interruptible.
static int interrupts_non_interruptible = 0;

/* When all threads are blocked in a system call, the Valgrind
   scheduler cannot poll the shared memory for gdbserver activity.  In
   such a case, vgdb will force the invokation of gdbserver using
   ptrace. To do that, vgdb 'pushes' a call to invoke_gdbserver
   on the stack using ptrace. invoke_gdbserver must not return.
   Instead, it must call give_control_back_to_vgdb.
   vgdb expects to receive a SIGSTOP, which this function generates.
   When vgdb gets this SIGSTOP, it knows invoke_gdbserver call
   is finished and can reset the Valgrind process in the state prior to
   the 'pushed call' (using ptrace again).
   This all works well. However, the user must avoid
   'kill-9ing' vgdb during such a pushed call, otherwise
   the SIGSTOP generated below will be seen by the Valgrind core,
   instead of being handled by vgdb. The OS will then handle the SIGSTOP
   by stopping the Valgrind process.
   We use SIGSTOP as this process cannot be masked. */

static void give_control_back_to_vgdb(void)
{
   /* cause a SIGSTOP to be sent to ourself, so that vgdb takes control.
      vgdb will then restore the stack so as to resume the activity
      before the ptrace (typically do_syscall_WRK). */
   if (VG_(kill)(VG_(getpid)(), VKI_SIGSTOP) != 0)
      vg_assert2(0, "SIGSTOP for vgdb could not be generated\n");

   /* If we arrive here, it means a call was pushed on the stack
      by vgdb, but during this call, vgdb and/or connection
      died. Alternatively, it is a bug in the vgdb<=>Valgrind gdbserver
      ptrace handling. */
   vg_assert2(0, 
              "vgdb did not took control. Did you kill vgdb ?\n"
              "busy %d vgdb_interrupted_tid %d\n",
              busy, vgdb_interrupted_tid);
}

/* Using ptrace calls, vgdb will force an invocation of gdbserver.
   VG_(invoke_gdbserver) is the entry point called through the
   vgdb ptrace technique. */
void VG_(invoke_gdbserver) ( int check )
{
   /* ******* Avoid non-reentrant function call from here ..... 
      till the ".... till here" below. */

   /* We need to determine the state of the various threads to decide
      if we directly invoke gdbserver or if we rather indicate to the
      scheduler to invoke the gdbserver.  To decide that, it is
      critical to avoid any "coregrind" function call as the ptrace
      might have stopped the process in the middle of this (possibly)
      non-rentrant function.  So, it is only when all threads are in
      an "interruptible" state that we can safely invoke
      gdbserver. Otherwise, we let the valgrind scheduler invoke
      gdbserver at the next poll.  This poll will be made very soon
      thanks to a call to VG_(force_vgdb_poll). */
   int n_tid;

   vg_assert (check == 0x8BADF00D);

   if (busy) {
      interrupts_while_busy++;
      give_control_back_to_vgdb();
   }
   interrupts_non_busy++;

   /* check if all threads are in an "interruptible" state.  If yes,
      we invoke gdbserver. Otherwise, we tell the scheduler to wake up
      asap. */
   for (n_tid = 1; n_tid < VG_N_THREADS; n_tid++) {
      switch (VG_(threads)[n_tid].status) {
      /* interruptible states. */
      case VgTs_WaitSys:
      case VgTs_Yielding:
         if (vgdb_interrupted_tid == 0) vgdb_interrupted_tid = n_tid;
         break;

      case VgTs_Empty:     
      case VgTs_Zombie:
         break;

      /* non interruptible states. */
      case VgTs_Init:
      case VgTs_Runnable:
         interrupts_non_interruptible++;
         VG_(force_vgdb_poll) ();
         give_control_back_to_vgdb();

      default:             vg_assert(0);
      }
   }

   /* .... till here.
      From here onwards, function calls are ok: it is
      safe to call valgrind core functions: all threads are blocked in
      a system call or are yielding or ... */
   dlog(1, "invoke_gdbserver running_tid %d vgdb_interrupted_tid %d\n",
        VG_(running_tid), vgdb_interrupted_tid);
   call_gdbserver (vgdb_interrupted_tid, vgdb_reason);
   vgdb_interrupted_tid = 0;
   dlog(1,
        "exit invoke_gdbserver running_tid %d\n", VG_(running_tid));
   give_control_back_to_vgdb();

   vg_assert2(0, "end of invoke_gdbserver reached");

}

Bool VG_(gdbserver_activity) (ThreadId tid)
{
   Bool ret;
   busy++;
   if (!gdbserver_called)
      call_gdbserver (tid, init_reason);
   switch (remote_desc_activity("VG_(gdbserver_activity)")) {
   case 0: ret = False; break;
   case 1: ret = True; break;
   case 2: 
      remote_finish(reset_after_error);
      call_gdbserver (tid, init_reason); 
      ret = False; 
      break;
   default: vg_assert (0);
   }
   busy--;
   return ret;
}

Bool VG_(gdbserver_report_signal) (Int vki_sigNo, ThreadId tid)
{
   dlog(1, "VG core calling VG_(gdbserver_report_signal) "
        "vki_nr %d %s gdb_nr %d %s tid %d\n", 
        vki_sigNo, VG_(signame)(vki_sigNo),
        target_signal_from_host (vki_sigNo),
        target_signal_to_name(target_signal_from_host (vki_sigNo)), 
        tid);

   /* if gdbserver is currently not connected, then signal
      is to be given to the process */
   if (!remote_connected()) {
      dlog(1, "not connected => pass\n");
      return True;
   }
   /* if gdb has informed gdbserver that this signal can be
      passed directly without informing gdb, then signal is
      to be given to the process. */
   if (pass_signals[target_signal_from_host(vki_sigNo)]) {
      dlog(1, "pass_signals => pass\n");
      return True;
   }
   
   /* indicate to gdbserver that there is a signal */
   gdbserver_signal_encountered (vki_sigNo);

   /* let gdbserver do some work, e.g. show the signal to the user */
   call_gdbserver (tid, signal_reason);
   
   /* ask gdbserver what is the final decision */
   if (gdbserver_deliver_signal (vki_sigNo)) {
      dlog(1, "gdbserver deliver signal\n");
      return True;
   } else {
      dlog(1, "gdbserver ignore signal\n");
      return False;
   }
}

void VG_(gdbserver_exit) (ThreadId tid, VgSchedReturnCode tids_schedretcode)
{
   dlog(1, "VG core calling VG_(gdbserver_exit) tid %d will exit\n", tid);
   if (remote_connected()) {
      /* Make sure vgdb knows we are about to die and why. */
      switch(tids_schedretcode) {
      case VgSrc_None:
         vg_assert (0);
      case VgSrc_ExitThread:
      case VgSrc_ExitProcess:
         gdbserver_process_exit_encountered ('W', VG_(threads)[tid].os_state.exitcode);
         call_gdbserver (tid, exit_reason);
         break;
      case VgSrc_FatalSig:
         gdbserver_process_exit_encountered ('X', VG_(threads)[tid].os_state.fatalsig);
         call_gdbserver (tid, exit_reason);
         break;
      default:
         vg_assert(0);
      }
   } else {
      dlog(1, "not connected\n");
   }

   /* Tear down the connection if it still exists. */
   VG_(gdbserver) (0);
}

// Check if single_stepping or if there is a break requested at iaddr. 
// If yes, call debugger
VG_REGPARM(1)
void VG_(helperc_CallDebugger) ( HWord iaddr )
{
   GS_Address* g;

   // For Vg_VgdbFull, after a fork, we might have calls to this helper
   // while gdbserver is not yet initialized.
   if (!gdbserver_called)
      return;

   if (valgrind_single_stepping() ||
       ((g = VG_(HT_lookup) (gs_addresses, (UWord)HT_addr(iaddr))) &&
        (g->kind == GS_break))) {
      if (iaddr == HT_addr(ignore_this_break_once)) {
         dlog(1, "ignoring ignore_this_break_once %s\n", 
              sym(ignore_this_break_once, /* is_code */ True));
         ignore_this_break_once = 0;
      } else {
         call_gdbserver (VG_(get_running_tid)(), break_reason);
      }
   }
}

/* software_breakpoint support --------------------------------------*/
/* When a block is instrumented for gdbserver, single step and breaks
   will be obeyed in this block.  However, if a jump to another block
   is executed while single_stepping is active, we must ensure that
   this block is also instrumented. For this, when a block is
   instrumented for gdbserver while single_stepping, the target of all
   the Jump instructions in this block will be checked to verify if
   the block is already instrumented for gdbserver.  The below will
   ensure that if not already instrumented for gdbserver, the target
   block translation containing addr will be invalidated.  The list of
   gdbserved Addr will also be kept so that translations can be
   dropped automatically by gdbserver when going out of single step
   mode.

   Call the below at translation time if the jump target is a constant. 
   Otherwise, rather use VG_(add_stmt_call_invalidate_if_not_gdbserved).

   To instrument the target exit statement, you can call
   VG_(add_stmt_call_invalidate_exit_target_if_not_gdbserved) rather
   than check the kind of target exit. */
static void VG_(invalidate_if_not_gdbserved) (Addr addr)
{
   if (valgrind_single_stepping())
      invalidate_if_jump_not_yet_gdbserved
         (addr, "gdbserver target jump (instrument)");
}

// same as VG_(invalidate_if_not_gdbserved) but is intended to be called 
// at runtime (only difference is the invalidate reason which traces 
// it is at runtime)
VG_REGPARM(1)
void VG_(helperc_invalidate_if_not_gdbserved) ( Addr addr )
{
   if (valgrind_single_stepping())
      invalidate_if_jump_not_yet_gdbserved
         (addr, "gdbserver target jump (runtime)");
}

static void VG_(add_stmt_call_invalidate_if_not_gdbserved)
     ( IRSB* sb_in,
       VexGuestLayout* layout, 
       VexGuestExtents* vge,
       IRTemp jmp, 
       IRSB* irsb)
{
   
   void*    fn;
   const HChar*   nm;
   IRExpr** args;
   Int      nargs;
   IRDirty* di;

   fn    = &VG_(helperc_invalidate_if_not_gdbserved);
   nm    = "VG_(helperc_invalidate_if_not_gdbserved)";
   args  = mkIRExprVec_1(IRExpr_RdTmp (jmp));
   nargs = 1;
   
   di = unsafeIRDirty_0_N( nargs/*regparms*/, nm, 
                           VG_(fnptr_to_fnentry)( fn ), args );

   di->nFxState = 0;

   addStmtToIRSB(irsb, IRStmt_Dirty(di));
}

/* software_breakpoint support --------------------------------------*/
/* If a tool wants to allow gdbserver to do something at Addr, then
   VG_(add_stmt_call_gdbserver) will add in IRSB a call to a helper
   function.  This helper function will check if the process must be
   stopped at the instruction Addr: either there is a break at Addr or
   the process is being single-stepped.  Typical usage of the below is to
   instrument an Ist_IMark to allow the debugger to interact at any
   instruction being executed.  As soon as there is one break in a block,
   then to allow single stepping in this block (and possible insertions
   of other breaks in the same sb_in while the process is stopped), a
   debugger statement will be inserted for all instructions of a block. */
static void VG_(add_stmt_call_gdbserver) 
     (IRSB* sb_in,                /* block being translated */
      VexGuestLayout* layout, 
      VexGuestExtents* vge,
      IRType gWordTy, IRType hWordTy,
      Addr  iaddr,                /* Addr of instruction being instrumented */
      UChar delta,                /* delta to add to iaddr to obtain IP */
      IRSB* irsb)                 /* irsb block to which call is added */
{
   void*    fn;
   const HChar*   nm;
   IRExpr** args;
   Int      nargs;
   IRDirty* di;

   /* first store the address in the program counter so that the check
      done by VG_(helperc_CallDebugger) will be based on the correct
      program counter.  We might make this more efficient by rather
      searching for assignement to program counter and instrumenting
      that but the below is easier and I guess that the optimiser will
      remove the redundant store. And in any case, when debugging a
      piece of code, the efficiency requirement is not critical: very
      few blocks will be instrumented for debugging. */

   /* For platforms on which the IP can differ from the addr of the instruction
      being executed, we need to add the delta to obtain the IP.
      This IP will be given to gdb (e.g. if a breakpoint is put at iaddr).

      For ARM, this delta will ensure that the thumb bit is set in the
      IP when executing thumb code. gdb uses this thumb bit a.o.
      to properly guess the next IP for the 'step' and 'stepi' commands. */
   vg_assert(delta <= 1);
   addStmtToIRSB(irsb, IRStmt_Put(layout->offset_IP ,
                                  mkIRExpr_HWord(iaddr + (Addr)delta)));

   fn    = &VG_(helperc_CallDebugger);
   nm    = "VG_(helperc_CallDebugger)";
   args  = mkIRExprVec_1(mkIRExpr_HWord (iaddr));
   nargs = 1;
   
   di = unsafeIRDirty_0_N( nargs/*regparms*/, nm, 
                           VG_(fnptr_to_fnentry)( fn ), args );

   /* Note: in fact, a debugger call can read whatever register
      or memory. It can also write whatever register or memory.
      So, in theory, we have to indicate the whole universe
      can be read and modified. It is however not critical
      to indicate precisely what is being read/written
      as such indications are needed for tool error detection
      and we do not want to have errors being detected for
      gdb interactions. */
   
   di->nFxState = 2;
   di->fxState[0].fx        = Ifx_Read;
   di->fxState[0].offset    = layout->offset_SP;
   di->fxState[0].size      = layout->sizeof_SP;
   di->fxState[0].nRepeats  = 0;
   di->fxState[0].repeatLen = 0;
   di->fxState[1].fx        = Ifx_Modify;
   di->fxState[1].offset    = layout->offset_IP;
   di->fxState[1].size      = layout->sizeof_IP;
   di->fxState[1].nRepeats  = 0;
   di->fxState[1].repeatLen = 0;

   addStmtToIRSB(irsb, IRStmt_Dirty(di));

}


/* Invalidate the target of the exit if needed:
   If target is constant, it is invalidated at translation time.
   Otherwise, a call to a helper function is generated to invalidate
   the translation at run time.
   The below is thus calling either VG_(invalidate_if_not_gdbserved)
   or VG_(add_stmt_call_invalidate_if_not_gdbserved).  */
static void VG_(add_stmt_call_invalidate_exit_target_if_not_gdbserved)
   (IRSB* sb_in,
    VexGuestLayout* layout,
    VexGuestExtents* vge,
    IRType gWordTy,
    IRSB* irsb)
{
   if (sb_in->next->tag == Iex_Const) {
     VG_(invalidate_if_not_gdbserved) (gWordTy == Ity_I64 ?
                                       sb_in->next->Iex.Const.con->Ico.U64 
                                       : sb_in->next->Iex.Const.con->Ico.U32);
   } else if (sb_in->next->tag == Iex_RdTmp) {
     VG_(add_stmt_call_invalidate_if_not_gdbserved)
       (sb_in, layout, vge, sb_in->next->Iex.RdTmp.tmp, irsb);
   } else {
     vg_assert (0); /* unexpected expression tag in exit. */
   }
}

IRSB* VG_(instrument_for_gdbserver_if_needed)
     (IRSB* sb_in,
      VexGuestLayout* layout,
      VexGuestExtents* vge,
      IRType gWordTy, IRType hWordTy)
{
   IRSB* sb_out;
   Int i;
   const VgVgdb instr_needed = VG_(gdbserver_instrumentation_needed) (vge);

   if (instr_needed == Vg_VgdbNo)
     return sb_in;


   /* here, we need to instrument for gdbserver */
   sb_out = deepCopyIRSBExceptStmts(sb_in);

   for (i = 0; i < sb_in->stmts_used; i++) {
      IRStmt* st = sb_in->stmts[i];
      
      if (!st || st->tag == Ist_NoOp) continue;
      
      if (st->tag == Ist_Exit && instr_needed == Vg_VgdbYes) {
        VG_(invalidate_if_not_gdbserved) 
          (hWordTy == Ity_I64 ? 
           st->Ist.Exit.dst->Ico.U64 : 
           st->Ist.Exit.dst->Ico.U32);
      }
      addStmtToIRSB( sb_out, st );
      if (st->tag == Ist_IMark) {
         /* For an Ist_Mark, add a call to debugger. */
         switch (instr_needed) {
         case Vg_VgdbNo: vg_assert (0);
         case Vg_VgdbYes:
         case Vg_VgdbFull:
            VG_(add_stmt_call_gdbserver) ( sb_in, layout, vge,
                                           gWordTy, hWordTy,
                                           st->Ist.IMark.addr,
                                           st->Ist.IMark.delta,
                                           sb_out);
            /* There is an optimisation possible here for Vg_VgdbFull:
               Put a guard ensuring we only call gdbserver if 'FullCallNeeded'.
               FullCallNeeded would be set to 1 we have just switched on
               Single Stepping or have just encountered a watchpoint
               or have just inserted a breakpoint.
               (as gdb by default removes and re-insert breakpoints), we would
               need to also implement the notion of 'breakpoint pending removal'
               to remove at the next 'continue/step' packet. */
            break;
         default: vg_assert (0);
         }
      }
   }

   if (instr_needed == Vg_VgdbYes) {
      VG_(add_stmt_call_invalidate_exit_target_if_not_gdbserved) (sb_in,
                                                                  layout, vge,
                                                                  gWordTy,
                                                                  sb_out);
   }

   return sb_out;
}

struct mon_out_buf {
   HChar buf[DATASIZ+1];
   int next;
   UInt ret;
};

static void mon_out (HChar c, void *opaque)
{
   struct mon_out_buf *b = (struct mon_out_buf *) opaque;
   b->ret++;
   b->buf[b->next] = c;
   b->next++;
   if (b->next == DATASIZ) {
      b->buf[b->next] = '\0';
      monitor_output(b->buf);
      b->next = 0;
   }
}
UInt VG_(gdb_printf) ( const HChar *format, ... )
{
   struct mon_out_buf b;

   b.next = 0;
   b.ret = 0;
   
   va_list vargs;
   va_start(vargs, format);
   VG_(vcbprintf) (mon_out, &b, format, vargs);
   va_end(vargs);
   
   if (b.next > 0) {
      b.buf[b.next] = '\0';
      monitor_output(b.buf);
   }
   return b.ret;
}

Int VG_(keyword_id) (const HChar* keywords, const HChar* input_word,
                     kwd_report_error report)
{
   const Int il = (input_word == NULL ? 0 : VG_(strlen) (input_word));
   HChar  iw[il+1];
   HChar  kwds[VG_(strlen)(keywords)+1];
   HChar  *kwdssaveptr;

   HChar* kw; /* current keyword, its length, its position */
   Int   kwl;
   Int   kpos = -1;

   Int pass; 
   /* pass 0 = search, optional pass 1 = output message multiple matches */

   Int pass1needed = 0;

   Int partial_match = -1;
   Int full_match = -1;

   if (input_word == NULL) {
      iw[0] = 0;
      partial_match = 0; /* to force an empty string to cause an error */
   } else {
      VG_(strcpy) (iw, input_word);
   }

   for (pass = 0; pass < 2; pass++) {
      VG_(strcpy) (kwds, keywords);
      if (pass == 1)
         VG_(gdb_printf) ("%s can match", 
                          (il == 0 ? "<empty string>" : iw));
      for (kw = VG_(strtok_r) (kwds, " ", &kwdssaveptr); 
           kw != NULL; 
           kw = VG_(strtok_r) (NULL, " ", &kwdssaveptr)) {
         kwl = VG_(strlen) (kw);
         kpos++;
         
         if (il > kwl) {
            ; /* ishtar !~ is */
         } else if (il == kwl) {
            if (VG_(strcmp) (kw, iw) == 0) {
               /* exact match */
               if (pass == 1)
                  VG_(gdb_printf) (" %s", kw);
               if (full_match != -1)
                  pass1needed++;
               full_match = kpos;
            }
         } else {
            /* il < kwl */
            if (VG_(strncmp) (iw, kw, il) == 0) {
               /* partial match */
               if (pass == 1)
                  VG_(gdb_printf) (" %s", kw);
               if (partial_match != -1)
                  pass1needed++;
               partial_match = kpos;
            }
         }
      }
      /* check for success or for no match at all */
      if (pass1needed == 0) {
         if (full_match != -1) {
            return full_match;
         } else {
            if (report == kwd_report_all && partial_match == -1) {
               VG_(gdb_printf) ("%s does not match any of '%s'\n", 
                                iw, keywords);
            }
            return partial_match;
         }
      }

      /* here we have duplicated match error */
      if (pass == 1 || report == kwd_report_none) {
         if (report != kwd_report_none) {
            VG_(gdb_printf) ("\n");
         }
         if (partial_match != -1 || full_match != -1)
            return -2;
         else
            return -1;
      }
   }
   /* UNREACHED */
   vg_assert (0);
}

/* True if string can be a 0x number */
static Bool is_zero_x (const HChar *s)
{
   if (strlen (s) >= 3 && s[0] == '0' && s[1] == 'x')
      return True;
   else
      return False;
}

/* True if string can be a 0b number */
static Bool is_zero_b (const HChar *s)
{
   if (strlen (s) >= 3 && s[0] == '0' && s[1] == 'b')
      return True;
   else
      return False;
}

Bool VG_(strtok_get_address_and_size) (Addr* address, 
                                       SizeT* szB, 
                                       HChar **ssaveptr)
{
   HChar* wa;
   HChar* ws;
   HChar* endptr;
   const HChar *ppc;

   wa = VG_(strtok_r) (NULL, " ", ssaveptr);
   ppc = wa;
   if (ppc == NULL || !VG_(parse_Addr) (&ppc, address)) {
      VG_(gdb_printf) ("missing or malformed address\n");
      *address = (Addr) 0;
      *szB = 0;
      return False;
   }
   ws = VG_(strtok_r) (NULL, " ", ssaveptr);
   if (ws == NULL) {
      /* Do nothing, i.e. keep current value of szB. */ ;
   } else if (is_zero_x (ws)) {
      *szB = VG_(strtoull16) (ws, &endptr);
   } else if (is_zero_b (ws)) {
      Int j;
      HChar *parsews = ws;
      Int n_bits = VG_(strlen) (ws) - 2;
      *szB = 0;
      ws = NULL; // assume the below loop gives a correct nr.
      for (j = 0; j < n_bits; j++) {
         if      ('0' == parsews[j+2]) { /* do nothing */ }
         else if ('1' == parsews[j+2]) *szB |= (1 << (n_bits-j-1));
         else {
            /* report malformed binary integer */
            ws = parsews;
            endptr = ws + j + 2;
            break;
         }
      }
   } else {
      *szB = VG_(strtoull10) (ws, &endptr);
   }

   if (ws != NULL && *endptr != '\0') {
      VG_(gdb_printf) ("malformed integer, expecting "
                       "hex 0x..... or dec ...... or binary .....b\n");
      *address = (Addr) 0;
      *szB = 0;
      return False;
   }
   return True;
}

void VG_(gdbserver_status_output)(void)
{
   const int nr_gdbserved_addresses 
      = (gs_addresses == NULL ? -1 : VG_(HT_count_nodes) (gs_addresses));
   const int nr_watchpoints
      = (gs_watches == NULL ? -1 : (int) VG_(sizeXA) (gs_watches));
   remote_utils_output_status();
   VG_(umsg)
      ("nr of calls to gdbserver: %d\n"
       "single stepping %d\n"
       "interrupts intr_tid %d gs_non_busy %d gs_busy %d tid_non_intr %d\n"
       "gdbserved addresses %d (-1 = not initialized)\n"
       "watchpoints %d (-1 = not initialized)\n"
       "vgdb-error %d\n"
       "hostvisibility %s\n",
       gdbserver_called,
       valgrind_single_stepping(),
       
       vgdb_interrupted_tid, 
       interrupts_non_busy, 
       interrupts_while_busy,
       interrupts_non_interruptible,
       
       nr_gdbserved_addresses,
       nr_watchpoints,
       VG_(dyn_vgdb_error),
       hostvisibility ? "yes" : "no");
}
