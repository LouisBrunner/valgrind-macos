
/*--------------------------------------------------------------------*/
/*--- Darwin-specific syscalls, etc.              syswrap-darwin.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2017 Apple Inc.
      Greg Parker  gparker@apple.com

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGO_darwin)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_debuglog.h"
#include "pub_core_debuginfo.h"    // VG_(di_notify_*)
#include "pub_core_transtab.h"     // VG_(discard_translations)
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_machine.h"      // VG_(get_SP)
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_oset.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_stacks.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_wordfm.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-darwin.h"    /* for decls of darwin-ish wrappers */
#include "priv_syswrap-main.h"

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <semaphore.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

#define msgh_request_port      msgh_remote_port
#define msgh_reply_port        msgh_local_port
#define BOOTSTRAP_MAX_NAME_LEN                  128
typedef HChar name_t[BOOTSTRAP_MAX_NAME_LEN];

typedef uint64_t mig_addr_t;


// Saved ports
static mach_port_t vg_host_port = 0;
static mach_port_t vg_task_port = 0;
static mach_port_t vg_bootstrap_port = 0;

// Run a thread from beginning to end and return the thread's
// scheduler-return-code.
static VgSchedReturnCode thread_wrapper(Word /*ThreadId*/ tidW)
{
   VgSchedReturnCode ret;
   ThreadId     tid = (ThreadId)tidW;
   Int          lwpid = VG_(gettid)();
   ThreadState* tst = VG_(get_ThreadState)(tid);

   VG_(debugLog)(1, "syswrap-darwin",
                    "thread_wrapper(tid=%u,lwpid=%d): entry\n",
                    tid, lwpid);

   vg_assert(tst->status == VgTs_Init);

   /* make sure we get the CPU lock before doing anything significant */
   VG_(acquire_BigLock)(tid, "thread_wrapper");

   if (0)
      VG_(printf)("thread tid %u started: stack = %p\n",
                  tid, (void *)&tid);

   /* Make sure error reporting is enabled in the new thread. */
   tst->err_disablement_level = 0;

   VG_TRACK(pre_thread_first_insn, tid);

   tst->os_state.lwpid = lwpid;
   tst->os_state.threadgroup = VG_(getpid)();

   /* Thread created with all signals blocked; scheduler will set the
      appropriate mask */

   ret = VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));

   vg_assert(tst->status == VgTs_Runnable);
   vg_assert(VG_(is_running_thread)(tid));

   VG_(debugLog)(1, "syswrap-darwin",
                    "thread_wrapper(tid=%u,lwpid=%d): done\n",
                    tid, lwpid);

   /* Return to caller, still holding the lock. */
   return ret;
}



/* Allocate a stack for this thread, if it doesn't already have one.
   Returns the initial stack pointer value to use, or 0 if allocation
   failed. */

Addr allocstack ( ThreadId tid )
{
   ThreadState* tst = VG_(get_ThreadState)(tid);
   VgStack*     stack;
   Addr         initial_SP;

   /* Either the stack_base and stack_init_SP are both zero (in which
      case a stack hasn't been allocated) or they are both non-zero,
      in which case it has. */

   if (tst->os_state.valgrind_stack_base == 0)
      vg_assert(tst->os_state.valgrind_stack_init_SP == 0);

   if (tst->os_state.valgrind_stack_base != 0)
      vg_assert(tst->os_state.valgrind_stack_init_SP != 0);

   /* If no stack is present, allocate one. */

   if (tst->os_state.valgrind_stack_base == 0) {
      stack = VG_(am_alloc_VgStack)( &initial_SP );
      if (stack) {
         tst->os_state.valgrind_stack_base    = (Addr)stack;
         tst->os_state.valgrind_stack_init_SP = initial_SP;
      }
   }

   VG_(debugLog)( 2, "syswrap-darwin", "stack for tid %u at %p; init_SP=%p\n",
                   tid, 
                   (void*)tst->os_state.valgrind_stack_base, 
                   (void*)tst->os_state.valgrind_stack_init_SP );

   vg_assert(VG_IS_32_ALIGNED(tst->os_state.valgrind_stack_init_SP));
   
   return tst->os_state.valgrind_stack_init_SP;
}


void find_stack_segment(ThreadId tid, Addr sp)
{
   ML_(guess_and_register_stack) (sp, VG_(get_ThreadState)(tid));
}


/* Run a thread all the way to the end, then do appropriate exit actions
   (this is the last-one-out-turn-off-the-lights bit). 
*/
static void run_a_thread_NORETURN ( Word tidW )
{
   Int               c;
   VgSchedReturnCode src;
   ThreadId          tid = (ThreadId)tidW;
   ThreadState*      tst;

   VG_(debugLog)(1, "syswrap-darwin", 
                    "run_a_thread_NORETURN(tid=%u): pre-thread_wrapper\n",
                    tid);

   tst = VG_(get_ThreadState)(tid);
   vg_assert(tst);

   /* Run the thread all the way through. */
   src = thread_wrapper(tid);  

   VG_(debugLog)(1, "syswrap-darwin", 
                    "run_a_thread_NORETURN(tid=%u): post-thread_wrapper\n",
                    tid);

   c = VG_(count_living_threads)();
   vg_assert(c >= 1); /* stay sane */

   /* Deregister thread's stack. */
   if (tst->os_state.stk_id != NULL_STK_ID)
      VG_(deregister_stack)(tst->os_state.stk_id);

   // Tell the tool this thread is exiting
   VG_TRACK( pre_thread_ll_exit, tid );

   /* If the thread is exiting with errors disabled, complain loudly;
      doing so is bad (does the user know this has happened?)  Also,
      in all cases, be paranoid and clear the flag anyway so that the
      thread slot is safe in this respect if later reallocated.  This
      should be unnecessary since the flag should be cleared when the
      slot is reallocated, in thread_wrapper(). */
   if (tst->err_disablement_level > 0) {
      VG_(umsg)(
         "WARNING: exiting thread has error reporting disabled.\n"
         "WARNING: possibly as a result of some mistake in the use\n"
         "WARNING: of the VALGRIND_DISABLE_ERROR_REPORTING macros.\n"
      );
      VG_(debugLog)(
         1, "syswrap-darwin", 
            "run_a_thread_NORETURN(tid=%u): "
            "WARNING: exiting thread has err_disablement_level = %u\n",
            tid, tst->err_disablement_level
      );
   }
   tst->err_disablement_level = 0;

   if (c == 1) {

      VG_(debugLog)(1, "syswrap-darwin", 
                       "run_a_thread_NORETURN(tid=%u): "
                          "last one standing\n",
                          tid);

      /* We are the last one standing.  Keep hold of the lock and
         carry on to show final tool results, then exit the entire system. 
         Use the continuation pointer set at startup in m_main. */
      ( * VG_(address_of_m_main_shutdown_actions_NORETURN) ) (tid, src);

   } else {

      mach_msg_header_t msg;

      VG_(debugLog)(1, "syswrap-darwin", 
                       "run_a_thread_NORETURN(tid=%u): "
                          "not last one standing\n",
                          tid);

      /* OK, thread is dead, but others still exist.  Just exit. */

      /* This releases the run lock */
      VG_(exit_thread)(tid);
      vg_assert(tst->status == VgTs_Zombie);

      /* tid is now invalid. */

      // GrP fixme exit race
      msg.msgh_bits = MACH_MSGH_BITS(17, MACH_MSG_TYPE_MAKE_SEND_ONCE);
      msg.msgh_request_port = VG_(gettid)();
      msg.msgh_reply_port = 0;
      msg.msgh_id = 3600;  // thread_terminate
      
      tst->status = VgTs_Empty;
      // GrP fixme race here! new thread may claim this V thread stack 
      // before we get out here!
      // GrP fixme use bsdthread_terminate for safe cleanup?
      mach_msg(&msg, MACH_SEND_MSG|MACH_MSG_OPTION_NONE, 
               sizeof(msg), 0, 0, MACH_MSG_TIMEOUT_NONE, 0);
      
      // DDD: This is reached sometimes on none/tests/manythreads, maybe
      // because of the race above.
      VG_(core_panic)("Thread exit failed?\n");
   }
   
   /*NOTREACHED*/
   vg_assert(0);
}


/* Allocate a stack for the main thread, and run it all the way to the
   end.  Although we already have a working VgStack
   (VG_(interim_stack)) it's better to allocate a new one, so that
   overflow detection works uniformly for all threads.
*/
void VG_(main_thread_wrapper_NORETURN)(ThreadId tid)
{
   Addr sp;
   VG_(debugLog)(1, "syswrap-darwin", 
                    "entering VG_(main_thread_wrapper_NORETURN)\n");

   sp = allocstack(tid);

   /* If we can't even allocate the first thread's stack, we're hosed.
      Give up. */
   vg_assert2(sp != 0, "Cannot allocate main thread's stack.");

   /* shouldn't be any other threads around yet */
   vg_assert( VG_(count_living_threads)() == 1 );
   
   call_on_new_stack_0_1( 
      (Addr)sp,             /* stack */
      0,                     /*bogus return address*/
      run_a_thread_NORETURN,  /* fn to call */
      (Word)tid              /* arg to give it */
   );

   /*NOTREACHED*/
   vg_assert(0);
}


void start_thread_NORETURN ( Word arg )
{
   ThreadState* tst = (ThreadState*)arg;
   ThreadId     tid = tst->tid;

   run_a_thread_NORETURN ( (Word)tid );
   /*NOTREACHED*/
   vg_assert(0);
}


void VG_(cleanup_thread) ( ThreadArchState* arch )
{
}  


/* ---------------------------------------------------------------------
   Message reporting, with duplicate removal
   ------------------------------------------------------------------ */

static WordFM* decaying_string_table = NULL; /* HChar* -> UWord */

static Word decaying_string_table_cmp ( UWord s1, UWord s2 ) {
   return (Word)VG_(strcmp)( (HChar*)s1, (HChar*)s2 );
}

static void log_decaying ( const HChar* format, ... ) PRINTF_CHECK(1, 2);
static void log_decaying ( const HChar* format, ... )
{
   // get the message into a stack-allocated string.
   HChar buf[256];
   VG_(memset)(buf, 0, sizeof(buf));
   va_list vargs;
   va_start(vargs,format);
   (void) VG_(vsnprintf)(buf, sizeof(buf), format, vargs);
   va_end(vargs);
   buf[sizeof(buf)-1] = 0;

   // Now see if it already exists in the table of strings that we have.
   if (!decaying_string_table) {
      decaying_string_table
         = VG_(newFM)( VG_(malloc), "syswrap-darwin.pd.1",
                       VG_(free), decaying_string_table_cmp );
   }

   const HChar* key = NULL;
   UWord        val = 0;
   if (!VG_(lookupFM)(decaying_string_table,
                      (UWord*)&key, &val, (UWord)&buf[0])) {
      // We haven't seen this string before, so strdup it and add
      // it to the table.
      vg_assert(key == NULL && val == 0);
      key = VG_(strdup)("syswrap-darwin.pd.2", buf);
      VG_(addToFM)(decaying_string_table, (UWord)key, (UWord)0);
   }

   vg_assert(key != NULL && key != &buf[0]);

   // So, finally, |key| is in the tree, and |val| is what it is
   // currently associated with.  Increment that counter.
   val++;
   Bool b = VG_(addToFM)(decaying_string_table, (UWord)key, (UWord)val);
   vg_assert(b);
   
   if (-1 != VG_(log2)( (UInt)val )) {
      if (val == 1)
         VG_(dmsg)("%s\n", key);
      else
         VG_(dmsg)("%s (repeated %lu times)\n", key, val);
   }
}


/* ---------------------------------------------------------------------
   Mach port tracking (based on syswrap-generic's fd tracker)
   ------------------------------------------------------------------ */

/* One of these is allocated for each open port.  */
typedef struct OpenPort
{
   mach_port_t port;
   mach_port_type_t type;         /* right type(s) */
   Int send_count;                /* number of send rights */
   HChar *name;                   /* bootstrap name or NULL */
   ExeContext *where;             /* first allocation only */
   struct OpenPort *next, *prev;
} OpenPort;

// strlen("0x12345678")
#define PORT_STRLEN (2+2*sizeof(mach_port_t))

/* List of allocated ports. */
static OpenPort *allocated_ports;

/* Count of open ports. */
static Int allocated_port_count = 0;

/* Create an entry for |port|, with no other info.  Assumes it doesn't
   already exist. */
static void port_create_vanilla(mach_port_t port)
{
   OpenPort* op
     = VG_(calloc)("syswrap-darwin.port_create_vanilla", sizeof(OpenPort), 1);
   op->port = port;
   /* Add it to the list. */
   op->next = allocated_ports;
   if (allocated_ports) allocated_ports->prev = op;
   allocated_ports = op;
   allocated_port_count++;
}

__attribute__((unused))
static Bool port_exists(mach_port_t port)
{
   OpenPort *i;

   /* Check to see if this port is already open. */
   i = allocated_ports;
   while (i) {
      if (i->port == port) {
         return True;
      }
      i = i->next;
   }
   
   return False;
}

static OpenPort *info_for_port(mach_port_t port)
{
   OpenPort *i;
   if (!port) return NULL;

   i = allocated_ports;
   while (i) {
      if (i->port == port) {
         return i;
      }
      i = i->next;
   }

   return NULL;
}


// Give a port a name, without changing its refcount
// GrP fixme don't override name if it already has a specific one
__private_extern__ void assign_port_name(mach_port_t port, const HChar *name)
{
   OpenPort *i;
   if (!port) return;
   vg_assert(name);

   i = info_for_port(port);
   vg_assert(i);

   if (i->name) VG_(free)(i->name);
   i->name = 
       VG_(malloc)("syswrap-darwin.mach-port-name", 
                   VG_(strlen)(name) + PORT_STRLEN + 1);
   VG_(sprintf)(i->name, name, port);
}


// Return the name of the given port or "UNKNOWN 0x1234" if not known.
static const HChar *name_for_port(mach_port_t port)
{
   static HChar buf[8 + PORT_STRLEN + 1];
   OpenPort *i;

   // hack
   if (port == VG_(gettid)()) return "mach_thread_self()";
   if (port == 0) return "NULL";

   i = allocated_ports;
   while (i) {
      if (i->port == port) {
         return i->name;
      }
      i = i->next;
   }

   VG_(sprintf)(buf, "NONPORT-%#x", port);
   return buf;
}

/* Note the fact that a port was just deallocated. */

static
void record_port_mod_refs(mach_port_t port, mach_port_type_t right, Int delta)
{
   OpenPort *i = allocated_ports;
   if (!port) return;

   while(i) {
      if(i->port == port) {
         vg_assert(right != MACH_PORT_TYPE_DEAD_NAME);
         if (right & MACH_PORT_TYPE_SEND) {
            // send rights are refcounted
            if (delta == INT_MIN) delta = -i->send_count; // INT_MIN == destroy
            i->send_count += delta;
            if (i->send_count > 0) i->type |= MACH_PORT_TYPE_SEND;
            else i->type &= ~MACH_PORT_TYPE_SEND;
         } 
         right = right & ~MACH_PORT_TYPE_SEND;
         if (right) {
            // other rights are not refcounted
            if (delta > 0) {
               i->type |= right;
            } else if (delta < 0) {
               i->type &= ~right;
            }
         }

         if (i->type != 0) return;

         // Port has no rights left. Kill it.
         // VG_(printf)("deleting port %p %s", i->port, i->name);
         if(i->prev)
            i->prev->next = i->next;
         else
            allocated_ports = i->next;
         if(i->next)
            i->next->prev = i->prev;
         if(i->name) 
            VG_(free) (i->name);
         VG_(free) (i);
         allocated_port_count--;
         return;
      }
      i = i->next;
   }

   VG_(printf)("UNKNOWN Mach port modified (port %#x delta %d)\n", port, delta);
}

static 
void record_port_insert_rights(mach_port_t port, mach_msg_type_name_t type)
{
   switch (type) {
   case MACH_MSG_TYPE_PORT_NAME:
      // this task has no rights for the name
      break;
   case MACH_MSG_TYPE_PORT_RECEIVE:
      // this task gets receive rights
      record_port_mod_refs(port, MACH_PORT_TYPE_RECEIVE, 1);
      break;
   case MACH_MSG_TYPE_PORT_SEND:
      // this task gets a send right
      record_port_mod_refs(port, MACH_PORT_TYPE_SEND, 1);
      break;
   case MACH_MSG_TYPE_PORT_SEND_ONCE:
      // this task gets send-once rights
      record_port_mod_refs(port, MACH_PORT_TYPE_SEND_ONCE, 1);
      break;
   default:
      vg_assert(0);
      break;
   }
}

static 
void record_port_dealloc(mach_port_t port)
{
   // deletes 1 send or send-once right (port can't have both)
   record_port_mod_refs(port, MACH_PORT_TYPE_SEND_RIGHTS, -1);
}

static 
void record_port_destroy(mach_port_t port)
{
   // deletes all rights to port
   record_port_mod_refs(port, MACH_PORT_TYPE_ALL_RIGHTS, INT_MIN);
}


/* Note the fact that a Mach port was just allocated or transferred.
   If the port is already known, increment its reference count. */
void record_named_port(ThreadId tid, mach_port_t port, 
                       mach_port_right_t right, const HChar *name)
{
   OpenPort *i;
   if (!port) return;

   /* Check to see if this port is already open. */
   i = allocated_ports;
   while (i) {
      if (i->port == port) {
         if (right != -1) record_port_mod_refs(port, MACH_PORT_TYPE(right), 1);
         return;
      }
      i = i->next;
   }

   /* Not already one: allocate an OpenPort */
   if (i == NULL) {
      i = VG_(malloc)("syswrap-darwin.mach-port", sizeof(OpenPort));

      i->prev = NULL;
      i->next = allocated_ports;
      if(allocated_ports) allocated_ports->prev = i;
      allocated_ports = i;
      allocated_port_count++;

      i->port = port;
      i->where = (tid == -1) ? NULL : VG_(record_ExeContext)(tid, 0);
      i->name = NULL;
      if (right != -1) {
         i->type = MACH_PORT_TYPE(right);
         i->send_count = (right == MACH_PORT_RIGHT_SEND) ? 1 : 0;
      } else {
         i->type = 0;
         i->send_count = 0;
      }
      
      assign_port_name(port, name);
   }
}


// Record opening of a nameless port.
static void record_unnamed_port(ThreadId tid, mach_port_t port, mach_port_right_t right)
{
   record_named_port(tid, port, right, "unnamed-%p");
}


/* Dump summary of open Mach ports, like VG_(show_open_fds) */
void VG_(show_open_ports)(void)
{
   OpenPort *i;
   
   VG_(message)(Vg_UserMsg, 
                "MACH PORTS: %d open at exit.\n", allocated_port_count);

   for (i = allocated_ports; i; i = i->next) {
      if (i->name) {
         VG_(message)(Vg_UserMsg, "Open Mach port 0x%x: %s\n", i->port,
                      i->name);
      } else {
         VG_(message)(Vg_UserMsg, "Open Mach port 0x%x\n", i->port);
      }

      if (i->where) {
         VG_(pp_ExeContext)(i->where);
         VG_(message)(Vg_UserMsg, "\n");
      }
   }

   VG_(message)(Vg_UserMsg, "\n");
}


/* ---------------------------------------------------------------------
   sync_mappings
   ------------------------------------------------------------------ */

typedef
   enum { CheckAlways=1, CheckEvery20, CheckNever }
   CheckHowOften;

static const HChar* show_CheckHowOften ( CheckHowOften cho ) {
   switch (cho) {
      case CheckAlways:   return "Always ";
      case CheckEvery20:  return "Every20";
      case CheckNever:    return "Never  ";
      default: vg_assert(0);
   }
}

/* Statistics for one particular resync-call set of arguments,
   as specified by key1, key2 and key3. */
typedef
   struct {
      CheckHowOften cho;
      const HChar*  key1;
      const HChar*  key2;
      UWord         key3;
      ULong         n_checks;
      ULong         n_mappings_added;
      ULong         n_mappings_removed;
   }
   SyncStats;

static Bool cmp_eqkeys_SyncStats ( SyncStats* ss1, SyncStats* ss2 ) {
   return ss1->key3 == ss2->key3
          && 0 == VG_(strcmp)(ss1->key1, ss2->key1)
          && 0 == VG_(strcmp)(ss1->key2, ss2->key2);
}

/* The filter data. */
#define N_SYNCSTATS 1000
static Int       syncstats_used = 0;
static SyncStats syncstats[N_SYNCSTATS];

/* Statistics overall, for the filter. */
static ULong n_syncsRequested = 0; // Total number requested
static ULong n_syncsPerformed = 0; // Number carried out (the rest skipped)


static
void update_syncstats ( CheckHowOften cho,
                        const HChar* key1, const HChar* key2,
                        UWord key3,
                        UInt n_mappings_added, UInt n_mappings_removed )
{
   SyncStats dummy = { CheckAlways, key1, key2, key3, 0, 0, 0 };
   Int i;
   for (i = 0; i < syncstats_used; i++) {
      if (cmp_eqkeys_SyncStats(&syncstats[i], &dummy))
         break;
   }
   vg_assert(i >= 0 && i <= syncstats_used);
   if (i == syncstats_used) {
      // alloc new
      vg_assert(syncstats_used < N_SYNCSTATS);
      syncstats_used++;
      syncstats[i] = dummy;
      syncstats[i].cho = cho;
   }
   vg_assert(cmp_eqkeys_SyncStats(&syncstats[i], &dummy));
   syncstats[i].n_checks++;
   syncstats[i].n_mappings_added   += (ULong)n_mappings_added;
   syncstats[i].n_mappings_removed += (ULong)n_mappings_removed;
   // reorder
   static UInt reorder_ctr = 0;
   if (i > 0 && 0 == (1 & reorder_ctr++)) {
      SyncStats tmp = syncstats[i-1];
      syncstats[i-1] = syncstats[i];
      syncstats[i] = tmp;
   }
}


static void maybe_show_syncstats ( void )
{
   Int i;

   // display
   if (0 == (n_syncsRequested & 0xFF)) {
      VG_(printf)("Resync filter: %'llu requested, %'llu performed (%llu%%)\n",
                  n_syncsRequested, n_syncsPerformed,
                  (100 * n_syncsPerformed) / 
                     (n_syncsRequested == 0 ? 1 : n_syncsRequested));
      for (i = 0; i < syncstats_used; i++) {
         if (i >= 40) break; // just show the top 40
         VG_(printf)("  [%3d] (%s) upd %6llu  diff %4llu+,%3llu-"
                     "  %s %s 0x%08llx\n",
                     i, show_CheckHowOften(syncstats[i].cho),
                     syncstats[i].n_checks, 
                     syncstats[i].n_mappings_added,
                     syncstats[i].n_mappings_removed,
                     syncstats[i].key1, syncstats[i].key2,
                     (ULong)syncstats[i].key3);
      }
      if (i < syncstats_used) {
        VG_(printf)("  and %d more entries not shown.\n", syncstats_used - i);
      }
      VG_(printf)("\n");
   }
}


Bool ML_(sync_mappings)(const HChar* when, const HChar* where, UWord num)
{
   // If VG(clo_resync_filter) == 0, the filter is disabled, and
   //   we must always honour the resync request.
   //
   // If VG(clo_resync_filter) == 1, the filter is enabled,
   //   so we try to avoid doing the sync if possible, but keep
   //   quiet.
   //
   // If VG(clo_resync_filter) == 2, the filter is enabled,
   //   so we try to avoid doing the sync if possible, and also
   //   periodically show stats, so that the filter can be updated.
   //   (by hand).

   if (VG_(clo_resync_filter) >= 2)
      maybe_show_syncstats();

   n_syncsRequested++;

   // Usually the number of segments added/removed in a single call is very
   // small e.g. 1.  But it sometimes gets up to at least 100 or so (eg. for
   // Quicktime).  So we use a repeat-with-bigger-buffers-until-success model,
   // because we can't do dynamic allocation within VG_(get_changed_segments),
   // because it's in m_aspacemgr.
   ChangedSeg* css = NULL;
   Int         css_size;
   Int         css_used;
   Int         i;
   Bool        ok;

   // -------------- BEGIN resync-filter-kludge --------------
   //
   // Some kludges to try and avoid the worst case cost hit of doing
   // zillions of resyncs (huge).  The idea is that many of the most
   // common resyncs never appear to cause a delta, so we just ignore
   // them (CheckNever).  Then, a bunch of them also happen a lot, but
   // only very occasionally cause a delta.  We resync after 20 of those
   // (CheckEvery20).  Finally, the rest form a long tail, so we always
   // resync after those (CheckAlways).
   //
   // Assume this is kernel-version and word-size specific, so develop
   // filters accordingly.  This might be overly conservative --
   // I don't know.

#  define STREQ(_s1, _s2) (0 == VG_(strcmp)((_s1),(_s2)))
   Bool when_in    = STREQ(when,  "in");
   Bool when_after = STREQ(when,  "after");
   Bool where_mmr  = STREQ(where, "mach_msg_receive");
   Bool where_mmrU = STREQ(where, "mach_msg_receive-UNHANDLED");
   Bool where_iuct = STREQ(where, "iokit_user_client_trap");
   Bool where_MwcN = STREQ(where, "ML_(wqthread_continue_NORETURN)");
   Bool where_woQR = STREQ(where, "workq_ops(QUEUE_REQTHREADS)");
   Bool where_woQ2 = STREQ(where, "workq_ops(QUEUE_REQTHREADS2)");
   Bool where_woTR = STREQ(where, "workq_ops(THREAD_RETURN)");
   Bool where_ke64 = STREQ(where, "kevent64");
#  undef STREQ

   vg_assert(
      1 >= ( (where_mmr ? 1 : 0) + (where_mmrU ? 1 : 0) 
             + (where_iuct ? 1 : 0) + (where_MwcN ? 1 : 0)
             + (where_woQR ? 1 : 0) + (where_woQ2 ? 1 : 0)
             + (where_woTR ? 1 : 0) + (where_ke64 ? 1 : 0)
   ));
   // merely to stop gcc complaining of non-use in the case where
   // there's no filter:
   vg_assert(when_in    == True || when_in    == False);
   vg_assert(when_after == True || when_after == False);

   CheckHowOften check = CheckAlways;

#  if DARWIN_VERS == DARWIN_10_9 && VG_WORDSIZE == 8
   /* ---------- BEGIN filter for 64-bit 10.9.x ---------- */
   if (when_after && where_mmr) {
      // "after mach_msg_receive <number>"
      switch (num) {
         case 0x00000000: // upd 12414 diff 36+,0-
            check = CheckEvery20;
            break;
         default:
            break;
      }
   }
   else
   if (when_after && where_mmrU) {
      // "after mach_msg_receive-UNHANDLED <number>"
      switch (num) {
         case 0x00000000: // upd 16687 diff 73+,0-
         case 0x00000001: // upd 5106 diff 89+,0-
         case 0x00000002: // upd 1609 diff 1+,0-
         case 0x00000003: // upd 1987 diff 6+,0-
         // case 0x00000b95: // upd 2894 diff 57+,1- <==dangerous
         case 0x000072d9: // upd 2616 diff 11+,0- 
         case 0x000072cb: // upd 2616 diff 9+,0-
         case 0x000074d5: // upd 172 diff 0+,0-
            check = CheckEvery20;
            break;
         default:
            break;
      }
   }
   else
   if (when_in && where_MwcN && num == 0x00000000) {
      // in ML_(wqthread_continue_NORETURN) 0x00000000
      // upd 4346 diff 0+,0- 
      check = CheckEvery20;
   }
   else
   if (when_after && where_woQR && num == 0x00000000) {
      // after workq_ops(QUEUE_REQTHREADS) 0x00000000
      // upd 14434 diff 102+,0-
      check = CheckEvery20;
   }
/* if (when_after && where_woQ2 && num == 0x00000000) {
      // after workq_ops(QUEUE_REQTHREADS2) 0x00000000
      // upd XXXX diff XX+,0-
      check = CheckEvery20;
   } */
   else
   if (when_after && where_woTR && num == 0x00000000) {
      // after workq_ops(THREAD_RETURN) 0x00000000
      // upd 14434 diff 102+,0-
      check = CheckEvery20;
   }
   else
   if (when_after && where_ke64 && num == 0x00000000) {
      // after kevent64 0x00000000
      // upd 1736 diff 78+,0- 
      check = CheckEvery20;
   }
   /* ----------- END filter for 64-bit 10.9.x ----------- */
#  endif /* DARWIN_VERS == DARWIN_10_9 && VG_WORDSIZE == 8 */

#  if DARWIN_VERS == DARWIN_10_10 && VG_WORDSIZE == 8
   /* ---------- BEGIN filter for 64-bit 10.10.x ---------- */
   if (when_after && where_mmr) {
      // "after mach_msg_receive <number>"
      switch (num) {
         case 0x00000000: // upd 2380 diff 23+,0-
            check = CheckEvery20;
            break;
         default:
            break;
      }
   }
   else
   if (when_after && where_mmrU) {
      // "after mach_msg_receive-UNHANDLED <number>"
      switch (num) {
         case 0x00000000: // upd 2370 diff 93+,1-  <==dangerous
         case 0x0000004f: // upd  212 diff 2+,0-
         case 0x00000b95: // upd  9826 diff 163+,1-  diff scale, dangerous
         case 0x00000ba5: // upd  304 diff 0+,0-
         case 0x0000157f: // upd  201 diff 2+,0-
         case 0x0000157d: // upd  197 diff 1+,0-        
         case 0x0000333d: // upd  112 diff 0+,0-
         case 0x0000333f: // upd  223 diff 10+,0-
         case 0x000072cd: // upd  8286 diff 98+,0-   diff scale
         case 0x000072ae: // upd  193 diff 10+,0-
         case 0x000072ec: // upd  319 diff 7+,0-
         case 0x77303074: // upd  113 diff 3+,0-
         case 0x10000000: // upd  314 diff 6+,0-
            check = CheckEvery20;
            break;
         default:
            break;
      }
   }
   else
   if (when_in && where_MwcN && num == 0x00000000) {
      // in ML_(wqthread_continue_NORETURN) 0x00000000
      // upd 1110 diff 37+,0-
      check = CheckEvery20;
   }
   else
   if (when_after && where_woQR && num == 0x00000000) {
      // after workq_ops(QUEUE_REQTHREADS) 0x00000000
      // upd 1099 diff 37+,0-
      check = CheckEvery20;
   }
/* if (when_after && where_woQ2 && num == 0x00000000) {
      // after workq_ops(QUEUE_REQTHREADS2) 0x00000000
      // upd XXXX diff XX+,0-
      check = CheckEvery20;
   } */
   else
   if (when_after && where_woTR && num == 0x00000000) {
      // after workq_ops(THREAD_RETURN) 0x00000000
      // 1239 diff 53+,0-
      check = CheckEvery20;
   }
   else
   if (when_after && where_ke64 && num == 0x00000000) {
      // after kevent64 0x00000000
      // upd 1463 diff 15+,0-
      check = CheckEvery20;
   }
   /* ----------- END filter for 64-bit 10.10.x ----------- */
#  endif /* DARWIN_VERS == DARWIN_10_10 && VG_WORDSIZE == 8 */

   /* Regardless of what the filter says, force a sync every 1 time in
      1000, to stop things getting too far out of sync. */
   {
     static UInt ctr1k = 0;
     ctr1k++;
     if ((ctr1k % 1000) == 0)
        check = CheckAlways;
   }

   /* If the filter is disabled, we must always check. */
   if (VG_(clo_resync_filter) == 0)
      check = CheckAlways;

   switch (check) {
      case CheckAlways:
         break;
      case CheckEvery20: {
         // only resync once every 20th time
         static UInt ctr10 = 0;
         ctr10++;
         if ((ctr10 % 20) != 0) return False;
         break;
      }
      case CheckNever:
         return False;
      default:
         vg_assert(0);
   }
   //
   // --------------- END resync-filter-kludge ---------------

   if (0 || VG_(clo_trace_syscalls)) {
       VG_(debugLog)(0, "syswrap-darwin",
                     "sync_mappings (%s) (\"%s\", \"%s\", 0x%lx)\n", 
                     show_CheckHowOften(check), when, where, num);
   }

   // 16 is enough for most cases, but small enough that overflow happens
   // occasionally and thus the overflow path gets some test coverage.
   css_size = 16;
   ok = False;
   while (!ok) {
      VG_(free)(css);   // css is NULL on first iteration;  that's ok.
      css = VG_(calloc)("sys_wrap.sync_mappings",
                        css_size, sizeof(ChangedSeg));
      ok = VG_(get_changed_segments)(when, where, css, css_size, &css_used);
      css_size *= 2;
   } 

   UInt css_added = 0, css_removed = 0;

   // Now add/remove them.
   for (i = 0; i < css_used; i++) {
      ChangedSeg* cs = &css[i];
      if (cs->is_added) {
         css_added++;
         ML_(notify_core_and_tool_of_mmap)(
               cs->start, cs->end - cs->start + 1,
               cs->prot, VKI_MAP_PRIVATE, 0, cs->offset);
         // should this call VG_(di_notify_mmap) also?
      } else {
         css_removed++;
         ML_(notify_core_and_tool_of_munmap)(
               cs->start, cs->end - cs->start + 1);
      }
      if (VG_(clo_trace_syscalls)) {
          if (cs->is_added) {
             VG_(debugLog)(0, "syswrap-darwin",
                "  added region 0x%010lx..0x%010lx prot %u at %s (%s)\n", 
                cs->start, cs->end + 1, (UInt)cs->prot, where, when);
	  } else {
             VG_(debugLog)(0, "syswrap-darwin",
                "  removed region 0x%010lx..0x%010lx at %s (%s)\n", 
                cs->start, cs->end + 1, where, when);
	  }
      }
   }

   VG_(free)(css);

   if (0)
      VG_(debugLog)(0, "syswrap-darwin", "SYNC: %d  %s  %s\n",
                    css_used, when, where);

   // Update the stats, so we can derive the filter above.
   n_syncsPerformed++;
   update_syncstats(check, when, where, num, css_added, css_removed);

   return css_used > 0;
}

/* ---------------------------------------------------------------------
   wrappers
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(darwin, name)
#define POST(name)      DEFN_POST_TEMPLATE(darwin, name)

#define PRE_FN(name)    vgSysWrap_darwin_##name##_before
#define POST_FN(name)   vgSysWrap_darwin_##name##_after

#define CALL_PRE(name) PRE_FN(name)(tid, layout, arrghs, status, flags)
#define CALL_POST(name) POST_FN(name)(tid, arrghs, status)

// Retrieve the current Mach thread
#define MACH_THREAD ((Addr)VG_(get_ThreadState)(tid)->os_state.lwpid)

// Set the POST handler for a mach_msg derivative
#define AFTER VG_(get_ThreadState)(tid)->os_state.post_mach_trap_fn

// Set or get values saved from Mach messages
#define MACH_ARG(x) VG_(get_ThreadState)(tid)->os_state.mach_args.x
#define MACH_REMOTE VG_(get_ThreadState)(tid)->os_state.remote_port
#define MACH_MSGH_ID VG_(get_ThreadState)(tid)->os_state.msgh_id

/* ---------------------------------------------------------------------
   darwin ioctl wrapper
   ------------------------------------------------------------------ */

PRE(ioctl)
{
   *flags |= SfMayBlock;

   /* Handle ioctls that don't take an arg first */
   switch (ARG2 /* request */) {
   case VKI_TIOCSCTTY:
   case VKI_TIOCEXCL:
   case VKI_TIOCSBRK:
   case VKI_TIOCCBRK:
   case VKI_TIOCPTYGRANT:
   case VKI_TIOCPTYUNLK:
   case VKI_DTRACEHIOC_REMOVE: 
   case VKI_BIOCFLUSH:
   case VKI_BIOCPROMISC:
      PRINT("ioctl ( %lu, 0x%lx )", ARG1, ARG2);
      PRE_REG_READ2(long, "ioctl",
                    unsigned int, fd, unsigned int, request);
      return;
   default:
      PRINT("ioctl ( %lu, 0x%lx, %#lx )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "ioctl",
                    unsigned int, fd, unsigned int, request, unsigned long, arg);
   }

   switch (ARG2 /* request */) {
   case VKI_TIOCGWINSZ:
      PRE_MEM_WRITE( "ioctl(TIOCGWINSZ)", ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
      PRE_MEM_READ( "ioctl(TIOCSWINSZ)",  ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCMBIS:
      PRE_MEM_READ( "ioctl(TIOCMBIS)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMBIC:
      PRE_MEM_READ( "ioctl(TIOCMBIC)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMSET:
      PRE_MEM_READ( "ioctl(TIOCMSET)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMGET:
      PRE_MEM_WRITE( "ioctl(TIOCMGET)",   ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_FIONBIO:
      PRE_MEM_READ( "ioctl(FIONBIO)",    ARG3, sizeof(int) );
      break;
   case VKI_FIOASYNC:
      PRE_MEM_READ( "ioctl(FIOASYNC)",   ARG3, sizeof(int) );
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      PRE_MEM_WRITE( "ioctl(FIONREAD)",  ARG3, sizeof(int) );
      break;


      /* These all use struct ifreq AFAIK */
      /* GrP fixme is sizeof(struct vki_if_req) correct if it's using a sockaddr? */
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFFLAGS)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFFLAGS)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMTU)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMTU)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFNETMASK)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNETMASK)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMETRIC)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMETRIC)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFDSTADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFDSTADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFBRDADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFBRDADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFCONF:         /* get iface list               */
      /* WAS:
         PRE_MEM_WRITE( "ioctl(SIOCGIFCONF)", ARG3, sizeof(struct ifconf));
         KERNEL_DO_SYSCALL(tid,RES);
         if (!VG_(is_kerror)(RES) && RES == 0)
         POST_MEM_WRITE(ARG3, sizeof(struct ifconf));
      */
      PRE_MEM_READ( "ioctl(SIOCGIFCONF)",
                    (Addr)&((struct vki_ifconf *)ARG3)->ifc_len,
                    sizeof(((struct vki_ifconf *)ARG3)->ifc_len));
      PRE_MEM_READ( "ioctl(SIOCGIFCONF)",
                    (Addr)&((struct vki_ifconf *)ARG3)->vki_ifc_buf,
                    sizeof(((struct vki_ifconf *)ARG3)->vki_ifc_buf));
      if ( ARG3 ) {
         // TODO len must be readable and writable
         // buf pointer only needs to be readable
         struct vki_ifconf *ifc = (struct vki_ifconf *) ARG3;
         PRE_MEM_WRITE( "ioctl(SIOCGIFCONF).ifc_buf",
                        (Addr)(ifc->vki_ifc_buf), ifc->ifc_len );
      }
      break;
                    
   case VKI_SIOCSIFFLAGS:        /* set flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFFLAGS)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFFLAGS)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      break;
   case VKI_SIOCSIFADDR:         /* set PA address               */
   case VKI_SIOCSIFDSTADDR:      /* set remote PA address        */
   case VKI_SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case VKI_SIOCSIFNETMASK:      /* set network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIF*ADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIF*ADDR)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_addr,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_addr) );
      break;
   case VKI_SIOCSIFMETRIC:       /* set metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMETRIC)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMETRIC)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_metric,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_metric) );
      break;
   case VKI_SIOCSIFMTU:          /* set MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMTU)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMTU)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_mtu,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_mtu) );
      break;
      /* Routing table calls.  */
#ifdef VKI_SIOCADDRT
   case VKI_SIOCADDRT:           /* add routing table entry      */
   case VKI_SIOCDELRT:           /* delete routing table entry   */
      PRE_MEM_READ( "ioctl(SIOCADDRT/DELRT)", ARG3, 
                    sizeof(struct vki_rtentry));
      break;
#endif

   case VKI_SIOCGPGRP:
      PRE_MEM_WRITE( "ioctl(SIOCGPGRP)", ARG3, sizeof(int) );
      break;
   case VKI_SIOCSPGRP:
      PRE_MEM_READ( "ioctl(SIOCSPGRP)", ARG3, sizeof(int) );
      //tst->sys_flags &= ~SfMayBlock;
      break;

   case VKI_FIODTYPE: 
      PRE_MEM_WRITE( "ioctl(FIONREAD)", ARG3, sizeof(int) );
      break;

   case VKI_DTRACEHIOC_ADDDOF: 
       break;

       // ttycom.h
   case VKI_TIOCGETA:
       PRE_MEM_WRITE( "ioctl(TIOCGETA)", ARG3, sizeof(struct vki_termios) );
       break;
   case VKI_TIOCSETA:
       PRE_MEM_READ( "ioctl(TIOCSETA)", ARG3, sizeof(struct vki_termios) );
       break;
   case VKI_TIOCGETD:
       PRE_MEM_WRITE( "ioctl(TIOCGETD)", ARG3, sizeof(int) );
       break;
   case VKI_TIOCSETD:
       PRE_MEM_READ( "ioctl(TIOCSETD)", ARG3, sizeof(int) );
       break;
   case VKI_TIOCPTYGNAME:
       PRE_MEM_WRITE( "ioctl(TIOCPTYGNAME)", ARG3, 128 );
       break;

   // filio.h
   case VKI_FIOCLEX:
       break;
   case VKI_FIONCLEX:
       break;

       // net/bpf.h
   case VKI_BIOCSETF:            /* set BPF filter               */
      /*
       * struct bpf_program has a 32-bit count of instructions,
       * followed by a pointer to an array of those instructions.
       * In 64-bit mode, there's padding between those two elements.
       *
       * So that we don't bogusly complain about the padding bytes,
       * we just report that we read bf_len and and bf_insns.
       *
       * We then make sure that what bf_insns points to is valid.
       */
      PRE_MEM_READ( "ioctl(BIOCSETF)",
                     (Addr)&((struct vki_bpf_program *)ARG3)->vki_bf_len,
                     sizeof(((struct vki_bpf_program *)ARG3)->vki_bf_len) );
      PRE_MEM_READ( "ioctl(BIOCSETF)",
                     (Addr)&((struct vki_bpf_program *)ARG3)->vki_bf_insns,
                     sizeof(((struct vki_bpf_program *)ARG3)->vki_bf_insns) );
      if ( ARG3 ) {
         /* bf_len * sizeof (*bf_insns) */
         struct vki_bpf_program *bp = (struct vki_bpf_program *)ARG3;
         if ( bp->bf_insns != NULL )
           PRE_MEM_READ( "ioctl(BIOCSETF) points to a struct bpf_program whose bf_insns member",
                          (Addr)(bp->vki_bf_insns),
                          bp->vki_bf_len * sizeof(*bp->vki_bf_insns) );
      }
      break;
   case VKI_BIOCSETIF:           /* set BPF interface            */
      PRE_MEM_RASCIIZ( "ioctl(BIOCSETIF)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      break;
   case VKI_BIOCSRTIMEOUT:       /* set BPF timeout              */
      /*
       * 64-bit struct timeval starts with a 64-bit "seconds since the
       * Epoch" value, followed by a 32-bit microseconds value.  The
       * resulting structure is padded to a multiple of 8 bytes, so
       * there are 4 padding bytes at the end.
       *
       * So that we don't bogusly complain about the padding bytes,
       * we just report that we read tv_sec and tv_usec.
       */
      PRE_MEM_READ( "ioctl(BIOCSRTIMEOUT)",
                     (Addr)&((struct vki_timeval *)ARG3)->vki_tv_sec,
                     sizeof(((struct vki_timeval *)ARG3)->vki_tv_sec) );
      PRE_MEM_READ( "ioctl(BIOCSRTIMEOUT)",
                     (Addr)&((struct vki_timeval *)ARG3)->vki_tv_usec,
                     sizeof(((struct vki_timeval *)ARG3)->vki_tv_usec) );
      break;
   case VKI_BIOCGDLTLIST:        /* get list of BPF DLTs         */
      PRE_MEM_READ( "ioctl(BIOCGDLTLIST).bfl_len",
                     (Addr)&((struct vki_bpf_dltlist *)ARG3)->vki_bfl_list,
                     sizeof(((struct vki_bpf_dltlist *)ARG3)->vki_bfl_list) );
      if ( ARG3 ) {
         /* bfl_len * sizeof (*bfl_list) */
         struct vki_bpf_dltlist *bdl = (struct vki_bpf_dltlist *)ARG3;
         if ( bdl->bfl_list != NULL )
           PRE_MEM_READ( "ioctl(BIOCGDLTLIST).bfl_len",
                          (Addr)&((struct vki_bpf_dltlist *)ARG3)->vki_bfl_len,
                          sizeof(((struct vki_bpf_dltlist *)ARG3)->vki_bfl_len) );
           PRE_MEM_WRITE( "ioctl(BIOCGDLTLIST) points to a struct bpf_dltlist whose bfl_list member",
                          (Addr)(bdl->vki_bfl_list),
                          bdl->bfl_len * sizeof(*bdl->vki_bfl_list) );
      }
      break;

   default: 
      ML_(PRE_unknown_ioctl)(tid, ARG2, ARG3);
      break;
   }
}


POST(ioctl)
{
   vg_assert(SUCCESS);
   switch (ARG2 /* request */) {
   case VKI_TIOCGWINSZ:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
   case VKI_TIOCMBIS:
   case VKI_TIOCMBIC:
   case VKI_TIOCMSET:
      break;
   case VKI_TIOCMGET:
      POST_MEM_WRITE( ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      POST_MEM_WRITE( ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      POST_MEM_WRITE( ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSCTTY:
      break;
   case VKI_FIONBIO:
      break;
   case VKI_FIOASYNC:
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;

      /* These all use struct ifreq AFAIK */
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_mtu,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_mtu) );
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->ifr_addr,
                sizeof(((struct vki_ifreq *)ARG3)->ifr_addr) );
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_metric,
                sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_metric) );
      break;
   case VKI_SIOCGIFCONF:         /* get iface list               */
      /* WAS:
         PRE_MEM_WRITE("ioctl(SIOCGIFCONF)", ARG3, sizeof(struct ifconf));
         KERNEL_DO_SYSCALL(tid,RES);
         if (!VG_(is_kerror)(RES) && RES == 0)
         POST_MEM_WRITE(ARG3, sizeof(struct ifconf));
      */
      if (RES == 0 && ARG3 ) {
         struct vki_ifconf *ifc = (struct vki_ifconf *) ARG3;
         if (ifc->vki_ifc_buf != NULL)
            POST_MEM_WRITE( (Addr)(ifc->vki_ifc_buf), ifc->ifc_len );
      }
      break;
                    
   case VKI_SIOCSIFFLAGS:        /* set flags                    */
   case VKI_SIOCSIFDSTADDR:      /* set remote PA address        */
   case VKI_SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case VKI_SIOCSIFNETMASK:      /* set network PA mask          */
   case VKI_SIOCSIFMETRIC:       /* set metric                   */
   case VKI_SIOCSIFADDR:         /* set PA address               */
   case VKI_SIOCSIFMTU:          /* set MTU size                 */
      break;

#ifdef VKI_SIOCADDRT
      /* Routing table calls.  */
   case VKI_SIOCADDRT:           /* add routing table entry      */
   case VKI_SIOCDELRT:           /* delete routing table entry   */
      break;
#endif

   case VKI_SIOCGPGRP:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SIOCSPGRP:
      break;

   case VKI_FIODTYPE: 
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;

   case VKI_DTRACEHIOC_REMOVE: 
   case VKI_DTRACEHIOC_ADDDOF: 
       break;

       // ttycom.h
   case VKI_TIOCGETA:
       POST_MEM_WRITE( ARG3, sizeof(struct vki_termios));
       break;
   case VKI_TIOCSETA:
       break;
   case VKI_TIOCGETD:
       POST_MEM_WRITE( ARG3, sizeof(int) );
       break;
   case VKI_TIOCSETD:
       break;
   case VKI_TIOCPTYGNAME:
       POST_MEM_WRITE( ARG3, 128);
       break;
   case VKI_TIOCSBRK:           /* set break bit                 */
   case VKI_TIOCCBRK:           /* clear break bit               */
   case VKI_TIOCPTYGRANT:
   case VKI_TIOCPTYUNLK:
       break;

       // bpf.h
   case VKI_BIOCGDLTLIST:        /* get list of BPF DLTs         */
      if (RES == 0 && ARG3 ) {
         /* bfl_len * sizeof (*bfl_list) */
         struct vki_bpf_dltlist *bdl = (struct vki_bpf_dltlist *)ARG3;
         if ( bdl->vki_bfl_list != NULL )
           POST_MEM_WRITE( (Addr)(bdl->vki_bfl_list),
                           bdl->bfl_len * sizeof(*bdl->vki_bfl_list) );
      }
      break;

   default:
      ML_(POST_unknown_ioctl)(tid, RES, ARG2, ARG3);
      break;
   }
}


/* ---------------------------------------------------------------------
   darwin fcntl wrapper
   ------------------------------------------------------------------ */
static const HChar *name_for_fcntl(UWord cmd) {
#define F(n) case VKI_##n: return #n
   switch (cmd) {
      F(F_CHKCLEAN);
      F(F_RDAHEAD);
      F(F_NOCACHE);
      F(F_FULLFSYNC);
      F(F_FREEZE_FS);
      F(F_THAW_FS);
      F(F_GLOBAL_NOCACHE);
      F(F_PREALLOCATE);
      F(F_SETSIZE);
      F(F_RDADVISE);
#     if DARWIN_VERS < DARWIN_10_9
      F(F_READBOOTSTRAP);
      F(F_WRITEBOOTSTRAP);
#     endif
      F(F_LOG2PHYS);
      F(F_GETPATH);
      F(F_PATHPKG_CHECK);
      F(F_ADDSIGS);
#     if DARWIN_VERS >= DARWIN_10_9
      F(F_ADDFILESIGS);
#     endif
#     if DARWIN_VERS >= DARWIN_10_11
      F(F_ADDFILESIGS_FOR_DYLD_SIM);
      F(F_BARRIERFSYNC);
      F(F_ADDFILESIGS_RETURN);
#     endif
   default:
      return "UNKNOWN";
   }
#undef F
}

PRE(fcntl)
{
   switch (ARG2) {
   // These ones ignore ARG3.
   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETOWN:
      PRINT("fcntl ( %lu, %lu )", ARG1,ARG2);
      PRE_REG_READ2(long, "fcntl", unsigned int, fd, unsigned int, cmd);
      break;

   // These ones use ARG3 as "arg".
   case VKI_F_DUPFD:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETOWN:
      PRINT("fcntl[ARG3=='arg'] ( %lu, %lu, %lu )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd, unsigned long, arg);
      break;

   // These ones use ARG3 as "lock".
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
      PRINT("fcntl[ARG3=='lock'] ( %lu, %lu, %#lx )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct flock64 *, lock);
      // GrP fixme mem read sizeof(flock64)
      if (ARG2 == VKI_F_SETLKW) 
         *flags |= SfMayBlock;
      break;
#  if DARWIN_VERS >= DARWIN_10_10
   case VKI_F_SETLKWTIMEOUT:
      PRINT("fcntl[ARG3=='locktimeout'] ( %lu, %lu, %#lx )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct flocktimeout *, lock);
      *flags |= SfMayBlock;
      break;
#  endif

       // none
   case VKI_F_CHKCLEAN:
   case VKI_F_RDAHEAD:
   case VKI_F_NOCACHE:
   case VKI_F_FULLFSYNC:
   case VKI_F_FREEZE_FS:
   case VKI_F_THAW_FS:
   case VKI_F_GLOBAL_NOCACHE:
      PRINT("fcntl ( %lu, %s, %lu )", ARG1, name_for_fcntl(ARG1), ARG2);
      PRE_REG_READ2(long, "fcntl", unsigned int, fd, unsigned int, cmd);
      break;

       // struct fstore
   case VKI_F_PREALLOCATE:
      PRINT("fcntl ( %lu, %s, %#lx )", ARG1, name_for_fcntl(ARG2), ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct fstore *, fstore);
      {
         struct vki_fstore *fstore = (struct vki_fstore *)ARG3;
         PRE_FIELD_READ( "fcntl(F_PREALLOCATE, fstore->fst_flags)", 
                         fstore->fst_flags );
         PRE_FIELD_READ( "fcntl(F_PREALLOCATE, fstore->fst_flags)", 
                         fstore->fst_posmode );
         PRE_FIELD_READ( "fcntl(F_PREALLOCATE, fstore->fst_flags)", 
                         fstore->fst_offset );
         PRE_FIELD_READ( "fcntl(F_PREALLOCATE, fstore->fst_flags)", 
                         fstore->fst_length );
         PRE_FIELD_WRITE( "fcntl(F_PREALLOCATE, fstore->fst_bytesalloc)", 
                          fstore->fst_bytesalloc);
      }
      break;

       // off_t
   case VKI_F_SETSIZE:
      PRINT("fcntl ( %lu, %s, %#lx )", ARG1, name_for_fcntl(ARG2), ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    vki_off_t *, offset);
      break;

       // struct radvisory
   case VKI_F_RDADVISE:
      PRINT("fcntl ( %lu, %s, %#lx )", ARG1, name_for_fcntl(ARG2), ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct vki_radvisory *, radvisory);
      {
         struct vki_radvisory *radvisory = (struct vki_radvisory *)ARG3;
         PRE_FIELD_READ( "fcntl(F_PREALLOCATE, radvisory->ra_offset)", 
                         radvisory->ra_offset );
         PRE_FIELD_READ( "fcntl(F_PREALLOCATE, radvisory->ra_count)", 
                         radvisory->ra_count );
      }
      break;

#  if DARWIN_VERS < DARWIN_10_9
       // struct fbootstraptransfer
   case VKI_F_READBOOTSTRAP:
   case VKI_F_WRITEBOOTSTRAP:
      PRINT("fcntl ( %lu, %s, %#lx )", ARG1, name_for_fcntl(ARG2), ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct fbootstraptransfer *, bootstrap);
      PRE_MEM_READ( "fcntl(F_READ/WRITEBOOTSTRAP, bootstrap)", 
                    ARG3, sizeof(struct vki_fbootstraptransfer) );
      break;
#  endif

       // struct log2phys (out)
   case VKI_F_LOG2PHYS:
      PRINT("fcntl ( %lu, %s, %#lx )", ARG1, name_for_fcntl(ARG2), ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct log2phys *, l2p);
      PRE_MEM_WRITE( "fcntl(F_LOG2PHYS, l2p)", 
                     ARG3, sizeof(struct vki_log2phys) );
      break;

       // char[maxpathlen] (out)
   case VKI_F_GETPATH:
      PRINT("fcntl ( %lu, %s, %#lx )", ARG1, name_for_fcntl(ARG2), ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    char *, pathbuf);
      PRE_MEM_WRITE( "fcntl(F_GETPATH, pathbuf)", 
                     ARG3, VKI_MAXPATHLEN );
      break;

       // char[maxpathlen] (in)
   case VKI_F_PATHPKG_CHECK:
      PRINT("fcntl ( %lu, %s, %#lx '%s')", ARG1, name_for_fcntl(ARG2), ARG3,
            (HChar *)ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    char *, pathbuf);
      PRE_MEM_RASCIIZ( "fcntl(F_PATHPKG_CHECK, pathbuf)", ARG3);
      break;

   case VKI_F_ADDSIGS: /* Add detached signatures (for code signing) */
      PRINT("fcntl ( %lu, %s )", ARG1, name_for_fcntl(ARG2));
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    vki_fsignatures_t *, sigs);

      {
         vki_fsignatures_t *fsigs = (vki_fsignatures_t*)ARG3;
         PRE_FIELD_READ( "fcntl(F_ADDSIGS, fsigs->fs_blob_start)",
                         fsigs->fs_blob_start);
         PRE_FIELD_READ( "fcntl(F_ADDSIGS, fsigs->fs_blob_size)",
                         fsigs->fs_blob_size);

         if (fsigs->fs_blob_start)
            PRE_MEM_READ( "fcntl(F_ADDSIGS, fsigs->fs_blob_start)",
                          (Addr)fsigs->fs_blob_start, fsigs->fs_blob_size);
      }
      break;

   case VKI_F_ADDFILESIGS: /* Add signature from same file (used by dyld for shared libs) */
      PRINT("fcntl ( %lu, %s, %#lx )", ARG1, name_for_fcntl(ARG2), ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    vki_fsignatures_t *, sigs);

      {
         vki_fsignatures_t *fsigs = (vki_fsignatures_t*)ARG3;
         PRE_FIELD_READ( "fcntl(F_ADDFILESIGS, fsigs->fs_blob_start)",
                         fsigs->fs_blob_start);
         PRE_FIELD_READ( "fcntl(F_ADDFILESIGS, fsigs->fs_blob_size)",
                         fsigs->fs_blob_size);
      }
      break;

#  if DARWIN_VERS >= DARWIN_10_11
   case VKI_F_ADDFILESIGS_FOR_DYLD_SIM: /* Add signature from same file, only if it is signed
                                           by Apple used by dyld for simulator */
      // FIXME: RK
      break;

   case VKI_F_BARRIERFSYNC: /* fsync + issue barrier to drive */
      // FIXME: RK
      break;

   case VKI_F_ADDFILESIGS_RETURN: /* Add signature from same file, return end offset in 
                                     structure on success */
      // FIXME: RK
      break;
#  endif

   default:
      PRINT("fcntl ( %lu, %lu [??] )", ARG1, ARG2);
      log_decaying("UNKNOWN fcntl %lu!", ARG2);
      break;
   }
}

POST(fcntl)
{
   vg_assert(SUCCESS);
   switch (ARG2) {
   case VKI_F_DUPFD:
      if (!ML_(fd_allowed)(RES, "fcntl(DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            ML_(record_fd_open_named)(tid, RES);
      }
      break;

   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETOWN:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETOWN:
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
#  if DARWIN_VERS >= DARWIN_10_10
   case VKI_F_SETLKWTIMEOUT:
       break;
#  endif

   case VKI_F_PREALLOCATE:
      {
         struct vki_fstore *fstore = (struct vki_fstore *)ARG3;
         POST_FIELD_WRITE( fstore->fst_bytesalloc );
      }
      break;

   case VKI_F_LOG2PHYS:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_log2phys) );
      break;

   case VKI_F_GETPATH:
      POST_MEM_WRITE( ARG3, 1+VG_(strlen)((char *)ARG3) );
      PRINT("\"%s\"", (char*)ARG3);
      break;

   default:
      // DDD: ugh, missing lots of cases here, not nice
      break;
   }
}

/* ---------------------------------------------------------------------
   unix syscalls
   ------------------------------------------------------------------ */

PRE(futimes)
{
   PRINT("futimes ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "futimes", int, fd, struct timeval *, tvp);
   if (!ML_(fd_allowed)(ARG1, "futimes", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else if (ARG2 != 0) {
      PRE_timeval_READ( "futimes(tvp[0])", ARG2 );
      PRE_timeval_READ( "futimes(tvp[1])", ARG2+sizeof(struct vki_timeval) );
   }
}

PRE(semget)
{
   PRINT("semget ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(long, "semget", vki_key_t, key, int, nsems, int, semflg);
}

PRE(semop)
{
   *flags |= SfMayBlock;
   PRINT("semop ( %ld, %#lx, %lu )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "semop",
                 int, semid, struct sembuf *, sops, vki_size_t, nsoops);
   ML_(generic_PRE_sys_semop)(tid, ARG1,ARG2,ARG3);
}

PRE(semctl)
{
   switch (ARG3) {
   case VKI_IPC_STAT:
   case VKI_IPC_SET:
      PRINT("semctl ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct semid_ds *, arg);
      break;
   case VKI_GETALL:
   case VKI_SETALL:
      PRINT("semctl ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, unsigned short *, arg);
      break;
   case VKI_SETVAL:
      PRINT("semctl ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, int, arg);
      break;
   default:
      PRINT("semctl ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
      PRE_REG_READ3(long, "semctl",
                    int, semid, int, semnum, int, cmd);
      break;
   }
   ML_(generic_PRE_sys_semctl)(tid, ARG1,ARG2,ARG3,ARG4);
}
POST(semctl)
{
   ML_(generic_POST_sys_semctl)(tid, RES,ARG1,ARG2,ARG3,ARG4);
}

PRE(sem_open)
{
   if (ARG2 & VKI_O_CREAT) {
      // 4-arg version
      PRINT("sem_open ( %#lx(%s), %ld, %lu, %lu )",
            ARG1, (HChar*)ARG1, SARG2, ARG3, ARG4);
      PRE_REG_READ4(vki_sem_t *, "sem_open",
                    const char *, name, int, oflag, vki_mode_t, mode,
                    unsigned int, value);
   } else {
      // 2-arg version
      PRINT("sem_open ( %#lx(%s), %ld )", ARG1, (HChar*)ARG1, SARG2);
      PRE_REG_READ2(vki_sem_t *, "sem_open",
                    const char *, name, int, oflag);
   }
   PRE_MEM_RASCIIZ( "sem_open(name)", ARG1 );

   /* Otherwise handle normally */
   *flags |= SfMayBlock;
}

PRE(sem_close)
{
   PRINT("sem_close( %#lx )", ARG1);
   PRE_REG_READ1(int, "sem_close", vki_sem_t *, sem);
}

PRE(sem_unlink)
{
   PRINT("sem_unlink(  %#lx(%s) )", ARG1, (HChar*)ARG1);
   PRE_REG_READ1(int, "sem_unlink", const char *, name);
   PRE_MEM_RASCIIZ( "sem_unlink(name)", ARG1 );
}

PRE(sem_post)
{
   PRINT("sem_post( %#lx )", ARG1);
   PRE_REG_READ1(int, "sem_post", vki_sem_t *, sem);
   *flags |= SfMayBlock;
}

PRE(sem_destroy)
{
  PRINT("sem_destroy( %#lx )", ARG1);
  PRE_REG_READ1(int, "sem_destroy", vki_sem_t *, sem);
  PRE_MEM_READ("sem_destroy(sem)", ARG1, sizeof(vki_sem_t));
}

PRE(sem_init)
{
  PRINT("sem_init( %#lx, %ld, %lu )", ARG1, SARG2, ARG3);
  PRE_REG_READ3(int, "sem_init", vki_sem_t *, sem,
                int, pshared, unsigned int, value);
  PRE_MEM_WRITE("sem_init(sem)", ARG1, sizeof(vki_sem_t));
}

POST(sem_init)
{
  POST_MEM_WRITE(ARG1, sizeof(vki_sem_t));
}

PRE(sem_wait)
{
   PRINT("sem_wait( %#lx )", ARG1);
   PRE_REG_READ1(int, "sem_wait", vki_sem_t *, sem);
   *flags |= SfMayBlock;
}

PRE(sem_trywait)
{
   PRINT("sem_trywait( %#lx )", ARG1);
   PRE_REG_READ1(int, "sem_trywait", vki_sem_t *, sem);
   *flags |= SfMayBlock;
}

PRE(kqueue)
{
    PRINT("kqueue()");
}

POST(kqueue)
{
   if (!ML_(fd_allowed)(RES, "kqueue", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_with_given_name)(tid, RES, NULL);
      }
   }
}

PRE(fileport_makeport)
{
    PRINT("fileport_makeport(fd:%#lx, portnamep:%#lx) FIXME",
      ARG1, ARG2);
}

PRE(guarded_open_np)
{
    PRINT("guarded_open_np(path:%#lx(%s), guard:%#lx, guardflags:%#lx, flags:%#lx) FIXME",
      ARG1, (char*)ARG1, ARG2, ARG3, ARG4);
}

PRE(guarded_kqueue_np)
{
    PRINT("guarded_kqueue_np(guard:%#lx, guardflags:%#lx) FIXME",
      ARG1, ARG2);
}

POST(guarded_kqueue_np)
{
   if (!ML_(fd_allowed)(RES, "guarded_kqueue_np", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_with_given_name)(tid, RES, NULL);
      }
   }
}

PRE(guarded_close_np)
{
    PRINT("guarded_close_np(fd:%#lx, guard:%#lx) FIXME",
      ARG1, ARG2);
}

PRE(change_fdguard_np)
{
    PRINT("change_fdguard_np(fd:%#lx, guard:%#lx, guardflags:%#lx, nguard:%#lx, nguardflags:%#lx, fdflagsp:%#lx) FIXME",
      ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
}

PRE(connectx)
{
    PRINT("connectx(s:%#lx, src:%#lx, srclen:%#lx, dsts:%#lx, dstlen:%#lx, ifscope:%#lx, aid:%#lx, out_cid:%#lx) FIXME",
      ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8);
}

PRE(disconnectx)
{
    PRINT("disconnectx(s:%#lx, aid:%#lx, cid:%#lx) FIXME",
      ARG1, ARG2, ARG3);
}


PRE(kevent)
{
   PRINT("kevent( %ld, %#lx, %ld, %#lx, %ld, %#lx )", 
         SARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(int,"kevent", int,kq, 
                 const struct vki_kevent *,changelist, int,nchanges, 
                 struct vki_kevent *,eventlist, int,nevents, 
                 const struct vki_timespec *,timeout);

   if (ARG3) PRE_MEM_READ ("kevent(changelist)", 
                           ARG2, ARG3 * sizeof(struct vki_kevent));
   if (ARG5) PRE_MEM_WRITE("kevent(eventlist)", 
                           ARG4, ARG5 * sizeof(struct vki_kevent));
   if (ARG6) PRE_MEM_READ ("kevent(timeout)", 
                           ARG6, sizeof(struct vki_timespec));

   *flags |= SfMayBlock;
}

POST(kevent)
{
   PRINT("kevent ret %ld dst %#lx (%zu)", RES, ARG4, sizeof(struct vki_kevent));
   if (RES > 0) POST_MEM_WRITE(ARG4, RES * sizeof(struct vki_kevent));
}


PRE(kevent64)
{
   PRINT("kevent64( %ld, %#lx, %ld, %#lx, %ld, %#lx )",
         SARG1, ARG2, SARG3, ARG4, SARG5, ARG6);
   PRE_REG_READ6(int,"kevent64", int,kq,
                 const struct vki_kevent64 *,changelist, int,nchanges,
                 struct vki_kevent64 *,eventlist, int,nevents,
                 const struct vki_timespec *,timeout);

   if (ARG3) PRE_MEM_READ ("kevent64(changelist)",
                           ARG2, ARG3 * sizeof(struct vki_kevent64));
   if (ARG5) PRE_MEM_WRITE("kevent64(eventlist)",
                           ARG4, ARG5 * sizeof(struct vki_kevent64));
   if (ARG6) PRE_MEM_READ ("kevent64(timeout)",
                           ARG6, sizeof(struct vki_timespec));

   *flags |= SfMayBlock;
}

POST(kevent64)
{
   PRINT("kevent64 ret %ld dst %#lx (%zu)", RES, ARG4, sizeof(struct vki_kevent64));
   if (RES > 0) {
      ML_(sync_mappings)("after", "kevent64", 0);
      POST_MEM_WRITE(ARG4, RES * sizeof(struct vki_kevent64));
   }
}


Addr pthread_starter = 0;
Addr wqthread_starter = 0;
SizeT pthread_structsize = 0;

PRE(bsdthread_register)
{
   PRINT("bsdthread_register( %#lx, %#lx, %lu )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int,"__bsdthread_register", void *,"threadstart", 
                 void *,"wqthread", size_t,"pthsize");

   pthread_starter = ARG1;
   wqthread_starter = ARG2;
   pthread_structsize = ARG3;
   ARG1 = (Word)&pthread_hijack_asm;
   ARG2 = (Word)&wqthread_hijack_asm;
}

PRE(workq_open)
{
   PRINT("workq_open()");
   PRE_REG_READ0(int, "workq_open");

   // This creates lots of threads and thread stacks under the covers, 
   // but we ignore them all until some work item starts running on it.
}

static const HChar *workqop_name(int op)
{
   switch (op) {
   case VKI_WQOPS_QUEUE_ADD:                  return "QUEUE_ADD";
   case VKI_WQOPS_QUEUE_REMOVE:               return "QUEUE_REMOVE";
   case VKI_WQOPS_THREAD_RETURN:              return "THREAD_RETURN";
   case VKI_WQOPS_THREAD_SETCONC:             return "THREAD_SETCONC";
   case VKI_WQOPS_QUEUE_NEWSPISUPP:           return "QUEUE_NEWSPISUPP";
   case VKI_WQOPS_QUEUE_REQTHREADS:           return "QUEUE_REQTHREADS";
   case VKI_WQOPS_QUEUE_REQTHREADS2:          return "QUEUE_REQTHREADS2";
   case VKI_WQOPS_THREAD_KEVENT_RETURN:       return "THREAD_KEVENT_RETURN";
   case VKI_WQOPS_SET_EVENT_MANAGER_PRIORITY: return "SET_EVENT_MANAGER_PRIORITY";
   case VKI_WQOPS_THREAD_WORKLOOP_RETURN:     return "THREAD_WORKLOOP_RETURN";
   case VKI_WQOPS_SHOULD_NARROW:              return "SHOULD_NARROW";
   default: return "?";
   }
}


PRE(workq_ops)
{
   PRINT("workq_ops( %ld(%s), %#lx, %ld )", SARG1, workqop_name(ARG1), ARG2,
         SARG3);
   PRE_REG_READ3(int,"workq_ops", int,"options", void *,"item", 
                 int,"priority");

   switch (ARG1) {
   case VKI_WQOPS_QUEUE_ADD:
   case VKI_WQOPS_QUEUE_REMOVE:
      // GrP fixme need anything here?
      // GrP fixme may block?
      break;
   case VKI_WQOPS_THREAD_RETURN: {
      // The interesting case. The kernel will do one of two things:
      // 1. Return normally. We continue; libc proceeds to stop the thread.
      //    V does nothing special here.
      // 2. Jump to wqthread_hijack. This wipes the stack and runs a 
      //    new work item, and never returns from workq_ops. 
      //    V handles this by longjmp() from wqthread_hijack back to the 
      //    scheduler, which continues at the new client SP/IP/state.
      //    This works something like V's signal handling.
      //    To the tool, this looks like workq_ops() sometimes returns 
      //    to a strange address.
      ThreadState *tst = VG_(get_ThreadState)(tid);
      tst->os_state.wq_jmpbuf_valid = True;
      *flags |= SfMayBlock;  // GrP fixme true?
      break;
   }
   case VKI_WQOPS_THREAD_SETCONC:
      // RK fixme need anything here?
      // RK fixme may block?
      break;
   case VKI_WQOPS_QUEUE_NEWSPISUPP:
      // JRS don't think we need to do anything here -- this just checks
      // whether some newer functionality is supported
      break;
   case VKI_WQOPS_QUEUE_REQTHREADS:
   case VKI_WQOPS_QUEUE_REQTHREADS2:
      // JRS uh, looks like it queues up a bunch of threads, or some such?
      *flags |= SfMayBlock; // the kernel sources take a spinlock, so play safe
      break;
   case VKI_WQOPS_THREAD_KEVENT_RETURN:
      // RK fixme need anything here?
      // perhaps similar to VKI_WQOPS_THREAD_RETURN above?
      break;
   case VKI_WQOPS_SET_EVENT_MANAGER_PRIORITY:
      // RK fixme this just sets scheduling priorities - don't think we need
      // to do anything here
      break;
   case VKI_WQOPS_THREAD_WORKLOOP_RETURN:
   case VKI_WQOPS_SHOULD_NARROW:
      // RK fixme need anything here?
      // RK fixme may block?
      break;
   default:
      VG_(printf)("UNKNOWN workq_ops option %ld\n", ARG1);
      break;
   }
}
POST(workq_ops)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   tst->os_state.wq_jmpbuf_valid = False;
   switch (ARG1) {
      case VKI_WQOPS_THREAD_RETURN:
         ML_(sync_mappings)("after", "workq_ops(THREAD_RETURN)", 0);
         break;
      case VKI_WQOPS_QUEUE_REQTHREADS:
         ML_(sync_mappings)("after", "workq_ops(QUEUE_REQTHREADS)", 0);
         break;
      case VKI_WQOPS_QUEUE_REQTHREADS2:
         ML_(sync_mappings)("after", "workq_ops(QUEUE_REQTHREADS2)", 0);
         break;
      default:
         break;
   }
}



PRE(__mac_syscall)
{
   PRINT("__mac_syscall( %#lx(%s), %ld, %#lx )",
         ARG1, (HChar*)ARG1, SARG2, ARG3);
   PRE_REG_READ3(int,"__mac_syscall", char *,"policy", 
                 int,"call", void *,"arg");

   // GrP fixme check call's arg?
   // GrP fixme check policy?
}


/* Not like syswrap-generic's sys_exit, which exits only one thread.
   More like syswrap-generic's sys_exit_group. */
PRE(exit)
{
   ThreadId     t;
   ThreadState* tst;

   PRINT("darwin exit( %ld )", SARG1);
   PRE_REG_READ1(void, "exit", int, status);

   tst = VG_(get_ThreadState)(tid);

   /* A little complex; find all the threads with the same threadgroup
      as this one (including this one), and mark them to exit */
   for (t = 1; t < VG_N_THREADS; t++) {
      if ( /* not alive */
           VG_(threads)[t].status == VgTs_Empty 
           /* GrP fixme zombie? */
         )
         continue;

      VG_(threads)[t].exitreason = VgSrc_ExitProcess;
      VG_(threads)[t].os_state.exitcode = ARG1;

      if (t != tid)
         VG_(get_thread_out_of_syscall)(t);     /* unblock it, if blocked */
   }

   /* We have to claim the syscall already succeeded. */
   SET_STATUS_Success(0);
}


PRE(sigaction)
{
   PRINT("sigaction ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sigaction",
                 int, signum, vki_sigaction_toK_t *, act,
                 vki_sigaction_fromK_t *, oldact);

   if (ARG2 != 0) {
      vki_sigaction_toK_t *sa = (vki_sigaction_toK_t *)ARG2;
      PRE_MEM_READ( "sigaction(act->sa_handler)",
                    (Addr)&sa->ksa_handler, sizeof(sa->ksa_handler));
      PRE_MEM_READ( "sigaction(act->sa_mask)",
                    (Addr)&sa->sa_mask, sizeof(sa->sa_mask));
      PRE_MEM_READ( "sigaction(act->sa_flags)",
                    (Addr)&sa->sa_flags, sizeof(sa->sa_flags));
   }
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sigaction(oldact)",
                     ARG3, sizeof(vki_sigaction_fromK_t));

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigaction)(ARG1, (const vki_sigaction_toK_t *)ARG2,
                                  (vki_sigaction_fromK_t *)ARG3)
   );
}
POST(sigaction)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(vki_sigaction_fromK_t));
}


PRE(__pthread_kill)
{
   PRINT("__pthread_kill ( %#lx, %ld )", ARG1, SARG2);
   PRE_REG_READ2(long, "__pthread_kill", vki_pthread_t*, thread, int, sig);
}


PRE(__pthread_sigmask)
{
    // arguments are identical to sigprocmask (how, sigset_t*, sigset_t*).
    UWord arg1;
    PRINT("__pthread_sigmask ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
    PRE_REG_READ3(long, "__pthread_sigmask",
                  int, how, vki_sigset_t *, set, vki_sigset_t *, oldset);
    if (ARG2 != 0)
        PRE_MEM_READ( "__pthread_sigmask(set)", ARG2, sizeof(vki_sigset_t));
    if (ARG3 != 0)
        PRE_MEM_WRITE( "__pthread_sigmask(oldset)", ARG3, sizeof(vki_sigset_t));
    
    /* Massage ARG1 ('how').  If ARG2 (the new mask) is NULL then the
     value of 'how' is irrelevant, and it appears that Darwin's libc
     passes zero, which is not equal to any of
     SIG_{BLOCK,UNBLOCK,SETMASK}.  This causes
     VG_(do_sys_sigprocmask) to complain, since it checks the 'how'
     value independently of the other args.  Solution: in this case,
     simply pass a valid (but irrelevant) value for 'how'. */
    /* Also, in this case the new set is passed to the kernel by
     reference, not value, as in some other sigmask related Darwin
     syscalls. */
    arg1 = ARG1;
    if (ARG2 == 0  /* the new-set is NULL */
        && ARG1 != VKI_SIG_BLOCK
        && ARG1 != VKI_SIG_UNBLOCK && ARG1 != VKI_SIG_SETMASK) {
        arg1 = VKI_SIG_SETMASK;
    }
    SET_STATUS_from_SysRes(
                           VG_(do_sys_sigprocmask) ( tid, arg1, (vki_sigset_t*)ARG2,
                                                    (vki_sigset_t*)ARG3 )
                           );
    
    if (SUCCESS)
        *flags |= SfPollAfter;
}
POST(__pthread_sigmask)
{
    vg_assert(SUCCESS);
    if (RES == 0 && ARG3 != 0)
        POST_MEM_WRITE( ARG3, sizeof(vki_sigset_t));
}


// SYS___sigwait 330
// int  sigwait(const sigset_t * __restrict, int * __restrict) __DARWIN_ALIAS_C(sigwait);
PRE(__sigwait)
{
    *flags |= SfMayBlock;
    PRINT("sys_sigwait ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
          ARG1,ARG2);
    PRE_REG_READ2(int, "sigwait",
                  const vki_sigset_t *, set, int *, sig);
    if (ARG1 != 0) {
        PRE_MEM_READ(  "sigwait(set)",  ARG1, sizeof(vki_sigset_t));
    }
    if (ARG2 != 0) {
        PRE_MEM_WRITE( "sigwait(sig)", ARG2, sizeof(int));
    }
}

POST(__sigwait)
{
    if (ARG2 != 0) {
        POST_MEM_WRITE( ARG2, sizeof(int));
    }
}

PRE(__pthread_canceled)
{
   *flags |= SfMayBlock; /* might kill this thread??? */
   /* I don't think so -- I think it just changes the cancellation
      state.  But taking no chances. */
   PRINT("__pthread_canceled ( %#lx )", ARG1);
   PRE_REG_READ1(long, "__pthread_canceled", void*, arg1);
}


PRE(__pthread_markcancel)
{
   *flags |= SfMayBlock; /* might kill this thread??? */
   PRINT("__pthread_markcancel ( %#lx )", ARG1);
   PRE_REG_READ1(long, "__pthread_markcancel", void*, arg1);
   /* Just let it go through.  No idea if this is correct. */
}


PRE(__disable_threadsignal)
{
   vki_sigset_t set;
   PRINT("__disable_threadsignal(%ld, %ld, %ld)", SARG1, SARG2, SARG3);
   /* I don't think this really looks at its arguments.  So don't
      bother to check them. */

   VG_(sigfillset)( &set );
   SET_STATUS_from_SysRes(
      VG_(do_sys_sigprocmask) ( tid, VKI_SIG_BLOCK, &set, NULL )
   );

   /* We don't expect that blocking all signals for this thread could
      cause any more to be delivered (how could it?), but just in case
      .. */
   if (SUCCESS)
      *flags |= SfPollAfter;
}


PRE(__pthread_chdir)
{
    PRINT("__pthread_chdir ( %#lx(%s) )", ARG1, (HChar*)ARG1);
    PRE_REG_READ1(long, "__pthread_chdir", const char *, path);
    PRE_MEM_RASCIIZ( "__pthread_chdir(path)", ARG1 );
}



PRE(__pthread_fchdir)
{
    PRINT("__pthread_fchdir ( %lu )", ARG1);
    PRE_REG_READ1(long, "__pthread_fchdir", unsigned int, fd);
}


PRE(kdebug_trace)
{
   PRINT("kdebug_trace(%ld, %ld, %ld, %ld, %ld, %ld)", 
         SARG1, SARG2, SARG3, SARG4, SARG5, SARG6);
   /*
     Don't check anything - some clients pass fewer arguments.
   PRE_REG_READ6(long, "kdebug_trace", 
                 int,"code", int,"arg1", int,"arg2", 
                 int,"arg3", int,"arg4", int,"arg5");
   */
}


PRE(seteuid)
{
    PRINT("seteuid(%lu)", ARG1);
    PRE_REG_READ1(long, "seteuid", vki_uid_t, "uid");
}


PRE(setegid)
{
    PRINT("setegid(%lu)", ARG1);
    PRE_REG_READ1(long, "setegid", vki_uid_t, "uid");
}

PRE(settid)
{
    PRINT("settid(%lu, %lu)", ARG1, ARG2);
    PRE_REG_READ2(long, "settid", vki_uid_t, "uid", vki_gid_t, "gid");
}

PRE(gettid)
{
    PRINT("gettid()");
    PRE_REG_READ0(long, gettid);
}

/* XXX need to check whether we need POST operations for
 * waitevent, watchevent, modwatch -- jpeach 
 */
PRE(watchevent)
{
    PRINT("watchevent(%#lx, %#lx)", ARG1, ARG2);
    PRE_REG_READ2(long, "watchevent",
        vki_eventreq *, "event", unsigned int, "eventmask");

    PRE_MEM_READ("watchevent(event)", ARG1, sizeof(vki_eventreq));
    PRE_MEM_READ("watchevent(eventmask)", ARG2, sizeof(unsigned int));
    *flags |= SfMayBlock;
}

#define WAITEVENT_FAST_POLL ((Addr)(struct timeval *)-1)
PRE(waitevent)
{
   PRINT("waitevent(%#lx, %#lx)", ARG1, ARG2);
   PRE_REG_READ2(long, "waitevent",
      vki_eventreq *, "event", struct timeval *, "timeout");
   PRE_MEM_WRITE("waitevent(event)", ARG1, sizeof(vki_eventreq));

   if (ARG2  &&  ARG2 != WAITEVENT_FAST_POLL) {
      PRE_timeval_READ("waitevent(timeout)", ARG2);
   }

   /* XXX ((timeval*)-1) is valid for ARG2 -- jpeach */
   *flags |= SfMayBlock;
}

POST(waitevent)
{
   POST_MEM_WRITE(ARG1, sizeof(vki_eventreq));
}

PRE(modwatch)
{
   PRINT("modwatch(%#lx, %#lx)", ARG1, ARG2);
   PRE_REG_READ2(long, "modwatch",
      vki_eventreq *, "event", unsigned int, "eventmask");

   PRE_MEM_READ("modwatch(event)", ARG1, sizeof(vki_eventreq));
   PRE_MEM_READ("modwatch(eventmask)", ARG2, sizeof(unsigned int));
}

PRE(getxattr)
{
   PRINT("getxattr(%#lx(%s), %#lx(%s), %#lx, %lu, %lu, %ld)",
         ARG1, (HChar *)ARG1, ARG2, (HChar *)ARG2, ARG3, ARG4, ARG5, SARG6);

   PRE_REG_READ6(vki_ssize_t, "getxattr",
                const char *, path, char *, name, void *, value,
                vki_size_t, size, uint32_t, position, int, options);
   PRE_MEM_RASCIIZ("getxattr(path)", ARG1);
   PRE_MEM_RASCIIZ("getxattr(name)", ARG2);
   if (ARG3)
      PRE_MEM_WRITE( "getxattr(value)", ARG3, ARG4);
}

POST(getxattr)
{
   vg_assert((vki_ssize_t)RES >= 0);
   if (ARG3)
      POST_MEM_WRITE(ARG3, (vki_ssize_t)RES);
}

PRE(fgetxattr)
{
   PRINT("fgetxattr(%ld, %#lx(%s), %#lx, %lu, %lu, %ld)",
         SARG1, ARG2, (HChar *)ARG2, ARG3, ARG4, ARG5, SARG6);

   PRE_REG_READ6(vki_ssize_t, "fgetxattr",
                 int, fd, char *, name, void *, value,
                 vki_size_t, size, uint32_t, position, int, options);
   PRE_MEM_RASCIIZ("getxattr(name)", ARG2);
   PRE_MEM_WRITE( "getxattr(value)", ARG3, ARG4);
}

POST(fgetxattr)
{
   vg_assert((vki_ssize_t)RES >= 0);
   POST_MEM_WRITE(ARG3, (vki_ssize_t)RES);
}

PRE(setxattr)
{
   PRINT("setxattr ( %#lx(%s), %#lx(%s), %#lx, %lu, %lu, %ld )", 
         ARG1, (HChar *)ARG1, ARG2, (HChar*)ARG2, ARG3, ARG4, ARG5, SARG6 );
   PRE_REG_READ6(int, "setxattr", 
                 const char *,"path", char *,"name", void *,"value", 
                 vki_size_t,"size", uint32_t,"position", int,"options" );
   
   PRE_MEM_RASCIIZ( "setxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "setxattr(name)", ARG2 );
   PRE_MEM_READ( "setxattr(value)", ARG3, ARG4 );
}


PRE(fsetxattr)
{
   PRINT( "fsetxattr ( %ld, %#lx(%s), %#lx, %lu, %lu, %ld )", 
          SARG1, ARG2, (HChar*)ARG2, ARG3, ARG4, ARG5, SARG6 );
   PRE_REG_READ6(int, "fsetxattr", 
                 int,"fd", char *,"name", void *,"value", 
                 vki_size_t,"size", uint32_t,"position", int,"options" );
   
   PRE_MEM_RASCIIZ( "fsetxattr(name)", ARG2 );
   PRE_MEM_READ( "fsetxattr(value)", ARG3, ARG4 );
}


PRE(removexattr)
{
   PRINT( "removexattr ( %#lx(%s), %#lx(%s), %ld )",
          ARG1, (HChar*)ARG1, ARG2, (HChar*)ARG2, SARG3 );
   PRE_REG_READ3(int, "removexattr",
                 const char*, "path", char*, "attrname", int, "options");
   PRE_MEM_RASCIIZ( "removexattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "removexattr(attrname)", ARG2 );
}


PRE(fremovexattr)
{
   PRINT( "fremovexattr ( %ld, %#lx(%s), %ld )",
          SARG1, ARG2, (HChar*)ARG2, SARG3 );
   PRE_REG_READ3(int, "fremovexattr",
                 int, "fd", char*, "attrname", int, "options");
   PRE_MEM_RASCIIZ( "removexattr(attrname)", ARG2 );
}


PRE(listxattr)
{
   PRINT( "listxattr ( %#lx(%s), %#lx, %lu, %ld )", 
          ARG1, (HChar *)ARG1, ARG2, ARG3, SARG4 );
   PRE_REG_READ4 (long, "listxattr", 
                 const char *,"path", char *,"namebuf", 
                 vki_size_t,"size", int,"options" );
   
   PRE_MEM_RASCIIZ( "listxattr(path)", ARG1 );
   PRE_MEM_WRITE( "listxattr(namebuf)", ARG2, ARG3 );
   *flags |= SfMayBlock;
}
POST(listxattr)
{
   vg_assert(SUCCESS);
   vg_assert((vki_ssize_t)RES >= 0);
   POST_MEM_WRITE( ARG2, (vki_ssize_t)RES );
}


PRE(flistxattr)
{
   PRINT( "flistxattr ( %ld, %#lx, %lu, %ld )", 
          SARG1, ARG2, ARG3, SARG4 );
   PRE_REG_READ4 (long, "flistxattr", 
                  int, "fd", char *,"namebuf", 
                 vki_size_t,"size", int,"options" );
   PRE_MEM_WRITE( "flistxattr(namebuf)", ARG2, ARG3 );
   *flags |= SfMayBlock;
}
POST(flistxattr)
{
   vg_assert(SUCCESS);
   vg_assert((vki_ssize_t)RES >= 0);
   POST_MEM_WRITE( ARG2, (vki_ssize_t)RES );
}


PRE(shmat)
{
   UWord arg2tmp;
   PRINT("shmat ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "shmat",
                 int, shmid, const void *, shmaddr, int, shmflg);
   arg2tmp = ML_(generic_PRE_sys_shmat)(tid, ARG1,ARG2,ARG3);
   if (arg2tmp == 0)
      SET_STATUS_Failure( VKI_EINVAL );
   else
      ARG2 = arg2tmp;  // used in POST
}
POST(shmat)
{
   ML_(generic_POST_sys_shmat)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(shmctl)
{
   PRINT("shmctl ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
   PRE_REG_READ3(long, "shmctl",
                 int, shmid, int, cmd, struct vki_shmid_ds *, buf);
   ML_(generic_PRE_sys_shmctl)(tid, ARG1,ARG2,ARG3);
}
POST(shmctl)
{
   ML_(generic_POST_sys_shmctl)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(shmdt)
{
   PRINT("shmdt ( %#lx )",ARG1);
   PRE_REG_READ1(long, "shmdt", const void *, shmaddr);
   if (!ML_(generic_PRE_sys_shmdt)(tid, ARG1))
      SET_STATUS_Failure( VKI_EINVAL );
}
POST(shmdt)
{
   ML_(generic_POST_sys_shmdt)(tid, RES,ARG1);
}

PRE(shmget)
{
   PRINT("shmget ( %ld, %lu, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "shmget", vki_key_t, key, vki_size_t, size, int, shmflg);
}

PRE(shm_open)
{
   PRINT("shm_open(%#lx(%s), %ld, %lu)", ARG1, (HChar *)ARG1, SARG2, ARG3);
   PRE_REG_READ3(long, "shm_open",
                 const char *,"name", int,"flags", vki_mode_t,"mode");

   PRE_MEM_RASCIIZ( "shm_open(filename)", ARG1 );

   *flags |= SfMayBlock;
}
POST(shm_open)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "shm_open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG1);
   }
}

PRE(shm_unlink)
{
   *flags |= SfMayBlock;
   PRINT("shm_unlink ( %#lx(%s) )", ARG1, (HChar*)ARG1);
   PRE_REG_READ1(long, "shm_unlink", const char *, pathname);
   PRE_MEM_RASCIIZ( "shm_unlink(pathname)", ARG1 );
}
POST(shm_unlink)
{
   /* My reading of the man page suggests that a call may cause memory
      mappings to change: "if no references exist at the time of the
      call to shm_unlink(), the resources are reclaimed immediately".
      So we need to resync here, sigh. */
   ML_(sync_mappings)("after", "shm_unlink", 0);
}

PRE(stat_extended)
{
   PRINT("stat_extended( %#lx(%s), %#lx, %#lx, %#lx )",
         ARG1, (HChar *)ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "stat_extended", char *, file_name, struct stat *, buf, 
                 void *, fsacl, vki_size_t *, fsacl_size);
   PRE_MEM_RASCIIZ( "stat_extended(file_name)",  ARG1 );
   PRE_MEM_WRITE(   "stat_extended(buf)",        ARG2, sizeof(struct vki_stat) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      PRE_MEM_WRITE("stat_extended(fsacl)",      ARG3, *(vki_size_t *)ARG4 );
   PRE_MEM_READ(    "stat_extended(fsacl_size)", ARG4, sizeof(vki_size_t) );
}
POST(stat_extended)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      POST_MEM_WRITE( ARG3, *(vki_size_t *)ARG4 );
   POST_MEM_WRITE( ARG4, sizeof(vki_size_t) );
}


PRE(lstat_extended)
{
   PRINT("lstat_extended( %#lx(%s), %#lx, %#lx, %#lx )",
         ARG1, (HChar *)ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "lstat_extended", char *, file_name, struct stat *, buf, 
                 void *, fsacl, vki_size_t *, fsacl_size);
   PRE_MEM_RASCIIZ( "lstat_extended(file_name)",  ARG1 );
   PRE_MEM_WRITE(   "lstat_extended(buf)",        ARG2, sizeof(struct vki_stat) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      PRE_MEM_WRITE("lstat_extended(fsacl)",      ARG3, *(vki_size_t *)ARG4 );
   PRE_MEM_READ(    "lstat_extended(fsacl_size)", ARG4, sizeof(vki_size_t) );
}
POST(lstat_extended)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      POST_MEM_WRITE( ARG3, *(vki_size_t *)ARG4 );
   POST_MEM_WRITE( ARG4, sizeof(vki_size_t) );
}


PRE(fstat_extended)
{
   PRINT("fstat_extended( %ld, %#lx, %#lx, %#lx )",
         SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "fstat_extended", int, fd, struct stat *, buf, 
                 void *, fsacl, vki_size_t *, fsacl_size);
   PRE_MEM_WRITE(   "fstat_extended(buf)",        ARG2, sizeof(struct vki_stat) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      PRE_MEM_WRITE("fstat_extended(fsacl)",      ARG3, *(vki_size_t *)ARG4 );
   PRE_MEM_READ(    "fstat_extended(fsacl_size)", ARG4, sizeof(vki_size_t) );
}
POST(fstat_extended)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      POST_MEM_WRITE( ARG3, *(vki_size_t *)ARG4 );
   POST_MEM_WRITE( ARG4, sizeof(vki_size_t) );
}


PRE(stat64_extended)
{
   PRINT("stat64_extended( %#lx(%s), %#lx, %#lx, %#lx )",
         ARG1, (HChar *)ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "stat64_extended", char *, file_name, struct stat64 *, buf, 
                 void *, fsacl, vki_size_t *, fsacl_size);
   PRE_MEM_RASCIIZ( "stat64_extended(file_name)",  ARG1 );
   PRE_MEM_WRITE(   "stat64_extended(buf)",        ARG2, sizeof(struct vki_stat64) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      PRE_MEM_WRITE("stat64_extended(fsacl)",      ARG3, *(vki_size_t *)ARG4 );
   PRE_MEM_READ(    "stat64_extended(fsacl_size)", ARG4, sizeof(vki_size_t) );
}
POST(stat64_extended)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      POST_MEM_WRITE( ARG3, *(vki_size_t *)ARG4 );
   POST_MEM_WRITE( ARG4, sizeof(vki_size_t) );
}


PRE(lstat64_extended)
{
   PRINT("lstat64_extended( %#lx(%s), %#lx, %#lx, %#lx )",
         ARG1, (HChar *)ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "lstat64_extended", char *, file_name, struct stat64 *, buf, 
                 void *, fsacl, vki_size_t *, fsacl_size);
   PRE_MEM_RASCIIZ( "lstat64_extended(file_name)",  ARG1 );
   PRE_MEM_WRITE(   "lstat64_extended(buf)",        ARG2, sizeof(struct vki_stat64) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      PRE_MEM_WRITE(   "lstat64_extended(fsacl)",   ARG3, *(vki_size_t *)ARG4 );
   PRE_MEM_READ(    "lstat64_extended(fsacl_size)", ARG4, sizeof(vki_size_t) );
}
POST(lstat64_extended)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      POST_MEM_WRITE( ARG3, *(vki_size_t *)ARG4 );
   POST_MEM_WRITE( ARG4, sizeof(vki_size_t) );
}


PRE(fstat64_extended)
{
   PRINT("fstat64_extended( %ld, %#lx, %#lx, %#lx )",
         SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "fstat64_extended", int, fd, struct stat64 *, buf, 
                 void *, fsacl, vki_size_t *, fsacl_size);
   PRE_MEM_WRITE(   "fstat64_extended(buf)",        ARG2, sizeof(struct vki_stat64) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      PRE_MEM_WRITE("fstat64_extended(fsacl)",      ARG3, *(vki_size_t *)ARG4 );
   PRE_MEM_READ(    "fstat64_extended(fsacl_size)", ARG4, sizeof(vki_size_t) );
}
POST(fstat64_extended)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
   if (ML_(safe_to_deref)( (void*)ARG4, sizeof(vki_size_t) ))
      POST_MEM_WRITE( ARG3, *(vki_size_t *)ARG4 );
   POST_MEM_WRITE( ARG4, sizeof(vki_size_t) );
}


PRE(fchmod_extended)
{
   /* DDD: Note: this is not really correct.  Handling of
      chmod_extended is broken in the same way. */
   PRINT("fchmod_extended ( %lu, %lu, %lu, %lu, %#lx )",
         ARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(long, "fchmod_extended", 
                 unsigned int, fildes, 
                 uid_t, uid,
                 gid_t, gid,
                 vki_mode_t, mode,
                 void* /*really,user_addr_t*/, xsecurity);
   /* DDD: relative to the xnu sources (kauth_copyinfilesec), this
      is just way wrong.  [The trouble is with the size, which depends on a
      non-trival kernel computation] */
   if (ARG5) {
      PRE_MEM_READ( "fchmod_extended(xsecurity)", ARG5, 
                    sizeof(struct vki_kauth_filesec) );
   }
}

PRE(chmod_extended)
{
   /* DDD: Note: this is not really correct.  Handling of
      fchmod_extended is broken in the same way. */
   PRINT("chmod_extended ( %#lx(%s), %ld, %ld, %ld, %#lx )",
         ARG1, ARG1 ? (HChar*)ARG1 : "(null)", ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(long, "chmod_extended", 
                 unsigned int, fildes, 
                 uid_t, uid,
                 gid_t, gid,
                 vki_mode_t, mode,
                 void* /*really,user_addr_t*/, xsecurity);
   PRE_MEM_RASCIIZ("chmod_extended(path)", ARG1);
   /* DDD: relative to the xnu sources (kauth_copyinfilesec), this
      is just way wrong.  [The trouble is with the size, which depends on a
      non-trival kernel computation] */
   if (ARG5) {
      PRE_MEM_READ( "chmod_extended(xsecurity)", ARG5, 
                    sizeof(struct vki_kauth_filesec) );
   }
}

PRE(open_extended)
{
   /* DDD: Note: this is not really correct.  Handling of
      {,f}chmod_extended is broken in the same way. */
   PRINT("open_extended ( %#lx(%s), %ld, %lu, %lu, %lu, %#lx )",
         ARG1, ARG1 ? (HChar*)ARG1 : "(null)",
	 SARG2, ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(long, "open_extended", 
                 char*, path,
                 int,   flags,
                 uid_t, uid,
                 gid_t, gid,
                 vki_mode_t, mode,
                 void* /*really,user_addr_t*/, xsecurity);
   PRE_MEM_RASCIIZ("open_extended(path)", ARG1);
   /* DDD: relative to the xnu sources (kauth_copyinfilesec), this
      is just way wrong.  [The trouble is with the size, which depends on a
      non-trival kernel computation] */
   if (ARG6)
      PRE_MEM_READ( "open_extended(xsecurity)", ARG6, 
                    sizeof(struct vki_kauth_filesec) );
}

// This is a ridiculous syscall.  Specifically, the 'entries' argument points
// to a buffer that contains one or more 'accessx_descriptor' structs followed
// by one or more strings.  Each accessx_descriptor contains a field,
// 'ad_name_offset', which points to one of the strings (or it can contain
// zero which means "reuse the string from the previous accessx_descriptor").
//
// What's really ridiculous is that we are only given the size of the overall
// buffer, not the number of accessx_descriptors, nor the number of strings.
// The kernel determines the number of accessx_descriptors by walking through
// them one by one, checking that the ad_name_offset points within the buffer,
// past the current point (or that it's a zero, unless its the first
// descriptor);  if so, we assume that this really is an accessx_descriptor,
// if not, we assume we've hit the strings section.  Gah.
//
// This affects us here because number of entries in the 'results' buffer is
// determined by the number of accessx_descriptors.  So we have to know that
// number in order to do PRE_MEM_WRITE/POST_MEM_WRITE of 'results'.  In
// practice, we skip the PRE_MEM_WRITE step because it's easier to do the
// computation after the syscall has succeeded, because the kernel will have
// checked for all the zillion different ways this syscall can fail, and we'll
// know we have a well-formed 'entries' buffer.  This means we might miss some
// uses of unaddressable memory but oh well.
//
PRE(access_extended)
{
   PRINT("access_extended( %#lx(%s), %lu, %#lx, %lu )",
         ARG1, (HChar *)ARG1, ARG2, ARG3, ARG4);
   // XXX: the accessx_descriptor struct contains padding, so this can cause
   // unnecessary undefined value errors.  But you arguably shouldn't be
   // passing undefined values to the kernel anyway...
   PRE_REG_READ4(int, "access_extended", void *, entries, vki_size_t, size, 
                 vki_errno_t *, results, vki_uid_t *, uid);
   PRE_MEM_READ("access_extended(entries)", ARG1, ARG2 );

   // XXX: as mentioned above, this check is too hard to do before the
   // syscall.
   //PRE_MEM_WRITE("access_extended(results)", ARG3, ??? );
}
POST(access_extended)
{
   // 'n_descs' is the number of descriptors we think are in the buffer.  We
   // start with the maximum possible value, which occurs if we have the
   // shortest possible string section.  The shortest string section allowed
   // consists of a single one-char string (plus the NUL char).  Hence the
   // '2'.
   struct vki_accessx_descriptor* entries = (struct vki_accessx_descriptor*)ARG1;
   SizeT size = ARG2;
   Int n_descs = (size - 2) / sizeof(struct accessx_descriptor);
   Int i;         // Current position in the descriptors section array.
   Int u;         // Upper bound on the length of the descriptors array
                  //   (recomputed each time around the loop)
   vg_assert(n_descs > 0);

   // Step through the descriptors, lowering 'n_descs' until we know we've
   // reached the string section.
   for (i = 0; True; i++) {
      // If we're past our estimate, we must be one past the end of the
      // descriptors section (ie. at the start of the string section).  Stop.
      if (i >= n_descs)
         break;

      // Get the array index for the string, but pretend momentarily that it
      // is actually another accessx_descriptor.  That gives us an upper bound
      // on the length of the descriptors section.  (Unless the index is zero,
      // in which case we have no new info.)
      u = entries[i].ad_name_offset / sizeof(struct vki_accessx_descriptor);
      if (u == 0) {
         vg_assert(i != 0);
         continue;
      }

      // If the upper bound is below our current estimate, revise that
      // estimate downwards.
      if (u < n_descs)
         n_descs = u;
   }

   // Sanity check.
   vg_assert(n_descs <= VKI_ACCESSX_MAX_DESCRIPTORS);

   POST_MEM_WRITE( ARG3, n_descs * sizeof(vki_errno_t) );
}


PRE(chflags)
{
   PRINT("chflags ( %#lx(%s), %lu )", ARG1, (HChar *)ARG1, ARG2);
   PRE_REG_READ2(int, "chflags", const char *,path, unsigned int,flags);
   PRE_MEM_RASCIIZ("chflags(path)", ARG1);

   // GrP fixme sanity-check flags value?
}

PRE(fchflags)
{
   PRINT("fchflags ( %ld, %lu )", SARG1, ARG2);
   PRE_REG_READ2(int, "fchflags", int,fd, unsigned int,flags);

   // GrP fixme sanity-check flags value?
}

PRE(stat64)
{
   PRINT("stat64 ( %#lx(%s), %#lx )", ARG1, (HChar *)ARG1, ARG2);
   PRE_REG_READ2(long, "stat", const char *,path, struct stat64 *,buf);
   PRE_MEM_RASCIIZ("stat64(path)", ARG1);
   PRE_MEM_WRITE( "stat64(buf)", ARG2, sizeof(struct vki_stat64) );
}
POST(stat64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
}

PRE(lstat64)
{
   PRINT("lstat64 ( %#lx(%s), %#lx )", ARG1, (HChar *)ARG1, ARG2);
   PRE_REG_READ2(long, "stat", const char *,path, struct stat64 *,buf);
   PRE_MEM_RASCIIZ("lstat64(path)", ARG1);
   PRE_MEM_WRITE( "lstat64(buf)", ARG2, sizeof(struct vki_stat64) );
}
POST(lstat64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
}

PRE(fstat64)
{
   PRINT("fstat64 ( %lu, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "fstat", unsigned int, fd, struct stat64 *, buf);
   PRE_MEM_WRITE( "fstat64(buf)", ARG2, sizeof(struct vki_stat64) );
}
POST(fstat64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
}

PRE(getfsstat)
{
   PRINT("getfsstat(%#lx, %ld, %ld)", ARG1, SARG2, SARG3);
   PRE_REG_READ3(int, "getfsstat",
                 struct vki_statfs *, buf, int, bufsize, int, flags);
   if (ARG1) {
      // ARG2 is a BYTE SIZE
      PRE_MEM_WRITE("getfsstat(buf)", ARG1, ARG2);
   }
}
POST(getfsstat)
{
   if (ARG1) {
      // RES is a STRUCT COUNT
      POST_MEM_WRITE(ARG1, RES * sizeof(struct vki_statfs));
   }
}

PRE(getfsstat64)
{
   PRINT("getfsstat64(%#lx, %ld, %ld)", ARG1, SARG2, SARG3);
   PRE_REG_READ3(int, "getfsstat64",
                 struct vki_statfs64 *, buf, int, bufsize, int, flags);
   if (ARG1) {
      // ARG2 is a BYTE SIZE
      PRE_MEM_WRITE("getfsstat64(buf)", ARG1, ARG2);
   }
}
POST(getfsstat64)
{
   if (ARG1) {
      // RES is a STRUCT COUNT
      POST_MEM_WRITE(ARG1, RES * sizeof(struct vki_statfs64));
   }
}

PRE(mount)
{
   // Nb: depending on 'flags', the 'type' and 'data' args may be ignored.
   // We are conservative and check everything, except the memory pointed to
   // by 'data'.
   *flags |= SfMayBlock;
   PRINT("sys_mount( %#lx(%s), %#lx(%s), %#lx, %#lx )",
         ARG1, (HChar*)ARG1, ARG2, (HChar*)ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "mount",
                 const char *, type, const char *, dir,
                 int, flags, void *, data);
   PRE_MEM_RASCIIZ( "mount(type)", ARG1);
   PRE_MEM_RASCIIZ( "mount(dir)", ARG2);
}


static void scan_attrlist(ThreadId tid, struct vki_attrlist *attrList, 
                          void *attrBuf, SizeT attrBufSize, 
                          void (*fn)(ThreadId, void *attrData, SizeT size)
                          )
{
   typedef struct {
      uint32_t attrBit;
      int32_t attrSize;
   } attrspec;
   static const attrspec commonattr[] = {
      // This order is important.
#if DARWIN_VERS >= DARWIN_10_6
      { ATTR_CMN_RETURNED_ATTRS,  sizeof(attribute_set_t) }, 
#endif
      { ATTR_CMN_NAME,            -1 }, 
      { ATTR_CMN_DEVID,           sizeof(dev_t) }, 
      { ATTR_CMN_FSID,            sizeof(fsid_t) }, 
      { ATTR_CMN_OBJTYPE,         sizeof(fsobj_type_t) }, 
      { ATTR_CMN_OBJTAG,          sizeof(fsobj_tag_t) }, 
      { ATTR_CMN_OBJID,           sizeof(fsobj_id_t) }, 
      { ATTR_CMN_OBJPERMANENTID,  sizeof(fsobj_id_t) }, 
      { ATTR_CMN_PAROBJID,        sizeof(fsobj_id_t) }, 
      { ATTR_CMN_SCRIPT,          sizeof(text_encoding_t) }, 
      { ATTR_CMN_CRTIME,          sizeof(struct timespec) }, 
      { ATTR_CMN_MODTIME,         sizeof(struct timespec) }, 
      { ATTR_CMN_CHGTIME,         sizeof(struct timespec) }, 
      { ATTR_CMN_ACCTIME,         sizeof(struct timespec) }, 
      { ATTR_CMN_BKUPTIME,        sizeof(struct timespec) }, 
      { ATTR_CMN_FNDRINFO,        32 /*FileInfo+ExtendedFileInfo, or FolderInfo+ExtendedFolderInfo*/ }, 
      { ATTR_CMN_OWNERID,         sizeof(uid_t) }, 
      { ATTR_CMN_GRPID,           sizeof(gid_t) }, 
      { ATTR_CMN_ACCESSMASK,      sizeof(uint32_t) }, 
      { ATTR_CMN_NAMEDATTRCOUNT,  sizeof(uint32_t) }, 
      { ATTR_CMN_NAMEDATTRLIST,   -1 }, 
      { ATTR_CMN_FLAGS,           sizeof(uint32_t) }, 
      { ATTR_CMN_USERACCESS,      sizeof(uint32_t) }, 
      { ATTR_CMN_EXTENDED_SECURITY, -1 }, 
      { ATTR_CMN_UUID,            sizeof(guid_t) }, 
      { ATTR_CMN_GRPUUID,         sizeof(guid_t) }, 
      { ATTR_CMN_FILEID,          sizeof(uint64_t) }, 
      { ATTR_CMN_PARENTID,        sizeof(uint64_t) }, 
#if DARWIN_VERS >= DARWIN_10_6
      { ATTR_CMN_FULLPATH,        -1 }, 
#endif
#if DARWIN_VERS >= DARWIN_10_8
      { ATTR_CMN_ADDEDTIME,       -1 }, 
#endif
      { 0,                        0 }
   };
   static const attrspec volattr[] = {
      // This order is important.
      { ATTR_VOL_INFO,            0 }, 
      { ATTR_VOL_FSTYPE,          sizeof(uint32_t) }, 
      { ATTR_VOL_SIGNATURE,       sizeof(uint32_t) }, 
      { ATTR_VOL_SIZE,            sizeof(off_t) }, 
      { ATTR_VOL_SPACEFREE,       sizeof(off_t) }, 
      { ATTR_VOL_SPACEAVAIL,      sizeof(off_t) }, 
      { ATTR_VOL_MINALLOCATION,   sizeof(off_t) }, 
      { ATTR_VOL_ALLOCATIONCLUMP, sizeof(off_t) }, 
      { ATTR_VOL_IOBLOCKSIZE,     sizeof(uint32_t) }, 
      { ATTR_VOL_OBJCOUNT,        sizeof(uint32_t) }, 
      { ATTR_VOL_FILECOUNT,       sizeof(uint32_t) }, 
      { ATTR_VOL_DIRCOUNT,        sizeof(uint32_t) }, 
      { ATTR_VOL_MAXOBJCOUNT,     sizeof(uint32_t) }, 
      { ATTR_VOL_MOUNTPOINT,      -1 }, 
      { ATTR_VOL_NAME,            -1 }, 
      { ATTR_VOL_MOUNTFLAGS,      sizeof(uint32_t) }, 
      { ATTR_VOL_MOUNTEDDEVICE,   -1 }, 
      { ATTR_VOL_ENCODINGSUSED,   sizeof(uint64_t) }, 
      { ATTR_VOL_CAPABILITIES,    sizeof(vol_capabilities_attr_t) }, 
#if DARWIN_VERS >= DARWIN_10_6
      { ATTR_VOL_UUID,            sizeof(uuid_t) }, 
#endif
      { ATTR_VOL_ATTRIBUTES,      sizeof(vol_attributes_attr_t) }, 
      { 0,                        0 }
   };
   static const attrspec dirattr[] = {
      // This order is important.
      { ATTR_DIR_LINKCOUNT,       sizeof(uint32_t) }, 
      { ATTR_DIR_ENTRYCOUNT,      sizeof(uint32_t) }, 
      { ATTR_DIR_MOUNTSTATUS,     sizeof(uint32_t) }, 
      { 0,                        0 }
   };
   static const attrspec fileattr[] = {
      // This order is important.
      { ATTR_FILE_LINKCOUNT,      sizeof(uint32_t) }, 
      { ATTR_FILE_TOTALSIZE,      sizeof(off_t) }, 
      { ATTR_FILE_ALLOCSIZE,      sizeof(off_t) }, 
      { ATTR_FILE_IOBLOCKSIZE,    sizeof(uint32_t) }, 
      { ATTR_FILE_CLUMPSIZE,      sizeof(uint32_t) }, 
      { ATTR_FILE_DEVTYPE,        sizeof(uint32_t) }, 
      { ATTR_FILE_FILETYPE,       sizeof(uint32_t) }, 
      { ATTR_FILE_FORKCOUNT,      sizeof(uint32_t) }, 
      { ATTR_FILE_FORKLIST,       -1 }, 
      { ATTR_FILE_DATALENGTH,     sizeof(off_t) }, 
      { ATTR_FILE_DATAALLOCSIZE,  sizeof(off_t) }, 
      { ATTR_FILE_DATAEXTENTS,    sizeof(extentrecord) }, 
      { ATTR_FILE_RSRCLENGTH,     sizeof(off_t) }, 
      { ATTR_FILE_RSRCALLOCSIZE,  sizeof(off_t) }, 
      { ATTR_FILE_RSRCEXTENTS,    sizeof(extentrecord) }, 
      { 0,                        0 }
   };
   static const attrspec forkattr[] = {
      // This order is important.
      { ATTR_FORK_TOTALSIZE,      sizeof(off_t) }, 
      { ATTR_FORK_ALLOCSIZE,      sizeof(off_t) }, 
      { 0,                        0 }
   };

   static const attrspec *attrdefs[5] = { 
      commonattr, volattr, dirattr, fileattr, forkattr 
   };
   attrgroup_t a[5];
   uint8_t *d, *dend;
   int g, i;

   vg_assert(attrList->bitmapcount == 5);
   VG_(memcpy)(a, &attrList->commonattr, sizeof(a));
   d = attrBuf;
   dend = d + attrBufSize;

#if DARWIN_VERS >= DARWIN_10_6
   // ATTR_CMN_RETURNED_ATTRS tells us what's really here, if set
   if (a[0] & ATTR_CMN_RETURNED_ATTRS) {
       // fixme range check this?
       a[0] &= ~ATTR_CMN_RETURNED_ATTRS;
       fn(tid, d, sizeof(attribute_set_t));
       VG_(memcpy)(a, d, sizeof(a));
   }
#endif

   for (g = 0; g < 5; g++) {
      for (i = 0; attrdefs[g][i].attrBit; i++) {
         uint32_t bit = attrdefs[g][i].attrBit;
         int32_t size = attrdefs[g][i].attrSize;

         if (a[g] & bit) {
             a[g] &= ~bit;  // clear bit for error check later
            if (size == -1) {
               attrreference_t *ref = (attrreference_t *)d;
               size = MIN(sizeof(attrreference_t), dend - d);
               fn(tid, d, size);
               if (size >= sizeof(attrreference_t)  &&  
                   d + ref->attr_dataoffset < dend) 
               {
                  fn(tid, d + ref->attr_dataoffset, 
                     MIN(ref->attr_length, dend - (d + ref->attr_dataoffset)));
               }
               d += size;
            } 
            else {
               size = MIN(size, dend - d);
               fn(tid, d, size);
               d += size;
            }
            
            if ((uintptr_t)d % 4) d += 4 - ((uintptr_t)d % 4);
            if (d > dend) d = dend;
         }
      }

      // Known bits are cleared. Die if any bits are left.
      if (a[g] != 0) {
         VG_(message)(Vg_UserMsg, "UNKNOWN attrlist flags %d:0x%x\n", g, a[g]);
      }
   }
}

static void get1attr(ThreadId tid, void *attrData, SizeT attrDataSize)
{
   POST_MEM_WRITE((Addr)attrData, attrDataSize);
}

static void set1attr(ThreadId tid, void *attrData, SizeT attrDataSize)
{
   PRE_MEM_READ("setattrlist(attrBuf value)", (Addr)attrData, attrDataSize);
}

PRE(getattrlist)
{
   PRINT("getattrlist(%#lx(%s), %#lx, %#lx, %lu, %lu)", 
         ARG1, (HChar *)ARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(int, "getattrlist", 
                 const char *,path, struct vki_attrlist *,attrList, 
                 void *,attrBuf, vki_size_t,attrBufSize, unsigned int,options);
   PRE_MEM_RASCIIZ("getattrlist(path)", ARG1);
   PRE_MEM_READ("getattrlist(attrList)", ARG2, sizeof(struct vki_attrlist));
   PRE_MEM_WRITE("getattrlist(attrBuf)", ARG3, ARG4);
}

POST(getattrlist) 
{
   if (ARG4 > sizeof(vki_uint32_t)) {
      // attrBuf is uint32_t size followed by attr data
      vki_uint32_t *sizep = (vki_uint32_t *)ARG3;
      POST_MEM_WRITE(ARG3, sizeof(vki_uint32_t));
      if (ARG5 & FSOPT_REPORT_FULLSIZE) {
         // *sizep is bytes required for return value, including *sizep
      } else {
         // *sizep is actual bytes returned, including *sizep
      }
      scan_attrlist(tid, (struct vki_attrlist *)ARG2, sizep+1, MIN(*sizep, ARG4), &get1attr);
   }
}


PRE(setattrlist)
{
   PRINT("setattrlist(%#lx(%s), %#lx, %#lx, %lu, %lu)", 
         ARG1, (HChar *)ARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(int, "setattrlist", 
                 const char *,path, struct vki_attrlist *,attrList, 
                 void *,attrBuf, vki_size_t,attrBufSize, unsigned int,options);
   PRE_MEM_RASCIIZ("setattrlist(path)", ARG1);
   PRE_MEM_READ("setattrlist(attrList)", ARG2, sizeof(struct vki_attrlist));
   scan_attrlist(tid, (struct vki_attrlist *)ARG2, (void*)ARG3, ARG4, &set1attr);
}


PRE(getdirentriesattr)
{
   PRINT("getdirentriesattr(%ld, %#lx, %#lx, %lu, %#lx, %#lx, %#lx, %lu)", 
         SARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8);
   PRE_REG_READ8(int, "getdirentriesattr", 
                 int,fd, struct vki_attrlist *,attrList, 
                 void *,attrBuf, size_t,attrBufSize, 
                 unsigned int *,count, unsigned int *,basep, 
                 unsigned int *,newState, unsigned int,options);
   PRE_MEM_READ("getdirentriesattr(attrList)", 
                ARG2, sizeof(struct vki_attrlist));
   PRE_MEM_WRITE("getdirentriesattr(attrBuf)", ARG3, ARG4);
   PRE_MEM_READ("getdirentriesattr(count)", ARG5, sizeof(unsigned int));
   PRE_MEM_WRITE("getdirentriesattr(count)", ARG5, sizeof(unsigned int));
   PRE_MEM_WRITE("getdirentriesattr(basep)", ARG6, sizeof(unsigned int));
   PRE_MEM_WRITE("getdirentriesattr(newState)", ARG7, sizeof(unsigned int));
}
POST(getdirentriesattr) 
{
   char *p, *end;
   unsigned int count;
   unsigned int i;

   POST_MEM_WRITE(ARG5, sizeof(unsigned int));
   POST_MEM_WRITE(ARG6, sizeof(unsigned int));
   POST_MEM_WRITE(ARG7, sizeof(unsigned int));

   // return buffer is concatenation of variable-size structs
   count = *(unsigned int *)ARG5;
   p = (char *)ARG3;
   end = (char *)ARG3 + ARG4;
   for (i = 0; i < count; i++) {
      vg_assert(p < end);  // failure is kernel bug or Valgrind bug
      p += *(unsigned int *)p;
   }

   POST_MEM_WRITE(ARG3, p - (char *)ARG3);

   PRINT("got %d records, %ld/%lu bytes\n",
         count, (Addr)p-(Addr)ARG3, ARG4);
}

PRE(exchangedata)
{
   PRINT("exchangedata(%#lx(%s), %#lx(%s), %lu)",
         ARG1, (HChar*)ARG1, ARG2, (HChar*)ARG2, ARG3);
   PRE_REG_READ3(int, "exchangedata", 
                 char *, path1, char *, path2, unsigned long, options);
   PRE_MEM_RASCIIZ( "exchangedata(path1)", ARG1 );
   PRE_MEM_RASCIIZ( "exchangedata(path2)", ARG2 );
}

PRE(fsctl)
{
   PRINT("fsctl ( %#lx(%s), %lu, %#lx, %lu )",
         ARG1, (HChar *)ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4( long, "fsctl", 
                  char *,"path", unsigned int,"request", 
                  void *,"data", unsigned int,"options");
   
   PRE_MEM_RASCIIZ( "fsctl(path)", ARG1 );

   switch (ARG2) {
   case VKI_afpfsByteRangeLock2FSCTL: {
      struct vki_ByteRangeLockPB2 *pb = (struct vki_ByteRangeLockPB2 *)ARG3;
      PRE_FIELD_READ("fsctl(afpfsByteRangeLock2, pb->offset)", 
                     pb->offset);
      PRE_FIELD_READ("fsctl(afpfsByteRangeLock2, pb->length)", 
                     pb->length);
      PRE_FIELD_READ("fsctl(afpfsByteRangeLock2, pb->unLockFlag)", 
                     pb->unLockFlag);
      PRE_FIELD_READ("fsctl(afpfsByteRangeLock2, pb->startEndFlag)", 
                     pb->startEndFlag);
      PRE_FIELD_READ("fsctl(afpfsByteRangeLock2, pb->fd)", 
                     pb->fd);

      PRE_FIELD_WRITE("fsctl(afpfsByteRangeLock2, pb->retRangeStart)", 
                      pb->retRangeStart);

      // GrP fixme check fd
      break;
   }
   case VKI_FSIOC_SYNC_VOLUME:
       PRE_MEM_READ( "fsctl(FSIOC_SYNC_VOLUME)", ARG3, sizeof(int) );
       break;

   default:
      // fsctl requests use ioctl encoding
      ML_(PRE_unknown_ioctl)(tid, ARG2, ARG3);
      break;
   }
}

POST(fsctl)
{
   switch (ARG2) {
   case VKI_afpfsByteRangeLock2FSCTL: {
      struct vki_ByteRangeLockPB2 *pb = (struct vki_ByteRangeLockPB2 *)ARG3;
      POST_FIELD_WRITE(pb->retRangeStart);
      break;
   }
   case VKI_FSIOC_SYNC_VOLUME:
       break;

   default:
      // fsctl requests use ioctl encoding
      ML_(POST_unknown_ioctl)(tid, RES, ARG2, ARG3);
      break;
   }
}

PRE(initgroups)
{
    PRINT("initgroups(%s, %#lx, %lu)", (HChar *)ARG1, ARG2, ARG3);
    PRE_REG_READ3(long, "initgroups",
        int, setlen, vki_gid_t *, gidset, vki_uid_t, gmuid);
    PRE_MEM_READ("gidset", ARG2, ARG1 * sizeof(vki_gid_t));
}


//--------- posix_spawn ---------//
/* Largely copied from PRE(sys_execve) in syswrap-generic.c, and from
   the simpler AIX equivalent (syswrap-aix5.c). */
// Pre_read a char** argument.
static void pre_argv_envp(Addr a, ThreadId tid, const HChar* s1, const HChar* s2)
{
   while (True) {
      Addr a_deref;
      Addr* a_p = (Addr*)a;
      PRE_MEM_READ( s1, (Addr)a_p, sizeof(Addr) );
      a_deref = *a_p;
      if (0 == a_deref)
         break;
      PRE_MEM_RASCIIZ( s2, a_deref );
      a += sizeof(char*);
   }
}
static SysRes simple_pre_exec_check ( const HChar* exe_name,
                                      Bool trace_this_child )
{
   Int fd, ret;
   SysRes res;
   Bool setuid_allowed;

   // Check it's readable
   res = VG_(open)(exe_name, VKI_O_RDONLY, 0);
   if (sr_isError(res)) {
      return res;
   }
   fd = sr_Res(res);
   VG_(close)(fd);

   // Check we have execute permissions.  We allow setuid executables
   // to be run only in the case when we are not simulating them, that
   // is, they to be run natively.
   setuid_allowed = trace_this_child  ? False  : True;
   ret = VG_(check_executable)(NULL/*&is_setuid*/,
                               exe_name, setuid_allowed);
   if (0 != ret) {
      return VG_(mk_SysRes_Error)(ret);
   }
   return VG_(mk_SysRes_Success)(0);
}
PRE(posix_spawn)
{
   HChar*       path = NULL;       /* path to executable */
   HChar**      envp = NULL;
   HChar**      argv = NULL;
   HChar**      arg2copy;
   HChar*       launcher_basename = NULL;
   Int          i, j, tot_args;
   SysRes       res;
   Bool         trace_this_child;

   /* args: pid_t* pid
            char*  path
            posix_spawn_file_actions_t* file_actions
            char** argv
            char** envp
   */
   PRINT("posix_spawn( %#lx, %#lx(%s), %#lx, %#lx, %#lx )",
         ARG1, ARG2, ARG2 ? (HChar*)ARG2 : "(null)", ARG3, ARG4, ARG5 );

   /* Standard pre-syscall checks */

   PRE_REG_READ5(int, "posix_spawn", vki_pid_t*, pid, char*, path,
                 void*, file_actions, char**, argv, char**, envp );
   PRE_MEM_WRITE("posix_spawn(pid)", ARG1, sizeof(vki_pid_t) );
   PRE_MEM_RASCIIZ("posix_spawn(path)", ARG2);
   // DDD: check file_actions
   if (ARG4 != 0)
      pre_argv_envp( ARG4, tid, "posix_spawn(argv)",
                                "posix_spawn(argv[i])" );
   if (ARG5 != 0)
      pre_argv_envp( ARG5, tid, "posix_spawn(envp)",
                                "posix_spawn(envp[i])" );

   if (0)
   VG_(printf)("posix_spawn( %#lx, %#lx(%s), %#lx, %#lx, %#lx )\n",
         ARG1, ARG2, ARG2 ? (HChar*)ARG2 : "(null)", ARG3, ARG4, ARG5 );

   /* Now follows a bunch of logic copied from PRE(sys_execve) in
      syswrap-generic.c. */

   /* Check that the name at least begins in client-accessible storage. */
   if (ARG2 == 0 /* obviously bogus */
       || !VG_(am_is_valid_for_client)( ARG2, 1, VKI_PROT_READ )) {
      SET_STATUS_Failure( VKI_EFAULT );
      return;
   }

   // Decide whether or not we want to follow along
   { // Make 'child_argv' be a pointer to the child's arg vector
     // (skipping the exe name)
     const HChar** child_argv = (const HChar**)ARG4;
     if (child_argv && child_argv[0] == NULL)
        child_argv = NULL;
     trace_this_child = VG_(should_we_trace_this_child)( (HChar*)ARG2, child_argv );
   }

   // Do the important checks:  it is a file, is executable, permissions are
   // ok, etc.  We allow setuid executables to run only in the case when
   // we are not simulating them, that is, they to be run natively.
   res = simple_pre_exec_check( (const HChar*)ARG2, trace_this_child );
   if (sr_isError(res)) {
      SET_STATUS_Failure( sr_Err(res) );
      return;
   }

   /* If we're tracing the child, and the launcher name looks bogus
      (possibly because launcher.c couldn't figure it out, see
      comments therein) then we have no option but to fail. */
   if (trace_this_child
       && (VG_(name_of_launcher) == NULL
           || VG_(name_of_launcher)[0] != '/')) {
      SET_STATUS_Failure( VKI_ECHILD ); /* "No child processes" */
      return;
   }

   /* Ok.  So let's give it a try. */
   VG_(debugLog)(1, "syswrap", "Posix_spawn of %s\n", (HChar*)ARG2);

   /* posix_spawn on Darwin is combining the fork and exec in one syscall.
      So, we should not terminate gdbserver : this is still the parent
      running, which will terminate its gdbserver when exiting.
      If the child process is traced, it will start a fresh gdbserver
      after posix_spawn. */

   // Set up the child's exe path.
   //
   if (trace_this_child) {

      // We want to exec the launcher.  Get its pre-remembered path.
      path = VG_(name_of_launcher);
      // VG_(name_of_launcher) should have been acquired by m_main at
      // startup.  The following two assertions should be assured by
      // the "If we're tracking the child .." test just above here.
      vg_assert(path);
      vg_assert(path[0] == '/');
      launcher_basename = path;

   } else {
      path = (HChar*)ARG2;
   }

   // Set up the child's environment.
   //
   // Remove the valgrind-specific stuff from the environment so the
   // child doesn't get vgpreload_core.so, vgpreload_<tool>.so, etc.
   // This is done unconditionally, since if we are tracing the child,
   // the child valgrind will set up the appropriate client environment.
   // Nb: we make a copy of the environment before trying to mangle it
   // as it might be in read-only memory (this was bug #101881).
   //
   // Then, if tracing the child, set VALGRIND_LIB for it.
   //
   if (ARG5 == 0) {
      envp = NULL;
   } else {
      envp = VG_(env_clone)( (HChar**)ARG5 );
      vg_assert(envp);
      VG_(env_remove_valgrind_env_stuff)( envp, /* ro_strings */ False, NULL);
   }

   if (trace_this_child) {
      // Set VALGRIND_LIB in ARG5 (the environment)
      VG_(env_setenv)( &envp, VALGRIND_LIB, VG_(libdir));
   }

   // Set up the child's args.  If not tracing it, they are
   // simply ARG4.  Otherwise, they are
   //
   // [launcher_basename] ++ VG_(args_for_valgrind) ++ [ARG2] ++ ARG4[1..]
   //
   // except that the first VG_(args_for_valgrind_noexecpass) args
   // are omitted.
   //
   if (!trace_this_child) {
      argv = (HChar**)ARG4;
   } else {
      vg_assert( VG_(args_for_valgrind) );
      vg_assert( VG_(args_for_valgrind_noexecpass) >= 0 );
      vg_assert( VG_(args_for_valgrind_noexecpass)
                   <= VG_(sizeXA)( VG_(args_for_valgrind) ) );
      /* how many args in total will there be? */
      // launcher basename
      tot_args = 1;
      // V's args
      tot_args += VG_(sizeXA)( VG_(args_for_valgrind) );
      tot_args -= VG_(args_for_valgrind_noexecpass);
      // name of client exe
      tot_args++;
      // args for client exe, skipping [0]
      arg2copy = (HChar**)ARG4;
      if (arg2copy && arg2copy[0]) {
         for (i = 1; arg2copy[i]; i++)
            tot_args++;
      }
      // allocate
      argv = VG_(malloc)( "di.syswrap.pre_sys_execve.1",
                          (tot_args+1) * sizeof(HChar*) );
      // copy
      j = 0;
      argv[j++] = launcher_basename;
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         if (i < VG_(args_for_valgrind_noexecpass))
            continue;
         argv[j++] = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      }
      argv[j++] = (HChar*)ARG2;
      if (arg2copy && arg2copy[0])
         for (i = 1; arg2copy[i]; i++)
            argv[j++] = arg2copy[i];
      argv[j++] = NULL;
      // check
      vg_assert(j == tot_args+1);
   }

   /* DDD: sort out the signal state.  What signal
      state does the child inherit from the parent?  */

   if (0) {
      HChar **cpp;
      VG_(printf)("posix_spawn: %s\n", path);
      for (cpp = argv; cpp && *cpp; cpp++)
         VG_(printf)("argv: %s\n", *cpp);
      if (1)
         for (cpp = envp; cpp && *cpp; cpp++)
            VG_(printf)("env: %s\n", *cpp);
   }

   /* Let the call go through as usual.  However, we have to poke
      the altered arguments back into the argument slots. */
   ARG2 = (UWord)path;
   ARG4 = (UWord)argv;
   ARG5 = (UWord)envp;

   /* not to mention .. */
   *flags |= SfMayBlock;
}
POST(posix_spawn)
{
   vg_assert(SUCCESS);
   if (ARG1 != 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_pid_t) );
   }
}


PRE(socket)
{
   PRINT("socket ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(long, "socket", int, domain, int, type, int, protocol);
}

POST(socket)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_socket)(tid, VG_(mk_SysRes_Success)(RES));
   SET_STATUS_from_SysRes(r);
}


PRE(setsockopt)
{
   PRINT("setsockopt ( %ld, %ld, %ld, %#lx, %ld )",
          SARG1, SARG2, SARG3, ARG4, SARG5);
   PRE_REG_READ5(long, "setsockopt",
                 int, s, int, level, int, optname,
                 const void *, optval, vki_socklen_t, optlen);
   ML_(generic_PRE_sys_setsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}


PRE(getsockopt)
{
   Addr optval_p = ARG4;
   Addr optlen_p = ARG5;
   PRINT("getsockopt ( %ld, %ld, %ld, %#lx, %#lx )",
          SARG1, SARG2, SARG3, ARG4, ARG5);
   PRE_REG_READ5(long, "getsockopt",
                 int, s, int, level, int, optname,
                 void *, optval, vki_socklen_t *, optlen);
   /* int getsockopt(int socket, int level, int option_name, 
                     void *restrict option_value,
                     socklen_t *restrict option_len); */
   /* vg_assert(sizeof(socklen_t) == sizeof(UInt)); */
   if (optval_p != (Addr)NULL) {
      ML_(buf_and_len_pre_check) ( tid, optval_p, optlen_p,
                                   "socketcall.getsockopt(optval)",
                                   "socketcall.getsockopt(optlen)" );
   }
   // DDD: #warning GrP fixme darwin-specific sockopts
}

POST(getsockopt)
{
   Addr optval_p = ARG4;
   Addr optlen_p = ARG5;
   vg_assert(SUCCESS);
   if (optval_p != (Addr)NULL) {
      ML_(buf_and_len_post_check) ( tid, VG_(mk_SysRes_Success)(RES),
                                    optval_p, optlen_p,
                                    "socketcall.getsockopt(optlen_out)" );
   // DDD: #warning GrP fixme darwin-specific sockopts
   }
}


PRE(connect)
{
   *flags |= SfMayBlock;
   PRINT("connect ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "connect",
                 int, sockfd, struct sockaddr *, serv_addr, int, addrlen);
   ML_(generic_PRE_sys_connect)(tid, ARG1,ARG2,ARG3);
}


PRE(accept)
{
   *flags |= SfMayBlock;
   PRINT("accept ( %ld, %#lx, %#lx )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "accept",
                 int, s, struct sockaddr *, addr, int *, addrlen);
   ML_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}

POST(accept)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(mkfifo)
{
   *flags |= SfMayBlock;
   PRINT("mkfifo ( %#lx(%s), %lx )", ARG1, (HChar *)ARG1, ARG2);
   PRE_REG_READ2(long, "mkfifo", const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "mkfifo(path)", ARG1 );
}

POST(mkfifo)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "mkfifo", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG1);
   }
}

PRE(sendto)
{
   *flags |= SfMayBlock;
   PRINT("sendto ( %ld, %s, %ld, %lu, %#lx, %ld )",
         SARG1, (HChar *)ARG2, SARG3, ARG4, ARG5, SARG6);
   PRE_REG_READ6(long, "sendto",
                 int, s, const void *, msg, int, len, 
                 unsigned int, flags, 
                 const struct sockaddr *, to, int, tolen);
   ML_(generic_PRE_sys_sendto)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sendfile)
{
#if VG_WORDSIZE == 4
   PRINT("sendfile(%ld, %ld, %llu, %#lx, %#lx, %ld)",
         SARG1, SARG2, LOHI64(ARG3, ARG4), ARG5, ARG6, SARG7);

   PRE_REG_READ7(long, "sendfile",
      int, fromfd, int, tofd,
      vki_uint32_t, offset_low32, vki_uint32_t, offset_high32,
      vki_uint64_t *, nwritten, struct sf_hdtr *, sf_header, int, flags);
   PRE_MEM_WRITE("sendfile(nwritten)", ARG5, sizeof(vki_uint64_t));
   if (ARG6) PRE_MEM_WRITE("sendfile(sf_header)", ARG6, sizeof(struct sf_hdtr));
#else
   PRINT("sendfile(%ld, %ld, %lu, %#lx, %#lx, %ld)",
         SARG1, SARG2, ARG3, ARG4, ARG5, SARG6);

   PRE_REG_READ6(long, "sendfile",
      int, fromfd, int, tofd,
      vki_uint64_t, offset, 
      vki_uint64_t *, nwritten, struct sf_hdtr *, sf_header, int, flags);
   PRE_MEM_WRITE("sendfile(nwritten)", ARG4, sizeof(vki_uint64_t));
   if (ARG5) PRE_MEM_WRITE("sendfile(sf_header)", ARG5, sizeof(struct sf_hdtr));
#endif

   *flags |= SfMayBlock;
}
POST(sendfile)
{
#if VG_WORDSIZE == 4
   POST_MEM_WRITE(ARG5, sizeof(vki_uint64_t));
   if (ARG6) POST_MEM_WRITE(ARG6, sizeof(struct sf_hdtr));
#else
   POST_MEM_WRITE(ARG4, sizeof(vki_uint64_t));
   if (ARG5) POST_MEM_WRITE(ARG5, sizeof(struct sf_hdtr));
#endif
}

PRE(recvfrom)
{
   *flags |= SfMayBlock;
   PRINT("recvfrom ( %ld, %#lx, %ld, %lu, %#lx, %#lx )",
          SARG1, ARG2, SARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(long, "recvfrom",
                 int, s, void *, buf, int, len, unsigned int, flags,
                 struct sockaddr *, from, int *, fromlen);
   ML_(generic_PRE_sys_recvfrom)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

POST(recvfrom)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_recvfrom)(tid, VG_(mk_SysRes_Success)(RES),
                                       ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}


PRE(sendmsg)
{
   *flags |= SfMayBlock;
   PRINT("sendmsg ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "sendmsg",
                 int, s, const struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_sendmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}


PRE(recvmsg)
{
   *flags |= SfMayBlock;
   PRINT("recvmsg ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}

POST(recvmsg)
{
   ML_(generic_POST_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2, RES);
}


PRE(shutdown)
{
   *flags |= SfMayBlock;
   PRINT("shutdown ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(int, "shutdown", int, s, int, how);
}


PRE(bind)
{
   PRINT("bind ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "bind",
                 int, sockfd, struct sockaddr *, my_addr, int, addrlen);
   ML_(generic_PRE_sys_bind)(tid, ARG1,ARG2,ARG3);
}


PRE(listen)
{
   PRINT("listen ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "listen", int, s, int, backlog);
}


PRE(getsockname)
{
   PRINT("getsockname ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getsockname",
                 int, s, struct sockaddr *, name, int *, namelen);
   ML_(generic_PRE_sys_getsockname)(tid, ARG1,ARG2,ARG3);
}

POST(getsockname)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getsockname)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}


PRE(getpeername)
{
   PRINT("getpeername ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getpeername",
                 int, s, struct sockaddr *, name, int *, namelen);
   ML_(generic_PRE_sys_getpeername)(tid, ARG1,ARG2,ARG3);
}

POST(getpeername)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getpeername)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}


PRE(socketpair)
{
   PRINT("socketpair ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
   PRE_REG_READ4(long, "socketpair",
                 int, d, int, type, int, protocol, int *, sv);
   ML_(generic_PRE_sys_socketpair)(tid, ARG1,ARG2,ARG3,ARG4);
}

POST(socketpair)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_socketpair)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3,ARG4);
}


PRE(gethostuuid)
{
   PRINT("gethostuuid ( %#lx, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(int,"gethostuuid", 
                 char *,"uuid_buf", 
                 const struct vki_timespec *,"timeout");

   PRE_MEM_WRITE("uuid_buf", ARG1, 16);
   PRE_MEM_READ("timeout", ARG2, sizeof(struct vki_timespec));

   *flags |= SfMayBlock;
}


POST(gethostuuid)
{
   POST_MEM_WRITE(ARG1, 16);
}

/* Darwin pipe() returns the two descriptors in two registers. */
PRE(pipe)
{
   PRINT("pipe ( )");
   PRE_REG_READ0(int, "pipe");
}

POST(pipe)
{
   Int p0, p1;
   vg_assert(SUCCESS);
   p0 = RES;
   p1 = RESHI;

   if (!ML_(fd_allowed)(p0, "pipe", tid, True) ||
       !ML_(fd_allowed)(p1, "pipe", tid, True)) {
      VG_(close)(p0);
      VG_(close)(p1);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, p0);
         ML_(record_fd_open_nameless)(tid, p1);
      }
   }
}


PRE(getlogin)
{
   PRINT("getlogin ( %#lx, %lu )", ARG1, ARG2);
   PRE_REG_READ2(long, "getlogin", 
                 char *,"namebuf", unsigned int,"namelen");

   PRE_MEM_WRITE("getlogin(namebuf)", ARG1, ARG2);
}

POST(getlogin)
{
   POST_MEM_WRITE(ARG1, ARG2);
}


PRE(ptrace)
{
   PRINT("ptrace ( %ld, %ld, %#lx, %ld )", SARG1, SARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "ptrace", 
                 int,"request", vki_pid_t,"pid", 
                 vki_caddr_t,"addr", int,"data");
    
   // Note: some code uses ptrace(random, 0, 0, 0) as a profiling mechanism. 

   // GrP fixme anything needed?
}


PRE(issetugid)
{
   PRINT("issetugid ( )");
   PRE_REG_READ0(long, "issetugid");
}


PRE(getdtablesize)
{
   PRINT("getdtablesize ( )");
   PRE_REG_READ0(long, "getdtablesize");
}

POST(getdtablesize)
{
   // Subtract Valgrind's fd range from client's dtable
   if (RES > VG_(fd_hard_limit)) SET_STATUS_Success(VG_(fd_hard_limit));
}

PRE(lseek)
{
   PRINT("lseek ( %lu, %ld, %ld )", ARG1, SARG2, SARG3);
   PRE_REG_READ4(vki_off_t, "lseek",
                 unsigned int,fd, int,offset_hi, int,offset_lo, 
                 unsigned int,whence);
}


PRE(pathconf)
{
   PRINT("pathconf(%#lx(%s), %ld)", ARG1, (HChar *)ARG1, SARG2);
   PRE_REG_READ2(long,"pathconf", const char *,"path", int,"name");
   PRE_MEM_RASCIIZ("pathconf(path)", ARG1);
}


PRE(fpathconf)
{
   PRINT("fpathconf(%ld, %ld)",  SARG1, SARG2);
   PRE_REG_READ2(long,"fpathconf", int,"fd", int,"name");

   if (!ML_(fd_allowed)(ARG1, "fpathconf", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
}


PRE(getdirentries)
{
   PRINT("getdirentries(%ld, %#lx, %ld, %#lx)", SARG1, ARG2, SARG3, ARG4);
   PRE_REG_READ4(int, "getdirentries", 
                 int, fd, char *, buf, int, nbytes, long *, basep);
   PRE_MEM_WRITE("getdirentries(basep)", ARG4, sizeof(long));
   PRE_MEM_WRITE("getdirentries(buf)", ARG2, ARG3);
}

POST(getdirentries) 
{
   POST_MEM_WRITE(ARG4, sizeof(long));
   // GrP fixme be specific about d_name?
   POST_MEM_WRITE(ARG2, RES);
}


PRE(getdirentries64)
{
   PRINT("getdirentries64(%ld, %#lx, %lu, %#lx)", SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(vki_ssize_t, "getdirentries", 
                 int,fd, char *,buf, vki_size_t,nbytes, vki_off_t *,basep);
   /* JRS 18-Nov-2014: it appears that sometimes |basep| doesn't point
      to valid memory and the kernel doesn't modify it.  I can't
      determine the conditions under which that happens.  But it
      causes Memcheck to complain, confusingly.  So disable this check
      for the time being.

      PRE_MEM_WRITE("getdirentries64(position)", ARG4, sizeof(vki_off_t));
   */
   PRE_MEM_WRITE("getdirentries64(buf)", ARG2, ARG3);
}
POST(getdirentries64) 
{
   /* Disabled; see comments in the PRE wrapper.
      POST_MEM_WRITE(ARG4, sizeof(vki_off_t));
   */
   // GrP fixme be specific about d_name? (fixme copied from 32 bit version)
   POST_MEM_WRITE(ARG2, RES);
}


PRE(statfs64)
{
   PRINT("statfs64 ( %#lx(%s), %#lx )", ARG1, (HChar *)ARG1, ARG2);
   PRE_REG_READ2(long, "statfs64", const char *, path, struct statfs64 *, buf);
   PRE_MEM_RASCIIZ( "statfs64(path)", ARG1 );
   PRE_MEM_WRITE( "statfs64(buf)", ARG2, sizeof(struct vki_statfs64) );
}
POST(statfs64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_statfs64) );
}


PRE(fstatfs64)
{
   PRINT("fstatfs64 ( %lu, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "fstatfs64",
                 unsigned int, fd, struct statfs *, buf);
   PRE_MEM_WRITE( "fstatfs64(buf)", ARG2, sizeof(struct vki_statfs64) );
}
POST(fstatfs64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_statfs64) );
}

PRE(csops)
{
   PRINT("csops ( %ld, %#lx, %#lx, %lu )", SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "csops",
                 vki_pid_t, pid, uint32_t, ops,
                 void *, useraddr, vki_size_t, usersize);

   PRE_MEM_WRITE( "csops(useraddr)", ARG3, ARG4 );

   // If the pid is ours, don't mark the program as KILL or HARD
   // Maybe we should keep track of this for later calls to STATUS
   if (!ARG1 || VG_(getpid)() == ARG1) {
      switch (ARG2) {
      case VKI_CS_OPS_MARKINVALID:
      case VKI_CS_OPS_MARKHARD:
      case VKI_CS_OPS_MARKKILL:
         SET_STATUS_Success(0);
      }
   }
}
POST(csops)
{
   POST_MEM_WRITE( ARG3, ARG4 );
}

PRE(auditon)
{
   PRINT("auditon ( %ld, %#lx, %lu )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(int,"auditon", 
                 int,"cmd", void*,"data", unsigned int,"length");

   switch (ARG1) {

   case VKI_A_SETPOLICY: 
   case VKI_A_SETKMASK:
   case VKI_A_SETQCTRL:
   case VKI_A_SETCOND:
   case VKI_A_SETCLASS:
   case VKI_A_SETPMASK:
   case VKI_A_SETFSIZE:
#if DARWIN_VERS >= DARWIN_10_6
   case VKI_A_SENDTRIGGER:
#endif
      // kernel reads data..data+length
      PRE_MEM_READ("auditon(data)", ARG2, ARG3);
      break;

   case VKI_A_GETKMASK:
   case VKI_A_GETPOLICY:
   case VKI_A_GETQCTRL:
   case VKI_A_GETFSIZE:
   case VKI_A_GETCOND:
      // kernel writes data..data+length
      // GrP fixme be precise about what gets written
      PRE_MEM_WRITE("auditon(data)", ARG2, ARG3);
      break;


   case VKI_A_GETCLASS:
   case VKI_A_GETPINFO:
   case VKI_A_GETPINFO_ADDR:
#if DARWIN_VERS >= DARWIN_10_6
   case VKI_A_GETSINFO_ADDR:
#endif
      // kernel reads and writes data..data+length
      // GrP fixme be precise about what gets read and written
      PRE_MEM_READ("auditon(data)", ARG2, ARG3);
      PRE_MEM_WRITE("auditon(data)", ARG2, ARG3);
      break;

   case VKI_A_SETKAUDIT:
   case VKI_A_SETSTAT:
   case VKI_A_SETUMASK:
   case VKI_A_SETSMASK:
   case VKI_A_GETKAUDIT:
   case VKI_A_GETCWD:
   case VKI_A_GETCAR:
   case VKI_A_GETSTAT:
      // unimplemented on darwin
      break;

   default:
      VG_(message)(Vg_UserMsg, "UNKNOWN auditon cmd %ld\n", ARG1);
      break;
   }
}
POST(auditon)
{
   switch (ARG1) {

   case VKI_A_SETPOLICY: 
   case VKI_A_SETKMASK:
   case VKI_A_SETQCTRL:
   case VKI_A_SETCOND:
   case VKI_A_SETCLASS:
   case VKI_A_SETPMASK:
   case VKI_A_SETFSIZE:
#if DARWIN_VERS >= DARWIN_10_6
   case VKI_A_SENDTRIGGER:
#endif
      // kernel reads data..data+length
      break;

   case VKI_A_GETKMASK:
   case VKI_A_GETPOLICY:
   case VKI_A_GETQCTRL:
   case VKI_A_GETFSIZE:
   case VKI_A_GETCOND:
      // kernel writes data..data+length
      // GrP fixme be precise about what gets written
      POST_MEM_WRITE(ARG2, ARG3);
      break;


   case VKI_A_GETCLASS:
   case VKI_A_GETPINFO:
   case VKI_A_GETPINFO_ADDR:
#if DARWIN_VERS >= DARWIN_10_6
   case VKI_A_GETSINFO_ADDR:
#endif
      // kernel reads and writes data..data+length
      // GrP fixme be precise about what gets read and written
      POST_MEM_WRITE(ARG2, ARG3);
      break;

   case VKI_A_SETKAUDIT:
   case VKI_A_SETSTAT:
   case VKI_A_SETUMASK:
   case VKI_A_SETSMASK:
   case VKI_A_GETKAUDIT:
   case VKI_A_GETCWD:
   case VKI_A_GETCAR:
   case VKI_A_GETSTAT:
      // unimplemented on darwin
      break;

   default:
      break;
   }    
}

PRE(getaudit_addr)
{
   PRINT("getaudit_addr(%#lx, %ld)", ARG1, SARG2);
   PRE_REG_READ1(void*, "auditinfo_addr", int, "length");
   PRE_MEM_WRITE("getaudit_addr(auditinfo_addr)", ARG1, ARG2);
}
POST(getaudit_addr)
{
   POST_MEM_WRITE(ARG1, ARG2);
}


PRE(mmap)
{
   // SysRes r;
   if (0) VG_(am_do_sync_check)("(PRE_MMAP)",__FILE__,__LINE__);

#if VG_WORDSIZE == 4
   PRINT("mmap ( %#lx, %lu, %ld, %ld, %ld, %llu )",
         ARG1, ARG2, SARG3, SARG4, SARG5, LOHI64(ARG6, ARG7) );
   PRE_REG_READ7(Addr, "mmap",
                 Addr,start, vki_size_t,length, int,prot, int,flags, int,fd, 
                 unsigned long,offset_hi, unsigned long,offset_lo);
   // GrP fixme V mmap and kernel mach_msg collided once - don't use 
   // V's mechanism for now
   // r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
   // (Off64T)LOHI64(ARG6, ARG7) );
#else
   PRINT("mmap ( %#lx, %lu, %ld, %ld, %ld, %ld )",
         ARG1, ARG2, SARG3, SARG4, SARG5, SARG6 );
   PRE_REG_READ6(long, "mmap",
                 Addr,start, vki_size_t,length, int,prot, int,flags, int,fd, 
                 Off64T,offset);
   // r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );

#endif

   // SET_STATUS_from_SysRes(r);
}

POST(mmap)
{
   if (RES != -1) {
      ML_(notify_core_and_tool_of_mmap)(RES, ARG2, ARG3, ARG4, ARG5, ARG6);
      // Try to load symbols from the region
      VG_(di_notify_mmap)( (Addr)RES, False/*allow_SkFileV*/,
                           -1/*don't use_fd*/ );
      ML_(sync_mappings)("after", "mmap", 0);
   }
}


/* This function holds common elements of PRE(__sysctl) and
   PRE(sysctlbyname). */
static void common_PRE_sysctl (
               /*IMPLICIT ARGS*/
               ThreadId tid, /*OUT*/SyscallStatus* status, /*OUT*/UWord* flags,
               /*!IMPLICIT ARGS*/
               Bool is_kern_dot_userstack,
               UWord oldp, UWord oldlenp,
               UWord newp, UWord newlen )
{
   if (oldlenp) {
      // writes *oldlenp
      PRE_MEM_WRITE("sysctl(oldlenp)", oldlenp, sizeof(size_t));
      if (oldp) {
         // also reads *oldlenp, and writes up to oldp[0..(*oldlenp)-1]
         PRE_MEM_READ("sysctl(oldlenp)", oldlenp, sizeof(size_t));
         PRE_MEM_WRITE("sysctl(oldp)", oldp, *(size_t*)oldlenp);
      }
   }
   if (newp) {
      PRE_MEM_READ("sysctl(newp)", newp, newlen);
   }

   // GrP fixme intercept KERN_PROCARGS and KERN_PROC_PID for our pid
   // (executable path and arguments and environment
        
   if (is_kern_dot_userstack) {
      // Intercept sysctl(kern.usrstack). The kernel's reply
      // would be Valgrind's stack, not the client's stack.
      // GrP fixme kern_usrstack64 */
      if (newp || newlen) {
         SET_STATUS_Failure(VKI_EPERM); // USRSTACK is read-only */
      } else {
         Addr* t_oldp = (Addr*)oldp;
         size_t* t_oldlenp = (size_t*)oldlenp;
         if (t_oldlenp) {
            // According to some searches on the net, it looks like
            // USRSTACK gives the address of the byte following the
            // highest byte of the stack.  As VG_(clstk_end) is the
            // address of the highest addressable byte, we add 1.
            Addr stack_end = VG_(clstk_end)+1;
            size_t oldlen = *t_oldlenp;
            // always return actual size
            *t_oldlenp = sizeof(Addr);
            if (t_oldp && oldlen >= sizeof(Addr)) {
               // oldp is big enough.  copy value and return 0
               *t_oldp = stack_end;
               SET_STATUS_Success(0);
            } else {
               // oldp isn't big enough.  copy as much as possible
               // and return ENOMEM
               if (t_oldp) VG_(memcpy)(t_oldp, &stack_end, oldlen);
               SET_STATUS_Failure(VKI_ENOMEM);
            }
         }
      }
   }

   if (!SUCCESS && !FAILURE) {
      // Don't set SfPostOnFail if we've already handled it locally.
      *flags |= SfPostOnFail;
   }
}


PRE(__sysctl)
{
   UWord name    = ARG1;
   UWord namelen = ARG2;
   UWord oldp    = ARG3;
   UWord oldlenp = ARG4;
   UWord newp    = ARG5;
   UWord newlen  = ARG6;

   PRINT( "__sysctl ( %#lx, %lu, %#lx, %#lx, %#lx, %#lx )", 
          name, namelen, oldp, oldlenp, newp, newlen );

   PRE_REG_READ6(int, "__sysctl", int*, name, unsigned int, namelen, 
                 void*, oldp, vki_size_t *, oldlenp, 
                 void*, newp, vki_size_t *, newlenp);

   PRE_MEM_READ("sysctl(name)", name, namelen);  // reads name[0..namelen-1]

   if (VG_(clo_trace_syscalls)) {
      UInt i;
      Int* t_name = (Int*)name;
      VG_(printf)(" mib: [ ");
      for (i = 0; i < namelen; i++) {
         VG_(printf)("%d ", t_name[i]);
      }
      VG_(printf)("]");
   }

   Int vKI_KERN_USRSTACKXX
      = VG_WORDSIZE == 4 ? VKI_KERN_USRSTACK32 : VKI_KERN_USRSTACK64; 
   Bool is_kern_dot_userstack
      = name && namelen == 2
        && ((Int*)name)[0] == VKI_CTL_KERN
        && ((Int*)name)[1] == vKI_KERN_USRSTACKXX;

   common_PRE_sysctl( /*IMPLICIT ARGS*/tid,status,flags,/*!IMPLICIT_ARGS*/
                      is_kern_dot_userstack, oldp, oldlenp, newp, newlen );
}

POST(__sysctl)
{
   UWord oldp    = ARG3;
   UWord oldlenp = ARG4;

   if (SUCCESS || ERR == VKI_ENOMEM) {
      // sysctl can write truncated data and return VKI_ENOMEM
      if (oldlenp) {
         POST_MEM_WRITE(oldlenp, sizeof(size_t));
      }
      if (oldp && oldlenp) {
         POST_MEM_WRITE(oldp, *(size_t*)oldlenp);
      }
   }
}


PRE(sigpending)
{
   PRINT( "sigpending ( %#lx )", ARG1 );
   PRE_REG_READ1(long, "sigpending", vki_sigset_t *, set);
   PRE_MEM_WRITE( "sigpending(set)", ARG1, sizeof(vki_sigset_t));
}
POST(sigpending)
{
   POST_MEM_WRITE( ARG1, sizeof(vki_sigset_t) ) ;
}


PRE(sigprocmask)
{
   UWord arg1;
   PRINT("sigprocmask ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sigprocmask",
                 int, how, vki_sigset_t *, set, vki_sigset_t *, oldset);
   if (ARG2 != 0)
      PRE_MEM_READ( "sigprocmask(set)", ARG2, sizeof(vki_sigset_t));
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sigprocmask(oldset)", ARG3, sizeof(vki_sigset_t));

   /* Massage ARG1 ('how').  If ARG2 (the new mask) is NULL then the
      value of 'how' is irrelevant, and it appears that Darwin's libc
      passes zero, which is not equal to any of
      SIG_{BLOCK,UNBLOCK,SETMASK}.  This causes
      VG_(do_sys_sigprocmask) to complain, since it checks the 'how'
      value independently of the other args.  Solution: in this case,
      simply pass a valid (but irrelevant) value for 'how'. */
   /* Also, in this case the new set is passed to the kernel by
      reference, not value, as in some other sigmask related Darwin
      syscalls. */
   arg1 = ARG1;
   if (ARG2 == 0  /* the new-set is NULL */
       && ARG1 != VKI_SIG_BLOCK
       && ARG1 != VKI_SIG_UNBLOCK && ARG1 != VKI_SIG_SETMASK) {
      arg1 = VKI_SIG_SETMASK;
   }
   SET_STATUS_from_SysRes(
      VG_(do_sys_sigprocmask) ( tid, arg1, (vki_sigset_t*)ARG2,
                                           (vki_sigset_t*)ARG3 )
   );

   if (SUCCESS)
      *flags |= SfPollAfter;
}

POST(sigprocmask)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(vki_sigset_t));
}


PRE(sigsuspend)
{
   /* Just hand this off to the kernel.  Is that really correct?  And
      shouldn't we at least set SfPollAfter?  These questions apply to
      all the Linux versions too. */
   /* I think the first arg is the 32-bit signal mask (by value), and
      the other two args are ignored. */
   *flags |= SfMayBlock;
   PRINT("sigsuspend ( mask=0x%08lx )", ARG1 );
   PRE_REG_READ1(int, "sigsuspend", int, sigmask);
}


/* Be careful about the 4th arg, since that is a uint64_t.  Hence 64-
   and 32-bit wrappers are different.

   ARG5 and ARG6 (buffer, buffersize) specify a buffer start and
   length in the usual way.  I have seen values NULL, 0 passed in some
   cases.  I left the calls to PRE_MEM_WRITE/READ unconditional on the
   basis that they don't do anything if the length is zero, so it's OK
   for the buffer pointer to be NULL in that case (meaning they don't
   complain).

   int proc_info(int32_t callnum, int32_t pid,
                 uint32_t flavor, uint64_t arg,
                 user_addr_t buffer, int32_t buffersize)
*/
PRE(proc_info)
{
#if VG_WORDSIZE == 4
   PRINT("proc_info(%d, %d, %u, %llu, %#lx, %d)",
         (Int)ARG1, (Int)ARG2, (UInt)ARG3, LOHI64(ARG4,ARG5), ARG6, (Int)ARG7);
   PRE_REG_READ7(int, "proc_info",
                 int, callnum, int, pid, unsigned int, flavor,
                 vki_uint32_t, arg_low32,
                 vki_uint32_t, arg_high32,
                 void*, buffer, int, buffersize);
   PRE_MEM_WRITE("proc_info(buffer)", ARG6, ARG7);
#else
   PRINT("proc_info(%d, %d, %u, %llu, %#lx, %d)",
         (Int)ARG1, (Int)ARG2, (UInt)ARG3, (ULong)ARG4, ARG5, (Int)ARG6);
   PRE_REG_READ6(int, "proc_info",
                 int, callnum, int, pid, unsigned int, flavor,
                 unsigned long long int, arg,
                 void*, buffer, int, buffersize);
   PRE_MEM_WRITE("proc_info(buffer)", ARG5, ARG6);
#endif
}

POST(proc_info)
{
#if VG_WORDSIZE == 4
   vg_assert(SUCCESS);

   // Intercept internal call to proc_setcontrol() where flavor = 2, arg = 0
   if (ARG1 == 5 && ARG3 == 2 && LOHI64(ARG4,ARG5) == 0 )
   {
       const HChar* new_name = (const HChar*) ARG6;
       if (new_name) {    // Paranoia
          ThreadState* tst = VG_(get_ThreadState)(tid);
          SizeT new_len = VG_(strlen)(new_name);
           
          /* Don't bother reusing the memory. This is a rare event. */
          tst->thread_name =
             VG_(realloc)("syscall(proc_info)", tst->thread_name, new_len + 1);
          VG_(strcpy)(tst->thread_name, new_name);
       }
   }
    
   POST_MEM_WRITE(ARG6, ARG7);
#else
   vg_assert(SUCCESS);

   // Intercept internal call to proc_setcontrol() where flavor = 2, arg = 0
   if (ARG1 == 5 && ARG3 == 2 && ARG4 == 0 )
   {
      const HChar* new_name = (const HChar*) ARG5;
      if (new_name) {    // Paranoia
         ThreadState* tst = VG_(get_ThreadState)(tid);
         SizeT new_len = VG_(strlen)(new_name);
            
         /* Don't bother reusing the memory. This is a rare event. */
         tst->thread_name =
            VG_(realloc)("syscall(proc_info)", tst->thread_name, new_len + 1);
         VG_(strcpy)(tst->thread_name, new_name);
       }
   }

   POST_MEM_WRITE(ARG5, ARG6);
#endif
}


/* ---------------------------------------------------------------------
   aio_*
   ------------------------------------------------------------------ */

// We must record the aiocbp for each aio_read() in a table so that when
// aio_return() is called we can mark the memory written asynchronously by
// aio_read() as having been written.  We don't have to do this for
// aio_write().  See bug 197227 for more details.
static OSet* aiocbp_table = NULL;
static Bool aio_init_done = False;

static void aio_init(void)
{
   aiocbp_table = VG_(OSetWord_Create)(VG_(malloc), "syswrap.aio", VG_(free));
   aio_init_done = True;
}

PRE(aio_return)
{
   // This assumes that the kernel looks at the struct pointer, but not the
   // contents of the struct.
   PRINT( "aio_return ( %#lx )", ARG1 );
   PRE_REG_READ1(long, "aio_return", struct vki_aiocb*, aiocbp);
   if (ML_(safe_to_deref)((struct vki_aiocb *)ARG1, sizeof(struct vki_aiocb))) {
      SET_STATUS_from_SysRes(VG_(do_syscall1)(SYSNO, ARG1));
      if (SUCCESS && RES >= 0) {
         struct vki_aiocb* aiocbp = (struct vki_aiocb*)ARG1;
         if (!aio_init_done) {
            aio_init();
         }
         if (VG_(OSetWord_Remove)(aiocbp_table, (UWord)aiocbp)) {
            POST_MEM_WRITE((Addr)aiocbp->aio_buf, aiocbp->aio_nbytes);
         }
      }
   } else {
      SET_STATUS_Failure(VKI_EINVAL);
   }

}

PRE(aio_suspend)
{
   // This assumes that the kernel looks at the struct pointers in the list,
   // but not the contents of the structs.
   PRINT( "aio_suspend ( %#lx )", ARG1 );
   PRE_REG_READ3(long, "aio_suspend",
                 const struct vki_aiocb * const *, aiocbp, int, nent,
                 const struct vki_timespec *, timeout);
   if (ARG2 > 0) {
      PRE_MEM_READ("aio_suspend(list)", ARG1, ARG2 * sizeof(struct vki_aiocb *));
   }
   if (ARG3) {
      PRE_MEM_READ ("aio_suspend(timeout)", ARG3, sizeof(struct vki_timespec));
   }
}

PRE(aio_error)
{
   // This assumes that the kernel looks at the struct pointer, but not the
   // contents of the struct.
   PRINT( "aio_error ( %#lx )", ARG1 );
   PRE_REG_READ1(long, "aio_error", struct vki_aiocb*, aiocbp);
}

PRE(aio_read)
{
   struct vki_aiocb* aiocbp = (struct vki_aiocb*)ARG1;

   PRINT( "aio_read ( %#lx )", ARG1 );
   PRE_REG_READ1(long, "aio_read", struct vki_aiocb*, aiocbp);
   PRE_MEM_READ( "aio_read(aiocbp)", ARG1, sizeof(struct vki_aiocb));

   if (ML_(safe_to_deref)(aiocbp, sizeof(struct vki_aiocb))) {
      if (ML_(fd_allowed)(aiocbp->aio_fildes, "aio_read", tid, /*isNewFd*/False)) {
         PRE_MEM_WRITE("aio_read(aiocbp->aio_buf)",
                       (Addr)aiocbp->aio_buf, aiocbp->aio_nbytes);
      } else {
         SET_STATUS_Failure( VKI_EBADF );
      }
   } else {
      SET_STATUS_Failure( VKI_EINVAL );
   }
}
POST(aio_read)
{
   // We have to record the fact that there is an asynchronous read request
   // pending.  When a successful aio_return() occurs for this aiocb, then we
   // will mark the memory as having been defined.
   struct vki_aiocb* aiocbp = (struct vki_aiocb*)ARG1;
   if (!aio_init_done) aio_init();
   // aiocbp shouldn't already be in the table -- if it was a dup, the kernel
   // should have caused the aio_read() to fail and we shouldn't have reached
   // here.
   VG_(OSetWord_Insert)(aiocbp_table, (UWord)aiocbp);
}

PRE(aio_write)
{
   struct vki_aiocb* aiocbp = (struct vki_aiocb*)ARG1;

   PRINT( "aio_write ( %#lx )", ARG1 );
   PRE_REG_READ1(long, "aio_write", struct vki_aiocb*, aiocbp);
   PRE_MEM_READ( "aio_write(aiocbp)", ARG1, sizeof(struct vki_aiocb));

   if (ML_(safe_to_deref)(aiocbp, sizeof(struct vki_aiocb))) {
      if (ML_(fd_allowed)(aiocbp->aio_fildes, "aio_write", tid, /*isNewFd*/False)) {
         PRE_MEM_READ("aio_write(aiocbp->aio_buf)",
                      (Addr)aiocbp->aio_buf, aiocbp->aio_nbytes);
      } else {
         SET_STATUS_Failure( VKI_EBADF );
      }
   } else {
      SET_STATUS_Failure( VKI_EINVAL );
   }
}

/* ---------------------------------------------------------------------
   mach_msg: formatted messages
   ------------------------------------------------------------------ */

static size_t desc_size(mach_msg_descriptor_t *desc)
{
   switch (desc->type.type) {
   case MACH_MSG_PORT_DESCRIPTOR:          return sizeof(desc->port);
   case MACH_MSG_OOL_DESCRIPTOR:           return sizeof(desc->out_of_line);
   case MACH_MSG_OOL_VOLATILE_DESCRIPTOR:  return sizeof(desc->out_of_line);
   case MACH_MSG_OOL_PORTS_DESCRIPTOR:     return sizeof(desc->ool_ports);
   default: 
      VG_(printf)("UNKNOWN mach message descriptor %d\n", desc->type.type);
      return sizeof(desc->type); // guess
   }
}


static void assign_port_names(mach_msg_ool_ports_descriptor_t *desc, 
                              const char *name)
{
   mach_msg_size_t i;
   mach_port_t *ports = (mach_port_t *)desc->address;
   for (i = 0; i < desc->count; i++) {
      assign_port_name(ports[i], name);
   }
}


static void import_complex_message(ThreadId tid, mach_msg_header_t *mh)
{
   mach_msg_body_t *body;
   mach_msg_size_t count, i;
   uint8_t *p;
   mach_msg_descriptor_t *desc;
   
   vg_assert(mh->msgh_bits & MACH_MSGH_BITS_COMPLEX);
   
   body = (mach_msg_body_t *)(mh+1);
   count = body->msgh_descriptor_count;
   p = (uint8_t *)(body+1);
   
   for (i = 0; i < count; i++) {
      desc = (mach_msg_descriptor_t *)p;
      p += desc_size(desc);
      
      switch (desc->type.type) {
      case MACH_MSG_PORT_DESCRIPTOR:
         // single port
         record_unnamed_port(tid, desc->port.name, -1);
         record_port_insert_rights(desc->port.name, desc->port.disposition);
         PRINT("got port %s;\n", name_for_port(desc->port.name));
         break;

      case MACH_MSG_OOL_DESCRIPTOR:
      case MACH_MSG_OOL_VOLATILE_DESCRIPTOR:
         // out-of-line memory - map it
         // GrP fixme how is VOLATILE different? do we care?
         // GrP fixme do other flags tell us anything? assume shared for now
         // GrP fixme more SF_ flags marking mach_msg memory might be nice
         // GrP fixme protection
         if (desc->out_of_line.size > 0) {
            Addr start = VG_PGROUNDDN((Addr)desc->out_of_line.address);
            Addr end = VG_PGROUNDUP((Addr)desc->out_of_line.address + 
                                    (Addr)desc->out_of_line.size);
            PRINT("got ool mem %p..%p;\n", desc->out_of_line.address, 
                  (char*)desc->out_of_line.address+desc->out_of_line.size);

            ML_(notify_core_and_tool_of_mmap)(
               start, end - start, VKI_PROT_READ|VKI_PROT_WRITE, 
               VKI_MAP_PRIVATE, -1, 0);
         }
         // GrP fixme mark only un-rounded part as initialized 
         break;

      case MACH_MSG_OOL_PORTS_DESCRIPTOR:
         // out-of-line array of ports - map it
         // GrP fixme see fixmes above
         PRINT("got %d ool ports %p..%#lx", desc->ool_ports.count, desc->ool_ports.address, (Addr)desc->ool_ports.address+desc->ool_ports.count*sizeof(mach_port_t));

         if (desc->ool_ports.count > 0) {
            Addr start = VG_PGROUNDDN((Addr)desc->ool_ports.address);
            Addr end = VG_PGROUNDUP((Addr)desc->ool_ports.address + desc->ool_ports.count * sizeof(mach_port_t));
            mach_port_t *ports = (mach_port_t *)desc->ool_ports.address;

            ML_(notify_core_and_tool_of_mmap)(
               start, end - start, VKI_PROT_READ|VKI_PROT_WRITE, 
               VKI_MAP_PRIVATE, -1, 0);

            PRINT(":");
            for (i = 0; i < desc->ool_ports.count; i++) {
               record_unnamed_port(tid, ports[i], -1);
               record_port_insert_rights(ports[i], desc->port.disposition);
               PRINT(" %s", name_for_port(ports[i]));
            }
         }
         PRINT(";\n");
         break;

      default:
         VG_(printf)("UNKNOWN Mach descriptor type %u!\n", desc->type.type);
         break;
      }
   }
}


static void pre_port_desc_read(ThreadId tid, mach_msg_port_descriptor_t *desc2)
{
#pragma pack(4)
   struct {
      mach_port_t name;
      mach_msg_size_t pad1;
      uint16_t pad2;
      uint8_t disposition;
      uint8_t type;
   } *desc = (void*)desc2;
#pragma pack()

   PRE_FIELD_READ("msg->desc.port.name",        desc->name);
   PRE_FIELD_READ("msg->desc.port.disposition", desc->disposition);
   PRE_FIELD_READ("msg->desc.port.type",        desc->type);
}


static void pre_ool_desc_read(ThreadId tid, mach_msg_ool_descriptor_t *desc2)
{
#pragma pack(4)
   struct {
      Addr address;
#if VG_WORDSIZE != 8
      mach_msg_size_t size;
#endif
      uint8_t deallocate;
      uint8_t copy;
      uint8_t pad1;
      uint8_t type;
#if VG_WORDSIZE == 8
      mach_msg_size_t size;
#endif
   } *desc = (void*)desc2;
#pragma pack()

   PRE_FIELD_READ("msg->desc.out_of_line.address",    desc->address);
   PRE_FIELD_READ("msg->desc.out_of_line.size",       desc->size);
   PRE_FIELD_READ("msg->desc.out_of_line.deallocate", desc->deallocate);
   PRE_FIELD_READ("msg->desc.out_of_line.copy",       desc->copy);
   PRE_FIELD_READ("msg->desc.out_of_line.type",       desc->type);
}

static void pre_oolports_desc_read(ThreadId tid, 
                                   mach_msg_ool_ports_descriptor_t *desc2)
{
#pragma pack(4)
   struct {
      Addr address;
#if VG_WORDSIZE != 8
      mach_msg_size_t size;
#endif
      uint8_t deallocate;
      uint8_t copy;
      uint8_t disposition;
      uint8_t type;
#if VG_WORDSIZE == 8
      mach_msg_size_t size;
#endif
   } *desc = (void*)desc2;
#pragma pack()

   PRE_FIELD_READ("msg->desc.ool_ports.address",     desc->address);
   PRE_FIELD_READ("msg->desc.ool_ports.size",        desc->size);
   PRE_FIELD_READ("msg->desc.ool_ports.deallocate",  desc->deallocate);
   PRE_FIELD_READ("msg->desc.ool_ports.copy",        desc->copy);
   PRE_FIELD_READ("msg->desc.ool_ports.disposition", desc->disposition);
   PRE_FIELD_READ("msg->desc.ool_ports.type",        desc->type);
}


// Returns the size of the descriptor area
// (mach_msg_body_t + any mach_msg_descriptor_t)
static size_t export_complex_message(ThreadId tid, mach_msg_header_t *mh)
{
   mach_msg_body_t *body;
   mach_msg_size_t count, i;
   uint8_t *p;
   mach_msg_descriptor_t *desc;
   
   vg_assert(mh->msgh_bits & MACH_MSGH_BITS_COMPLEX);
   
   body = (mach_msg_body_t *)(mh+1);
   PRE_MEM_READ("msg->msgh_descriptor_count)", (Addr)body, sizeof(*body));

   count = body->msgh_descriptor_count;
   p = (uint8_t *)(body+1);
   
   for (i = 0; i < count; i++) {
      desc = (mach_msg_descriptor_t *)p;
      p += desc_size(desc);
      
      switch (desc->type.type) {
      case MACH_MSG_PORT_DESCRIPTOR:
         // single port; no memory map effects
         pre_port_desc_read(tid, &desc->port);
         break;

      case MACH_MSG_OOL_DESCRIPTOR:
      case MACH_MSG_OOL_VOLATILE_DESCRIPTOR:
         // out-of-line memory - unmap it if it's marked dealloc
         // GrP fixme need to remap if message fails?
         // GrP fixme how is VOLATILE different? do we care?
         // GrP fixme struct is different for lp64
         pre_ool_desc_read(tid, &desc->out_of_line);

         if (desc->out_of_line.deallocate  &&  desc->out_of_line.size > 0) {
            vm_size_t size = desc->out_of_line.size;
            Addr start = VG_PGROUNDDN((Addr)desc->out_of_line.address);
            Addr end = VG_PGROUNDUP((Addr)desc->out_of_line.address + size);
            PRINT("kill ool mem %p..%#lx; ", desc->out_of_line.address, 
                  (Addr)desc->out_of_line.address + size);
            ML_(notify_core_and_tool_of_munmap)(start, end - start);
         }
         break;

      case MACH_MSG_OOL_PORTS_DESCRIPTOR:
         // out-of-line array of ports - unmap it if it's marked dealloc
         // GrP fixme need to remap if message fails?
         // GrP fixme struct different for lp64
         pre_oolports_desc_read(tid, &desc->ool_ports);

         if (desc->ool_ports.deallocate  &&  desc->ool_ports.count > 0) {
            vm_size_t size = desc->ool_ports.count * sizeof(mach_port_t);
            Addr start = VG_PGROUNDDN((Addr)desc->ool_ports.address);
            Addr end = VG_PGROUNDUP((Addr)desc->ool_ports.address + size);
            PRINT("kill ool port array %p..%#lx; ", desc->ool_ports.address, 
                  (Addr)desc->ool_ports.address + size);
            ML_(notify_core_and_tool_of_munmap)(start, end - start);
         }
         break;
      default:
         VG_(printf)("UNKNOWN Mach descriptor type %u!\n", desc->type.type);
         break;
      }
   }

   return (size_t)((Addr)p - (Addr)body);
}


/* ---------------------------------------------------------------------
   mach_msg: host-related messages
   ------------------------------------------------------------------ */


POST(host_info)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_type_number_t host_info_outCnt;
      integer_t host_info_out[14];
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (reply->RetCode) PRINT("mig return %d", reply->RetCode);
}

PRE(host_info)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      host_flavor_t flavor;
      mach_msg_type_number_t host_info_outCnt;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("host_info(mach_host_self(), flavor %d)", req->flavor);

   AFTER = POST_FN(host_info);
}


POST(host_page_size)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      vm_size_t out_page_size;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      PRINT("page size %llu", (ULong)reply->out_page_size);
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}

PRE(host_page_size)
{
   PRINT("host_page_size(mach_host_self(), ...)");
    
   AFTER = POST_FN(host_page_size);
}


POST(host_get_io_master)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t io_master;
      /* end of the kernel processed data */
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   assign_port_name(reply->io_master.name, "io_master-%p");
   PRINT("%s", name_for_port(reply->io_master.name));
}

PRE(host_get_io_master)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//   } Request;
//#pragma pack()

   // Request *req = (Request *)ARG1;

   PRINT("host_get_io_master(mach_host_self())");

   AFTER = POST_FN(host_get_io_master);
}


POST(host_get_clock_service)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t clock_serv;
      /* end of the kernel processed data */
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   assign_port_name(reply->clock_serv.name, "clock-%p");
   PRINT("%s", name_for_port(reply->clock_serv.name));
}

PRE(host_get_clock_service)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      clock_id_t clock_id;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("host_get_clock_service(mach_host_self(), %d)", req->clock_id);

   AFTER = POST_FN(host_get_clock_service);
}


PRE(host_request_notification)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t notify_port;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      host_flavor_t notify_type;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   if (MACH_REMOTE == mach_task_self()) { 
      if (req->notify_type == 0) {
         PRINT("host_request_notification(mach_host_self(), %s, %s)", 
               "HOST_NOTIFY_CALENDAR_CHANGE", 
               name_for_port(req->notify_port.name));
      } else {
         PRINT("host_request_notification(mach_host_self(), %d, %s)",
               req->notify_type, 
               name_for_port(req->notify_port.name));
      } 
   } else {
      PRINT("host_request_notification(%s, %d, %s)",
            name_for_port(MACH_REMOTE), 
            req->notify_type, 
            name_for_port(req->notify_port.name));
   }

    // GrP fixme only do this on success
   assign_port_name(req->notify_port.name, "host_notify-%p");
}


PRE(host_create_mach_voucher)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        NDR_record_t NDR;
        mach_msg_type_number_t recipesCnt;
        uint8_t recipes[5120];
    } Request;
#pragma pack()
    
    Request *req = (Request *)ARG1;

    PRINT("host_create_mach_voucher(count %u)",
          req->recipesCnt);
    
    AFTER = POST_FN(host_create_mach_voucher);
}


POST(host_create_mach_voucher)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        /* start of the kernel processed data */
        mach_msg_body_t msgh_body;
        mach_msg_port_descriptor_t voucher;
        /* end of the kernel processed data */
    } Reply;
#pragma pack()
    
    Reply *reply = (Reply *)ARG1;

    // RK fixme properly parse this return type
    PRINT("got voucher %#x ", reply->voucher.name);
}


PRE(host_get_special_port)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        NDR_record_t NDR;
        int node;
        int which;
    } Request;
#pragma pack()
    
    Request *req = (Request *)ARG1;

    PRINT("host_get_special_port(node %d)", req->node);
    
    switch (req->which) {
        case HOST_PORT:
            PRINT("host_get_special_port(%s, HOST_PORT)",
                  name_for_port(MACH_REMOTE));
            break;
        case HOST_PRIV_PORT:
            PRINT("host_get_special_port(%s, HOST_PRIV_PORT)",
                  name_for_port(MACH_REMOTE));
            break;
        case HOST_IO_MASTER_PORT:
            PRINT("host_get_special_port(%s, HOST_IO_MASTER_PORT)",
                  name_for_port(MACH_REMOTE));
            break;
        // Not provided by kernel
        case HOST_DYNAMIC_PAGER_PORT:
            PRINT("host_get_special_port(%s, HOST_DYNAMIC_PAGER_PORT)",
                  name_for_port(MACH_REMOTE));
            break;
        case HOST_AUDIT_CONTROL_PORT:
            PRINT("host_get_special_port(%s, HOST_AUDIT_CONTROL_PORT)",
                  name_for_port(MACH_REMOTE));
            break;
        case HOST_USER_NOTIFICATION_PORT:
            PRINT("host_get_special_port(%s, HOST_USER_NOTIFICATION_PORT)",
                  name_for_port(MACH_REMOTE));
            break;
        // ...

        default:
            PRINT("host_get_special_port(%s, %d)",
                  name_for_port(MACH_REMOTE), req->which);
            break;
    }
    
    MACH_ARG(host_get_special_port.which) = req->which;
    
    AFTER = POST_FN(host_get_special_port);
}


POST(host_get_special_port)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        /* start of the kernel processed data */
        mach_msg_body_t msgh_body;
        mach_msg_port_descriptor_t port;
        /* end of the kernel processed data */
    } Reply;
#pragma pack()
    
    Reply *reply = (Reply *)ARG1;
    
    PRINT("got port %#x ", reply->port.name);

    /* The required entry in the allocated_ports list (mapping) might
     not exist, due perhaps to broken syscall wrappers (mach__N etc).
     Create a minimal entry so that assign_port_name below doesn't
     cause an assertion. */
    if (!port_exists(reply->port.name)) {
        port_create_vanilla(reply->port.name);
    }
    
    switch (MACH_ARG(host_get_special_port.which)) {
        case HOST_PORT:
            assign_port_name(reply->port.name, "port-%p");
            break;
        case HOST_PRIV_PORT:
            assign_port_name(reply->port.name, "priv-%p");
            break;
        case HOST_IO_MASTER_PORT:
            assign_port_name(reply->port.name, "io-master-%p");
            break;
        // Not provided by kernel
        case HOST_DYNAMIC_PAGER_PORT:
            assign_port_name(reply->port.name, "dynamic-pager-%p");
            break;
        case HOST_AUDIT_CONTROL_PORT:
            assign_port_name(reply->port.name, "audit-control-%p");
            break;
        case HOST_USER_NOTIFICATION_PORT:
            assign_port_name(reply->port.name, "user-notification-%p");
            break;
        // ...

        default:
            assign_port_name(reply->port.name, "special-%p");
            break;
    }
    
    PRINT("%s", name_for_port(reply->port.name));
}

/* ---------------------------------------------------------------------
   mach_msg: messages to a task
   ------------------------------------------------------------------ */

// JRS 2011-Aug-25: just guessing here.  I have no clear idea how
// these structs are derived.  They obviously relate to the various
// .def files in the xnu sources, and can also be found in some
// form in /usr/include/mach/*.h, but not sure how these all
// relate to each other.

PRE(mach_port_set_context)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_vm_address_t context;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_set_context(%s, %s, 0x%llx)",
        name_for_port(MACH_REMOTE), 
        name_for_port(req->name), req->context);

   AFTER = POST_FN(mach_port_set_context);
}

POST(mach_port_set_context)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//      NDR_record_t NDR;
//      kern_return_t RetCode;
//   } Reply;
//#pragma pack()
}


// JRS 2011-Aug-25 FIXME completely bogus
PRE(task_get_exception_ports)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//      NDR_record_t NDR;
//      exception_mask_t exception_mask;
//   } Request;
//#pragma pack()

   PRINT("task_get_exception_ports(BOGUS)");
   AFTER = POST_FN(task_get_exception_ports);
}

POST(task_get_exception_ports)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//      /* start of the kernel processed data */
//      mach_msg_body_t msgh_body;
//      mach_msg_port_descriptor_t old_handlers[32];
//      /* end of the kernel processed data */
//      NDR_record_t NDR;
//      mach_msg_type_number_t masksCnt;
//      exception_mask_t masks[32];
//      exception_behavior_t old_behaviors[32];
//      thread_state_flavor_t old_flavors[32];
//   } Reply;
//#pragma pack()
}


///////////////////////////////////////////////////

PRE(mach_port_type)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_type(%s, %s, ...)", 
         name_for_port(MACH_REMOTE), name_for_port(req->name));

   AFTER = POST_FN(mach_port_type);
}

POST(mach_port_type)
{
}


PRE(mach_port_extract_member)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_port_name_t pset;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_extract_member(%s, 0x%x, 0x%x)", 
         name_for_port(MACH_REMOTE), 
         req->name, req->pset);

   AFTER = POST_FN(mach_port_extract_member);

   // GrP fixme port tracker?
}

POST(mach_port_extract_member)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (reply->RetCode) PRINT("mig return %d", reply->RetCode);
}


PRE(mach_port_allocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_right_t right;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_allocate(mach_task_self(), %d, ...)", req->right);

   MACH_ARG(mach_port_allocate.right) = req->right;

   AFTER = POST_FN(mach_port_allocate);
}

POST(mach_port_allocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_port_name_t name;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         // GrP fixme port tracking is too imprecise
         // vg_assert(!port_exists(reply->name));
         record_unnamed_port(tid, reply->name, MACH_ARG(mach_port_allocate.right));
         PRINT("got port 0x%x", reply->name);
      } else {
         VG_(printf)("UNKNOWN inserted port 0x%x into remote task\n", reply->name);
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_port_deallocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_deallocate(%s, %s)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name));

   MACH_ARG(mach_port.port) = req->name;

   AFTER = POST_FN(mach_port_deallocate);

   // Must block to prevent race (other thread allocates and 
   // notifies after we deallocate but before we notify)
   *flags &= ~SfMayBlock;
}

POST(mach_port_deallocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         // Must have cleared SfMayBlock in PRE to prevent race
         record_port_dealloc(MACH_ARG(mach_port.port));
      } else {
         VG_(printf)("UNKNOWN remote port dealloc\n");
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_port_get_refs)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_port_right_t right;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_get_refs(%s, %s, 0x%x)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name), req->right);

   MACH_ARG(mach_port_mod_refs.port) = req->name;
   MACH_ARG(mach_port_mod_refs.right) = req->right;
    
   AFTER = POST_FN(mach_port_get_refs);
}

POST(mach_port_get_refs)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_port_urefs_t refs;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      PRINT("got refs=%d", reply->refs);
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_port_mod_refs)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_port_right_t right;
      mach_port_delta_t delta;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_mod_refs(%s, %s, 0x%x, 0x%x)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name), req->right, req->delta);

   MACH_ARG(mach_port_mod_refs.port) = req->name;
   MACH_ARG(mach_port_mod_refs.right) = req->right;
   MACH_ARG(mach_port_mod_refs.delta) = req->delta;
    
   AFTER = POST_FN(mach_port_mod_refs);

   // Must block to prevent race (other thread allocates and 
   // notifies after we deallocate but before we notify)
   *flags &= ~SfMayBlock;
}

POST(mach_port_mod_refs)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         // Must have cleared SfMayBlock in PRE to prevent race
         record_port_mod_refs(MACH_ARG(mach_port_mod_refs.port), 
                              MACH_PORT_TYPE(MACH_ARG(mach_port_mod_refs.right)), 
                              MACH_ARG(mach_port_mod_refs.delta));
      } else {
         VG_(printf)("UNKNOWN remote port mod refs\n");
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_port_get_set_status)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_get_set_status(%s, %s)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name));

   AFTER = POST_FN(mach_port_get_set_status);
}

POST(mach_port_get_set_status)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//      /* start of the kernel processed data */
//      mach_msg_body_t msgh_body;
//      mach_msg_ool_descriptor_t members;
//      /* end of the kernel processed data */
//      NDR_record_t NDR;
//      mach_msg_type_number_t membersCnt;
//      mach_msg_trailer_t trailer;
//   } Reply;
//#pragma pack()

   // Reply *reply = (Reply *)ARG1;

   // GrP fixme nothing to do?
}


PRE(mach_port_move_member)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        NDR_record_t NDR;
        mach_port_name_t member;
        mach_port_name_t after;
    } Request;
#pragma pack()

    Request *req = (Request *)ARG1;

    PRINT("mach_port_move_member(%s, %s, %s)", 
          name_for_port(MACH_REMOTE), 
          name_for_port(req->member), 
          name_for_port(req->after));
    /*
    MACH_ARG(mach_port_move_member.member) = req->member;
    MACH_ARG(mach_port_move_member.after) = req->after;
    */
    AFTER = POST_FN(mach_port_move_member);
}

POST(mach_port_move_member)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      // fixme port set tracker?
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_port_destroy)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_destroy(%s, %s)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name));

   MACH_ARG(mach_port.port) = req->name;

   AFTER = POST_FN(mach_port_destroy);

   // Must block to prevent race (other thread allocates and 
   // notifies after we deallocate but before we notify)
   *flags &= ~SfMayBlock;
}

POST(mach_port_destroy)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         // Must have cleared SfMayBlock in PRE to prevent race
         record_port_destroy(MACH_ARG(mach_port.port));
      } else {
         VG_(printf)("UNKNOWN remote port destroy\n");
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_port_request_notification)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t notify;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_msg_id_t msgid;
      mach_port_mscount_t sync;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_request_notification(%s, %s, %d, %d, %d, %d, ...)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name), req->msgid, req->sync, 
         req->notify.name, req->notify.disposition);

   AFTER = POST_FN(mach_port_request_notification);
}

POST(mach_port_request_notification)
{
   // GrP fixme port tracker? not sure
}


PRE(mach_port_insert_right)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t poly;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      mach_port_name_t name;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_insert_right(%s, %s, %d, %d)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name), req->poly.name, req->poly.disposition);

   AFTER = POST_FN(mach_port_insert_right);

   if (MACH_REMOTE == mach_task_self()) {
      // GrP fixme import_complex_message handles everything?
      // what about export_complex_message for MOVE variants?
   } else {
      VG_(printf)("UNKNOWN mach_port_insert_right into remote task!\n");
      // GrP fixme also may remove rights from this task?
   }

   // GrP fixme port tracker?
}

POST(mach_port_insert_right)
{
}


PRE(mach_port_extract_right)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_msg_type_name_t msgt_name;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;
   
   PRINT("mach_port_extract_right(%s, %s, %d)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name), req->msgt_name);
   
   AFTER = POST_FN(mach_port_extract_right);
   
   // fixme port tracker?
}

POST(mach_port_extract_right)
{
   // fixme import_complex_message handles the returned result, right?
}


PRE(mach_port_get_attributes)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_port_flavor_t flavor;
      mach_msg_type_number_t port_info_outCnt;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_get_attributes(%s, %s, %d, ..., %d)", 
         name_for_port(MACH_REMOTE), 
         name_for_port(req->name), req->flavor, req->port_info_outCnt);

   AFTER = POST_FN(mach_port_get_attributes);
}

POST(mach_port_get_attributes)
{
}


PRE(mach_port_set_attributes)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_port_flavor_t flavor;
      mach_msg_type_number_t port_infoCnt;
      integer_t port_info[10];
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_set_attributes(%s, %s, %d, ..., %d)", 
        name_for_port(MACH_REMOTE), 
        name_for_port(req->name), req->flavor, req->port_infoCnt);

   AFTER = POST_FN(mach_port_set_attributes);
}

POST(mach_port_set_attributes)
{
}


PRE(mach_port_insert_member)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_port_name_t name;
      mach_port_name_t pset;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_port_insert_member(%s, 0x%x, 0x%x)", 
         name_for_port(MACH_REMOTE), req->name, req->pset);

   AFTER = POST_FN(mach_port_insert_member);

   // GrP fixme port tracker?
}

POST(mach_port_insert_member)
{
}


PRE(task_get_special_port)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      int which_port;
   } Request;
#pragma pack()
   
   Request *req = (Request *)ARG1;
   
   switch (req->which_port) {
   case TASK_KERNEL_PORT:
      PRINT("task_get_special_port(%s, TASK_KERNEL_PORT)", 
            name_for_port(MACH_REMOTE));
      break;
   case TASK_HOST_PORT:
      PRINT("task_get_special_port(%s, TASK_HOST_PORT)", 
            name_for_port(MACH_REMOTE));
      break;
   case TASK_BOOTSTRAP_PORT:
      PRINT("task_get_special_port(%s, TASK_BOOTSTRAP_PORT)", 
            name_for_port(MACH_REMOTE));
      break;
#if DARWIN_VERS < DARWIN_10_8
   /* These disappeared in 10.8 */
   case TASK_WIRED_LEDGER_PORT:
      PRINT("task_get_special_port(%s, TASK_WIRED_LEDGER_PORT)", 
            name_for_port(MACH_REMOTE));
      break;
   case TASK_PAGED_LEDGER_PORT:
      PRINT("task_get_special_port(%s, TASK_PAGED_LEDGER_PORT)", 
            name_for_port(MACH_REMOTE));
      break;
#endif
   default:
      PRINT("task_get_special_port(%s, %d)", 
            name_for_port(MACH_REMOTE), req->which_port);
      break;
   }
   
   MACH_ARG(task_get_special_port.which_port) = req->which_port;
   
   AFTER = POST_FN(task_get_special_port);
}

POST(task_get_special_port)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t special_port;
      /* end of the kernel processed data */
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;

   PRINT("got port %#x ", reply->special_port.name);

   switch (MACH_ARG(task_get_special_port.which_port)) {
   case TASK_BOOTSTRAP_PORT:
      vg_bootstrap_port = reply->special_port.name;
      assign_port_name(reply->special_port.name, "bootstrap");
      break;
   case TASK_KERNEL_PORT:
      assign_port_name(reply->special_port.name, "kernel");
      break;
   case TASK_HOST_PORT:
      assign_port_name(reply->special_port.name, "host");
      break;
#if DARWIN_VERS < DARWIN_10_8
   /* These disappeared in 10.8 */
   case TASK_WIRED_LEDGER_PORT:
      assign_port_name(reply->special_port.name, "wired-ledger");
      break;
   case TASK_PAGED_LEDGER_PORT:
      assign_port_name(reply->special_port.name, "paged-ledger");
      break;
#endif
   default:
      assign_port_name(reply->special_port.name, "special-%p");
      break;
   }

   PRINT("%s", name_for_port(reply->special_port.name));
}


PRE(task_set_special_port)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        /* start of the kernel processed data */
        mach_msg_body_t msgh_body;
        mach_msg_port_descriptor_t special_port;
        /* end of the kernel processed data */
        NDR_record_t NDR;
        int which_port;
    } Request;
#pragma pack()
    
    Request *req = (Request *)ARG1;

    PRINT("got port %#x ", req->special_port.name);
    
    // MACH_ARG(task_set_special_port.which_port) = req->which_port;
    PRINT("%s", name_for_port(req->special_port.name));
    
    AFTER = POST_FN(task_set_special_port);
}

POST(task_set_special_port)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        NDR_record_t NDR;
        kern_return_t RetCode;
    } Reply;
#pragma pack()
    
    Reply *reply = (Reply *)ARG1;
    if (!reply->RetCode) {
    } else {
        PRINT("mig return %d", reply->RetCode);
    }
}


PRE(semaphore_create)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      int policy;
      int value;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("semaphore_create(%s, ..., %d, %d)",
         name_for_port(MACH_REMOTE), req->policy, req->value);

   AFTER = POST_FN(semaphore_create);
}

POST(semaphore_create)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t semaphore;
      /* end of the kernel processed data */
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   assign_port_name(reply->semaphore.name, "semaphore-%p");
   PRINT("%s", name_for_port(reply->semaphore.name));
}


PRE(semaphore_destroy)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t semaphore;
      /* end of the kernel processed data */
      mach_msg_trailer_t trailer;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("semaphore_destroy(%s, %s)", 
         name_for_port(MACH_REMOTE), name_for_port(req->semaphore.name));

   record_port_destroy(req->semaphore.name);

   AFTER = POST_FN(semaphore_destroy);
}

POST(semaphore_destroy)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;        
   if (!reply->RetCode) {
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}

PRE(task_policy_set)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      task_policy_flavor_t flavor;
      mach_msg_type_number_t policy_infoCnt;
      integer_t policy_info[16];
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("task_policy_set(%s) flavor:%d", name_for_port(MACH_REMOTE), req->flavor);

   AFTER = POST_FN(task_policy_set);
}

POST(task_policy_set)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;        
   if (!reply->RetCode) {
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_ports_register)
{
//#pragma pack(4)
//    typedef struct {
//       mach_msg_header_t Head;
//       /* start of the kernel processed data */
//       mach_msg_body_t msgh_body;
//       mach_msg_ool_ports_descriptor_t init_port_set;
//       /* end of the kernel processed data */
//       NDR_record_t NDR;
//       mach_msg_type_number_t init_port_setCnt;
//    } Request;
//#pragma pack()
    
    // Request *req = (Request *)ARG1;
    
    PRINT("mach_ports_register(%s)", name_for_port(MACH_REMOTE));
    
    AFTER = POST_FN(mach_ports_register);
}

POST(mach_ports_register)
{
#pragma pack(4)
    typedef struct {
       mach_msg_header_t Head;
       NDR_record_t NDR;
       kern_return_t RetCode;
    } Reply;
#pragma pack()
    
    Reply *reply = (Reply *)ARG1;
    if (!reply->RetCode) {
    } else {
        PRINT("mig return %d", reply->RetCode);
    }
}


PRE(mach_ports_lookup)
{
//#pragma pack(4)
//   typedef struct {
//       mach_msg_header_t Head;
//   } Request;
//#pragma pack()

   // Request *req = (Request *)ARG1;

   PRINT("mach_ports_lookup(%s)", name_for_port(MACH_REMOTE));

   AFTER = POST_FN(mach_ports_lookup);
}

POST(mach_ports_lookup)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//      /* start of the kernel processed data */
//      mach_msg_body_t msgh_body;
//      mach_msg_ool_ports_descriptor_t init_port_set;
//      /* end of the kernel processed data */
//      NDR_record_t NDR;
//      mach_msg_type_number_t init_port_setCnt;
//   } Reply;
//#pragma pack()

    // Reply *reply = (Reply *)ARG1;
}


PRE(task_info)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        NDR_record_t NDR;
        task_flavor_t flavor;
        mach_msg_type_number_t task_info_outCnt;
    } Request;
#pragma pack()
    
    Request *req = (Request *)ARG1;
    
    PRINT("task_info(%s) flavor:%d", name_for_port(MACH_REMOTE), req->flavor);
    
    AFTER = POST_FN(task_info);
}

POST(task_info)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        NDR_record_t NDR;
        kern_return_t RetCode;
        mach_msg_type_number_t task_info_outCnt;
        integer_t task_info_out[52];
    } Reply;
#pragma pack()
    
    Reply *reply = (Reply *)ARG1;
    if (!reply->RetCode) {
    } else {
        PRINT("mig return %d", reply->RetCode);
    }
}


PRE(task_set_info)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        NDR_record_t NDR;
        task_flavor_t flavor;
        mach_msg_type_number_t task_info_inCnt;
        integer_t task_info_in[52];
    } Request;
#pragma pack()
    
    Request *req = (Request *)ARG1;
    
    PRINT("task_set_info(%s) flavor:%d", name_for_port(MACH_REMOTE), req->flavor);
    
    AFTER = POST_FN(task_set_info);
}

POST(task_set_info)
{
#pragma pack(4)
    typedef struct {
        mach_msg_header_t Head;
        NDR_record_t NDR;
        kern_return_t RetCode;
    } Reply;
#pragma pack()
    
    Reply *reply = (Reply *)ARG1;
    if (!reply->RetCode) {
    } else {
        PRINT("mig return %d", reply->RetCode);
    }
}

PRE(task_threads)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//   } Request;
//#pragma pack()

   // Request *req = (Request *)ARG1;

   PRINT("task_threads(%s)", name_for_port(MACH_REMOTE));

   AFTER = POST_FN(task_threads);
}

POST(task_threads)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_ool_ports_descriptor_t act_list;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      mach_msg_type_number_t act_listCnt;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (MACH_REMOTE == vg_task_port) {
      assign_port_names(&reply->act_list, "thread-%p");
   } else {
      assign_port_names(&reply->act_list, "remote-thread-%p");
   }
}


PRE(task_suspend)
{
   PRINT("task_suspend(%s)", name_for_port(MACH_REMOTE));

   if (MACH_REMOTE == vg_task_port) {
      // GrP fixme self-suspend
      vg_assert(0);
   } else {
      // suspend other - no problem
   }

   AFTER = POST_FN(task_suspend);
}

POST(task_suspend)
{
}


PRE(task_resume)
{
   PRINT("task_resume(%s)", name_for_port(MACH_REMOTE));

   if (MACH_REMOTE == vg_task_port) {
      // GrP fixme self-resume
      vg_assert(0);
   } else {
      // resume other - no problem
   }

   AFTER = POST_FN(task_resume);
}

POST(task_resume)
{
}


PRE(vm_allocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      vm_address_t address;
      vm_size_t size;
      int flags;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("vm_allocate (%s, at %#llx, size %llu, flags %#x)", 
         name_for_port(MACH_REMOTE), 
         (ULong)req->address, (ULong)req->size, req->flags);

   MACH_ARG(vm_allocate.size) = req->size;
   MACH_ARG(vm_allocate.flags) = req->flags;

   AFTER = POST_FN(vm_allocate);
}

POST(vm_allocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      vm_address_t address;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
        PRINT("allocated at %#llx", (ULong)reply->address);
         // requesting 0 bytes returns address 0 with no error
         if (MACH_ARG(vm_allocate.size)) {
            ML_(notify_core_and_tool_of_mmap)(
                  reply->address, MACH_ARG(vm_allocate.size), 
                  VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_ANON, -1, 0);
         }
      } else {
         PRINT("allocated at %#llx in remote task %s",
               (ULong)reply->address, 
               name_for_port(MACH_REMOTE));
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(vm_deallocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      vm_address_t address;
      vm_size_t size;
   } Request;
#pragma pack()
   
   Request *req = (Request *)ARG1;
   
   PRINT("vm_deallocate(%s, at %#llx, size %llu)", 
         name_for_port(MACH_REMOTE), 
         (ULong)req->address, (ULong)req->size);
   
   MACH_ARG(vm_deallocate.address) = req->address;
   MACH_ARG(vm_deallocate.size) = req->size;
   
   AFTER = POST_FN(vm_deallocate);

   // Must block to prevent race (other thread allocates and 
   // notifies after we deallocate but before we notify)
   *flags &= ~SfMayBlock;
}

POST(vm_deallocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         if (MACH_ARG(vm_deallocate.size)) {
            Addr start = VG_PGROUNDDN(MACH_ARG(vm_deallocate.address));
            Addr end = VG_PGROUNDUP(MACH_ARG(vm_deallocate.address) + 
                                    MACH_ARG(vm_deallocate.size));
            // Must have cleared SfMayBlock in PRE to prevent race
            ML_(notify_core_and_tool_of_munmap)(start, end - start);
         }
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}
   

PRE(vm_protect)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      vm_address_t address;
      vm_size_t size;
      boolean_t set_maximum;
      vm_prot_t new_protection;
   } Request;
#pragma pack()
   
   Request *req = (Request *)ARG1;
   
   PRINT("vm_protect(%s, at %#llx, size %llu, set_max %d, prot %d)", 
         name_for_port(MACH_REMOTE),
         (ULong)req->address, (ULong)req->size, 
         req->set_maximum, req->new_protection);
   
   MACH_ARG(vm_protect.address) = req->address;
   MACH_ARG(vm_protect.size) = req->size;
   MACH_ARG(vm_protect.set_maximum) = req->set_maximum;
   MACH_ARG(vm_protect.new_protection) = req->new_protection;
   
   AFTER = POST_FN(vm_protect);
}

POST(vm_protect)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         Addr start = VG_PGROUNDDN(MACH_ARG(vm_protect.address));
         Addr end = VG_PGROUNDUP(MACH_ARG(vm_protect.address) + 
                                 MACH_ARG(vm_protect.size));
         UInt prot = MACH_ARG(vm_protect.new_protection);
         if (MACH_ARG(vm_protect.set_maximum)) {
             // GrP fixme mprotect max
             VG_(printf)("UNKNOWN vm_protect set maximum");
            //VG_(mprotect_max_range)(start, end-start, prot);
         } else {
            ML_(notify_core_and_tool_of_mprotect)(start, end-start, prot);
            VG_(di_notify_vm_protect)(start, end-start, prot);
         }
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(vm_inherit)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      vm_address_t address;
      vm_size_t size;
      vm_inherit_t new_inheritance;
   } Request;
#pragma pack()
   
   Request *req = (Request *)ARG1;
   
   PRINT("vm_inherit(%s, at %#llx, size %llu, value %d)", 
         name_for_port(MACH_REMOTE), 
         (ULong)req->address, (ULong)req->size, 
         req->new_inheritance);
   
   AFTER = POST_FN(vm_inherit);
}

POST(vm_inherit)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         // GrP fixme do something?
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(vm_read)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      vm_address_t address;
      vm_size_t size;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("vm_read(from %s at %#llx size %llu)", 
         name_for_port(MACH_REMOTE),
         (ULong)req->address, (ULong)req->size);
   
   MACH_ARG(vm_read.addr) = req->address;
   MACH_ARG(vm_read.size) = req->size;

   AFTER = POST_FN(vm_read);
}

POST(vm_read)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//      /* start of the kernel processed data */
//      mach_msg_body_t msgh_body;
//      mach_msg_ool_descriptor_t data;
//      /* end of the kernel processed data */
//      NDR_record_t NDR;
//      mach_msg_type_number_t dataCnt;
//   } Reply;
//#pragma pack()

   // Reply *reply = (Reply *)ARG1;

   if (MACH_REMOTE == vg_task_port) {
      // vm_read from self
      // GrP fixme copy initialized state
   }
}



PRE(mach_vm_read)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t address;
      mach_vm_size_t size;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_vm_read(from %s at 0x%llx size %llu)", 
         name_for_port(MACH_REMOTE), req->address, req->size);
   
   MACH_ARG(mach_vm_read.addr) = req->address;
   MACH_ARG(mach_vm_read.size) = req->size;

   AFTER = POST_FN(mach_vm_read);
}

POST(mach_vm_read)
{
//#pragma pack(4)
//   typedef struct {
//      mach_msg_header_t Head;
//      /* start of the kernel processed data */
//      mach_msg_body_t msgh_body;
//      mach_msg_ool_descriptor_t data;
//      /* end of the kernel processed data */
//      NDR_record_t NDR;
//      mach_msg_type_number_t dataCnt;
//   } Reply;
//#pragma pack()

   // Reply *reply = (Reply *)ARG1;

   if (MACH_REMOTE == vg_task_port) {
      // vm_read from self
      // GrP fixme copy initialized state
   }
}


PRE(vm_read_overwrite)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      vm_address_t address;
      vm_size_t size;
      vm_address_t data;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("vm_read_overwrite(from %s at %#llx size %llu to %#llx)", 
         name_for_port(MACH_REMOTE),
         (ULong)req->address, (ULong)req->size, (ULong)req->data);
   
   MACH_ARG(vm_read_overwrite.addr) = req->address;
   MACH_ARG(vm_read_overwrite.size) = req->size;
   MACH_ARG(vm_read_overwrite.data) = req->data;

   PRE_MEM_WRITE("vm_read_overwrite(data)", req->data, req->size);

   AFTER = POST_FN(vm_read_overwrite);
}

POST(vm_read_overwrite)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      vm_size_t outsize;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (reply->RetCode) {
       PRINT("mig return %d", reply->RetCode);
   } else {
      PRINT("read %llu bytes", (unsigned long long)reply->outsize);
      if (MACH_REMOTE == vg_task_port) {
         // vm_read_overwrite from self
         // GrP fixme copy initialized state
         POST_MEM_WRITE(MACH_ARG(vm_read_overwrite.data), reply->outsize);
      } else {
         // vm_read_overwrite from remote
         POST_MEM_WRITE(MACH_ARG(vm_read_overwrite.data), reply->outsize);
      }
   }
}


PRE(vm_copy)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      vm_address_t source_address;
      vm_size_t size;
      vm_address_t dest_address;
   } Request;
#pragma pack()
   
   Request *req = (Request *)ARG1;
   
   PRINT("vm_copy(%s, %#llx, %lld, %#llx)", 
         name_for_port(MACH_REMOTE), 
         (ULong)req->source_address,
         (ULong)req->size, (ULong)req->dest_address);

   MACH_ARG(vm_copy.src) = req->source_address;
   MACH_ARG(vm_copy.dst) = req->dest_address;
   MACH_ARG(vm_copy.size) = req->size;
   
   AFTER = POST_FN(vm_copy);
}

POST(vm_copy)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         // GrP fixme set dst's initialization equal to src's
         // and wipe any symbols or translations in dst
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(vm_map)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t object;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      vm_address_t address;
      vm_size_t size;
      vm_address_t mask;
      int flags;
      vm_offset_t offset;
      boolean_t copy;
      vm_prot_t cur_protection;
      vm_prot_t max_protection;
      vm_inherit_t inheritance;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   // GrP fixme check these
   PRINT("vm_map(in %s, at %#llx, size %llu, from %s ...)", 
         name_for_port(MACH_REMOTE), 
         (ULong)req->address, (ULong)req->size, 
         name_for_port(req->object.name));

   MACH_ARG(vm_map.size) = req->size;
   MACH_ARG(vm_map.copy) = req->copy;
   MACH_ARG(vm_map.protection) = (req->cur_protection & req->max_protection);

   AFTER = POST_FN(vm_map);
}

POST(vm_map)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      vm_address_t address;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      // GrP fixme check src and dest tasks
     PRINT("mapped at %#llx", (ULong)reply->address);
      // GrP fixme max prot
      ML_(notify_core_and_tool_of_mmap)(
            reply->address, VG_PGROUNDUP(MACH_ARG(vm_map.size)), 
            MACH_ARG(vm_map.protection), VKI_MAP_SHARED, -1, 0);
      // GrP fixme VKI_MAP_PRIVATE if !copy?
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(vm_remap)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t src_task;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      vm_address_t target_address;
      vm_size_t size;
      vm_address_t mask;
      boolean_t anywhere;
      vm_address_t src_address;
      boolean_t copy;
      vm_inherit_t inheritance;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   // GrP fixme check src and dest tasks

   if (VG_(clo_trace_syscalls)) {
      mach_port_name_t source_task = req->src_task.name;
      if (source_task == mach_task_self()) {
         PRINT("vm_remap(mach_task_self(), "
               "to %#llx size %llu, from mach_task_self() at %#llx, ...)",
               (ULong)req->target_address,
               (ULong)req->size, (ULong)req->src_address);
      } else {
         PRINT("vm_remap(mach_task_self(), "
               "to %#llx size %llu, from task %u at %#llx, ...)",
               (ULong)req->target_address, (ULong)req->size, 
               source_task, (ULong)req->src_address);
      }
   }

   // arg1 is task
   // vt->syscall_arg2 = req->target_address;
   MACH_ARG(vm_remap.size) = req->size;
   // vt->syscall_arg4 = req->copy;

   AFTER = POST_FN(vm_remap);
}

POST(vm_remap)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      vm_address_t target_address;
      vm_prot_t cur_protection;
      vm_prot_t max_protection;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      // GrP fixme check src and dest tasks
      UInt prot = reply->cur_protection & reply->max_protection;
      // GrP fixme max prot
      PRINT("mapped at %#llx", (ULong)reply->target_address);
      ML_(notify_core_and_tool_of_mmap)(
            reply->target_address, VG_PGROUNDUP(MACH_ARG(vm_remap.size)), 
            prot, VKI_MAP_SHARED, -1, 0);
      // GrP fixme VKI_MAP_FIXED if !copy?
      // GrP fixme copy initialized bits from source to dest if source_task is also mach_task_self
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_make_memory_entry_64)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t parent_entry;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      memory_object_size_t size;
      memory_object_offset_t offset;
      vm_prot_t permission;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_make_memory_entry_64(%s, %llu, %llu, %d, ..., %u)", 
         name_for_port(MACH_REMOTE), 
         req->size, req->offset, req->permission, req->parent_entry.type);

   AFTER = POST_FN(mach_make_memory_entry_64);
}

POST(mach_make_memory_entry_64)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t object;
      NDR_record_t NDR;
      memory_object_size_t size;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (reply->Head.msgh_bits & MACH_MSGH_BITS_COMPLEX) {
      assign_port_name(reply->object.name, "memory-%p");
      PRINT("%s", name_for_port(reply->object.name));
   }
}


PRE(vm_purgable_control)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      vm_address_t address;
      vm_purgable_t control;
      int state;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("vm_purgable_control(%s, %#llx, %d, %d)", 
         name_for_port(MACH_REMOTE), 
         (ULong)req->address, req->control, req->state);

   // GrP fixme verify address?

   AFTER = POST_FN(vm_purgable_control);
}

POST(vm_purgable_control)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      int state;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_vm_purgable_control)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t address;
      vm_purgable_t control;
      int state;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_vm_purgable_control(%s, 0x%llx, %d, %d)", 
         name_for_port(MACH_REMOTE), 
         (ULong)req->address, req->control, req->state);

   // GrP fixme verify address?

   AFTER = POST_FN(mach_vm_purgable_control);
}

POST(mach_vm_purgable_control)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      int state;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_vm_allocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t address;
      mach_vm_size_t size;
      int flags;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_vm_allocate (%s, at 0x%llx, size %llu, flags 0x%x)", 
         name_for_port(MACH_REMOTE), 
         req->address, req->size, req->flags);

   MACH_ARG(mach_vm_allocate.size) = req->size;
   MACH_ARG(mach_vm_allocate.flags) = req->flags;

   AFTER = POST_FN(mach_vm_allocate);
}

POST(mach_vm_allocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_vm_address_t address;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         PRINT("allocated at 0x%llx", reply->address);
         // requesting 0 bytes returns address 0 with no error
         if (MACH_ARG(mach_vm_allocate.size)) {
            ML_(notify_core_and_tool_of_mmap)(
                  reply->address, MACH_ARG(mach_vm_allocate.size), 
                  VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_ANON, -1, 0);
         }
      } else {
         PRINT("allocated at 0x%llx in remote task %s", reply->address, 
               name_for_port(MACH_REMOTE));
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_vm_deallocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t address;
      mach_vm_size_t size;
   } Request;
#pragma pack()
   
   Request *req = (Request *)ARG1;
   
   PRINT("mach_vm_deallocate(%s, at 0x%llx, size %llu)", 
         name_for_port(MACH_REMOTE), 
         req->address, req->size);
   
   MACH_ARG(mach_vm_deallocate.address) = req->address;
   MACH_ARG(mach_vm_deallocate.size) = req->size;
   
   AFTER = POST_FN(mach_vm_deallocate);

   // Must block to prevent race (other thread allocates and 
   // notifies after we deallocate but before we notify)
   *flags &= ~SfMayBlock;
}

POST(mach_vm_deallocate)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         if (MACH_ARG(mach_vm_deallocate.size)) {
            Addr start = VG_PGROUNDDN(MACH_ARG(mach_vm_deallocate.address));
            Addr end = VG_PGROUNDUP(MACH_ARG(mach_vm_deallocate.address) + 
                                    MACH_ARG(mach_vm_deallocate.size));
            // Must have cleared SfMayBlock in PRE to prevent race
            ML_(notify_core_and_tool_of_munmap)(start, end - start);
         }
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}
   

PRE(mach_vm_protect)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t address;
      mach_vm_size_t size;
      boolean_t set_maximum;
      vm_prot_t new_protection;
   } Request;
#pragma pack()
   
   Request *req = (Request *)ARG1;
   
   PRINT("mach_vm_protect(%s, at 0x%llx, size %llu, set_max %d, prot %d)", 
         name_for_port(MACH_REMOTE), req->address, req->size, 
         req->set_maximum, req->new_protection);
   
   MACH_ARG(mach_vm_protect.address) = req->address;
   MACH_ARG(mach_vm_protect.size) = req->size;
   MACH_ARG(mach_vm_protect.set_maximum) = req->set_maximum;
   MACH_ARG(mach_vm_protect.new_protection) = req->new_protection;
   
   AFTER = POST_FN(mach_vm_protect);
}

POST(mach_vm_protect)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         Addr start = VG_PGROUNDDN(MACH_ARG(mach_vm_protect.address));
         Addr end = VG_PGROUNDUP(MACH_ARG(mach_vm_protect.address) + 
                                 MACH_ARG(mach_vm_protect.size));
         UInt prot = MACH_ARG(mach_vm_protect.new_protection);
         if (MACH_ARG(mach_vm_protect.set_maximum)) {
            // DDD: #warning GrP fixme mprotect max
            //VG_(mprotect_max_range)(start, end-start, prot);
         } else {
            ML_(notify_core_and_tool_of_mprotect)(start, end-start, prot);
         }
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_vm_inherit)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t address;
      mach_vm_size_t size;
      vm_inherit_t new_inheritance;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;
   
   PRINT("mach_vm_inherit(to %s, at 0x%llx, size %llu, value %u)", 
         name_for_port(MACH_REMOTE), 
         req->address, req->size, req->new_inheritance);

   AFTER = POST_FN(mach_vm_inherit);
}

POST(mach_vm_inherit)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      // no V-visible side effects
      // GrP fixme except maybe fork/exec
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_vm_copy)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t source_address;
      mach_vm_size_t size;
      mach_vm_address_t dest_address;
   } Request;
#pragma pack()
   
   Request *req = (Request *)ARG1;
   
   PRINT("mach_vm_copy(%s, 0x%llx, %llu, 0x%llx)", 
         name_for_port(MACH_REMOTE), 
         req->source_address, req->size, req->dest_address);
   
   // arg1 is task
   // vt->syscall_arg2 = req->source_address;
   // vt->syscall_arg3 = req->size;
   // vt->syscall_arg4 = req->dest_address;
   
   AFTER = POST_FN(mach_vm_copy);
}

POST(mach_vm_copy)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;
   
   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         // GrP fixme set dest's initialization equal to src's
         // BUT vm_copy allocates no memory
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}

PRE(mach_vm_read_overwrite)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t address;
      mach_vm_size_t size;
      mach_vm_address_t data;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_vm_read_overwrite(%s, 0x%llx, %llu, 0x%llx)",
         name_for_port(MACH_REMOTE),
         req->address, req->size, req->data);

   AFTER = POST_FN(mach_vm_read_overwrite);
}

POST(mach_vm_read_overwrite)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_vm_size_t outsize;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      if (MACH_REMOTE == vg_task_port) {
         // GrP fixme set dest's initialization equal to src's
         // BUT vm_copy allocates no memory
      }
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}

PRE(mach_vm_map)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t object;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      mach_vm_address_t address;
      mach_vm_size_t size;
      mach_vm_address_t mask;
      int flags;
      memory_object_offset_t offset;
      boolean_t copy;
      vm_prot_t cur_protection;
      vm_prot_t max_protection;
      vm_inherit_t inheritance;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   // GrP fixme check these
   PRINT("mach_vm_map(in %s->%s at 0x%llx, size %llu, cur_prot:%x max_prot:%x ...)", 
         name_for_port(req->Head.msgh_remote_port), 
         name_for_port(req->object.name),
         req->address, req->size, 
         req->cur_protection,
         req->max_protection);

   MACH_ARG(mach_vm_map.size) = req->size;
   MACH_ARG(mach_vm_map.copy) = req->copy;
   MACH_ARG(mach_vm_map.protection) = 
      (req->cur_protection & req->max_protection);

   AFTER = POST_FN(mach_vm_map);
}

POST(mach_vm_map)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_vm_address_t address;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      // GrP fixme check src and dest tasks
      PRINT("mapped at 0x%llx", reply->address);
#     if 0
      // GrP fixme max prot
      ML_(notify_core_and_tool_of_mmap)(
            reply->address, VG_PGROUNDUP(MACH_ARG(mach_vm_map.size)), 
            MACH_ARG(mach_vm_map.protection), VKI_MAP_SHARED, -1, 0);
      // GrP fixme VKI_MAP_PRIVATE if !copy?
#     else
      ML_(sync_mappings)("after", "mach_vm_map", 0);
#     endif
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_vm_remap)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t src_task;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      mach_vm_address_t target_address;
      mach_vm_size_t size;
      mach_vm_offset_t mask;
      int flags;
      mach_vm_address_t src_address;
      boolean_t copy;
      vm_inherit_t inheritance;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   // GrP fixme check these
   PRINT("mach_vm_remap(in %s, at 0x%llx, size %llu, from %s ...)",
         name_for_port(MACH_REMOTE),
         req->target_address, req->size,
         name_for_port(req->src_task.name));

   MACH_ARG(mach_vm_remap.size) = req->size;
   MACH_ARG(mach_vm_remap.copy) = req->copy;

   AFTER = POST_FN(mach_vm_remap);
}

POST(mach_vm_remap)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_vm_address_t target_address;
      vm_prot_t cur_protection;
      vm_prot_t max_protection;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
      // GrP fixme check src and dest tasks
      PRINT("mapped at 0x%llx", reply->target_address);
      // GrP fixme max prot
      ML_(notify_core_and_tool_of_mmap)(
            reply->target_address, VG_PGROUNDUP(MACH_ARG(mach_vm_remap.size)),
            reply->cur_protection, VKI_MAP_SHARED, -1, 0);
      // GrP fixme VKI_MAP_PRIVATE if !copy?
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


PRE(mach_vm_region_recurse)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      mach_vm_address_t address;
      natural_t nesting_depth;
      mach_msg_type_number_t infoCnt;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("mach_vm_region_recurse(in %s, at 0x%llx, depth %u, count %u)", 
         name_for_port(MACH_REMOTE), 
         req->address, req->nesting_depth, req->infoCnt);

   AFTER = POST_FN(mach_vm_region_recurse);
}

POST(mach_vm_region_recurse)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_vm_address_t address;
      mach_vm_size_t size;
      natural_t nesting_depth;
      mach_msg_type_number_t infoCnt;
      int info[19];
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (!reply->RetCode) {
       PRINT("got region at 0x%llx, size %llu, depth %u, count %u", 
             reply->address, reply->size, 
             reply->nesting_depth, reply->infoCnt);
       // GrP fixme mark info contents beyond infoCnt as bogus
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}


/* ---------------------------------------------------------------------
   mach_msg: messages to thread
   ------------------------------------------------------------------ */



POST(thread_terminate)
{
}


PRE(thread_terminate)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   Bool self_terminate = (mh->msgh_request_port == MACH_THREAD);

   PRINT("thread_terminate(%s)", name_for_port(mh->msgh_request_port));

   AFTER = POST_FN(thread_terminate);

   if (self_terminate) {
      // Terminating this thread.
      // Copied from sys_exit.
      ThreadState *tst = VG_(get_ThreadState)(tid);
      tst->exitreason = VgSrc_ExitThread;
      tst->os_state.exitcode = 0;  // GrP fixme anything better?
      // What we would like to do is:
      //   SET_STATUS_Success(0);
      // but that doesn't work, because this is a MACH-class syscall,
      // and SET_STATUS_Success creates a UNIX-class syscall result.
      // Hence we have to laboriously construct the full SysRes "by hand"
      // and use that to set the syscall return status.
#if defined(VGA_x86)
      SET_STATUS_from_SysRes(
         VG_(mk_SysRes_x86_darwin)(
            VG_DARWIN_SYSCALL_CLASS_MACH,
            False/*success*/, 0, 0
         )
      );
#elif defined(VGA_amd64)
       SET_STATUS_from_SysRes(
         VG_(mk_SysRes_amd64_darwin)(
            VG_DARWIN_SYSCALL_CLASS_MACH,
            False/*success*/, 0, 0
         )
      );
#else
#error unknown architecture
#endif
      *flags &= ~SfMayBlock;  // clear flag set by PRE(mach_msg)
   } else {
      // Terminating some other thread.
      // Do keep the scheduler lock while terminating any other thread. 
      // Otherwise we might halt the other thread while it holds the lock, 
      // which would deadlock the process.
      // GrP fixme good enough?
      // GrP fixme need to clean up other thread's valgrind data?
   }
}


POST(thread_create)
{
}


PRE(thread_create)
{
   PRINT("thread_create(mach_task_self(), ...)");

   AFTER = POST_FN(thread_create);

   // GrP fixme
   VG_(core_panic)("thread_create() unimplemented");
}


PRE(thread_create_running)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      thread_state_flavor_t flavor;
      mach_msg_type_number_t new_stateCnt;
      natural_t new_state[144];
   } Request;
#pragma pack()
   
   Request *req;
   thread_state_t regs;
   ThreadState *new_thread;
   
   PRINT("thread_create_running(mach_task_self(), ...)");
   
   // The new thread will immediately begin execution, 
   // so we need to hijack the register state here.
   
   req = (Request *)ARG1;
   regs = (thread_state_t)req->new_state;
   
   // Build virtual thread.
   new_thread = build_thread(regs, req->flavor, req->new_stateCnt);
   
   // Edit the thread state to send to the real kernel.
   hijack_thread_state(regs, req->flavor, req->new_stateCnt, new_thread);

   AFTER = POST_FN(thread_create_running);
}


POST(thread_create_running)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t child_act;
      /* end of the kernel processed data */
   } Reply;
#pragma pack()
   
   Reply *reply = (Reply *)ARG1;

   assign_port_name(reply->child_act.name, "thread-%p");
   PRINT("%s", name_for_port(reply->child_act.name));
}


PRE(bsdthread_create)
{
   ThreadState *tst;

   PRINT("bsdthread_create( %#lx, %#lx, %#lx, %#lx, %#lx )", 
         ARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(pthread_t,"bsdthread_create", 
                 void *,"func", void *,"func_arg", void *,"stack", 
                 pthread_t,"thread", unsigned int,"flags");

   // The kernel will call V's pthread_hijack() to launch the thread.
   // Here we allocate the thread state and pass it to pthread_hijack()
   // via the func_arg parameter.
   
   tst = VG_(get_ThreadState)(VG_(alloc_ThreadState)());
   allocstack(tst->tid);

   tst->os_state.func_arg = (Addr)ARG2;
   ARG2 = (Word)tst;

   // Create a semaphore that pthread_hijack will signal once it starts
   // POST(bsdthread_create) needs to wait for the new memory map to appear
   semaphore_create(mach_task_self(), &tst->os_state.child_go, 
                    SYNC_POLICY_FIFO, 0);
   semaphore_create(mach_task_self(), &tst->os_state.child_done, 
                    SYNC_POLICY_FIFO, 0);
}

POST(bsdthread_create)
{ 
   // Tell new thread's pthread_hijack to proceed, and wait for it to finish.
   // We hold V's lock on the child's behalf.
   // If we return before letting pthread_hijack do its thing, V thinks 
   // the new pthread struct is still unmapped when we return to libc, 
   // causing false errors.

   ThreadState *tst = (ThreadState *)ARG2;
   semaphore_signal(tst->os_state.child_go);
   semaphore_wait(tst->os_state.child_done);
   semaphore_destroy(mach_task_self(), tst->os_state.child_go);
   semaphore_destroy(mach_task_self(), tst->os_state.child_done);

   // GrP fixme semaphore destroy needed when thread creation fails
   // GrP fixme probably other cleanup too
   // GrP fixme spinlocks might be good enough?

   // DDD: I'm not at all sure this is the right spot for this.  It probably
   // should be in pthread_hijack instead, just before the call to
   // start_thread_NORETURN(), call_on_new_stack_0_1(), but we don't have the
   // parent tid value there...
   vg_assert(VG_(owns_BigLock_LL)(tid));
   VG_TRACK ( pre_thread_ll_create, tid, tst->tid );
}


PRE(bsdthread_terminate)
{
   ThreadState *tst;

   PRINT("bsdthread_terminate( %#lx, %lx, %s, %s )", 
         ARG1, ARG2, name_for_port(ARG3), name_for_port(ARG4));
   PRE_REG_READ4(int,"bsdthread_terminate", 
                 void *,"freeaddr", size_t,"freesize", 
                 mach_port_t,"kport", mach_port_t,"joinsem");

   // Free memory and signal semaphore.
   // GrP fixme errors?
   if (ARG4) semaphore_signal((semaphore_t)ARG4);
   if (ARG1  &&  ARG2) {
       ML_(notify_core_and_tool_of_munmap)(ARG1, ARG2);
#      if DARWIN_VERS >= DARWIN_10_8
       /* JRS 2012 Aug 02: ugly hack: vm_deallocate disappeared from
          the mig output.  Work around it for the time being. */
       VG_(do_syscall2)(__NR_munmap, ARG1, ARG2);
#      else
       vm_deallocate(mach_task_self(), (vm_address_t)ARG1, (vm_size_t)ARG2);
#      endif
   }

   // Tell V to terminate the thread.
   // Copied from sys_exit.
   tst = VG_(get_ThreadState)(tid);
   tst->exitreason = VgSrc_ExitThread;
   tst->os_state.exitcode = 0;  // GrP fixme anything better?
   SET_STATUS_Success(0);
}


POST(thread_suspend)
{
}

PRE(thread_suspend)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   Bool self_suspend = (mh->msgh_request_port == MACH_THREAD);

   PRINT("thread_suspend(%s)", name_for_port(mh->msgh_request_port));

   AFTER = POST_FN(thread_suspend);

   if (self_suspend) {
       // Don't keep the scheduler lock while self-suspending.
       // Otherwise we might halt while still holding the lock, 
       // which would deadlock the process.
       *flags |= SfMayBlock;
   } else {
       // Do keep the scheduler lock while suspending any other thread. 
       // Otherwise we might halt the other thread while it holds the lock, 
       // which would deadlock the process.
       *flags &= ~SfMayBlock;
   }
}


POST(thread_resume)
{
}

PRE(thread_resume)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   Bool self_resume = (mh->msgh_request_port == MACH_THREAD);

   PRINT("thread_resume(%s)", name_for_port(mh->msgh_request_port));

   AFTER = POST_FN(thread_resume);

   if (self_resume) {
       // This doesn't make much sense.  If we are resuming ourself, we can't
       // already be running.  So I don't see how we can ever get here.
       vg_assert(0);
   } else {
       // Resuming some other thread.  It might not yet come back to life
       // (if the suspend count is still above zero) so make sure we keep
       // holding the lock.
       *flags &= ~SfMayBlock;
   }
}


POST(thread_get_state)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_type_number_t old_stateCnt;
      natural_t old_state[144];
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;
   // mach_port_t thread = MACH_ARG(thread_get_state.thread);
   thread_state_flavor_t flavor = MACH_ARG(thread_get_state.flavor);

   if (!reply->RetCode) {
      thread_state_from_vex((thread_state_t)reply->old_state, 
                             flavor, reply->old_stateCnt, 
                             &VG_(get_ThreadState)(tid)->arch.vex);
   } else {
      PRINT("mig return %d", reply->RetCode);
   }
}

PRE(thread_get_state)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      thread_state_flavor_t flavor;
      mach_msg_type_number_t old_stateCnt;
   } Request;
#pragma pack()
    
   Request *req = (Request *)ARG1;
   // Bool self = (req->Head.msgh_request_port == MACH_THREAD);

   // GrP fixme   if (self) {
   PRINT("thread_get_state(%s, %d)", 
         name_for_port(req->Head.msgh_request_port), req->flavor);
       /*} else {
       PRINT("thread_get_state(0x%x, %d)", 
             req->Head.msgh_request_port, req->flavor);
             }*/

   // Hack the thread state after making the real call.
   MACH_ARG(thread_get_state.thread) = req->Head.msgh_request_port;
   MACH_ARG(thread_get_state.flavor) = req->flavor;

   AFTER = POST_FN(thread_get_state);
}


PRE(thread_policy)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   // Bool self = (mh->msgh_request_port == MACH_THREAD);

   // GrP fixme   if (self) {
      PRINT("thread_policy(%s, ...)", name_for_port(mh->msgh_request_port));
      /*} else {
      PRINT("thread_policy(thread 0x%x, ...)", mh->msgh_request_port);
      }*/

   AFTER = POST_FN(thread_policy);
}

POST(thread_policy)
{
}


PRE(thread_policy_set)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;

   PRINT("thread_policy_set(%s, ...)", name_for_port(mh->msgh_request_port));

   AFTER = POST_FN(thread_policy_set);
}

POST(thread_policy_set)
{
}


PRE(thread_info)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;

   PRINT("thread_info(%s, ...)", name_for_port(mh->msgh_request_port));
   // GrP fixme does any thread info need to be hijacked?

   AFTER = POST_FN(thread_info);
}

POST(thread_info)
{
   // GrP fixme mark unused parts of thread_info_out as uninitialized?
}



/* ---------------------------------------------------------------------
   mach_msg: messages to bootstrap port
   ------------------------------------------------------------------ */


POST(bootstrap_register)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      kern_return_t RetCode;
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if (reply->RetCode) PRINT("mig return %d", reply->RetCode);
}

PRE(bootstrap_register)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t service_port;
      /* end of the kernel processed data */
      NDR_record_t NDR;
      name_t service_name;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("bootstrap_register(port 0x%x, \"%s\")",
         req->service_port.name, req->service_name);

   /* The required entry in the allocated_ports list (mapping) might
      not exist, due perhaps to broken syscall wrappers (mach__N etc).
      Create a minimal entry so that assign_port_name below doesn't
      cause an assertion. */
   if (!port_exists(req->service_port.name)) {
      port_create_vanilla(req->service_port.name);
   }

   assign_port_name(req->service_port.name, req->service_name);

   AFTER = POST_FN(bootstrap_register);
}


POST(bootstrap_look_up)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      /* start of the kernel processed data */
      mach_msg_body_t msgh_body;
      mach_msg_port_descriptor_t service_port;
      /* end of the kernel processed data */
      mach_msg_trailer_t trailer;
   } Reply;
#pragma pack()

   Reply *reply = (Reply *)ARG1;

   if ((reply->Head.msgh_bits & MACH_MSGH_BITS_COMPLEX)  &&  
       reply->service_port.name) 
   {
       assign_port_name(reply->service_port.name, 
                        MACH_ARG(bootstrap_look_up.service_name));
       PRINT("%s", name_for_port(reply->service_port.name));
   } else {
       PRINT("not found");
   }
   VG_(free)(MACH_ARG(bootstrap_look_up.service_name));
}

PRE(bootstrap_look_up)
{
#pragma pack(4)
   typedef struct {
      mach_msg_header_t Head;
      NDR_record_t NDR;
      name_t service_name;
   } Request;
#pragma pack()

   Request *req = (Request *)ARG1;

   PRINT("bootstrap_look_up(\"%s\")", req->service_name);

   MACH_ARG(bootstrap_look_up.service_name) =
      VG_(strdup)("syswrap-darwin.bootstrap-name", req->service_name);

   AFTER = POST_FN(bootstrap_look_up);
}


/* ---------------------------------------------------------------------
   mach_msg: receiver-specific handlers
   ------------------------------------------------------------------ */


POST(mach_msg_receive)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;

   // GrP fixme don't know of anything interesting here currently
   // import_complex_message handles everything
   // PRINT("UNHANDLED reply %d", mh->msgh_id);

   // Assume the call may have mapped or unmapped memory
   ML_(sync_mappings)("after", "mach_msg_receive", mh->msgh_id);
}

PRE(mach_msg_receive)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   
   PRINT("mach_msg_receive(port %s)", name_for_port(mh->msgh_reply_port));
   
   AFTER = POST_FN(mach_msg_receive);

   // no message sent, only listening for a reply
   // assume message may block
   *flags |= SfMayBlock;
}


PRE(mach_msg_bootstrap)
{
   // message to bootstrap port

   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   
   switch (mh->msgh_id) {
   case 403:
      CALL_PRE(bootstrap_register);
      return;
   case 404:
      CALL_PRE(bootstrap_look_up);
      return;
      
   default:
      PRINT("UNHANDLED bootstrap message [id %d, to %s, reply 0x%x]\n", 
            mh->msgh_id, name_for_port(mh->msgh_request_port),
            mh->msgh_reply_port);
      return;
   }
}


PRE(mach_msg_host)
{
   // message to host self - check for host-level kernel calls

   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;

   switch (mh->msgh_id) {
   case 200:
      CALL_PRE(host_info);
      return;
   case 202:
      CALL_PRE(host_page_size);
      return;
   case 205:
      CALL_PRE(host_get_io_master);
      return;
   case 206:
      CALL_PRE(host_get_clock_service);
      return;
   case 217:
      CALL_PRE(host_request_notification);
      return;
   case 222:
      CALL_PRE(host_create_mach_voucher);
      return;
           
   case 412:
      CALL_PRE(host_get_special_port);
      return;

   default:
      // unknown message to host self
      log_decaying("UNKNOWN host message [id %d, to %s, reply 0x%x]", 
                   mh->msgh_id, name_for_port(mh->msgh_request_port), 
                   mh->msgh_reply_port);
      return;
   }
} 

// JRS 2011-Aug-25: these magic numbers (3201 etc) come from
// /usr/include/mach/mach_port.h et al (grep in /usr/include
// for them)
PRE(mach_msg_task)
{
   // message to a task port

   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;

   switch (mh->msgh_id) {
   case 3201:
      CALL_PRE(mach_port_type);
      return;
   case 3204:
      CALL_PRE(mach_port_allocate);
      return;
   case 3205:
      CALL_PRE(mach_port_destroy);
      return;
   case 3206:
      CALL_PRE(mach_port_deallocate);
      return;
   case 3207:
      CALL_PRE(mach_port_get_refs);
      return;
   case 3208:
      CALL_PRE(mach_port_mod_refs);
      return;
   case 3211:
      CALL_PRE(mach_port_get_set_status);
      return;
   case 3212:
      CALL_PRE(mach_port_move_member);
      return;
   case 3213:
      CALL_PRE(mach_port_request_notification);
      return;
   case 3214:
      CALL_PRE(mach_port_insert_right);
      return;
   case 3215:
      CALL_PRE(mach_port_extract_right);
      return;
   case 3217:
      CALL_PRE(mach_port_get_attributes);
      return;
   case 3218:
      CALL_PRE(mach_port_set_attributes);
      return;
   case 3226:
      CALL_PRE(mach_port_insert_member);
      return;
   case 3227:
      CALL_PRE(mach_port_extract_member);
      return;

   case 3229:
      CALL_PRE(mach_port_set_context);
      return;

   case 3402:
      CALL_PRE(task_threads);
      return;
   case 3403:
      CALL_PRE(mach_ports_register);
      return;
   case 3404:
      CALL_PRE(mach_ports_lookup);
      return;
   case 3405:
      CALL_PRE(task_info);
      return;
   case 3406:
      CALL_PRE(task_set_info);
      return;
   case 3407:
      CALL_PRE(task_suspend);
      return;
   case 3408:
      CALL_PRE(task_resume);
      return;
   case 3409:
      CALL_PRE(task_get_special_port);
      return;
   case 3410:
      CALL_PRE(task_set_special_port);
      return;
   case 3411:
      CALL_PRE(thread_create);
      return;
   case 3412:
      CALL_PRE(thread_create_running);
      return;

   case 3414:
      CALL_PRE(task_get_exception_ports);
      return;
      
   case 3418:
      CALL_PRE(semaphore_create);
      return;
   case 3419:
      CALL_PRE(semaphore_destroy);
      return;
   case 3420:
      CALL_PRE(task_policy_set);
      return;

#if DARWIN_VERS >= DARWIN_10_12
   case 3444:
      CALL_PRE(task_register_dyld_image_infos);
      return;

   case 3447:
      CALL_PRE(task_register_dyld_shared_cache_image_info);
      return;
#endif /* DARWIN_VERS >= DARWIN_10_12 */
      
   case 3801:
      CALL_PRE(vm_allocate);
      return;
   case 3802:
      CALL_PRE(vm_deallocate);
      return;
   case 3803:
      CALL_PRE(vm_protect);
      return;
   case 3804:
      CALL_PRE(vm_inherit);
      return;
   case 3805:
      CALL_PRE(vm_read);
      return;
   case 3808:
      CALL_PRE(vm_copy);
      return;
   case 3809:
      CALL_PRE(vm_read_overwrite);
      return;
   case 3812:
      CALL_PRE(vm_map);
      return;
   case 3814:
      CALL_PRE(vm_remap);
      return;
   case 3825:
      CALL_PRE(mach_make_memory_entry_64);
      return;
   case 3830:
      CALL_PRE(vm_purgable_control);
      return;

   case 4800:
      CALL_PRE(mach_vm_allocate);
      return;
   case 4801:
      CALL_PRE(mach_vm_deallocate);
      return;
   case 4802:
      CALL_PRE(mach_vm_protect);
      return;
   case 4803:
      CALL_PRE(mach_vm_inherit);
      return;
   case 4804:
      CALL_PRE(mach_vm_read);
      return;
   case 4807:
      CALL_PRE(mach_vm_copy);
      return;
   case 4808:
      CALL_PRE(mach_vm_read_overwrite);
      return;
   case 4811:
      CALL_PRE(mach_vm_map);
      return;
   case 4813:
      CALL_PRE(mach_vm_remap);
      return;
   case 4815:
      CALL_PRE(mach_vm_region_recurse);
      return;
   case 4817:
      CALL_PRE(mach_make_memory_entry_64);
      return;
   case 4818:
      CALL_PRE(mach_vm_purgable_control);
      return;

   default:
      // unknown message to task self
      log_decaying("UNKNOWN task message [id %d, to %s, reply 0x%x]",
                   mh->msgh_id, name_for_port(mh->msgh_remote_port),
                   mh->msgh_reply_port);
      return;
   }
} 


PRE(mach_msg_thread)
{
   // message to local thread - check for thread-level kernel calls

   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;

   switch (mh->msgh_id) {
   case 3600: 
      CALL_PRE(thread_terminate);
      return;
   case 3603:
      CALL_PRE(thread_get_state);
      return;
   case 3605: 
      CALL_PRE(thread_suspend);
      return;
   case 3606:
      CALL_PRE(thread_resume);
      return;
   case 3612: 
      CALL_PRE(thread_info);
      return;
   case 3616: 
      CALL_PRE(thread_policy);
      return;
   case 3617: 
      CALL_PRE(thread_policy_set);
      return;
   default:
      // unknown message to a thread
      VG_(printf)("UNKNOWN thread message [id %d, to %s, reply 0x%x]\n", 
                  mh->msgh_id, name_for_port(mh->msgh_request_port), 
                  mh->msgh_reply_port);
      return;
   }
}


static int is_thread_port(mach_port_t port)
{
   if (port == 0) return False;

   return VG_(lwpid_to_vgtid)(port) != VG_INVALID_THREADID;
}


static int is_task_port(mach_port_t port)
{
   if (port == 0) return False;

   if (port == vg_task_port) return True;

   return (0 == VG_(strncmp)("task-", name_for_port(port), 5));
}


/* ---------------------------------------------------------------------
   mach_msg: base handlers
   ------------------------------------------------------------------ */

PRE(mach_msg)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   mach_msg_option_t option = (mach_msg_option_t)ARG2;
   // mach_msg_size_t send_size = (mach_msg_size_t)ARG3;
   mach_msg_size_t rcv_size = (mach_msg_size_t)ARG4;
   // mach_port_t rcv_name = (mach_port_t)ARG5;
   size_t complex_header_size = 0;

   PRE_REG_READ7(long, "mach_msg", 
                 mach_msg_header_t*,"msg", mach_msg_option_t,"option", 
                 mach_msg_size_t,"send_size", mach_msg_size_t,"rcv_size", 
                 mach_port_t,"rcv_name", mach_msg_timeout_t,"timeout", 
                 mach_port_t,"notify");

   // Assume default POST handler until specified otherwise
   AFTER = NULL;

   // Assume call may block unless specified otherwise
   *flags |= SfMayBlock;

   if (option & MACH_SEND_MSG) {
      // Validate outgoing message header
      PRE_MEM_READ("mach_msg(msg.msgh_bits)", 
                   (Addr)&mh->msgh_bits, sizeof(mh->msgh_bits));
      // msgh_size not required, use parameter instead
      PRE_MEM_READ("mach_msg(msg.msgh_remote_port)", 
                   (Addr)&mh->msgh_remote_port, sizeof(mh->msgh_remote_port));
      PRE_MEM_READ("mach_msg(msg.msgh_local_port)", 
                   (Addr)&mh->msgh_local_port, sizeof(mh->msgh_local_port));
      // msgh_reserved not required
      PRE_MEM_READ("mach_msg(msg.msgh_id)", 
                   (Addr)&mh->msgh_id, sizeof(mh->msgh_id));
      
      if (mh->msgh_bits & MACH_MSGH_BITS_COMPLEX) {
         // Validate typed message data and handle memory map changes.
         complex_header_size = export_complex_message(tid, mh);
      }

      // GrP fixme handle sender-specified message trailer
      // (but is this only for too-secure processes?)
      // JRS 11 Nov 2014: this assertion is OK for <= 10.9 but fails on 10.10
#     if DARWIN_VERS >= DARWIN_10_10
      if (mh->msgh_bits & MACH_SEND_TRAILER) {
         log_decaying("UNKNOWN mach_msg unhandled MACH_SEND_TRAILER option");
      }
#     else
      vg_assert(! (mh->msgh_bits & MACH_SEND_TRAILER));
#     endif

      MACH_REMOTE = mh->msgh_remote_port;
      MACH_MSGH_ID = mh->msgh_id;
   }

   if (option & MACH_RCV_MSG) {
      // Pre-validate receive buffer
      PRE_MEM_WRITE("mach_msg(receive buffer)", (Addr)mh, rcv_size);
   }

   // Call a PRE handler. The PRE handler may set an AFTER handler.

   if (!(option & MACH_SEND_MSG)) {
      // no message sent, receive only
      CALL_PRE(mach_msg_receive);
      return;
   }
   else if (mh->msgh_request_port == vg_host_port) {
      // message sent to mach_host_self()
      CALL_PRE(mach_msg_host);
      return;
   }
   else if (is_task_port(mh->msgh_request_port)) {
      // message sent to a task
      CALL_PRE(mach_msg_task);
      return;
   }
   else if (mh->msgh_request_port == vg_bootstrap_port) {
      // message sent to bootstrap port
      CALL_PRE(mach_msg_bootstrap);
      return;
   }
   else if (is_thread_port(mh->msgh_request_port)) {
      // message sent to one of this process's threads
      CALL_PRE(mach_msg_thread);
      return;
   }
   else {
      // this is an attempt to optimize mapping sync
      // but there are always some cases hard to find 
#if 0
      Bool do_mapping_update = False;
      // sorted by msgh_id, we suppose that msgh_id are different for each service,
      // which is obviously not true...
      switch (mh->msgh_id) {
         // com.apple.windowserver.active
         case 29008: // this one opens a port type 'a'

         // com.apple.windowserver.active 'a' port
         case 29000:
         case 29822:
         case 29820: // adds a vm mapping
         case 29809: // contains a ool mem
         case 29800: // opens a port type 'b'
         case 29873:
         case 29876: // adds a vm mapping

         // com.apple.windowserver.active 'b' port
         case 29624:
         case 29625:
         case 29506:
         case 29504:
         case 29509:
         case 29315:
         case 29236:
         case 29473:
         case 29268:
         case 29237: // contains a ool mem
         case 29360:
         case 29301:
         case 29287:
         case 29568:
         case 29570: // contains a ool mem
         case 29211:
         case 29569: // contains a ool mem
         case 29374:
         case 29246:
         case 29239:
         case 29272:
            if (mh->msgh_id == 29820 ||
               mh->msgh_id == 29876)
               do_mapping_update = True;

            PRINT("com.apple.windowserver.active service mach_msg [id %d, to %s, reply 0x%x]",
               mh->msgh_id, name_for_port(mh->msgh_request_port),
               mh->msgh_reply_port);
            break;

         // com.apple.FontServer
         case 13024:
            PRINT("com.apple.FontServerservice mach_msg [id %d, to %s, reply 0x%x]",
               mh->msgh_id, name_for_port(mh->msgh_request_port),
               mh->msgh_reply_port);
            break;

         // com.apple.system.notification_center
         case 78945698:
         case 78945701:
         case 78945695: // contains a ool mem
         case 78945694:
         case 78945700:
            if (mh->msgh_id == 78945695)
               do_mapping_update = False;
            PRINT("com.apple.system.notification_center mach_msg [id %d, to %s, reply 0x%x]",
               mh->msgh_id, name_for_port(mh->msgh_request_port),
               mh->msgh_reply_port);
            break;

         // com.apple.CoreServices.coreservicesd
         case 10000:
         case 10019:
         case 10002: // adds vm mappings
         case 10003: // adds vm mappings
         case 14007:
         case 13000:
         case 13001:
         case 13011:
         case 13016: // contains a ool mem
            if (mh->msgh_id == 10002|| 
                mh->msgh_id == 10003)
               do_mapping_update = True;
            PRINT("com.apple.CoreServices.coreservicesd mach_msg [id %d, to %s, reply 0x%x]",
               mh->msgh_id, name_for_port(mh->msgh_request_port),
               mh->msgh_reply_port);
            break;

         // com.apple.system.logger
         case 118:
            PRINT("com.apple.system.logger mach_msg [id %d, to %s, reply 0x%x]",
               mh->msgh_id, name_for_port(mh->msgh_request_port),
               mh->msgh_reply_port);
            break;

         // com.apple.coreservices.launchservicesd, and others
         case 1999646836: // might adds vm mapping
            if (mh->msgh_id == 1999646836)
               do_mapping_update = True;
            PRINT("om.apple.coreservices.launchservicesd mach_msg [id %d, to %s, reply 0x%x]",
               mh->msgh_id, name_for_port(mh->msgh_request_port),
               mh->msgh_reply_port);
            break;

         // com.apple.ocspd
         case 33012:
            PRINT("com.apple.ocspd mach_msg [id %d, to %s, reply 0x%x]",
               mh->msgh_id, name_for_port(mh->msgh_request_port),
               mh->msgh_reply_port);

         default:
            // arbitrary message to arbitrary port
            do_mapping_update = True;
            PRINT("UNHANDLED mach_msg [id %d, to %s, reply 0x%x]",
               mh->msgh_id, name_for_port(mh->msgh_request_port),
               mh->msgh_reply_port);
      }

      // this is an optimization, don't check mapping on known mach_msg
      if (do_mapping_update)
         AFTER = POST_FN(mach_msg_unhandled);
      else
         AFTER = POST_FN(mach_msg_unhandled_check);
#else
      AFTER = POST_FN(mach_msg_unhandled);
#endif

      // Assume the entire message body may be read.
      // GrP fixme generates false positives for unknown protocols
      /*
      PRE_MEM_READ("mach_msg(payload)", 
                   (Addr)((char*)mh + sizeof(mach_msg_header_t) + complex_header_size), 
                   send_size - sizeof(mach_msg_header_t) - complex_header_size);
      */
      return;
   }
}

POST(mach_msg)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   mach_msg_option_t option = (mach_msg_option_t)ARG2;

   if (option & MACH_RCV_MSG) {
      if (RES != 0) {
         // error during send or receive
         // GrP fixme need to clean up port rights?
      } else {
         mach_msg_trailer_t *mt = 
             (mach_msg_trailer_t *)((Addr)mh + round_msg(mh->msgh_size));
           
         // Assume the entire received message and trailer is initialized
         // GrP fixme would being more specific catch any bugs?
         POST_MEM_WRITE((Addr)mh, 
                        round_msg(mh->msgh_size) + mt->msgh_trailer_size);
         
         if (mh->msgh_bits & MACH_MSGH_BITS_COMPLEX) {
             // Update memory map for out-of-line message data
             import_complex_message(tid, mh);
         }
      }
   }
   
   // Call handler chosen by PRE(mach_msg)
   if (AFTER) {
      (*AFTER)(tid, arrghs, status);
   }
}


POST(mach_msg_unhandled)
{
   mach_msg_header_t *mh = (mach_msg_header_t *)ARG1;
   ML_(sync_mappings)("after", "mach_msg_receive-UNHANDLED", mh->msgh_id);
}

POST(mach_msg_unhandled_check)
{
   if (ML_(sync_mappings)("after", "mach_msg_receive (unhandled_check)", 0))
      PRINT("mach_msg_unhandled_check tid:%d missed mapping change()", tid);
}


/* ---------------------------------------------------------------------
   other Mach traps
   ------------------------------------------------------------------ */

PRE(mach_reply_port)
{
   PRINT("mach_reply_port()");
}

POST(mach_reply_port)
{
   record_named_port(tid, RES, MACH_PORT_RIGHT_RECEIVE, "reply-%p");
   PRINT("reply port %s", name_for_port(RES));
}


PRE(mach_thread_self)
{
   PRINT("mach_thread_self()");
}

POST(mach_thread_self)
{
   record_named_port(tid, RES, MACH_PORT_RIGHT_SEND, "thread-%p");
   PRINT("thread %#lx", RES);
}


PRE(mach_host_self)
{
   PRINT("mach_host_self()");
}

POST(mach_host_self)
{
   vg_host_port = RES;
   record_named_port(tid, RES, MACH_PORT_RIGHT_SEND, "mach_host_self()");
   PRINT("host %#lx", RES);
}


PRE(mach_task_self)
{
   PRINT("mach_task_self()");
}

POST(mach_task_self)
{
   vg_task_port = RES;
   record_named_port(tid, RES, MACH_PORT_RIGHT_SEND, "mach_task_self()");
   PRINT("task %#lx", RES);
}


PRE(syscall_thread_switch)
{
   PRINT("syscall_thread_switch(%s, %ld, %ld)",
      name_for_port(ARG1), SARG2, SARG3);
   PRE_REG_READ3(long, "syscall_thread_switch", 
                 mach_port_t,"thread", int,"option", natural_t,"timeout");

   *flags |= SfMayBlock;
}


PRE(semaphore_signal)
{
   PRINT("semaphore_signal(%s)", name_for_port(ARG1));
   PRE_REG_READ1(long, "semaphore_signal", semaphore_t,"semaphore");
}


PRE(semaphore_signal_all)
{
   PRINT("semaphore_signal_all(%s)", name_for_port(ARG1));
   PRE_REG_READ1(long, "semaphore_signal_all", semaphore_t,"semaphore");
}


PRE(semaphore_signal_thread)
{
   PRINT("semaphore_signal_thread(%s, %s)", 
         name_for_port(ARG1), name_for_port(ARG2));
   PRE_REG_READ2(long, "semaphore_signal_thread", 
                 semaphore_t,"semaphore", mach_port_t,"thread");
}


PRE(semaphore_wait)
{
   PRINT("semaphore_wait(%s)", name_for_port(ARG1));
   PRE_REG_READ1(long, "semaphore_signal", semaphore_t,"semaphore");

   *flags |= SfMayBlock;
}


PRE(semaphore_wait_signal)
{
   PRINT("semaphore_wait_signal(%s, %s)", 
         name_for_port(ARG1), name_for_port(ARG2));
   PRE_REG_READ2(long, "semaphore_wait_signal", 
                 semaphore_t,"wait_semaphore", 
                 semaphore_t,"signal_semaphore");

   *flags |= SfMayBlock;
}


PRE(semaphore_timedwait)
{
   PRINT("semaphore_timedwait(%s, %g seconds)",
         name_for_port(ARG1), ARG2+ARG3/1000000000.0);
   PRE_REG_READ3(long, "semaphore_wait_signal", 
                 semaphore_t,"semaphore", 
                 int,"wait_time_hi", 
                 int,"wait_time_lo");
   
   *flags |= SfMayBlock;
}


PRE(semaphore_timedwait_signal)
{
   PRINT("semaphore_wait_signal(wait %s, signal %s, %g seconds)",
         name_for_port(ARG1), name_for_port(ARG2), ARG3+ARG4/1000000000.0);
   PRE_REG_READ4(long, "semaphore_wait_signal", 
                 semaphore_t,"wait_semaphore", 
                 semaphore_t,"signal_semaphore", 
                 int,"wait_time_hi", 
                 int,"wait_time_lo");

   *flags |= SfMayBlock;
}


PRE(__semwait_signal)
{
   /* 10.5 args: int cond_sem, int mutex_sem,
                 int timeout, int relative,
                 time_t tv_sec, time_t tv_nsec */
   PRINT("__semwait_signal(wait %s, signal %s, %ld, %ld, %lds:%ldns)", 
         name_for_port(ARG1), name_for_port(ARG2), SARG3, SARG4, SARG5, SARG6);
   PRE_REG_READ6(long, "__semwait_signal", 
                 int,"cond_sem", int,"mutex_sem",
                 int,"timeout", int,"relative", 
                 vki_time_t,"tv_sec", int,"tv_nsec");

   *flags |= SfMayBlock;
}
// GrP provided this alternative version for 10.6, but NjN
// reckons the 10.5 is is still correct for 10.6.  So, retaining
// Greg's version as a comment just in case we need it later.
//PRE(__old_semwait_signal)
//{
//   /* 10.5 args: int cond_sem, int mutex_sem,
//                 int timeout, int relative,
//                 const timespec *ts */
//   PRINT("__old_semwait_signal(wait %s, signal %s, %ld, %ld, %#lx)",
//         name_for_port(ARG1), name_for_port(ARG2), ARG3, ARG4, ARG5);
//   PRE_REG_READ5(int, "__old_semwait_signal", 
//                 int,cond_sem, int,mutex_sem,
//                 int,timeout, int,relative, 
//                 const struct vki_timespec *,ts);
//   
//   if (ARG5) PRE_MEM_READ ("__old_semwait_signal(ts)",
//                           ARG5, sizeof(struct vki_timespec));
//   
//   *flags |= SfMayBlock;
//}


PRE(task_for_pid)
{
   PRINT("task_for_pid(%s, %ld, %#lx)", name_for_port(ARG1), SARG2, ARG3);
   PRE_REG_READ3(long, "task_for_pid", 
                 mach_port_t,"target", 
                 vki_pid_t, "pid", mach_port_t *,"task");
   PRE_MEM_WRITE("task_for_pid(task)", ARG3, sizeof(mach_port_t));
}

POST(task_for_pid)
{
   mach_port_t task;

   POST_MEM_WRITE(ARG3, sizeof(mach_port_t));

   task = *(mach_port_t *)ARG3;
   record_named_port(tid, task, MACH_PORT_RIGHT_SEND, "task-%p");
   PRINT("task 0x%x", task);
}


PRE(pid_for_task)
{
   PRINT("pid_for_task(%s, %#lx)", name_for_port(ARG1), ARG2);
   PRE_REG_READ2(long, "task_for_pid", mach_port_t,"task", vki_pid_t *,"pid");
   PRE_MEM_WRITE("task_for_pid(pid)", ARG2, sizeof(vki_pid_t));
}

POST(pid_for_task)
{
   vki_pid_t pid;

   POST_MEM_WRITE(ARG2, sizeof(vki_pid_t));

   pid = *(vki_pid_t *)ARG2;
   PRINT("pid %u", pid);
}


PRE(mach_timebase_info)
{
   PRINT("mach_timebase_info(%#lx)", ARG1);
   PRE_REG_READ1(long, "mach_timebase_info", void *,"info");
   PRE_MEM_WRITE("mach_timebase_info(info)", ARG1, sizeof(struct vki_mach_timebase_info));
}

POST(mach_timebase_info)
{
   POST_MEM_WRITE(ARG1, sizeof(struct vki_mach_timebase_info));
}


PRE(mach_wait_until)
{
#if VG_WORDSIZE == 8
   PRINT("mach_wait_until(%lu)", ARG1);
   PRE_REG_READ1(long, "mach_wait_until", 
                 unsigned long long,"deadline");
#else
   PRINT("mach_wait_until(%llu)", LOHI64(ARG1, ARG2));
   PRE_REG_READ2(long, "mach_wait_until", 
                 int,"deadline_hi", int,"deadline_lo");
#endif   
   *flags |= SfMayBlock;
}


PRE(mk_timer_create)
{
   PRINT("mk_timer_create()");
   PRE_REG_READ0(long, "mk_timer_create");
}

POST(mk_timer_create)
{
   record_named_port(tid, RES, MACH_PORT_RIGHT_SEND, "mk_timer-%p");
}


PRE(mk_timer_destroy)
{
   PRINT("mk_timer_destroy(%s)", name_for_port(ARG1));
   PRE_REG_READ1(long, "mk_timer_destroy", mach_port_t,"name");

   // Must block to prevent race (other thread allocates and 
   // notifies after we deallocate but before we notify)
   *flags &= ~SfMayBlock;
}

POST(mk_timer_destroy)
{
   // Must have cleared SfMayBlock in PRE to prevent race
   record_port_destroy(ARG1);
}


PRE(mk_timer_arm)
{
#if VG_WORDSIZE == 8
   PRINT("mk_timer_arm(%s, %lu)", name_for_port(ARG1), ARG2);
   PRE_REG_READ2(long, "mk_timer_arm", mach_port_t,"name", 
                 unsigned long,"expire_time");
#else
   PRINT("mk_timer_arm(%s, %llu)", name_for_port(ARG1), LOHI64(ARG2, ARG3));
   PRE_REG_READ3(long, "mk_timer_arm", mach_port_t,"name", 
                 int,"expire_time_hi", int,"expire_time_lo");
#endif
}


PRE(mk_timer_cancel)
{
   PRINT("mk_timer_cancel(%s, %#lx)", name_for_port(ARG1), ARG2);
   PRE_REG_READ2(long, "mk_timer_cancel", 
                 mach_port_t,"name", Addr,"result_time");
   if (ARG2) {
      PRE_MEM_WRITE("mk_timer_cancel(result_time)", ARG2,sizeof(vki_uint64_t));
   }
}

POST(mk_timer_cancel)
{
   if (ARG2) {
      POST_MEM_WRITE(ARG2, sizeof(vki_uint64_t));
   }
}


PRE(iokit_user_client_trap)
{
   PRINT("iokit_user_client_trap(%s, %ld, %lx, %lx, %lx, %lx, %lx, %lx)",
         name_for_port(ARG1), ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8);
   PRE_REG_READ8(kern_return_t, "iokit_user_client_trap", 
                 mach_port_t,connect, unsigned int,index, 
                 uintptr_t,p1, uintptr_t,p2, uintptr_t,p3, 
                 uintptr_t,p4, uintptr_t,p5, uintptr_t,p6);

   // can't do anything else with this in general
   // might be able to use connect+index to choose something sometimes
}

POST(iokit_user_client_trap)
{
   ML_(sync_mappings)("after", "iokit_user_client_trap", ARG2);
}


PRE(swtch)
{
   PRINT("swtch ( )");
   PRE_REG_READ0(long, "swtch");

   *flags |= SfMayBlock;
}


PRE(swtch_pri)
{
   PRINT("swtch_pri ( %ld )", SARG1);
   PRE_REG_READ1(long, "swtch_pri", int,"pri");

   *flags |= SfMayBlock;
}


PRE(FAKE_SIGRETURN)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */
   /* This handles the fake signal-return system call created by
      sigframe-x86-darwin.c. */
   /* See also comments just below on PRE(sigreturn). */

   PRINT("FAKE_SIGRETURN ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Remove the signal frame from this thread's (guest) stack,
      in the process restoring the pre-signal guest state. */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}


PRE(sigreturn)
{
   /* This is the "real" sigreturn.  But because we construct all the
      signal frames ourselves (of course, in m_sigframe), this cannot
      happen as a result of normal signal delivery.  I think it
      happens only when doing siglongjmp, in which case Darwin's Libc
      appears to use it for two different purposes: to mess with the
      per-thread sigaltstack flags (as per arg 2), or to restore the
      thread's state from a ucontext* (as per arg 1). */

   PRINT("sigreturn ( uctx=%#lx, infostyle=%#lx )", ARG1, ARG2);

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   if (ARG2 == VKI_UC_SET_ALT_STACK) {
      /* This is confusing .. the darwin kernel sources imply there is
         a per-thread on-altstack/not-on-altstack flag, which is set
         by this flag.  Just ignore it and claim success for the time
         being. */
      VG_(debugLog)(1, "syswrap-darwin",
                       "WARNING: Ignoring sigreturn( ..., "
                       "UC_SET_ALT_STACK );\n");
      SET_STATUS_Success(0);
      return;
   }
   if (ARG2 == VKI_UC_RESET_ALT_STACK) {
      /* Ditto */
      VG_(debugLog)(1, "syswrap-darwin",
                       "WARNING: Ignoring sigreturn( ..., "
                       "UC_RESET_ALT_STACK );\n");
      SET_STATUS_Success(0);
      return;
   }

   /* Otherwise claim this isn't supported.  (Could be
      catastrophic).

      What do we have to do if we do need to support it?

      1. Change the second argument of VG_(sigframe_destroy) from
         "Bool isRT" to "UInt sysno", so we can pass the syscall
         number, so it can distinguish this case from the
         __NR_DARWIN_FAKE_SIGRETURN case.

      2. In VG_(sigframe_destroy), look at sysno to distinguish the
         cases.  For __NR_DARWIN_FAKE_SIGRETURN, behave as at present.
         For this case, restore the thread's CPU state (or at least
         the integer regs) from the ucontext in ARG1 (and do all the
         other "signal-returns" stuff too).

      3. For (2), how do we know where the ucontext is?  One way is to
         temporarily copy ARG1 into this thread's guest_EBX (or any
         other int reg), and have VG_(sigframe_destroy) read
         guest_EBX.  Why is it ok to trash guest_EBX (or any other int
         reg)?  Because VG_(sigframe_destroy) is just about to
         overwrite all the regs anyway -- since the primary purpose of
         calling it is to restore the register state from the ucontext
         pointed to by ARG1.

      Hey, it's uggerly.  But at least it's documented.
   */
   /* But in the meantime ... */
   VG_(debugLog)(0, "syswrap-darwin",
                    "WARNING: Ignoring sigreturn( uctx=..., 0 );\n");
   VG_(debugLog)(0, "syswrap-darwin",
                    "WARNING: Thread/program/Valgrind "
                    "will likely segfault now.\n");
   VG_(debugLog)(0, "syswrap-darwin",
                    "WARNING: Please file a bug report at "
                    "http://www.valgrind.org.\n");
   SET_STATUS_Failure( VKI_ENOSYS );
}


/* ---------------------------------------------------------------------
   machine-dependent traps
   ------------------------------------------------------------------ */

#if defined(VGA_x86)
static VexGuestX86SegDescr* alloc_zeroed_x86_LDT ( void )
{
   Int nbytes = VEX_GUEST_X86_LDT_NENT * sizeof(VexGuestX86SegDescr);
   return VG_(calloc)("syswrap-darwin.ldt", nbytes, 1);
}
#endif

PRE(thread_fast_set_cthread_self)
{
   PRINT("thread_fast_set_cthread_self ( %#lx )", ARG1);
   PRE_REG_READ1(void, "thread_fast_set_cthread_self", struct pthread_t *, self);

#if defined(VGA_x86)
   // Point the USER_CTHREAD ldt entry (slot 6, reg 0x37) at this pthread
   {
      VexGuestX86SegDescr *ldt;
      ThreadState *tst = VG_(get_ThreadState)(tid);
      ldt = (VexGuestX86SegDescr *)tst->arch.vex.guest_LDT;
      if (!ldt) {
         ldt = alloc_zeroed_x86_LDT();
         tst->arch.vex.guest_LDT = (HWord)ldt;
      }
      VG_(memset)(&ldt[6], 0, sizeof(ldt[6]));
      ldt[6].LdtEnt.Bits.LimitLow = 1;
      ldt[6].LdtEnt.Bits.LimitHi = 0;
      ldt[6].LdtEnt.Bits.BaseLow = ARG1 & 0xffff;
      ldt[6].LdtEnt.Bits.BaseMid = (ARG1 >> 16) & 0xff;
      ldt[6].LdtEnt.Bits.BaseHi = (ARG1 >> 24) & 0xff;
      ldt[6].LdtEnt.Bits.Pres = 1; // ACC_P
      ldt[6].LdtEnt.Bits.Dpl = 3; // ACC_PL_U
      ldt[6].LdtEnt.Bits.Type = 0x12; // ACC_DATA_W
      ldt[6].LdtEnt.Bits.Granularity = 1;  // SZ_G
      ldt[6].LdtEnt.Bits.Default_Big = 1;  // SZ_32
      
      tst->os_state.pthread = ARG1;
      tst->arch.vex.guest_GS = 0x37;

      // What we would like to do is:
      //   SET_STATUS_Success(0x37);
      // but that doesn't work, because this is a MDEP-class syscall,
      // and SET_STATUS_Success creates a UNIX-class syscall result.
      // Hence we have to laboriously construct the full SysRes "by hand"
      // and use that to set the syscall return status.
      SET_STATUS_from_SysRes(
         VG_(mk_SysRes_x86_darwin)(
            VG_DARWIN_SYSNO_CLASS(__NR_thread_fast_set_cthread_self),
            False, 0, 0x37
         )
      );
   }

#elif defined(VGA_amd64)
   // GrP fixme bigger hack than x86
   {
      ThreadState *tst = VG_(get_ThreadState)(tid);
      tst->os_state.pthread = ARG1;
      tst->arch.vex.guest_GS_CONST = ARG1;
      // SET_STATUS_Success(0x60);
      // see comments on x86 case just above
      SET_STATUS_from_SysRes(
         VG_(mk_SysRes_amd64_darwin)(
            VG_DARWIN_SYSNO_CLASS(__NR_thread_fast_set_cthread_self),
            False, 0, 0x60
         )
      );
   }

#else
#error unknown architecture
#endif
}


/* ---------------------------------------------------------------------
   Added for OSX 10.6 (Snow Leopard)
   ------------------------------------------------------------------ */

#if DARWIN_VERS >= DARWIN_10_6

PRE(psynch_mutexwait)
{
   PRINT("psynch_mutexwait(BOGUS)");
   *flags |= SfMayBlock;
}
POST(psynch_mutexwait)
{
}

PRE(psynch_mutexdrop)
{
   PRINT("psynch_mutexdrop(BOGUS)");
   *flags |= SfMayBlock;
}
POST(psynch_mutexdrop)
{
}

PRE(psynch_cvbroad)
{
   PRINT("psynch_cvbroad(BOGUS)");
}
POST(psynch_cvbroad)
{
}

PRE(psynch_cvsignal)
{
   PRINT("psynch_cvsignal(BOGUS)");
}
POST(psynch_cvsignal)
{
}

PRE(psynch_cvwait)
{
   PRINT("psynch_cvwait(BOGUS)");
   *flags |= SfMayBlock;
}
POST(psynch_cvwait)
{
}

PRE(psynch_rw_rdlock)
{
   PRINT("psynch_rw_rdlock(BOGUS)");
   *flags |= SfMayBlock;
}
POST(psynch_rw_rdlock)
{
}

PRE(psynch_rw_wrlock)
{
   PRINT("psynch_rw_wrlock(BOGUS)");
   *flags |= SfMayBlock;
}
POST(psynch_rw_wrlock)
{
}

PRE(psynch_rw_unlock)
{
   PRINT("psynch_rw_unlock(BOGUS)");
}
POST(psynch_rw_unlock)
{
}

PRE(__thread_selfid)
{
   PRINT("__thread_selfid ()");
   PRE_REG_READ0(vki_uint64_t, "__thread_selfid");
}

PRE(fsgetpath)
{
#if VG_WORDSIZE == 4
   PRINT("fsgetpath(%#lx, %ld, %#lx {%u,%u}, %llu)", 
         ARG1, ARG2, ARG3,
         ((unsigned int *)ARG3)[0], ((unsigned int *)ARG3)[1],
         LOHI64(ARG4, ARG5));
   PRE_REG_READ5(ssize_t, "fsgetpath", 
                 void*,"buf", size_t,"bufsize", 
                 fsid_t *,"fsid",
                 vki_uint32_t, "objid_low32", vki_uint32_t, "objid_high32");
#else
   PRINT("fsgetpath(%#lx, %ld, %#lx {%u,%u}, %lu)", 
         ARG1, ARG2, ARG3,
         ((unsigned int *)ARG3)[0],
         ((unsigned int *)ARG3)[1], ARG4);
   PRE_REG_READ4(ssize_t, "fsgetpath", 
                 void*,"buf", size_t,"bufsize", 
                 fsid_t *,"fsid", uint64_t,"objid");
#endif
   PRE_MEM_READ("fsgetpath(fsid)", ARG3, sizeof(fsid_t));
   PRE_MEM_WRITE("fsgetpath(buf)", ARG1, ARG2);
}

POST(fsgetpath)
{
   POST_MEM_WRITE(ARG1, RES);
}

PRE(audit_session_self)
{
  PRINT("audit_session_self()");
}
POST(audit_session_self)
{
  record_named_port(tid, RES, MACH_PORT_RIGHT_SEND, "audit-session-%p");
  PRINT("audit-session %#lx", RES);
}

#endif /* DARWIN_VERS >= DARWIN_10_6 */


/* ---------------------------------------------------------------------
   Added for OSX 10.7 (Lion)
   ------------------------------------------------------------------ */

#if DARWIN_VERS >= DARWIN_10_7

PRE(psynch_cvclrprepost)
{
   PRINT("psynch_cvclrprepost(BOGUS)");
   *flags |= SfMayBlock;
}
POST(psynch_cvclrprepost)
{
}

#endif /* DARWIN_VERS >= DARWIN_10_7 */


/* ---------------------------------------------------------------------
   Added for OSX 10.8 (Mountain Lion)
   ------------------------------------------------------------------ */

/* About munge tags, eg munge_wllww.

   Means the syscall takes 5 args.  For a 64 bit process each arg
   occupies one 64-bit value and so the mapping to ARGn macros is
   direct.  For a 32 bit process, this is more complex: 'w' denotes a
   32-bit word and 'l' a 64-bit word.  Hence the wllww denotation
   indicates that, in a 64 bit process, the args are: ARG1 ARG2 ARG3
   ARG4 ARG5, but in a 32 bit process they are: ARG1 ARG3:ARG2
   ARG5:ARG4 ARG6 ARG7.  And we have to laboriously reconstruct them
   in order to get sane values for the arguments in 32-bit
   processes. */

static void munge_wwl(UWord* a1, UWord* a2, ULong* a3,
                      UWord aRG1, UWord aRG2, UWord aRG3, UWord aRG4)
{
#  if defined(VGA_x86)
   *a1 = aRG1; *a2 = aRG2; *a3 = LOHI64(aRG3,aRG4);
#  else
   *a1 = aRG1; *a2 = aRG2; *a3 = aRG3;
#  endif
}

static void munge_wll(UWord* a1, ULong* a2, ULong* a3,
                      UWord aRG1, UWord aRG2, UWord aRG3,
                      UWord aRG4, UWord aRG5)
{
#  if defined(VGA_x86)
   *a1 = aRG1; *a2 = LOHI64(aRG2,aRG3); *a3 = LOHI64(aRG4,aRG5);
#  else
   *a1 = aRG1; *a2 = aRG2; *a3 = aRG3;
#  endif
}

static void munge_wwlw(UWord* a1, UWord* a2, ULong* a3, UWord* a4,
                       UWord aRG1, UWord aRG2, UWord aRG3,
                       UWord aRG4, UWord aRG5)
{
#  if defined(VGA_x86)
   *a1 = aRG1; *a2 = aRG2; *a3 = LOHI64(aRG3,aRG4); *a4 = aRG5;
#  else
   *a1 = aRG1; *a2 = aRG2; *a3 = aRG3; *a4 = aRG4;
#  endif
}

static void munge_wwwl(UWord* a1, UWord* a2, UWord* a3, ULong* a4,
                       UWord aRG1, UWord aRG2, UWord aRG3,
                       UWord aRG4, UWord aRG5)
{
#  if defined(VGA_x86)
   *a1 = aRG1; *a2 = aRG2; *a3 = aRG3; *a4 = LOHI64(aRG4,aRG5);
#  else
   *a1 = aRG1; *a2 = aRG2; *a3 = aRG3; *a4 = aRG4;
#  endif
}

static void munge_wllww(UWord* a1, ULong* a2, ULong* a3, UWord* a4, UWord* a5,
                        UWord aRG1, UWord aRG2, UWord aRG3,
                        UWord aRG4, UWord aRG5, UWord aRG6, UWord aRG7)
{
#  if defined(VGA_x86)
   *a1 = aRG1; *a2 = LOHI64(aRG2,aRG3); *a3 = LOHI64(aRG4,aRG5);
   *a4 = aRG6; *a5 = aRG7;
#  else
   *a1 = aRG1; *a2 = aRG2; *a3 = aRG3; *a4 = aRG4; *a5 = aRG5;
#  endif
}

static void munge_wwllww(UWord* a1, UWord* a2, ULong* a3,
                         ULong* a4, UWord* a5, UWord* a6,
                         UWord aRG1, UWord aRG2, UWord aRG3, UWord aRG4,
                         UWord aRG5, UWord aRG6, UWord aRG7, UWord aRG8)
{
#  if defined(VGA_x86)
   *a1 = aRG1; *a2 = aRG2;
   *a3 = LOHI64(aRG3,aRG4); *a4 = LOHI64(aRG5,aRG6);
   *a5 = aRG7; *a6 = aRG8;
#  else
   *a1 = aRG1; *a2 = aRG2; *a3 = aRG3; *a4 = aRG4; *a5 = aRG5; *a6 = aRG6;
#  endif
}

#if DARWIN_VERS >= DARWIN_10_8

PRE(kernelrpc_mach_vm_allocate_trap)
{
   UWord a1; UWord a2; ULong a3; UWord a4;
   munge_wwlw(&a1, &a2, &a3, &a4, ARG1, ARG2, ARG3, ARG4, ARG5);
   PRINT("kernelrpc_mach_vm_allocate_trap"
         "(target:%s, address:%p, size:%#llx, flags:%#lx)",
         name_for_port(a1), *(void**)a2, a3, a4);
   PRE_MEM_WRITE("kernelrpc_mach_vm_allocate_trap(address)",
                 a2, sizeof(void*));
}
POST(kernelrpc_mach_vm_allocate_trap)
{
   UWord a1; UWord a2; ULong a3; UWord a4;
   munge_wwlw(&a1, &a2, &a3, &a4, ARG1, ARG2, ARG3, ARG4, ARG5);
   PRINT("address:%p size:%#llx", *(void**)a2, a3);
   if (ML_(safe_to_deref)((void*)a2, sizeof(void*))) {
      POST_MEM_WRITE(a2, sizeof(void*));
   }
   if (a1 == mach_task_self()) {
#     if 1
      ML_(sync_mappings)("POST(kernelrpc_mach_vm_allocate_trap)", "??", 0);
#     else
      /* This is nearly right, but not always -- sometimes the mapping
         appears to be r--, for some reason.  Hence resync. */
      ML_(notify_core_and_tool_of_mmap)(
         *(UWord*)a2, a3,
         VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_ANON, -1, 0);
#     endif
   }
}

PRE(kernelrpc_mach_vm_deallocate_trap)
{
   UWord a1; ULong a2; ULong a3;
   munge_wll(&a1, &a2, &a3, ARG1, ARG2, ARG3, ARG4, ARG5);
   PRINT("kernelrpc_mach_vm_deallocate_trap"
         "(target:%#lx, address:%#llx, size:%#llx)", a1, a2, a3);
}
POST(kernelrpc_mach_vm_deallocate_trap)
{
   UWord a1; ULong a2; ULong a3;
   munge_wll(&a1, &a2, &a3, ARG1, ARG2, ARG3, ARG4, ARG5);
   // kernelrpc_mach_vm_deallocate_trap could be call with
   // address ==0 && size == 0,
   // we shall not notify any unmap then
   if (a3)
      ML_(notify_core_and_tool_of_munmap)(a2, a3);
}

PRE(kernelrpc_mach_vm_protect_trap)
{
   UWord a1; ULong a2; ULong a3; UWord a4; UWord a5;
   munge_wllww(&a1, &a2, &a3, &a4, &a5,
               ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
   PRINT("kernelrpc_mach_vm_protect_trap"
         "(task:%#lx, address:%#llx, size:%#llx,"
         " set_maximum:%#lx, new_prot:%#lx)", a1, a2, a3, a4, a5);
}
POST(kernelrpc_mach_vm_protect_trap)
{
   UWord a1; ULong a2; ULong a3; UWord a4; UWord a5;
   munge_wllww(&a1, &a2, &a3, &a4, &a5,
               ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
   if (/*a4 set_maximum == 0 && */a1 == mach_task_self()) {
      ML_(notify_core_and_tool_of_mprotect)((Addr)a2, (SizeT)a3, (Int)a5);
      VG_(di_notify_vm_protect)((Addr)a2, (SizeT)a3, (UInt)a5);
   }
}

PRE(kernelrpc_mach_port_allocate_trap)
{
   // munge_www -- no need to call helper
   PRINT("kernelrpc_mach_port_allocate_trap(task:%#lx, mach_port_right_t:%#lx)",
         ARG1, ARG2);
   PRE_MEM_WRITE("kernelrpc_mach_port_allocate_trap(name)",
                 ARG3, sizeof(mach_port_name_t));
}
POST(kernelrpc_mach_port_allocate_trap)
{
   // munge_www -- no need to call helper
   POST_MEM_WRITE(ARG3, sizeof(mach_port_name_t));
   PRINT(", name:%#x", *(mach_port_name_t*)ARG3);
   record_unnamed_port(tid, *(mach_port_name_t *)ARG3, ARG2);
}

PRE(kernelrpc_mach_port_destroy_trap)
{
   // munge_ww -- no need to call helper
   PRINT("kernelrpc_mach_port_destroy_trap(task:%#lx, name:%#lx)", ARG1, ARG2);
   record_port_destroy(ARG2);
}

PRE(kernelrpc_mach_port_deallocate_trap)
{
   // munge_ww -- no need to call helper
   PRINT("kernelrpc_mach_port_deallocate_trap(task:%#lx, name:%#lx ) FIXME",
         ARG1, ARG2);
}
POST(kernelrpc_mach_port_deallocate_trap)
{
   // munge_ww -- no need to call helper
}

PRE(kernelrpc_mach_port_mod_refs_trap)
{
   // munge_wwww -- no need to call helper
   PRINT("kernelrpc_mach_port_mod_refs_trap"
         "(task:%#lx, name:%#lx, right:%#lx refs:%#lx) FIXME",
         ARG1, ARG2, ARG3, ARG4);
}

PRE(kernelrpc_mach_port_move_member_trap)
{
   // munge_www -- no need to call helper
   PRINT("kernelrpc_mach_port_move_member_trap"
         "(task:%#lx, name:%#lx, after:%#lx ) FIXME",
         ARG1, ARG2, ARG3);
}

PRE(kernelrpc_mach_port_insert_right_trap)
{
   //munge_wwww -- no need to call helper
   PRINT("kernelrpc_mach_port_insert_right_trap(FIXME)"
         "(%lx,%lx,%lx,%lx)", ARG1, ARG2, ARG3, ARG4);
}

PRE(kernelrpc_mach_port_insert_member_trap)
{
   // munge_www -- no need to call helper
   PRINT("kernelrpc_mach_port_insert_member_trap(FIXME)"
         "(%lx,%lx,%lx)", ARG1, ARG2, ARG3);
}

PRE(kernelrpc_mach_port_extract_member_trap)
{
   // munge_www -- no need to call helper
   PRINT("kernelrpc_mach_port_extract_member_trap(FIXME)"
         "(%lx,%lx,%lx)", ARG1, ARG2, ARG3);
}

PRE(iopolicysys)
{
   // munge_???
   PRINT("iopolicysys(FIXME)(0x%lx, 0x%lx, 0x%lx)", ARG1, ARG2, ARG3);
   /* mem effects unknown */
}
POST(iopolicysys)
{
   // munge_???
}

PRE(process_policy)
{
   // munge_???
   PRINT("process_policy(FIXME)("
         "scope:0x%lx, action:0x%lx, policy:0x%lx, policy_subtype:0x%lx,"
         " attr:%lx, target_pid:%lx, target_threadid:%lx)",
         ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
   /* mem effects unknown */
}
POST(process_policy)
{
   // munge_???
}

#endif /* DARWIN_VERS >= DARWIN_10_8 */


/* ---------------------------------------------------------------------
   Added for OSX 10.9 (Mavericks)
   ------------------------------------------------------------------ */

#if DARWIN_VERS >= DARWIN_10_9

PRE(kernelrpc_mach_vm_map_trap)
{
   UWord a1; UWord a2; ULong a3; ULong a4; UWord a5; UWord a6;
   munge_wwllww(&a1, &a2, &a3, &a4, &a5, &a6,
                ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8);
   PRINT("kernelrpc_mach_vm_map_trap"
         "(target:%#lx, address:%p, size:%#llx,"
         " mask:%#llx, flags:%#lx, cur_prot:%#lx)",
         a1, *(void**)a2, a3, a4, a5, a6);
   PRE_MEM_WRITE("kernelrpc_mach_vm_map_trap(address)", a2, sizeof(void*));
}
POST(kernelrpc_mach_vm_map_trap)
{
   UWord a1; UWord a2; ULong a3; ULong a4; UWord a5; UWord a6;
   munge_wwllww(&a1, &a2, &a3, &a4, &a5, &a6,
                ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8);
   PRINT("-> address:%p", *(void**)a2);
   if (ML_(safe_to_deref)((void*)a2, sizeof(void*))) {
      POST_MEM_WRITE(a2, sizeof(void*));
   }
   ML_(notify_core_and_tool_of_mmap)(
      *(mach_vm_address_t*)a2, a3,
      VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_ANON, -1, 0);
   // ML_(sync_mappings)("after", "kernelrpc_mach_vm_map_trap", 0);
}

PRE(kernelrpc_mach_port_construct_trap)
{
   UWord a1; UWord a2; ULong a3; UWord a4;
   munge_wwlw(&a1, &a2, &a3, &a4, ARG1, ARG2, ARG3, ARG4, ARG5);
   PRINT("kernelrpc_mach_port_construct_trap"
         "(target: %s, options: %#lx, content: %llx, name: %p)",
         name_for_port(a1), a2, a3, *(mach_port_name_t**)a4);
   PRE_MEM_WRITE("kernelrpc_mach_port_construct_trap(name)", a4,
                 sizeof(mach_port_name_t*));
}
POST(kernelrpc_mach_port_construct_trap)
{
   UWord a1; UWord a2; ULong a3; UWord a4;
   munge_wwlw(&a1, &a2, &a3, &a4, ARG1, ARG2, ARG3, ARG4, ARG5);
   PRINT("-> name:%p", *(mach_port_name_t**)a4);
   if (ML_(safe_to_deref)((mach_port_name_t*)a4, sizeof(mach_port_name_t*))) {
      POST_MEM_WRITE(a4, sizeof(mach_port_name_t*));
   }
}

PRE(kernelrpc_mach_port_destruct_trap)
{
   UWord a1; UWord a2; UWord a3; ULong a4;
   munge_wwwl(&a1, &a2, &a3, &a4, ARG1, ARG2, ARG3, ARG4, ARG5);
   PRINT("kernelrpc_mach_port_destruct_trap(FIXME)"
         "(%lx,%lx,%lx,%llx)", a1, a2, a3, a4);
}

PRE(kernelrpc_mach_port_guard_trap)
{
   UWord a1; UWord a2; ULong a3; UWord a4;
   munge_wwlw(&a1, &a2, &a3, &a4, ARG1, ARG2, ARG3, ARG4, ARG5);
   PRINT("kernelrpc_mach_port_guard_trap(FIXME)"
         "(%lx,%lx,%llx,%lx)", a1, a2, a3, a4);
}

PRE(kernelrpc_mach_port_unguard_trap)
{
   // munge_wwl
   UWord a1; UWord a2; ULong a3;
   munge_wwl(&a1, &a2, &a3, ARG1, ARG2, ARG3, ARG4);
   PRINT("kernelrpc_mach_port_unguard_trap(FIXME)"
         "(%lx,%lx,%llx)", a1, a2, a3);
}

#endif /* DARWIN_VERS >= DARWIN_10_9 */


/* ---------------------------------------------------------------------
   Added for OSX 10.10 (Yosemite)
   ------------------------------------------------------------------ */

#if DARWIN_VERS >= DARWIN_10_10

PRE(necp_match_policy)
{
   // int necp_match_policy(uint8_t *parameters, size_t parameters_size,
   //                       struct necp_aggregate_result *returned_result)
   PRINT("necp_match_policy(FIXME)(%lx,%lu, %lx)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "necp_match_policy", uint8_t*, parameters,
                 size_t, parameters_size, struct necp_aggregate_result*,
                 returned_result);
   PRE_MEM_READ("necp_match_policy(returned_result)", ARG1, ARG2);
}
POST(necp_match_policy)
{
   POST_MEM_WRITE(ARG3, sizeof(struct vki_necp_aggregate_result));
}

PRE(sysctlbyname)
{
   UWord name    = ARG1;
   UWord namelen = ARG2;
   UWord oldp    = ARG3;
   UWord oldlenp = ARG4;
   UWord newp    = ARG5;
   UWord newlen  = ARG6;  // FIXME: or newlenp ??

   PRINT( "sysctlbyname ( %#lx,%lu, %#lx,%#lx, %#lx,%lu )", 
          name, namelen, oldp, oldlenp, newp, newlen );

   PRE_REG_READ6(int, "sysctlbyname", char*, name, size_t, namelen,
                 void*, oldp, vki_size_t *, oldlenp, 
                 void*, newp, vki_size_t *, newlenp);  //  <---<<

   // reads name[0..namelen-1]
   PRE_MEM_READ("sysctlbyname(name)", name, namelen);

   if (VG_(clo_trace_syscalls)) {
      UInt i;
      const HChar* t_name = (const HChar*)name;
      VG_(printf)(" name: ");
      for (i = 0; i < namelen; i++) {
         VG_(printf)("%c", t_name[i]);
      }
      VG_(printf)(" ");
   }
  
   Bool is_kern_dot_userstack
      = False;

   common_PRE_sysctl( /*IMPLICIT ARGS*/tid,status,flags,/*!IMPLICIT_ARGS*/
                      is_kern_dot_userstack, oldp, oldlenp, newp, newlen );
}
POST(sysctlbyname)
{
   UWord oldp    = ARG3;
   UWord oldlenp = ARG4;

   if (SUCCESS || ERR == VKI_ENOMEM) {
      // sysctl can write truncated data and return VKI_ENOMEM
      if (oldlenp) {
         POST_MEM_WRITE(oldlenp, sizeof(size_t));
      }
      if (oldp && oldlenp) {
         POST_MEM_WRITE(oldp, *(size_t*)oldlenp);
      }
   }
}

PRE(getattrlistbulk)
{
   // int getattrlistbulk(int dirfd, struct attrlist *alist,
   //                     void *attributeBuffer, size_t bufferSize,
   //                     uint64_t options);
   // Presumably the last arg is value-pair in the 32 bit case.
   PRINT("getattrlistbulk(FIXME)(%ld, %lx, %lx,%lu, %lu)",
         SARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(int, "getattrlistbulk", int, dirfd, void*, list,
                 void*, attributeBuffer, size_t, bufferSize,
                 uint32_t, options_lo32);
   PRE_MEM_READ("getattrlistbulk(alist)", ARG2, sizeof(struct vki_attrlist));
   PRE_MEM_WRITE("getattrlistbulk(attributeBuffer)", ARG3, ARG4);
}
POST(getattrlistbulk)
{
   // FIXME: this isn't right.  It seems as if what is returned is a
   // set of variable-length records -- see complication in
   // POST(getattrlist).  For now, just paint the entire result buffer
   // as defined.  Sigh.
   vg_assert(SUCCESS);
   if (ARG3 && /* "at least one output element was written" */RES > 0)
      POST_MEM_WRITE(ARG3, ARG4);
}

PRE(faccessat)
{
    PRINT("faccessat(FIXME)(fd:%ld, path:%#lx(%s), amode:%#lx, flag:%#lx)",
        ARG1, ARG2, (HChar*)ARG2, ARG3, ARG4);
    PRE_REG_READ4(int, "faccessat",
                  int, fd, user_addr_t, path, int, amode, int, flag);
}

PRE(fstatat64)
{
    PRINT("fstatat64(FIXME)(fd:%ld, path:%#lx(%s), ub:%#lx, flag:%#lx)",
        ARG1, ARG2, (HChar*)ARG2, ARG3, ARG4);
    PRE_REG_READ4(int, "fstatat64",
                  int, fd, user_addr_t, path, user_addr_t, ub, int, flag);
}

PRE(readlinkat)
{
    Word  saved = SYSNO;
    
    PRINT("readlinkat ( %ld, %#lx(%s), %#lx, %ld )",
          SARG1, ARG2, (HChar*)ARG2, ARG3, SARG4);
    PRE_REG_READ4(long, "readlinkat",
                  int, dfd, const char *, path, char *, buf, int, bufsiz);
    PRE_MEM_RASCIIZ( "readlinkat(path)", ARG2 );
    PRE_MEM_WRITE( "readlinkat(buf)", ARG3,ARG4 );
    
    /*
     * Refer to coregrind/m_syswrap/syswrap-linux.c
     */
    {
        /* Normal case */
        SET_STATUS_from_SysRes( VG_(do_syscall4)(saved, ARG1, ARG2, ARG3, ARG4));
    }
    
    if (SUCCESS && RES > 0)
        POST_MEM_WRITE( ARG3, RES );
}

PRE(bsdthread_ctl)
{
   // int bsdthread_ctl(user_addr_t cmd, user_addr_t arg1, 
   //                   user_addr_t arg2, user_addr_t arg3)
   PRINT("bsdthread_ctl(FIXME)(%lx,%lx,%lx,%lx)", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "bsdthreadctl",
                 void*, cmd, void*, arg1, void*, arg2, void*, arg3);
}

PRE(csrctl)
{
   switch (ARG1) {
   case VKI_CSR_CHECK:
     PRINT("csrctl(op:CSR_CHECK, useraddr:%#lx, usersize:%#lx)", ARG2, ARG3);
   PRE_REG_READ3(int, "csrctl",
                 uint32_t, op, user_addr_t, useraddr, user_addr_t, usersize);
     PRE_MEM_READ( "csrctl(useraddr)", ARG2, ARG3 );
     break;

   case VKI_CSR_GET_ACTIVE_CONFIG:
      PRINT("csrctl(op:CSR_GET_ACTIVE_CONFIG, useraddr:%#lx, usersize:%#lx)", ARG2, ARG3);
      PRE_REG_READ3(int, "csrctl",
                    uint32_t, op, user_addr_t, useraddr, user_addr_t, usersize);
      PRE_MEM_WRITE( "csrctl(useraddr)", ARG2, ARG3 );
      break;

   default:
      PRINT("csrctl(op:%ld [??], useraddr:%#lx, usersize:%#lx)", ARG1, ARG2, ARG3);
      log_decaying("UNKNOWN csrctl %ld!", ARG1);
      break;
   }
}
POST(csrctl)
{
   vg_assert(SUCCESS);
   switch (ARG1) {
   case VKI_CSR_GET_ACTIVE_CONFIG:
      POST_MEM_WRITE( ARG2, ARG3 );
      break;

   default:
      break;
   }
}

PRE(guarded_open_dprotected_np)
{
    PRINT("guarded_open_dprotected_np("
        "path:%#lx(%s), guard:%#lx, guardflags:%#lx, flags:%#lx, "
        "dpclass:%#lx, dpflags: %#lx) FIXME",
        ARG1, (HChar*)ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
}

PRE(guarded_write_np)
{
    PRINT("guarded_write_np(fd:%ld, guard:%#lx, cbuf:%#lx, nbyte:%llu) FIXME",
        ARG1, ARG2, ARG3, (ULong)ARG4);
}

PRE(guarded_pwrite_np)
{
    PRINT("guarded_pwrite_np(fd:%ld, guard:%#lx, buf:%#lx, nbyte:%llu, offset:%lld) FIXME",
        ARG1, ARG2, ARG3, (ULong)ARG4, (Long)ARG5);
}

PRE(guarded_writev_np)
{
    PRINT("guarded_writev_np(fd:%ld, guard:%#lx, iovp:%#lx, iovcnt:%llu) FIXME",
        ARG1, ARG2, ARG3, (ULong)ARG4);
}

PRE(openat)
{
   if (ARG3 & VKI_O_CREAT) {
      // 4-arg version
      PRINT("sys_openat ( %ld, %#" FMT_REGWORD "x(%s), %ld, %ld )",
            SARG1, ARG2, (HChar*)(Addr)ARG2, SARG3, SARG4);
      PRE_REG_READ4(long, "openat",
                    int, dfd, const char *, filename, int, flags, int, mode);
   } else {
     // 3-arg version
     PRINT("sys_openat ( %ld, %#" FMT_REGWORD "x(%s), %ld )",
           SARG1, ARG2, (HChar*)(Addr)ARG2, SARG3);
     PRE_REG_READ3(long, "openat",
                   int, dfd, const char *, filename, int, flags);
   }
   PRE_MEM_RASCIIZ( "openat(filename)", ARG2 );

   /* For absolute filenames, dfd is ignored.  If dfd is AT_FDCWD,
      filename is relative to cwd.  When comparing dfd against AT_FDCWD,
      be sure only to compare the bottom 32 bits. */
   if (ML_(safe_to_deref)( (void*)(Addr)ARG2, 1 )
       && *(Char *)(Addr)ARG2 != '/'
       && ((Int)ARG1) != ((Int)VKI_AT_FDCWD)
       && !ML_(fd_allowed)(ARG1, "openat", tid, False))
      SET_STATUS_Failure( VKI_EBADF );

   /* Otherwise handle normally */
   *flags |= SfMayBlock;
}

POST(openat)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "openat", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)(Addr)ARG2);
   }
}

#endif /* DARWIN_VERS >= DARWIN_10_10 */


/* ---------------------------------------------------------------------
   Added for OSX 10.11 (El Capitan)
   ------------------------------------------------------------------ */

#if DARWIN_VERS >= DARWIN_10_11

PRE(kevent_qos)
{
   PRINT("kevent_qos( %ld, %#lx, %ld, %#lx, %ld, %#lx, %ld, %ld )",
         SARG1, ARG2, SARG3, ARG4, SARG5, ARG6, SARG7, ARG8);
   PRE_REG_READ8(int,"kevent_qos",
                 int,kq,
                 const struct vki_kevent_qos_s *,changelist,
                 int,nchanges,
                 struct vki_kevent_qos_s *,eventlist,
                 int,nevents,
                 void*,data_out,
                 size_t*,data_available,
                 unsigned int,flags);

   if (ARG3) PRE_MEM_READ ("kevent_qos(changelist)",
                           ARG2, ARG3 * sizeof(struct vki_kevent_qos_s));
   if (ARG5) PRE_MEM_WRITE("kevent_qos(eventlist)",
                           ARG4, ARG5 * sizeof(struct vki_kevent_qos_s));
   if (ARG7) PRE_MEM_WRITE("kevent_qos(data_out)",
                           ARG6, ARG7 * sizeof(void*));

   *flags |= SfMayBlock;
}

POST(kevent_qos)
{
   PRINT("kevent_qos ret %ld dst %#lx (%zu)", RES, ARG4, sizeof(struct vki_kevent_qos_s));
   if (RES > 0) {
      ML_(sync_mappings)("after", "kevent_qos", 0);
      POST_MEM_WRITE(ARG4, RES * sizeof(struct vki_kevent_qos_s));
   }
}


PRE(pselect)
{
   *flags |= SfMayBlock;
   PRINT("pselect ( %ld, %#lx, %#lx, %#lx, %#lx, %#lx )", SARG1, ARG2, ARG3,
         ARG4, ARG5, ARG6);
   PRE_REG_READ5(long, "pselect",
                 int, n, vki_fd_set *, readfds, vki_fd_set *, writefds,
                 vki_fd_set *, exceptfds, struct vki_timeval *, timeout);
   // XXX: this possibly understates how much memory is read.
   if (ARG2 != 0)
      PRE_MEM_READ( "pselect(readfds)",
		     ARG2, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG3 != 0)
      PRE_MEM_READ( "pselect(writefds)",
		     ARG3, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG4 != 0)
      PRE_MEM_READ( "pselect(exceptfds)",
		     ARG4, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG5 != 0)
      PRE_timeval_READ( "pselect(timeout)", ARG5 );
   if (ARG6 != 0)
      PRE_MEM_READ( "pselect(sigmask)", ARG6, sizeof(vki_sigset_t) );
}

#endif /* DARWIN_VERS >= DARWIN_10_11 */


/* ---------------------------------------------------------------------
 Added for macOS 10.12 (Sierra)
 ------------------------------------------------------------------ */

#if DARWIN_VERS >= DARWIN_10_12

PRE(getentropy)
{
    PRINT("getentropy(buffer:%#lx, size:%ld) FIXME", ARG1, ARG2);
    PRE_REG_READ2(int, "getentropy",
                  void*, buffer, size_t, size);
}

static const HChar *ulop_name(int op)
{
   switch (op) {
      case VKI_UL_UNFAIR_LOCK:          return "UL_UNFAIR_LOCK";
      case VKI_UL_COMPARE_AND_WAIT:     return "UL_COMPARE_AND_WAIT";
      default: return "??";
   }
}

PRE(ulock_wake)
{
    PRINT("ulock_wake(operation:%ld, addr:%#lx, wake_value:%ld) FIXME",
        ARG1, ARG2, ARG3);
    PRE_REG_READ3(int, "ulock_wake",
                  uint32_t, operation, void*, addr, uint64_t, wake_value);
}

PRE(ulock_wait)
{
    uint ul_opcode = ARG1 & VKI_UL_OPCODE_MASK;
    uint ul_flags = ARG1 & VKI_UL_FLAGS_MASK;

    switch (ul_opcode) {
    case VKI_UL_UNFAIR_LOCK:
    case VKI_UL_COMPARE_AND_WAIT: {
      const char* name = ulop_name(ul_opcode);
      PRINT("ulock_wait(operation:%s (flags: %#x), addr:%#lx, value:%ld, timeout:%ld)",
            name, ul_flags, ARG2, ARG3, ARG4);
      PRE_REG_READ4(int, "ulock_wait",
                    uint32_t, operation, void*, addr, uint64_t, value, uint32_t, timeout);
      PRE_MEM_READ("ulock_wait(addr)", ARG2, 4 );
      break;
    }

    default:
      PRINT("ulock_wait(operation:%ld (opcode: %u [??], flags: %#x), addr:%#lx, value:%ld, timeout:%ld)", ARG1, ul_opcode, ul_flags, ARG2, ARG3, ARG4);
      log_decaying("UNKNOWN ulock_wait %ld (opcode: %u [??], flags: %#x)!", ARG1, ul_opcode, ul_flags);
      break;
    }
}

PRE(host_create_mach_voucher_trap)
{
    // munge_wwww -- no need to call helper
    PRINT("host_create_mach_voucher_trap"
        "(host:%#lx, recipes:%#lx, recipes_size:%ld, voucher:%#lx) FIXME",
        ARG1, ARG2, ARG3, ARG4);
}

PRE(task_register_dyld_image_infos)
{
//#pragma pack(4)
//    typedef struct {
//       mach_msg_header_t Head;
//       /* start of the kernel processed data */
//       mach_msg_body_t msgh_body;
//       mach_msg_ool_descriptor_t dyld_images;
//       /* end of the kernel processed data */
//       NDR_record_t NDR;
//       mach_msg_type_number_t dyld_imagesCnt;
//    } Request;
//#pragma pack()
    
    // Request *req = (Request *)ARG1;
    
    PRINT("task_register_dyld_image_infos(%s)", name_for_port(MACH_REMOTE));
    
    AFTER = POST_FN(task_register_dyld_image_infos);
}

POST(task_register_dyld_image_infos)
{
#pragma pack(4)
    typedef struct {
       mach_msg_header_t Head;
       NDR_record_t NDR;
       kern_return_t RetCode;
    } Reply;
#pragma pack()
    
    Reply *reply = (Reply *)ARG1;
    if (!reply->RetCode) {
    } else {
        PRINT("mig return %d", reply->RetCode);
    }
}

PRE(task_register_dyld_shared_cache_image_info)
{
//#pragma pack(4)
//    typedef struct {
//       mach_msg_header_t Head;
//       NDR_record_t NDR;
//       dyld_kernel_image_info_t dyld_cache_image;
//       boolean_t no_cache;
//       boolean_t private_cache;
//    } Request;
//#pragma pack()
    
    // Request *req = (Request *)ARG1;
    
    PRINT("task_register_dyld_shared_cache_image_info(%s)",
        name_for_port(MACH_REMOTE));
    
    AFTER = POST_FN(task_register_dyld_shared_cache_image_info);
}

POST(task_register_dyld_shared_cache_image_info)
{
#pragma pack(4)
    typedef struct {
       mach_msg_header_t Head;
       NDR_record_t NDR;
       kern_return_t RetCode;
    } Reply;
#pragma pack()
    
    Reply *reply = (Reply *)ARG1;
    if (!reply->RetCode) {
    } else {
        PRINT("mig return %d", reply->RetCode);
    }
}

PRE(mach_generate_activity_id)
{
    // munge_www -- no need to call helper
    PRINT("mach_generate_activity_id"
        "(target:%s, count:%ld)",
        name_for_port(ARG1), ARG2);
    PRE_REG_READ3(long, "mach_generate_activity_id",
                  mach_port_name_t, target, int, count, uint64_t *, activity_id);
    if (ARG2 <= 0 || ARG2 > MACH_ACTIVITY_ID_COUNT_MAX) {
       SET_STATUS_Failure( VKI_EINVAL );
    }
    if (ML_(safe_to_deref)( (void*)ARG3, sizeof(vki_uint64_t*) )) {
       PRE_MEM_WRITE( "mach_generate_activity_id(activity_id)", ARG3, sizeof(vki_uint64_t) );
    } else {
       SET_STATUS_Failure( VKI_EFAULT );
    }
}

POST(mach_generate_activity_id)
{
    if (ML_(safe_to_deref)( (void*)ARG3, sizeof(vki_uint64_t*) )) {
       POST_MEM_WRITE( ARG3, sizeof(vki_uint64_t) );
       PRINT("-> activity_id:%#llx", *(uint64_t*)ARG3);
    }
}

#endif /* DARWIN_VERS >= DARWIN_10_12 */

/* ---------------------------------------------------------------------
 Added for macOS 10.13 (High Sierra)
 ------------------------------------------------------------------ */

#if DARWIN_VERS >= DARWIN_10_13

PRE(openat_nocancel)
{
   if (ARG3 & VKI_O_CREAT) {
      // 4-arg version
      PRINT("openat_nocancel ( %ld, %#" FMT_REGWORD "x(%s), %ld, %ld )",
            SARG1, ARG2, (HChar*)(Addr)ARG2, SARG3, SARG4);
      PRE_REG_READ4(long, "openat_nocancel",
                    int, dfd, const char *, filename, int, flags, int, mode);
   } else {
     // 3-arg version
     PRINT("openat_nocancel ( %ld, %#" FMT_REGWORD "x(%s), %ld )",
           SARG1, ARG2, (HChar*)(Addr)ARG2, SARG3);
     PRE_REG_READ3(long, "openat_nocancel",
                   int, dfd, const char *, filename, int, flags);
   }
   PRE_MEM_RASCIIZ( "openat_nocancel(filename)", ARG2 );

   /* For absolute filenames, dfd is ignored.  If dfd is AT_FDCWD,
      filename is relative to cwd.  When comparing dfd against AT_FDCWD,
      be sure only to compare the bottom 32 bits. */
   if (ML_(safe_to_deref)( (void*)(Addr)ARG2, 1 )
       && *(Char *)(Addr)ARG2 != '/'
       && ((Int)ARG1) != ((Int)VKI_AT_FDCWD)
       && !ML_(fd_allowed)(ARG1, "openat_nocancel", tid, False))
      SET_STATUS_Failure( VKI_EBADF );

   /* Otherwise handle normally */
   *flags |= SfMayBlock;
}
POST(openat_nocancel)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "openat_nocancel", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)(Addr)ARG2);
   }
}

#endif /* DARWIN_VERS >= DARWIN_10_13 */

/* ---------------------------------------------------------------------
   syscall tables
   ------------------------------------------------------------------ */

/* Add a Darwin-specific, arch-independent wrapper to a syscall table. */

#define MACX_(sysno, name) \
           WRAPPER_ENTRY_X_(darwin, VG_DARWIN_SYSNO_INDEX(sysno), name) 

#define MACXY(sysno, name) \
           WRAPPER_ENTRY_XY(darwin, VG_DARWIN_SYSNO_INDEX(sysno), name)

#define _____(sysno) GENX_(sysno, sys_ni_syscall)  /* UNIX style only */

/*
     _____ : unsupported by the kernel (sys_ni_syscall) (UNIX-style only)
             unfortunately misused for Mach too, causing assertion failures
  // _____ : unimplemented in valgrind
     GEN   : handlers are in syswrap-generic.c
     MAC   : handlers are in this file
        X_ : PRE handler only
        XY : PRE and POST handlers
*/
const SyscallTableEntry ML_(syscall_table)[] = {
// _____(__NR_syscall),   // 0
   MACX_(__NR_exit,        exit), 
   GENX_(__NR_fork,        sys_fork), 
   GENXY(__NR_read,        sys_read), 
   GENX_(__NR_write,       sys_write), 
   GENXY(__NR_open,        sys_open), 
   GENXY(__NR_close,       sys_close), 
   GENXY(__NR_wait4,       sys_wait4), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(8)),     // old creat
   GENX_(__NR_link,        sys_link), 
   GENX_(__NR_unlink,      sys_unlink), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(11)),    // old execv
   GENX_(__NR_chdir,       sys_chdir), 
   GENX_(__NR_fchdir,      sys_fchdir), 
   GENX_(__NR_mknod,       sys_mknod), 
   GENX_(__NR_chmod,       sys_chmod), 
   GENX_(__NR_chown,       sys_chown), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(17)),    // old break
   MACXY(__NR_getfsstat,   getfsstat), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(19)),    // old lseek
   GENX_(__NR_getpid,      sys_getpid),     // 20
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(21)),    // old mount 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(22)),    // old umount
   GENX_(__NR_setuid,      sys_setuid), 
   GENX_(__NR_getuid,      sys_getuid), 
   GENX_(__NR_geteuid,     sys_geteuid), 
   MACX_(__NR_ptrace,      ptrace), 
   MACXY(__NR_recvmsg,     recvmsg), 
   MACX_(__NR_sendmsg,     sendmsg), 
   MACXY(__NR_recvfrom,    recvfrom), 
   MACXY(__NR_accept,      accept), 
   MACXY(__NR_getpeername, getpeername), 
   MACXY(__NR_getsockname, getsockname), 
   GENX_(__NR_access,      sys_access), 
   MACX_(__NR_chflags,     chflags), 
   MACX_(__NR_fchflags,    fchflags), 
   GENX_(__NR_sync,        sys_sync), 
   GENX_(__NR_kill,        sys_kill), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(38)),    // old stat
   GENX_(__NR_getppid,     sys_getppid), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(40)),    // old lstat
   GENXY(__NR_dup,         sys_dup), 
   MACXY(__NR_pipe,        pipe), 
   GENX_(__NR_getegid,     sys_getegid), 
#if DARWIN_VERS >= DARWIN_10_7
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(44)),    // old profil
#else
// _____(__NR_profil),
#endif
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(45)),    // old ktrace
   MACXY(__NR_sigaction,   sigaction), 
   GENX_(__NR_getgid,      sys_getgid), 
   MACXY(__NR_sigprocmask, sigprocmask), 
   MACXY(__NR_getlogin,    getlogin), 
// _____(__NR_setlogin), 
// _____(__NR_acct), 
   MACXY(__NR_sigpending,  sigpending),
   GENXY(__NR_sigaltstack, sys_sigaltstack), 
   MACXY(__NR_ioctl,       ioctl), 
// _____(__NR_reboot), 
// _____(__NR_revoke), 
   GENX_(__NR_symlink,     sys_symlink),   // 57
   GENX_(__NR_readlink,    sys_readlink), 
   GENX_(__NR_execve,      sys_execve), 
   GENX_(__NR_umask,       sys_umask),     // 60
   GENX_(__NR_chroot,      sys_chroot), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(62)),    // old fstat
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(63)),    // used internally, reserved
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(64)),    // old getpagesize
   GENX_(__NR_msync,       sys_msync), 
   GENX_(__NR_vfork,       sys_fork),              // (We treat vfork as fork.)
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(67)),    // old vread
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(68)),    // old vwrite
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(69)),    // old sbrk
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(70)),    // old sstk
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(71)),    // old mmap
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(72)),    // old vadvise
   GENXY(__NR_munmap,      sys_munmap), 
   GENXY(__NR_mprotect,    sys_mprotect), 
   GENX_(__NR_madvise,     sys_madvise), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(76)),    // old vhangup
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(77)),    // old vlimit
   GENXY(__NR_mincore,     sys_mincore), 
   GENXY(__NR_getgroups,   sys_getgroups), 
// _____(__NR_setgroups),   // 80
   GENX_(__NR_getpgrp,     sys_getpgrp), 
   GENX_(__NR_setpgid,     sys_setpgid), 
   GENXY(__NR_setitimer,   sys_setitimer), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(84)),    // old wait
// _____(__NR_swapon), 
   GENXY(__NR_getitimer,   sys_getitimer), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(87)),    // old gethostname
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(88)),    // old sethostname
   MACXY(__NR_getdtablesize, getdtablesize), 
   GENXY(__NR_dup2,        sys_dup2), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(91)),    // old getdopt
   MACXY(__NR_fcntl,       fcntl), 
   GENX_(__NR_select,      sys_select), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(94)),    // old setdopt
   GENX_(__NR_fsync,       sys_fsync), 
   GENX_(__NR_setpriority, sys_setpriority), 
   MACXY(__NR_socket,      socket), 
   MACX_(__NR_connect,     connect), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(99)),    // old accept
   GENX_(__NR_getpriority, sys_getpriority),   // 100
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(101)),   // old send
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(102)),   // old recv
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(103)),   // old sigreturn
   MACX_(__NR_bind,        bind), 
   MACX_(__NR_setsockopt,  setsockopt), 
   MACX_(__NR_listen,      listen), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(107)),   // old vtimes
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(108)),   // old sigvec
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(109)),   // old sigblock
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(110)),   // old sigsetmask
   MACX_(__NR_sigsuspend,  sigsuspend),            // old sigsuspend
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(112)),   // old sigstack
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(113)),   // old recvmsg
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(114)),   // old sendmsg
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(115)),   // old vtrace
   GENXY(__NR_gettimeofday, sys_gettimeofday), 
   GENXY(__NR_getrusage,   sys_getrusage), 
   MACXY(__NR_getsockopt,  getsockopt), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(119)),   // old resuba
   GENXY(__NR_readv,       sys_readv),        // 120
   GENX_(__NR_writev,      sys_writev), 
// _____(__NR_settimeofday), 
   GENX_(__NR_fchown,      sys_fchown), 
   GENX_(__NR_fchmod,      sys_fchmod), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(125)),   // old recvfrom
// _____(__NR_setreuid), 
// _____(__NR_setregid), 
   GENX_(__NR_rename,      sys_rename), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(129)),   // old truncate
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(130)),   // old ftruncate
   GENX_(__NR_flock,       sys_flock), 
   MACXY(__NR_mkfifo,      mkfifo),
   MACX_(__NR_sendto,      sendto), 
   MACX_(__NR_shutdown,    shutdown), 
   MACXY(__NR_socketpair,  socketpair), 
   GENX_(__NR_mkdir,       sys_mkdir), 
   GENX_(__NR_rmdir,       sys_rmdir), 
   GENX_(__NR_utimes,      sys_utimes), 
   MACX_(__NR_futimes,     futimes), 
// _____(__NR_adjtime),     // 140
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(141)),   // old getpeername
   MACXY(__NR_gethostuuid, gethostuuid), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(143)),   // old sethostid
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(144)),   // old getrlimit
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(145)),   // old setrlimit
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(146)),   // old killpg
   GENX_(__NR_setsid,      sys_setsid), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(148)),   // old setquota
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(149)),   // old qquota
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(150)),   // old getsockname 
// _____(__NR_getpgid), 
// _____(__NR_setprivexec), 
   GENXY(__NR_pread,       sys_pread64), 
   GENX_(__NR_pwrite,      sys_pwrite64), 
// _____(__NR_nfssvc), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(156)),   // old getdirentries
   GENXY(__NR_statfs,      sys_statfs), 
   GENXY(__NR_fstatfs,     sys_fstatfs), 
// _____(__NR_unmount), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(160)),   // old async_daemon
// _____(__NR_getfh), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(162)),   // old getdomainname
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(163)),   // old setdomainname
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(164)),   // ???
// _____(__NR_quotactl), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(166)),   // old exportfs
   MACX_(__NR_mount,       mount), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(168)),   // old ustat
   MACXY(__NR_csops,       csops),                 // code-signing ops
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(170)),   // old table
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(171)),   // old wait3
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(172)),   // old rpause
// _____(__NR_waitid), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(174)),   // old getdents
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(175)),   // old gc_control
// _____(__NR_add_profil), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(177)),   // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(178)),   // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(179)),   // ???
   MACX_(__NR_kdebug_trace, kdebug_trace),     // 180
   GENX_(__NR_setgid,      sys_setgid), 
   MACX_(__NR_setegid,     setegid), 
   MACX_(__NR_seteuid,     seteuid), 
   MACX_(__NR_sigreturn,   sigreturn), 
// _____(__NR_chud), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(186)),   // ??? 
#if DARWIN_VERS >= DARWIN_10_6
// _____(__NR_fdatasync), 
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(187)),   // ??? 
#endif
   GENXY(__NR_stat,        sys_newstat), 
   GENXY(__NR_fstat,       sys_newfstat), 
   GENXY(__NR_lstat,       sys_newlstat), 
   MACX_(__NR_pathconf,    pathconf), 
   MACX_(__NR_fpathconf,   fpathconf), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(193)),   // ???
   GENXY(__NR_getrlimit,   sys_getrlimit), 
   GENX_(__NR_setrlimit,   sys_setrlimit), 
   MACXY(__NR_getdirentries, getdirentries), 
   MACXY(__NR_mmap,        mmap), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(198)),   // __syscall
   MACX_(__NR_lseek,       lseek), 
   GENX_(__NR_truncate,    sys_truncate64),   // 200
   GENX_(__NR_ftruncate,   sys_ftruncate64), 
   MACXY(__NR___sysctl,    __sysctl), 
   GENX_(__NR_mlock,       sys_mlock), 
   GENX_(__NR_munlock,     sys_munlock), 
// _____(__NR_undelete), 
// _____(__NR_ATsocket), 
// _____(__NR_ATgetmsg), 
// _____(__NR_ATputmsg), 
// _____(__NR_ATPsndreq), 
// _____(__NR_ATPsndrsp), 
// _____(__NR_ATPgetreq), 
// _____(__NR_ATPgetrsp), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(213)),   // Reserved for AppleTalk
#if DARWIN_VERS >= DARWIN_10_6
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(214)),   // old kqueue_from_portset_np
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(215)),   // old kqueue_portset_np
#else
// _____(__NR_kqueue_from_portset_np), 
// _____(__NR_kqueue_portset_np), 
#endif
// _____(__NR_mkcomplex), 
// _____(__NR_statv), 
// _____(__NR_lstatv), 
// _____(__NR_fstatv), 
   MACXY(__NR_getattrlist, getattrlist),   // 220
   MACX_(__NR_setattrlist, setattrlist), 
   MACXY(__NR_getdirentriesattr, getdirentriesattr), 
   MACX_(__NR_exchangedata,      exchangedata), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(224)),   // checkuseraccess
// _____(__NR_searchfs), 
   GENX_(__NR_delete,      sys_unlink), 
// _____(__NR_copyfile), 
#if DARWIN_VERS >= DARWIN_10_6
// _____(__NR_fgetattrlist), 
// _____(__NR_fsetattrlist), 
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(228)),   // ?? 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(229)),   // ?? 
#endif
   GENXY(__NR_poll,        sys_poll), 
   MACX_(__NR_watchevent,  watchevent), 
   MACXY(__NR_waitevent,   waitevent), 
   MACX_(__NR_modwatch,    modwatch), 
   MACXY(__NR_getxattr,    getxattr), 
   MACXY(__NR_fgetxattr,   fgetxattr), 
   MACX_(__NR_setxattr,    setxattr), 
   MACX_(__NR_fsetxattr,   fsetxattr), 
   MACX_(__NR_removexattr, removexattr), 
   MACX_(__NR_fremovexattr, fremovexattr), 
   MACXY(__NR_listxattr,   listxattr),    // 240
   MACXY(__NR_flistxattr,  flistxattr), 
   MACXY(__NR_fsctl,       fsctl), 
   MACX_(__NR_initgroups,  initgroups), 
   MACXY(__NR_posix_spawn, posix_spawn), 
#if DARWIN_VERS >= DARWIN_10_6
// _____(__NR_ffsctl), 
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(245)),   // ???
#endif
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(246)),   // ???
// _____(__NR_nfsclnt), 
// _____(__NR_fhopen), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(249)),   // ???
// _____(__NR_minherit), 
// _____(__NR_semsys), 
// _____(__NR_msgsys), 
// _____(__NR_shmsys), 
   MACXY(__NR_semctl,      semctl), 
   MACX_(__NR_semget,      semget), 
   MACX_(__NR_semop,       semop), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(257)),   // ???
// _____(__NR_msgctl), 
// _____(__NR_msgget), 
// _____(__NR_msgsnd),   // 260
// _____(__NR_msgrcv), 
   MACXY(__NR_shmat,       shmat), 
   MACXY(__NR_shmctl,      shmctl), 
   MACXY(__NR_shmdt,       shmdt), 
   MACX_(__NR_shmget,      shmget), 
   MACXY(__NR_shm_open,    shm_open), 
   MACXY(__NR_shm_unlink,  shm_unlink), 
   MACX_(__NR_sem_open,    sem_open), 
   MACX_(__NR_sem_close,   sem_close), 
   MACX_(__NR_sem_unlink,  sem_unlink), 
   MACX_(__NR_sem_wait,    sem_wait), 
   MACX_(__NR_sem_trywait, sem_trywait), 
   MACX_(__NR_sem_post,    sem_post), 
   // 274 seems to have been repurposed for 10.10.  Was sem_getvalue,
   //     has become sysctlbyname.  See below.
   MACXY(__NR_sem_init,    sem_init), 
   MACX_(__NR_sem_destroy, sem_destroy), 
   MACX_(__NR_open_extended,  open_extended),    // 277
// _____(__NR_umask_extended), 
   MACXY(__NR_stat_extended,  stat_extended), 
   MACXY(__NR_lstat_extended, lstat_extended),   // 280
   MACXY(__NR_fstat_extended, fstat_extended), 
   MACX_(__NR_chmod_extended, chmod_extended), 
   MACX_(__NR_fchmod_extended,fchmod_extended), 
   MACXY(__NR_access_extended,access_extended), 
   MACX_(__NR_settid,         settid), 
#if DARWIN_VERS >= DARWIN_10_8
   MACX_(__NR_gettid, gettid),  // 286
#endif
// _____(__NR_setsgroups), 
// _____(__NR_getsgroups), 
// _____(__NR_setwgroups), 
// _____(__NR_getwgroups), 
// _____(__NR_mkfifo_extended), 
// _____(__NR_mkdir_extended), 
// _____(__NR_identitysvc), 
// _____(__NR_shared_region_check_np), 
// _____(__NR_shared_region_map_np), 
#if DARWIN_VERS >= DARWIN_10_6
// _____(__NR_vm_pressure_monitor), 
// _____(__NR_psynch_rw_longrdlock), 
// _____(__NR_psynch_rw_yieldwrlock), 
// _____(__NR_psynch_rw_downgrade), 
// _____(__NR_psynch_rw_upgrade), 
   MACXY(__NR_psynch_mutexwait, psynch_mutexwait), // 301
   MACXY(__NR_psynch_mutexdrop, psynch_mutexdrop), // 302
   MACXY(__NR_psynch_cvbroad,   psynch_cvbroad),   // 303
   MACXY(__NR_psynch_cvsignal,  psynch_cvsignal),  // 304
   MACXY(__NR_psynch_cvwait,    psynch_cvwait),    // 305
   MACXY(__NR_psynch_rw_rdlock, psynch_rw_rdlock), // 306
   MACXY(__NR_psynch_rw_wrlock, psynch_rw_wrlock), // 307
   MACXY(__NR_psynch_rw_unlock, psynch_rw_unlock), // 308
// _____(__NR_psynch_rw_unlock2), 
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(296)),   // old load_shared_file 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(297)),   // old reset_shared_file 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(298)),   // old new_system_shared_regions 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(299)),   // old shared_region_map_file_np 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(300)),   // old shared_region_make_private_np
// _____(__NR___pthread_mutex_destroy), 
// _____(__NR___pthread_mutex_init), 
// _____(__NR___pthread_mutex_lock), 
// _____(__NR___pthread_mutex_trylock), 
// _____(__NR___pthread_mutex_unlock), 
// _____(__NR___pthread_cond_init), 
// _____(__NR___pthread_cond_destroy), 
// _____(__NR___pthread_cond_broadcast), 
// _____(__NR___pthread_cond_signal), 
#endif
// _____(__NR_getsid), 
// _____(__NR_settid_with_pid), 
#if DARWIN_VERS >= DARWIN_10_7
   MACXY(__NR_psynch_cvclrprepost, psynch_cvclrprepost), // 312
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(308)),   // old __pthread_cond_timedwait
#endif
// _____(__NR_aio_fsync), 
   MACX_(__NR_aio_return,     aio_return),
   MACX_(__NR_aio_suspend,    aio_suspend), 
// _____(__NR_aio_cancel), 
   MACX_(__NR_aio_error,      aio_error), 
   MACXY(__NR_aio_read,       aio_read), 
   MACX_(__NR_aio_write,      aio_write), 
// _____(__NR_lio_listio),   // 320
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(321)),   // ???

#if DARWIN_VERS >= DARWIN_10_8
   MACXY(__NR_iopolicysys, iopolicysys), 
   MACXY(__NR_process_policy, process_policy),
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(322)),   // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(323)),   // ???
#endif
// _____(__NR_mlockall), 
// _____(__NR_munlockall), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(326)),   // ???
   MACX_(__NR_issetugid,               issetugid), 
   MACX_(__NR___pthread_kill,          __pthread_kill),
   MACX_(__NR___pthread_sigmask,       __pthread_sigmask), 
   MACXY(__NR___sigwait,               __sigwait),  // 330
   MACX_(__NR___disable_threadsignal,  __disable_threadsignal), 
   MACX_(__NR___pthread_markcancel,    __pthread_markcancel), 
   MACX_(__NR___pthread_canceled,      __pthread_canceled),
   MACX_(__NR___semwait_signal,        __semwait_signal), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(335)),   // old utrace
   MACXY(__NR_proc_info,               proc_info),  // 336
   MACXY(__NR_sendfile,    sendfile), 
   MACXY(__NR_stat64,      stat64), 
   MACXY(__NR_fstat64,     fstat64), 
   MACXY(__NR_lstat64,     lstat64),    // 340
   MACXY(__NR_stat64_extended,  stat64_extended), 
   MACXY(__NR_lstat64_extended, lstat64_extended), 
   MACXY(__NR_fstat64_extended, fstat64_extended),
   MACXY(__NR_getdirentries64, getdirentries64), 
   MACXY(__NR_statfs64,    statfs64), 
   MACXY(__NR_fstatfs64,   fstatfs64), 
   MACXY(__NR_getfsstat64, getfsstat64), 
   MACX_(__NR___pthread_chdir,  __pthread_chdir),
   MACX_(__NR___pthread_fchdir, __pthread_fchdir),
// _____(__NR_audit), 
   MACXY(__NR_auditon,     auditon), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(352)),   // ???
// _____(__NR_getauid), 
// _____(__NR_setauid), 
// _____(__NR_getaudit), 
// _____(__NR_setaudit), 
   MACXY(__NR_getaudit_addr, getaudit_addr),
// _____(__NR_setaudit_addr), 
// _____(__NR_auditctl), 
   MACXY(__NR_bsdthread_create,     bsdthread_create),   // 360
   MACX_(__NR_bsdthread_terminate,  bsdthread_terminate), 
   MACXY(__NR_kqueue,      kqueue), 
   MACXY(__NR_kevent,      kevent), 
   GENX_(__NR_lchown,      sys_lchown), 
// _____(__NR_stack_snapshot), 
   MACX_(__NR_bsdthread_register, bsdthread_register), 
   MACX_(__NR_workq_open,  workq_open), 
   MACXY(__NR_workq_ops,   workq_ops), 
#if DARWIN_VERS >= DARWIN_10_6
   MACXY(__NR_kevent64,      kevent64), 
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(369)),   // ???
#endif
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(370)),   // old semwait_signal
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(371)),   // old semwait_signal_nocancel
#if DARWIN_VERS >= DARWIN_10_6
   MACX_(__NR___thread_selfid, __thread_selfid), 
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(372)),   // ???
#endif
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(373)),   // ???
#if DARWIN_VERS < DARWIN_10_11
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(374)),   // ???
#endif
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(375)),   // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(376)),   // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(377)),   // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(378)),   // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(379)),   // ???
// _____(__NR___mac_execve),   // 380
   MACX_(__NR___mac_syscall, __mac_syscall),
// _____(__NR___mac_get_file),
// _____(__NR___mac_set_file),
// _____(__NR___mac_get_link),
// _____(__NR___mac_set_link),
// _____(__NR___mac_get_proc),
// _____(__NR___mac_set_proc),
// _____(__NR___mac_get_fd),
// _____(__NR___mac_set_fd),
// _____(__NR___mac_get_pid),
// _____(__NR___mac_get_lcid),
// _____(__NR___mac_get_lctx),
// _____(__NR___mac_set_lctx),
// _____(__NR_setlcid),
// _____(__NR_getlcid),
   // GrP fixme need any special nocancel handling?
   GENXY(__NR_read_nocancel,     sys_read),
   GENX_(__NR_write_nocancel,    sys_write),
   GENXY(__NR_open_nocancel,     sys_open),
   GENXY(__NR_close_nocancel,    sys_close),
   GENXY(__NR_wait4_nocancel,    sys_wait4),   // 400
   MACXY(__NR_recvmsg_nocancel,  recvmsg),
   MACX_(__NR_sendmsg_nocancel,  sendmsg),
   MACXY(__NR_recvfrom_nocancel, recvfrom),
   MACXY(__NR_accept_nocancel,   accept),
   GENX_(__NR_msync_nocancel,    sys_msync),
   MACXY(__NR_fcntl_nocancel,    fcntl),
   GENX_(__NR_select_nocancel,   sys_select),
   GENX_(__NR_fsync_nocancel,    sys_fsync),
   MACX_(__NR_connect_nocancel,  connect),
   MACX_(__NR_sigsuspend_nocancel, sigsuspend),
   GENXY(__NR_readv_nocancel,    sys_readv),
   GENX_(__NR_writev_nocancel,   sys_writev),
   MACX_(__NR_sendto_nocancel,   sendto),
   GENXY(__NR_pread_nocancel,    sys_pread64),
   GENX_(__NR_pwrite_nocancel,   sys_pwrite64),
// _____(__NR_waitid_nocancel),
   GENXY(__NR_poll_nocancel,     sys_poll),
// _____(__NR_msgsnd_nocancel),
// _____(__NR_msgrcv_nocancel),
   MACX_(__NR_sem_wait_nocancel, sem_wait), // 420
// _____(__NR_aio_suspend_nocancel),
// _____(__NR___sigwait_nocancel),
   MACX_(__NR___semwait_signal_nocancel, __semwait_signal), 
// _____(__NR___mac_mount),
// _____(__NR___mac_get_mount),
// _____(__NR___mac_getfsstat),
#if DARWIN_VERS >= DARWIN_10_6
   MACXY(__NR_fsgetpath, fsgetpath), 
   MACXY(__NR_audit_session_self, audit_session_self),
// _____(__NR_audit_session_join),
#endif
#if DARWIN_VERS >= DARWIN_10_9
    MACX_(__NR_fileport_makeport, fileport_makeport),
// _____(__NR_fileport_makefd),                         // 431
// _____(__NR_audit_session_port),                      // 432
// _____(__NR_pid_suspend),                             // 433
// _____(__NR_pid_resume),                              // 434
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(435)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(436)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(437)),        // ???
// _____(__NR_shared_region_map_and_slide_np),          // 438
// _____(__NR_kas_info),                                // 439
// _____(__NR_memorystatus_control),                    // 440
    MACX_(__NR_guarded_open_np, guarded_open_np),
    MACX_(__NR_guarded_close_np, guarded_close_np),
    MACX_(__NR_guarded_kqueue_np, guarded_kqueue_np),
    MACX_(__NR_change_fdguard_np, change_fdguard_np),
    MACX_(__NR_connectx, connectx),
    MACX_(__NR_disconnectx, disconnectx),
#endif
#if DARWIN_VERS >= DARWIN_10_10
   MACXY(__NR_sysctlbyname,        sysctlbyname),       // 274
   MACXY(__NR_necp_match_policy,   necp_match_policy),  // 460
   MACXY(__NR_getattrlistbulk,     getattrlistbulk),    // 461
   MACXY(__NR_openat,              openat),             // 463
#if DARWIN_VERS >= DARWIN_10_13
   MACXY(__NR_openat_nocancel,     openat_nocancel),    // 464
#endif
   MACX_(__NR_faccessat,           faccessat),          // 466
   MACX_(__NR_fstatat64,           fstatat64),          // 470
   MACX_(__NR_readlinkat,          readlinkat),         // 473
   MACX_(__NR_bsdthread_ctl,       bsdthread_ctl),      // 478
   MACX_(__NR_csrctl,              csrctl),             // 483
   MACX_(__NR_guarded_open_dprotected_np, guarded_open_dprotected_np),  // 484
   MACX_(__NR_guarded_write_np, guarded_write_np),      // 485
   MACX_(__NR_guarded_pwrite_np, guarded_pwrite_np),    // 486
   MACX_(__NR_guarded_writev_np, guarded_writev_np),    // 487
// _____(__NR___mremap_encrypted),                      // 489
#endif
#if DARWIN_VERS >= DARWIN_10_11
// _____(__NR_kdebug_trace_string),                     // 178
   MACXY(__NR_kevent_qos, kevent_qos),                  // 374
   MACX_(__NR_pselect, pselect),                        // 394
// _____(__NR_netagent_trigger),                        // 490
// _____(__NR_stack_snapshot_with_config),              // 491
// _____(__NR_microstackshot),                          // 492
// _____(__NR_grab_pgo_data),                           // 493
// _____(__NR_persona),                                 // 494
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(495)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(496)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(497)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(498)),        // ???
// _____(__NR_work_interval_ctl),                       // 499
#endif
#if DARWIN_VERS >= DARWIN_10_12
// _____(__NR_kdebug_typefilter),                       // 177
// _____(__NR_clonefileat),                             // 462
// _____(__NR_renameatx_np),                            // 488
   MACX_(__NR_getentropy, getentropy),                  // 500
// _____(__NR_necp_open),                               // 501
// _____(__NR_necp_client_action),                      // 502
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(503)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(504)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(505)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(506)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(507)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(508)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(509)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(510)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(511)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(512)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(513)),        // ???
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(514)),        // ???
   MACX_(__NR_ulock_wait, ulock_wait),                  // 515
   MACX_(__NR_ulock_wake, ulock_wake),                  // 516
// _____(__NR_fclonefileat),                            // 517
// _____(__NR_fs_snapshot),                             // 518
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(519)),        // ???
// _____(__NR_terminate_with_payload),                  // 520
// _____(__NR_abort_with_payload),                      // 521
#endif
#if DARWIN_VERS >= DARWIN_10_13
// _____(__NR_thread_selfcounts),                       // 186
// _____(__NR_kevent_id,                                // 375
// _____(__NR_necp_session_open),                       // 522
// _____(__NR_necp_session_action),                     // 523
// _____(__NR_setattrlistat),                           // 524
// _____(__NR_net_qos_guideline),                       // 525
// _____(__NR_fmount),                                  // 526
// _____(__NR_ntp_adjtime),                             // 527
// _____(__NR_ntp_gettime),                             // 528
// _____(__NR_os_fault_with_payload),                   // 529
#endif
// _____(__NR_MAXSYSCALL)
   MACX_(__NR_DARWIN_FAKE_SIGRETURN, FAKE_SIGRETURN)
};


// Mach traps use negative syscall numbers. 
// Use ML_(mach_trap_table)[-mach_trap_number] .
// cf xnu sources osfmk/kern/syscall_sw.c

const SyscallTableEntry ML_(mach_trap_table)[] = {
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(0)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(1)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(2)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(3)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(4)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(5)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(6)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(7)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(8)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(9)), 

#  if DARWIN_VERS >= DARWIN_10_8
   MACXY(__NR_kernelrpc_mach_vm_allocate_trap, kernelrpc_mach_vm_allocate_trap),
#  else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(10)), 
#  endif

   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(11)), 

#  if DARWIN_VERS >= DARWIN_10_8
   MACXY(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(12), kernelrpc_mach_vm_deallocate_trap),
#  else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(12)), 
#  endif

   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(13)), 

#  if DARWIN_VERS >= DARWIN_10_8
   MACXY(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(14), kernelrpc_mach_vm_protect_trap),
#  endif

#  if DARWIN_VERS >= DARWIN_10_9
   MACXY(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(15), kernelrpc_mach_vm_map_trap),
#  endif

#  if DARWIN_VERS < DARWIN_10_8
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(14)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(15)), 
#  endif

#  if DARWIN_VERS >= DARWIN_10_8
   MACXY(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(16), kernelrpc_mach_port_allocate_trap),
   MACX_(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(17), kernelrpc_mach_port_destroy_trap),
   MACX_(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(18), kernelrpc_mach_port_deallocate_trap),
   MACX_(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(19), kernelrpc_mach_port_mod_refs_trap),
   MACX_(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(20), kernelrpc_mach_port_move_member_trap),
   MACX_(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(21), kernelrpc_mach_port_insert_right_trap),
   MACX_(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(22), kernelrpc_mach_port_insert_member_trap),
   MACX_(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(23), kernelrpc_mach_port_extract_member_trap),
#  else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(16)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(17)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(18)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(19)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(20)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(21)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(22)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(23)), 
#  endif

#  if DARWIN_VERS >= DARWIN_10_9
   MACXY(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(24), kernelrpc_mach_port_construct_trap),
   MACX_(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(25), kernelrpc_mach_port_destruct_trap),
#  else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(24)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(25)),
#  endif

   MACXY(__NR_mach_reply_port, mach_reply_port), 
   MACXY(__NR_thread_self_trap, mach_thread_self), 
   MACXY(__NR_task_self_trap, mach_task_self), 
   MACXY(__NR_host_self_trap, mach_host_self), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(30)), 
   MACXY(__NR_mach_msg_trap, mach_msg), 
// _____(__NR_mach_msg_overwrite_trap), 
   MACX_(__NR_semaphore_signal_trap, semaphore_signal), 
   MACX_(__NR_semaphore_signal_all_trap, semaphore_signal_all), 
   MACX_(__NR_semaphore_signal_thread_trap, semaphore_signal_thread), 
   MACX_(__NR_semaphore_wait_trap, semaphore_wait), 
   MACX_(__NR_semaphore_wait_signal_trap, semaphore_wait_signal), 
   MACX_(__NR_semaphore_timedwait_trap, semaphore_timedwait), 
   MACX_(__NR_semaphore_timedwait_signal_trap, semaphore_timedwait_signal), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(40)),    // -40

#  if DARWIN_VERS >= DARWIN_10_9
   MACX_(__NR_kernelrpc_mach_port_guard_trap, kernelrpc_mach_port_guard_trap),
   MACX_(__NR_kernelrpc_mach_port_unguard_trap, kernelrpc_mach_port_unguard_trap),
#  else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(41)),
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(42)),
#  endif

#  if DARWIN_VERS >= DARWIN_10_12
   MACXY(__NR_mach_generate_activity_id, mach_generate_activity_id),
#  elif DARWIN_VERS >= DARWIN_10_10
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(43)),
#  elif defined(VGA_x86) || DARWIN_VERS == DARWIN_10_9
// _____(__NR_map_fd),
#  else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(43)),
#  endif

// _____(__NR_task_name_for_pid), 
   MACXY(__NR_task_for_pid, task_for_pid), 
   MACXY(__NR_pid_for_task, pid_for_task), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(47)), 
#if defined(VGA_x86)
// _____(__NR_macx_swapon), 
// _____(__NR_macx_swapoff), 
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(48)),
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(49)),
#endif
#if DARWIN_VERS >= DARWIN_10_13
// _____(__NR_thread_get_special_reply_port,            // 50
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(50)), 
#endif /* DARWIN_VERS >= DARWIN_10_13 */
#if defined(VGA_x86)
// _____(__NR_macx_triggers), 
// _____(__NR_macx_backing_store_suspend), 
// _____(__NR_macx_backing_store_recovery), 
#else
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(51)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(52)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(53)),
#endif
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(54)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(55)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(56)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(57)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(58)), 
   MACX_(__NR_swtch_pri, swtch_pri), 
   MACX_(__NR_swtch, swtch),   // -60
   MACX_(__NR_syscall_thread_switch, syscall_thread_switch), 
// _____(__NR_clock_sleep_trap), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(63)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(64)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(65)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(66)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(67)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(68)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(69)),
#if DARWIN_VERS >= DARWIN_10_12
   MACX_(__NR_host_create_mach_voucher_trap, host_create_mach_voucher_trap),
#else 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(70)), 
#endif
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(71)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(72)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(73)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(74)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(75)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(76)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(77)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(78)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(79)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(80)),   // -80
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(81)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(82)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(83)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(84)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(85)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(86)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(87)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(88)), 
   MACXY(__NR_mach_timebase_info, mach_timebase_info), 
   MACX_(__NR_mach_wait_until, mach_wait_until), 
   MACXY(__NR_mk_timer_create, mk_timer_create), 
   MACXY(__NR_mk_timer_destroy, mk_timer_destroy), 
   MACX_(__NR_mk_timer_arm, mk_timer_arm), 
   MACXY(__NR_mk_timer_cancel, mk_timer_cancel), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(95)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(96)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(97)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(98)), 
   _____(VG_DARWIN_SYSCALL_CONSTRUCT_MACH(99)), 
   MACXY(__NR_iokit_user_client_trap, iokit_user_client_trap), // -100
};


// Machine-dependent traps have wacky syscall numbers, and use the Mach trap 
// calling convention instead of the syscall convention.
// Use ML_(mdep_trap_table)[syscallno - ML_(mdep_trap_base)] .

#if defined(VGA_x86)
const SyscallTableEntry ML_(mdep_trap_table)[] = {
   MACX_(__NR_thread_fast_set_cthread_self, thread_fast_set_cthread_self), 
};
#elif defined(VGA_amd64)
const SyscallTableEntry ML_(mdep_trap_table)[] = {
   MACX_(__NR_thread_fast_set_cthread_self, thread_fast_set_cthread_self), 
};
#else
#error unknown architecture
#endif

const UInt ML_(syscall_table_size) = 
            sizeof(ML_(syscall_table)) / sizeof(ML_(syscall_table)[0]);

const UInt ML_(mach_trap_table_size) = 
            sizeof(ML_(mach_trap_table)) / sizeof(ML_(mach_trap_table)[0]);

const UInt ML_(mdep_trap_table_size) = 
            sizeof(ML_(mdep_trap_table)) / sizeof(ML_(mdep_trap_table)[0]);

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
