/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                 ct_threads.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2013, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

#include "global.h"

#include "pub_tool_threadstate.h"

/* forward decls */
static exec_state* exec_state_save(void);
static exec_state* exec_state_restore(void);
static exec_state* push_exec_state(int);
static exec_state* top_exec_state(void);

static exec_stack current_states;


/*------------------------------------------------------------*/
/*--- Support for multi-threading                          ---*/
/*------------------------------------------------------------*/


/*
 * For Valgrind, MT is cooperative (no preemting in our code),
 * so we don't need locks...
 *
 * Per-thread data:
 *  - BBCCs
 *  - call stack
 *  - call hash
 *  - event counters: last, current
 *
 * Even when ignoring MT, we need this functions to set up some
 * datastructures for the process (= Thread 1).
 */

/* current running thread */
ThreadId CLG_(current_tid);

static thread_info* thread[VG_N_THREADS];

thread_info** CLG_(get_threads)()
{
  return thread;
}

thread_info* CLG_(get_current_thread)()
{
  return thread[CLG_(current_tid)];
}

void CLG_(init_threads)()
{
    Int i;
    for(i=0;i<VG_N_THREADS;i++)
	thread[i] = 0;
    CLG_(current_tid) = VG_INVALID_THREADID;
}

/* switches through all threads and calls func */
void CLG_(forall_threads)(void (*func)(thread_info*))
{
  Int t, orig_tid = CLG_(current_tid);

  for(t=1;t<VG_N_THREADS;t++) {
    if (!thread[t]) continue;
    CLG_(switch_thread)(t);
    (*func)(thread[t]);
  }
  CLG_(switch_thread)(orig_tid);
}


static
thread_info* new_thread(void)
{
    thread_info* t;

    t = (thread_info*) CLG_MALLOC("cl.threads.nt.1",
                                  sizeof(thread_info));

    /* init state */
    CLG_(init_exec_stack)( &(t->states) );
    CLG_(init_call_stack)( &(t->calls) );
    CLG_(init_fn_stack)  ( &(t->fns) );
    /* t->states.entry[0]->cxt = CLG_(get_cxt)(t->fns.bottom); */

    /* event counters */
    t->lastdump_cost   = CLG_(get_eventset_cost)( CLG_(sets).full );
    t->sighandler_cost = CLG_(get_eventset_cost)( CLG_(sets).full );
    CLG_(init_cost)( CLG_(sets).full, t->lastdump_cost );
    CLG_(init_cost)( CLG_(sets).full, t->sighandler_cost );

    /* init data containers */
    CLG_(init_fn_array)( &(t->fn_active) );
    CLG_(init_bbcc_hash)( &(t->bbccs) );
    CLG_(init_jcc_hash)( &(t->jccs) );
    
    return t;
}


void CLG_(switch_thread)(ThreadId tid)
{
  if (tid == CLG_(current_tid)) return;

  CLG_DEBUG(0, ">> thread %d (was %d)\n", tid, CLG_(current_tid));

  if (CLG_(current_tid) != VG_INVALID_THREADID) {    
    /* save thread state */
    thread_info* t = thread[CLG_(current_tid)];

    CLG_ASSERT(t != 0);

    /* current context (including signal handler contexts) */
    exec_state_save();
    CLG_(copy_current_exec_stack)( &(t->states) );
    CLG_(copy_current_call_stack)( &(t->calls) );
    CLG_(copy_current_fn_stack)  ( &(t->fns) );

    CLG_(copy_current_fn_array) ( &(t->fn_active) );
    /* If we cumulate costs of threads, use TID 1 for all jccs/bccs */
    if (!CLG_(clo).separate_threads) t = thread[1];
    CLG_(copy_current_bbcc_hash)( &(t->bbccs) );
    CLG_(copy_current_jcc_hash) ( &(t->jccs) );
  }

  CLG_(current_tid) = tid;
  CLG_ASSERT(tid < VG_N_THREADS);

  if (tid != VG_INVALID_THREADID) {
    thread_info* t;

    /* load thread state */

    if (thread[tid] == 0) thread[tid] = new_thread();
    t = thread[tid];

    /* current context (including signal handler contexts) */
    CLG_(set_current_exec_stack)( &(t->states) );
    exec_state_restore();
    CLG_(set_current_call_stack)( &(t->calls) );
    CLG_(set_current_fn_stack)  ( &(t->fns) );
    
    CLG_(set_current_fn_array)  ( &(t->fn_active) );
    /* If we cumulate costs of threads, use TID 1 for all jccs/bccs */
    if (!CLG_(clo).separate_threads) t = thread[1];
    CLG_(set_current_bbcc_hash) ( &(t->bbccs) );
    CLG_(set_current_jcc_hash)  ( &(t->jccs) );
  }
}


void CLG_(run_thread)(ThreadId tid)
{
    /* check for dumps needed */
    static ULong bbs_done = 0;
    static HChar buf[512];

    if (CLG_(clo).dump_every_bb >0) {
       if (CLG_(stat).bb_executions - bbs_done > CLG_(clo).dump_every_bb) {
           VG_(sprintf)(buf, "--dump-every-bb=%llu", CLG_(clo).dump_every_bb);
	   CLG_(dump_profile)(buf, False);
           bbs_done = CLG_(stat).bb_executions;
       }
    }

    /* now check for thread switch */
    CLG_(switch_thread)(tid);
}

void CLG_(pre_signal)(ThreadId tid, Int sigNum, Bool alt_stack)
{
    exec_state *es;

    CLG_DEBUG(0, ">> pre_signal(TID %d, sig %d, alt_st %s)\n",
	     tid, sigNum, alt_stack ? "yes":"no");

    /* switch to the thread the handler runs in */
    CLG_(switch_thread)(tid);

    /* save current execution state */
    exec_state_save();

    /* setup new cxtinfo struct for this signal handler */
    es = push_exec_state(sigNum);
    CLG_(zero_cost)( CLG_(sets).full, es->cost );
    CLG_(current_state).cost = es->cost;
    es->call_stack_bottom = CLG_(current_call_stack).sp;

    /* setup current state for a spontaneous call */
    CLG_(init_exec_state)( &CLG_(current_state) );
    CLG_(current_state).sig = sigNum;
    CLG_(push_cxt)(0);
}

/* Run post-signal if the stackpointer for call stack is at
 * the bottom in current exec state (e.g. a signal handler)
 *
 * Called from CLG_(pop_call_stack)
 */
void CLG_(run_post_signal_on_call_stack_bottom)()
{
    exec_state* es = top_exec_state();
    CLG_ASSERT(es != 0);
    CLG_ASSERT(CLG_(current_state).sig >0);

    if (CLG_(current_call_stack).sp == es->call_stack_bottom)
	CLG_(post_signal)( CLG_(current_tid), CLG_(current_state).sig );
}

void CLG_(post_signal)(ThreadId tid, Int sigNum)
{
    exec_state* es;
    UInt fn_number, *pactive;

    CLG_DEBUG(0, ">> post_signal(TID %d, sig %d)\n",
	     tid, sigNum);

    /* thread switching potentially needed, eg. with instrumentation off */
    CLG_(switch_thread)(tid);
    CLG_ASSERT(sigNum == CLG_(current_state).sig);

    /* Unwind call stack of this signal handler.
     * This should only be needed at finalisation time
     */
    es = top_exec_state();
    CLG_ASSERT(es != 0);
    while(CLG_(current_call_stack).sp > es->call_stack_bottom)
      CLG_(pop_call_stack)();
    
    if (CLG_(current_state).cxt) {
      /* correct active counts */
      fn_number = CLG_(current_state).cxt->fn[0]->number;
      pactive = CLG_(get_fn_entry)(fn_number);
      (*pactive)--;
      CLG_DEBUG(0, "  set active count of %s back to %d\n",
	       CLG_(current_state).cxt->fn[0]->name, *pactive);
    }

    if (CLG_(current_fn_stack).top > CLG_(current_fn_stack).bottom) {
	/* set fn_stack_top back.
	 * top can point to 0 if nothing was executed in the signal handler;
	 * this is possible at end on unwinding handlers.
	 */
	if (*(CLG_(current_fn_stack).top) != 0) {
	    CLG_(current_fn_stack).top--;
	    CLG_ASSERT(*(CLG_(current_fn_stack).top) == 0);
	}
      if (CLG_(current_fn_stack).top > CLG_(current_fn_stack).bottom)
	CLG_(current_fn_stack).top--;
    }

    /* sum up costs */
    CLG_ASSERT(CLG_(current_state).cost == es->cost);
    CLG_(add_and_zero_cost)( CLG_(sets).full,
			    thread[CLG_(current_tid)]->sighandler_cost,
			    CLG_(current_state).cost );
    
    /* restore previous context */
    es->sig = -1;
    current_states.sp--;
    es = top_exec_state();
    CLG_(current_state).sig = es->sig;
    exec_state_restore();

    /* There is no way to reliable get the thread ID we are switching to
     * after this handler returns. So we sync with actual TID at start of
     * CLG_(setup_bb)(), which should be the next for callgrind.
     */
}



/*------------------------------------------------------------*/
/*--- Execution states in a thread & signal handlers       ---*/
/*------------------------------------------------------------*/

/* Each thread can be interrupted by a signal handler, and they
 * themselves again. But as there's no scheduling among handlers
 * of the same thread, we don't need additional stacks.
 * So storing execution contexts and
 * adding separators in the callstack(needed to not intermix normal/handler
 * functions in contexts) should be enough.
 */

/* not initialized: call_stack_bottom, sig */
void CLG_(init_exec_state)(exec_state* es)
{
  es->collect = CLG_(clo).collect_atstart;
  es->cxt  = 0;
  es->jmps_passed = 0;
  es->bbcc = 0;
  es->nonskipped = 0;
}


static exec_state* new_exec_state(Int sigNum)
{
    exec_state* es;
    es = (exec_state*) CLG_MALLOC("cl.threads.nes.1",
                                  sizeof(exec_state));

    /* allocate real cost space: needed as incremented by
     * simulation functions */
    es->cost       = CLG_(get_eventset_cost)(CLG_(sets).full);
    CLG_(zero_cost)( CLG_(sets).full, es->cost );
    CLG_(init_exec_state)(es);
    es->sig        = sigNum;
    es->call_stack_bottom  = 0;

    return es;
}

void CLG_(init_exec_stack)(exec_stack* es)
{
  Int i;

  /* The first element is for the main thread */
  es->entry[0] = new_exec_state(0);
  for(i=1;i<MAX_SIGHANDLERS;i++)
    es->entry[i] = 0;
  es->sp = 0;
}

void CLG_(copy_current_exec_stack)(exec_stack* dst)
{
  Int i;

  dst->sp = current_states.sp;
  for(i=0;i<MAX_SIGHANDLERS;i++)
    dst->entry[i] = current_states.entry[i];
}

void CLG_(set_current_exec_stack)(exec_stack* dst)
{
  Int i;

  current_states.sp = dst->sp;
  for(i=0;i<MAX_SIGHANDLERS;i++)
    current_states.entry[i] = dst->entry[i];
}


/* Get top context info struct of current thread */
static
exec_state* top_exec_state(void)
{
  Int sp = current_states.sp;
  exec_state* es;

  CLG_ASSERT((sp >= 0) && (sp < MAX_SIGHANDLERS));
  es = current_states.entry[sp];
  CLG_ASSERT(es != 0);
  return es;
}

/* Allocates a free context info structure for a new entered
 * signal handler, putting it on the context stack.
 * Returns a pointer to the structure.
 */
static exec_state* push_exec_state(int sigNum)
{   
  Int sp;
  exec_state* es;

  current_states.sp++;
  sp = current_states.sp;

  CLG_ASSERT((sigNum > 0) && (sigNum <= _VKI_NSIG));
  CLG_ASSERT((sp > 0) && (sp < MAX_SIGHANDLERS));
  es = current_states.entry[sp];
  if (!es) {
    es = new_exec_state(sigNum);
    current_states.entry[sp] = es;
  }
  else
    es->sig = sigNum;

  return es;
}

/* Save current context to top cxtinfo struct */
static
exec_state* exec_state_save(void)
{
  exec_state* es = top_exec_state();

  es->cxt         = CLG_(current_state).cxt;
  es->collect     = CLG_(current_state).collect;
  es->jmps_passed = CLG_(current_state).jmps_passed;
  es->bbcc        = CLG_(current_state).bbcc;
  es->nonskipped  = CLG_(current_state).nonskipped;
  CLG_ASSERT(es->cost == CLG_(current_state).cost);

  CLG_DEBUGIF(1) {
    CLG_DEBUG(1, "  cxtinfo_save(sig %d): collect %s, jmps_passed %d\n",
	     es->sig, es->collect ? "Yes": "No", es->jmps_passed);	
    CLG_(print_bbcc)(-9, es->bbcc);
    CLG_(print_cost)(-9, CLG_(sets).full, es->cost);
  }

  /* signal number does not need to be saved */
  CLG_ASSERT(CLG_(current_state).sig == es->sig);

  return es;
}

static
exec_state* exec_state_restore(void)
{
  exec_state* es = top_exec_state();
  
  CLG_(current_state).cxt     = es->cxt;
  CLG_(current_state).collect = es->collect;
  CLG_(current_state).jmps_passed = es->jmps_passed;
  CLG_(current_state).bbcc    = es->bbcc;
  CLG_(current_state).nonskipped = es->nonskipped;
  CLG_(current_state).cost    = es->cost;
  CLG_(current_state).sig     = es->sig;
    
  CLG_DEBUGIF(1) {
	CLG_DEBUG(1, "  exec_state_restore(sig %d): collect %s, jmps_passed %d\n",
		  es->sig, es->collect ? "Yes": "No", es->jmps_passed);
	CLG_(print_bbcc)(-9, es->bbcc);
	CLG_(print_cxt)(-9, es->cxt, 0);
	CLG_(print_cost)(-9, CLG_(sets).full, es->cost);
  }

  return es;
}
