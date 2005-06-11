
/*--------------------------------------------------------------------*/
/*--- Thread modelling.                            m_threadmodel.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind,  a dynamic binary instrumentation
   framework.

   Copyright (C) 2005 Jeremy Fitzhardinge
      jeremy@goop.org

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

/* 
   This file implements an abstract thread model, by which a client's
   thread usage is validated.  The model the file implements is
   intended to be enough to contain pthreads, or perhaps a superset,
   but it is not pthread-specific; that's done in in vg_pthreadmodel.c

   While the primary client of this file is vg_pthreadmodel.c, it is
   also intended that clients can make direct use of this file for
   home-grown threading libraries.  It is therefore useful for both
   validating the library itself as well as the users of the library.

   A note on terminology: 

   The states referred to in this file ("blocked state", "zombie
   state") are specific to this threads model, and have nothing do to
   with the scheduler status for a thread.  For example, a thread
   could be "blocked" in a lock but be in VgTs_Runnable status,
   because the lock is actually a spinlock.

   "Fails" means "reports an error" and possibly means that the model
   is getting out of sync with the actual implementation.  This model
   only reports problems to the user, and doesn't attempt to actually
   change the behaviour of the implementation.

   NB: 

   This file assumes there's a 1:1 relationship between application
   threads and Valgrind threads, which means that 1:N and M:N thread
   models are not (yet) supported.  At some point we may need to
   introduce a separate notion of a "thread" for modelling purposes.
 */

//:: 
//:: #include "core.h"
//:: 
//:: struct thread;
//:: struct mutex;
//:: struct condvar;
//:: 
//:: static const Bool debug_thread = False;
//:: static const Bool debug_mutex = False;
//:: 
//:: /* --------------------------------------------------
//::    Thread lifetime
//::    
//::    Threads are all expressed in terms of internal ThreadIds.  The
//::    thread library interface needs to map from the library's identifers
//::    to ThreadIds.
//::    -------------------------------------------------- */
//:: 
//:: /* Per-thread state.  We maintain our own here rather than hanging it
//::    off ThreadState, so that we have the option of not having a 1:1
//::    relationship between modelled threads and Valgrind threads. */
//:: struct thread
//:: {
//::    ThreadId	tid;
//::    ThreadId	creator;
//:: 
//::    Bool		detached;	/* thread is detached */
//:: 
//::    enum thread_state {
//::       TS_Alive,			/* alive */
//::       TS_Zombie,		/* waiting to be joined on (detached is False) */
//::       TS_Dead,			/* all dead */
//::       
//::       TS_Running,		/* running */
//::       TS_MutexBlocked,		/* blocked on mutex */
//::       TS_CVBlocked,		/* blocked on condvar */
//::       TS_JoinBlocked,		/* blocked in join */
//::    }		state;
//:: 
//::    struct mutex		*mx_blocked; /* mutex we're blocked on (state==TS_MutexBlocked) */
//::    struct condvar	*cv_blocked; /* condvar we're blocked on (state==TS_CVBlocked) */
//::    struct thread	*th_blocked; /* thread we're blocked on (state==TS_JoinBlocked) */
//:: 
//::    ExeContext	*ec_created;	/* where created */
//::    ExeContext	*ec_blocked;	/* where blocked/unblocked */
//:: };
//:: 
//:: enum thread_error
//:: {
//::    THE_NotExist,		/* thread doesn't exist */
//::    THE_NotAlive,		/* thread isn't alive (use after death) */
//::    THE_Rebirth,			/* thread already alive */
//::    THE_Blocked,			/* thread not supposed to be blocked */
//::    THE_NotBlocked,		/* thread supposed to be blocked */
//::    THE_Detached,		/* thread is detached */
//:: };
//:: 
//:: struct thread_error_data
//:: {
//::    enum thread_error err;
//::    struct thread     *th;
//::    const Char        *action;
//:: };
//:: 
//:: static const Char *pp_threadstate(const struct thread *th)
//:: {
//::    if (th == NULL)
//::       return "non-existent";
//:: 
//::    switch(th->state) {
//::    case TS_Alive:	return "alive";
//::    case TS_Zombie:	return "zombie";
//::    case TS_Dead:	return "dead";
//::    case TS_Running:	return "running";
//::    case TS_MutexBlocked:return "mutex-blocked";
//::    case TS_CVBlocked:	return "cv-blocked";
//::    case TS_JoinBlocked:	return "join-blocked";
//::    default:		return "???";
//::    }
//:: }
//:: 
//:: static void thread_validate(struct thread *th)
//:: {
//::    switch(th->state) {
//::    case TS_Alive:
//::    case TS_Running:
//::    case TS_Dead:
//::    case TS_Zombie:
//::       vg_assert(th->mx_blocked == NULL);
//::       vg_assert(th->cv_blocked == NULL);
//::       vg_assert(th->th_blocked == NULL);
//::       break;
//:: 
//::    case TS_MutexBlocked:
//::       vg_assert(th->mx_blocked != NULL);
//::       vg_assert(th->cv_blocked == NULL);
//::       vg_assert(th->th_blocked == NULL);
//::       break;
//:: 
//::    case TS_CVBlocked:
//::       vg_assert(th->mx_blocked == NULL);
//::       vg_assert(th->cv_blocked != NULL);
//::       vg_assert(th->th_blocked == NULL);
//::       break;
//:: 
//::    case TS_JoinBlocked:
//::       vg_assert(th->mx_blocked == NULL);
//::       vg_assert(th->cv_blocked == NULL);
//::       vg_assert(th->th_blocked != NULL);
//::       break;
//::    }
//:: }
//:: 
//:: static void thread_setstate(struct thread *th, enum thread_state state)
//:: {
//::    ExeContext *ec;
//:: 
//::    if (th->state == state)
//::       return;
//:: 
//::    ec = VG_(record_ExeContext)(th->tid);
//:: 
//::    switch(state) {
//::    case TS_Alive:
//::    case TS_Dead:
//::       th->ec_created = ec;
//::       break;
//:: 
//::    case TS_Running:
//::    case TS_MutexBlocked:
//::    case TS_CVBlocked:
//::    case TS_JoinBlocked:
//::    case TS_Zombie:
//::       th->ec_blocked = ec;
//::    }
//:: 
//::    th->state = state;
//::    if (debug_thread)
//::       VG_(printf)("setting thread(%d) -> %s\n", th->tid, pp_threadstate(th));
//::    thread_validate(th);
//:: }
//:: 
//:: static void do_thread_run(struct thread *th)
//:: {
//::    th->mx_blocked = NULL;
//::    th->cv_blocked = NULL;
//::    th->th_blocked = NULL;
//::    thread_setstate(th, TS_Running);
//:: }
//:: 
//:: static void do_thread_block_mutex(struct thread *th, struct mutex *mx)
//:: {
//::    th->mx_blocked = mx;
//::    th->cv_blocked = NULL;
//::    th->th_blocked = NULL;
//::    thread_setstate(th, TS_MutexBlocked);
//:: }
//:: 
//:: static void do_thread_block_condvar(struct thread *th, struct condvar *cv)
//:: {
//::    th->mx_blocked = NULL;
//::    th->cv_blocked = cv;
//::    th->th_blocked = NULL;
//::    thread_setstate(th, TS_CVBlocked);
//:: }
//:: 
//:: static void do_thread_block_join(struct thread *th, struct thread *joinee)
//:: {
//::    th->mx_blocked = NULL;
//::    th->cv_blocked = NULL;
//::    th->th_blocked = joinee;
//::    thread_setstate(th, TS_JoinBlocked);
//:: }
//:: 
//:: static void do_thread_block_zombie(struct thread *th)
//:: {
//::    th->mx_blocked = NULL;
//::    th->cv_blocked = NULL;
//::    th->th_blocked = NULL;
//::    thread_setstate(th, TS_Zombie);
//:: }
//:: 
//:: static void do_thread_dead(struct thread *th)
//:: {
//::    th->mx_blocked = NULL;
//::    th->cv_blocked = NULL;
//::    th->th_blocked = NULL;
//::    thread_setstate(th, TS_Dead);
//:: }
//:: 
//:: static SkipList sk_threads = VG_SKIPLIST_INIT(struct thread, tid, VG_(cmp_UInt), NULL, VG_AR_CORE);
//:: 
//:: static struct thread *thread_get(ThreadId tid)
//:: {
//::    return VG_(SkipList_Find_Exact)(&sk_threads, &tid);
//:: }
//:: 
//:: static void thread_report(ThreadId tid, enum thread_error err, const Char *action)
//:: {
//::    Char *errstr = "?";
//::    struct thread *th = thread_get(tid);
//::    struct thread_error_data errdata;
//:: 
//::    switch(err) {
//::    case THE_NotExist:	errstr = "non existent"; break;
//::    case THE_NotAlive:	errstr = "not alive"; break;
//::    case THE_Rebirth:	errstr = "re-born"; break;
//::    case THE_Blocked:	errstr = "blocked"; break;
//::    case THE_NotBlocked:	errstr = "not blocked"; break;
//::    case THE_Detached:	errstr = "detached"; break;
//::    }
//:: 
//::    errdata.err = err;
//::    errdata.th = th;
//::    errdata.action = action;
//::    
//::    VG_(maybe_record_error)(VG_(get_running_tid)(), ThreadErr, 0, errstr, &errdata);
//:: }
//:: 
//:: static void pp_thread_error(Error *err)
//:: {
//::    struct thread_error_data *errdata = VG_(get_error_extra)(err);
//::    struct thread *th = errdata->th;
//::    Char *errstr = VG_(get_error_string)(err);
//::    
//::    VG_(message)(Vg_UserMsg, "Found %s thread in state %s while %s",
//::                 errstr, pp_threadstate(th), errdata->action);
//::    VG_(pp_ExeContext)(VG_(get_error_where)(err));
//:: 
//::    if (th) {
//::       VG_(message)(Vg_UserMsg, " Thread %d was %s",
//:: 		   th->tid, th->state == TS_Dead ? "destroyed" : "created");
//::       VG_(pp_ExeContext)(th->ec_created);
//::    }
//:: }
//:: 
//:: /* Thread creation */
//:: void VG_(tm_thread_create)(ThreadId creator, ThreadId tid, Bool detached)
//:: {
//::    struct thread *th = thread_get(tid);
//:: 
//::    if (debug_thread)
//::       VG_(printf)("thread %d creates %d %s\n", creator, tid, detached ? "detached" : "");
//::    if (th != NULL) {
//::       if (th->state != TS_Dead)
//:: 	 thread_report(tid, THE_Rebirth, "creating");
//::    } else {
//::       th = VG_(SkipNode_Alloc)(&sk_threads);
//::       th->tid = tid;
//::       VG_(SkipList_Insert)(&sk_threads, th);
//::    }
//:: 
//::    th->creator = creator;
//::    th->detached = detached;
//::    th->mx_blocked = NULL;
//::    th->cv_blocked = NULL;
//::    th->th_blocked = NULL;
//:: 
//::    thread_setstate(th, TS_Alive);
//::    do_thread_run(th);
//:: }
//:: 
//:: Bool VG_(tm_thread_exists)(ThreadId tid)
//:: {
//::    struct thread *th = thread_get(tid);
//:: 
//::    return th && th->state != TS_Dead;
//:: }
//:: 
//:: /* A thread is terminating itself
//::     - fails if tid has already terminated
//::     - if detached, tid becomes invalid for all further operations
//::     - if not detached, the thread remains in a Zombie state until
//::       someone joins on it
//::  */
//:: void VG_(tm_thread_exit)(ThreadId tid)
//:: {
//::    struct thread *th = thread_get(tid);
//:: 
//::    if (th == NULL)
//::       thread_report(tid, THE_NotExist, "exiting");
//::    else {
//::       struct thread *joiner;
//:: 
//::       switch(th->state) {
//::       case TS_Dead:
//::       case TS_Zombie:	/* already exited once */
//:: 	 thread_report(tid, THE_NotAlive, "exiting");
//:: 	 break;
//:: 
//::       case TS_MutexBlocked:
//::       case TS_CVBlocked:
//::       case TS_JoinBlocked:
//:: 	    thread_report(tid, THE_Blocked, "exiting");
//:: 	    break;
//:: 
//::       case TS_Alive:
//::       case TS_Running:
//:: 	 /* OK */
//:: 	 break;
//::       }
//:: 
//::       /* ugly - walk all threads to find people joining with us */
//::       /* In pthreads its an error to have multiple joiners, but that
//:: 	 seems a bit specific to implement here; there should a way
//:: 	 for the thread library binding to handle this. */
//::       for(joiner = VG_(SkipNode_First)(&sk_threads);
//:: 	  joiner != NULL;
//:: 	  joiner = VG_(SkipNode_Next)(&sk_threads, joiner)) {
//:: 	 if (joiner->state == TS_JoinBlocked && joiner->th_blocked == th) {
//:: 	    /* found someone - wake them up */
//:: 	    do_thread_run(joiner);
//:: 
//:: 	    /* we're dead */
//:: 	    do_thread_dead(th);
//:: 	 }
//::       }
//:: 
//::       if (th->state != TS_Dead)
//:: 	 do_thread_block_zombie(th);
//::    }
//:: }
//:: 
//:: void VG_(tm_thread_detach)(ThreadId tid)
//:: {
//::    struct thread *th = thread_get(tid);
//:: 
//::    if (th == NULL)
//::       thread_report(tid, THE_NotExist, "detaching");
//::    else {
//::       if (th->detached)
//:: 	 thread_report(tid, THE_Detached, "detaching");
//::       else {
//:: 	 /* XXX look for waiters */
//:: 	 th->detached = True;
//::       }
//::    }
//:: }
//:: 
//:: /* One thread blocks until another has terminated
//::     - fails if joinee is detached
//::     - fails if joinee doesn't exist
//::     - once the join completes, joinee is dead
//::  */
//:: void VG_(tm_thread_join)(ThreadId joinerid, ThreadId joineeid)
//:: {
//::    struct thread *joiner = thread_get(joinerid);
//::    struct thread *joinee = thread_get(joineeid);
//:: 
//::    /* First, check the joinee thread's state */
//::    if (joinee == NULL)
//::       thread_report(joineeid, THE_NotExist, "joining as joinee");
//::    else {
//::       switch(joinee->state) {
//::       case TS_Alive:		/* really shouldn't see them in this state... */
//::       case TS_Running:
//::       case TS_Zombie:
//::       case TS_MutexBlocked:
//::       case TS_CVBlocked:
//::       case TS_JoinBlocked:
//:: 	 /* OK */
//:: 	 break;
//:: 
//::       case TS_Dead:
//:: 	 thread_report(joineeid, THE_NotAlive, "joining as joinee");
//:: 	 break;
//::       }
//::    }
//:: 
//::    /* now the joiner... */
//::    if (joiner == NULL)
//::       thread_report(joineeid, THE_NotExist, "joining as joiner");
//::    else {
//::       switch(joiner->state) {
//::       case TS_Alive:		/* ? */
//::       case TS_Running:		/* OK */
//:: 	 break;
//:: 
//::       case TS_Zombie:		/* back from the dead */
//::       case TS_Dead:
//:: 	 thread_report(joineeid, THE_NotAlive, "joining as joiner");
//:: 	 break;
//:: 
//::       case TS_MutexBlocked:
//::       case TS_CVBlocked:
//::       case TS_JoinBlocked:
//:: 	 thread_report(joineeid, THE_Blocked, "joining as joiner");
//:: 	 break;
//::       }
//:: 
//::       if (joinee->detached)
//:: 	 thread_report(joineeid, THE_Detached, "joining as joiner");
//::       else {
//:: 	 /* block if the joinee hasn't exited yet */
//:: 	 if (joinee) {
//:: 	    switch(joinee->state) {
//:: 	    case TS_Dead:
//:: 	       break;
//:: 
//:: 	    default:
//:: 	       if (joinee->state == TS_Zombie)
//:: 		  do_thread_dead(joinee);
//:: 	       else
//:: 		  do_thread_block_join(joiner, joinee);
//:: 	    }
//:: 	 }
//::       }
//::    }
//:: }
//:: 
//:: /* Context switch to a new thread */
//:: void VG_(tm_thread_switchto)(ThreadId tid)
//:: {
//::    VG_TRACK( thread_run, tid );
//:: }
//:: 
//:: static void thread_block_mutex(ThreadId tid, struct mutex *mx)
//:: {
//::    struct thread *th = thread_get(tid);
//:: 
//::    if (th == NULL) {
//::       /* should an unknown thread doing something make it spring to life? */
//::       thread_report(tid, THE_NotExist, "blocking on mutex");
//::       return;
//::    }
//::    switch(th->state) {
//::    case TS_Dead:
//::    case TS_Zombie:
//::       thread_report(th->tid, THE_NotAlive, "blocking on mutex");
//::       break;
//:: 
//::    case TS_MutexBlocked:
//::    case TS_CVBlocked:
//::    case TS_JoinBlocked:
//::       thread_report(th->tid, THE_Blocked, "blocking on mutex");
//::       break;
//:: 
//::    case TS_Alive:
//::    case TS_Running:		/* OK */
//::       break;
//::    }
//:: 
//::    do_thread_block_mutex(th, mx);
//:: }
//:: 
//:: static void thread_unblock_mutex(ThreadId tid, struct mutex *mx, const Char *action)
//:: {
//::    struct thread *th = thread_get(tid);
//:: 
//::    if (th == NULL) {
//::       /* should an unknown thread doing something make it spring to life? */
//::       thread_report(tid, THE_NotExist, "giving up on mutex");
//::       return;
//::    }
//:: 
//::    switch(th->state) {
//::    case TS_MutexBlocked:	/* OK */
//::       break;
//:: 
//::    case TS_Alive:
//::    case TS_Running:
//::       thread_report(tid, THE_NotBlocked, action);
//::       break;
//:: 
//::    case TS_CVBlocked:
//::    case TS_JoinBlocked:
//::       thread_report(tid, THE_Blocked, action);
//::       break;
//:: 
//::    case TS_Dead:
//::    case TS_Zombie:
//::       thread_report(tid, THE_NotAlive, action);
//::       break;
//::    }
//:: 
//::    do_thread_run(th);
//:: }
//:: 
//:: /* --------------------------------------------------
//::    Mutexes
//:: 
//::    This models simple, non-recursive mutexes.
//::    -------------------------------------------------- */
//:: 
//:: struct mutex
//:: {
//::    Addr		mutex;		/* address of mutex */
//::    ThreadId	owner;		/* owner if state == MX_Locked */
//::    enum mutex_state {
//::       MX_Init,
//::       MX_Free,
//::       MX_Locked,
//::       MX_Unlocking,		/* half-unlocked */
//::       MX_Dead
//::    }		state;		/* mutex state */
//:: 
//::    ExeContext	*ec_create;	/* where created/destroyed */
//::    ExeContext	*ec_locked;	/* where last locked/unlocked */
//:: };
//:: 
//:: enum mutex_error
//:: {
//::    MXE_NotExist,		/* never existed */
//::    MXE_NotInit,			/* not initialized (use after destroy) */
//::    MXE_ReInit,			/* already initialized */
//::    MXE_NotLocked,		/* not locked */
//::    MXE_Locked,			/* is locked */
//::    MXE_Deadlock,		/* deadlock detected */
//::    MXE_NotOwner,		/* non-owner trying to change lock */
//:: };
//:: 
//:: struct mutex_error_data
//:: {
//::    enum mutex_error err;
//::    struct mutex     *mx;
//::    const Char       *action;
//:: };
//:: 
//:: static struct mutex *mutex_get(Addr mutexp);
//:: 
//:: static const Char *pp_mutexstate(const struct mutex *mx)
//:: {
//::    static Char buf[20];
//:: 
//::    switch(mx->state) {
//::    case MX_Init: return "Init";
//::    case MX_Free: return "Free";
//::    case MX_Dead: return "Dead";
//:: 
//::    case MX_Locked:
//::       VG_(sprintf)(buf, "Locked by tid %d", mx->owner);
//::       break;
//:: 
//::    case MX_Unlocking:
//::       VG_(sprintf)(buf, "Being unlocked by tid %d", mx->owner);
//::       break;
//:: 
//::    default:
//::       VG_(sprintf)(buf, "?? %d", mx->state);
//::       break;
//::    }
//:: 
//::    return buf;
//:: }
//:: 
//:: static void mutex_setstate(ThreadId tid, struct mutex *mx, enum mutex_state st)
//:: {
//::    ExeContext *ec = VG_(record_ExeContext)(tid);
//:: 
//::    switch(st) {
//::    case MX_Init:
//::    case MX_Dead:
//::       mx->ec_create = ec;
//::       break;
//:: 
//::    case MX_Unlocking:
//::    case MX_Locked:
//::    case MX_Free:
//::       mx->ec_locked = ec;
//::       break;
//::    }
//:: 
//::    mx->state = st;
//::    if (debug_mutex)
//::       VG_(printf)("setting mutex(%p) -> %s\n", mx->mutex, pp_mutexstate(mx));
//:: }
//:: 
//:: static void mutex_report(ThreadId tid, Addr mutexp, enum mutex_error err, const Char *action)
//:: {
//::    Char *errstr="?";
//::    struct mutex *mx = mutex_get(mutexp);
//::    struct mutex_error_data errdata;
//:: 
//::    switch(err) {
//::    case MXE_NotExist:		errstr="non-existent"; break;
//::    case MXE_NotInit:		errstr="uninitialized"; break;
//::    case MXE_ReInit:		errstr="already initialized"; break;
//::    case MXE_NotLocked:		errstr="not locked"; break;
//::    case MXE_Locked:		errstr="locked"; break;
//::    case MXE_NotOwner:		errstr="unowned"; break;
//::    case MXE_Deadlock:		errstr="deadlock on"; break;
//::    }
//:: 
//::    errdata.err = err;
//::    errdata.mx = mx;
//::    errdata.action = action;
//::    
//::    VG_(maybe_record_error)(tid, MutexErr, 0, errstr, &errdata);
//:: }
//:: 
//:: static void pp_mutex_error(Error *err)
//:: {
//::    struct mutex_error_data *errdata = VG_(get_error_extra)(err);
//::    struct mutex *mx = errdata->mx;
//::    Char *errstr = VG_(get_error_string)(err);
//::    
//::    VG_(message)(Vg_UserMsg, "Found %s mutex %p while %s",
//::                 errstr, mx ? mx->mutex : 0, errdata->action);
//::    VG_(pp_ExeContext)(VG_(get_error_where)(err));
//:: 
//::    switch (mx->state) {
//::       case MX_Init:
//::       case MX_Dead:
//::          break;
//::       case MX_Locked:
//::          VG_(message)(Vg_UserMsg, " Mutex was locked by thread %d", mx->owner);
//::          VG_(pp_ExeContext)(mx->ec_locked);
//::          break;
//::       case MX_Unlocking:
//::          VG_(message)(Vg_UserMsg, " Mutex being unlocked");
//::          VG_(pp_ExeContext)(mx->ec_locked);
//::          break;
//::       case MX_Free:
//::          VG_(message)(Vg_UserMsg, " Mutex was unlocked");
//::          VG_(pp_ExeContext)(mx->ec_locked);
//::          break;
//::    }
//:: 
//::    VG_(message)(Vg_UserMsg, " Mutex was %s",
//::                 mx->state == MX_Dead ? "destroyed" : "created");
//::    VG_(pp_ExeContext)(mx->ec_create);
//:: }
//:: 
//:: static SkipList sk_mutex = VG_SKIPLIST_INIT(struct mutex, mutex, VG_(cmp_Addr), NULL, VG_AR_CORE);
//:: 
//:: static struct mutex *mutex_get(Addr mutexp)
//:: {
//::    return VG_(SkipList_Find_Exact)(&sk_mutex, &mutexp);
//:: }
//:: 
//:: static Bool mx_is_initialized(Addr mutexp)
//:: {
//::    const struct mutex *mx = mutex_get(mutexp);
//:: 
//::    return mx && mx->state != MX_Dead;
//:: }
//:: 
//:: static struct mutex *mutex_check_initialized(ThreadId tid, Addr mutexp, const Char *action)
//:: {
//::    struct mutex *mx;
//:: 
//::    vg_assert(tid != VG_INVALID_THREADID);
//:: 
//::    if (!mx_is_initialized(mutexp)) {
//::       mutex_report(tid, mutexp, MXE_NotInit, action);
//::       VG_(tm_mutex_init)(tid, mutexp);
//::    }
//:: 
//::    mx = mutex_get(mutexp);
//::    vg_assert(mx != NULL);
//:: 
//::    return mx;
//:: }
//:: 
//:: #if 0
//:: static Bool mx_is_locked(Addr mutexp)
//:: {
//::    const struct mutex *mx = mutex_get(mutexp);
//:: 
//::    return mx && (mx->state == MX_Locked);
//:: }
//:: #endif
//:: 
//:: /* Mutex at mutexp is initialized.  This must be done before any
//::    further mutex operations are OK. Fails if:
//::     - mutexp already exists (and is locked?)
//:: */
//:: void VG_(tm_mutex_init)(ThreadId tid, Addr mutexp)
//:: {
//::    struct mutex *mx = mutex_get(mutexp);
//:: 
//::    if (mx == NULL) {
//::       mx = VG_(SkipNode_Alloc)(&sk_mutex);
//::       mx->mutex = mutexp;
//::       VG_(SkipList_Insert)(&sk_mutex, mx);
//::    } else if (mx->state != MX_Dead)
//::       mutex_report(tid, mutexp, MXE_ReInit, "initializing");
//:: 
//::    mx->owner = VG_INVALID_THREADID;
//:: 
//::    mutex_setstate(tid, mx, MX_Init);
//::    mutex_setstate(tid, mx, MX_Free);
//:: }
//:: 
//:: Bool VG_(tm_mutex_exists)(Addr mutexp)
//:: {
//::    return mx_is_initialized(mutexp);
//:: }
//:: 
//:: /* Mutex is being destroyed.  Fails if:
//::     - mutex was not initialized
//::     - mutex is locked (?)
//::  */
//:: void VG_(tm_mutex_destroy)(ThreadId tid, Addr mutexp)
//:: {
//::    struct mutex *mx = mutex_get(mutexp);
//:: 
//::    if (mx == NULL)
//::       mutex_report(tid, mutexp, MXE_NotExist, "destroying");
//::    else {
//::       switch(mx->state) {
//::       case MX_Dead:
//:: 	 mutex_report(tid, mutexp, MXE_NotInit, "destroying");
//:: 	 break;
//:: 
//::       case MX_Locked:
//::       case MX_Unlocking:
//:: 	 mutex_report(tid, mutexp, MXE_Locked, "destroying");
//:: 	 VG_(tm_mutex_unlock)(tid, mutexp);
//:: 	 break;
//:: 
//::       case MX_Init:
//::       case MX_Free:
//:: 	 /* OK */
//:: 	 break;
//::       }
//::       mutex_setstate(tid, mx, MX_Dead);
//::    }
//:: }
//:: 
//:: /* A thread attempts to lock a mutex.  If "blocking" then the thread
//::    is put into a blocked state until the lock is acquired.  Fails if:
//::     - tid is invalid
//::     - mutex has not been initialized
//::     - thread is blocked on another object (?)
//::     - blocking on this mutex could cause a deadlock
//::    (Lock rank detection?)
//::  */
//:: void VG_(tm_mutex_trylock)(ThreadId tid, Addr mutexp)
//:: {
//::    struct mutex *mx;
//:: 
//::    mx = mutex_check_initialized(tid, mutexp, "trylocking");
//:: 
//::    thread_block_mutex(tid, mx);
//:: 
//::    if (mx->state == MX_Locked && mx->owner == tid) /* deadlock */
//::       mutex_report(tid, mutexp, MXE_Deadlock, "trylocking");
//:: 
//::    VG_TRACK( pre_mutex_lock, tid, (void *)mutexp );
//:: }
//:: 
//:: /* Give up waiting for a mutex.  Fails if:
//::     - thread is not currently blocked on the mutex
//::  */
//:: void VG_(tm_mutex_giveup)(ThreadId tid, Addr mutexp)
//:: {
//::    struct mutex *mx;
//:: 
//::    mx = mutex_check_initialized(tid, mutexp, "giving up");
//:: 
//::    thread_unblock_mutex(tid, mx, "giving up on mutex");
//:: }
//:: 
//:: /* A thread acquires a mutex.  Fails if:
//::     - thread is not blocked waiting for the mutex
//::     - mutex is held by another thread
//::  */
//:: void VG_(tm_mutex_acquire)(ThreadId tid, Addr mutexp)
//:: {
//::    struct mutex *mx;
//:: 
//::    mx = mutex_check_initialized(tid, mutexp, "acquiring");   
//::    
//::    switch(mx->state) {
//::    case MX_Unlocking:		/* ownership transfer or relock */
//::       VG_TRACK( post_mutex_unlock, mx->owner, (void *)mutexp );
//::       if (mx->owner != tid)
//:: 	 thread_unblock_mutex(tid, mx, "acquiring mutex");
//::       break;
//:: 
//::    case MX_Free:
//::       thread_unblock_mutex(tid, mx, "acquiring mutex");
//::       break;
//:: 
//::    case MX_Locked:
//::       if (debug_mutex)
//:: 	 VG_(printf)("mutex=%p mx->state=%s\n", mutexp, pp_mutexstate(mx));
//::       VG_TRACK( post_mutex_unlock, mx->owner, (void *)mutexp );
//::       mutex_report(tid, mutexp, MXE_Locked, "acquiring");
//::       thread_unblock_mutex(tid, mx, "acquiring mutex");
//::       break;
//:: 
//::    case MX_Init:
//::    case MX_Dead:
//::       vg_assert(0);
//::    } 
//::      
//::    mx->owner = tid;
//::    mutex_setstate(tid, mx, MX_Locked);
//:: 
//::    VG_TRACK( post_mutex_lock, tid, (void *)mutexp );
//:: }
//:: 
//:: /* Try unlocking a lock.  This will move it into a state where it can
//::    either be unlocked, or change ownership to another thread.  If
//::    unlock fails, it will remain locked. */
//:: void VG_(tm_mutex_tryunlock)(ThreadId tid, Addr mutexp)
//:: {
//::    struct thread *th;
//::    struct mutex *mx;
//:: 
//::    mx = mutex_check_initialized(tid, mutexp, "try-unlocking");
//:: 
//::    th = thread_get(tid);
//:: 
//::    if (th == NULL)
//::       thread_report(tid, THE_NotExist, "try-unlocking mutex");
//::    else {
//::       switch(th->state) {
//::       case TS_Alive:
//::       case TS_Running:		/* OK */
//:: 	 break;
//:: 
//::       case TS_Dead:
//::       case TS_Zombie:
//:: 	 thread_report(tid, THE_NotAlive, "try-unlocking mutex");
//:: 	 break;
//:: 
//::       case TS_JoinBlocked:
//::       case TS_CVBlocked:
//::       case TS_MutexBlocked:
//:: 	 thread_report(tid, THE_Blocked, "try-unlocking mutex");
//:: 	 do_thread_run(th);
//:: 	 break;
//::       }
//::    }
//:: 
//::    switch(mx->state) {
//::    case MX_Locked:
//::       if (mx->owner != tid)
//:: 	 mutex_report(tid, mutexp, MXE_NotOwner, "try-unlocking");
//::       break;
//:: 
//::    case MX_Free:
//::       mutex_report(tid, mutexp, MXE_NotLocked, "try-unlocking");
//::       break;
//:: 
//::    case MX_Unlocking:
//::       mutex_report(tid, mutexp, MXE_NotLocked, "try-unlocking");
//::       break;
//:: 
//::    case MX_Init:
//::    case MX_Dead:
//::       vg_assert(0);
//::    }
//:: 
//::    mutex_setstate(tid, mx, MX_Unlocking);
//:: }
//:: 
//:: /* Finish unlocking a Mutex.  The mutex can validly be in one of three
//::    states:
//::    - Unlocking
//::    - Locked, owned by someone else (someone else got it in the meantime)
//::    - Free (someone else completed a lock-unlock cycle)
//::  */
//:: void VG_(tm_mutex_unlock)(ThreadId tid, Addr mutexp)
//:: {
//::    struct mutex *mx;
//::    struct thread *th;
//:: 
//::    mx = mutex_check_initialized(tid, mutexp, "unlocking mutex");
//:: 
//::    th = thread_get(tid);
//:: 
//::    if (th == NULL)
//::       thread_report(tid, THE_NotExist, "unlocking mutex");
//::    else {
//::       switch(th->state) {
//::       case TS_Alive:
//::       case TS_Running:		/* OK */
//:: 	 break;
//:: 
//::       case TS_Dead:
//::       case TS_Zombie:
//:: 	 thread_report(tid, THE_NotAlive, "unlocking mutex");
//:: 	 break;
//:: 
//::       case TS_JoinBlocked:
//::       case TS_CVBlocked:
//::       case TS_MutexBlocked:
//:: 	 thread_report(tid, THE_Blocked, "unlocking mutex");
//:: 	 do_thread_run(th);
//:: 	 break;
//::       }
//::    }
//:: 
//::    switch(mx->state) {
//::    case MX_Locked:
//::       /* Someone else might have taken ownership in the meantime */
//::       if (mx->owner == tid)
//:: 	 mutex_report(tid, mutexp, MXE_Locked, "unlocking");
//::       break;
//:: 
//::    case MX_Free:
//::       /* OK - nothing to do */
//::       break;
//:: 
//::    case MX_Unlocking:
//::       /* OK - we need to complete the unlock */
//::       VG_TRACK( post_mutex_unlock, tid, (void *)mutexp );
//::       mutex_setstate(tid, mx, MX_Free);
//::       break;
//:: 
//::    case MX_Init:
//::    case MX_Dead:
//::       vg_assert(0);
//::    }
//:: }
//:: 
//:: /* --------------------------------------------------
//::    Condition variables
//::    -------------------------------------------------- */
//:: 
//:: struct condvar_waiter
//:: {
//::    ThreadId		waiter;
//::    
//::    struct condvar	*condvar;
//::    struct mutex		*mutex;
//:: 
//::    struct condvar_waiter	*next;
//:: };
//:: 
//:: struct condvar 
//:: {
//::    Addr		condvar;
//:: 
//::    enum condvar_state {
//::       CV_Dead,
//::       CV_Alive,
//::    }		state;
//:: 
//::    struct condvar_waiter	*waiters;	// XXX skiplist?
//::    
//::    ExeContext	*ec_created;	// where created
//::    ExeContext	*ec_signalled;	// where last signalled
//:: };
//:: 
//:: enum condvar_err {
//::    CVE_NotExist,
//::    CVE_NotInit,
//::    CVE_ReInit,
//::    CVE_Busy,
//::    CVE_Blocked,
//:: };
//:: 
//:: static SkipList sk_condvar = VG_SKIPLIST_INIT(struct condvar, condvar, VG_(cmp_Addr),
//:: 					   NULL, VG_AR_CORE);
//:: 
//:: static struct condvar *condvar_get(Addr condp)
//:: {
//::    return VG_(SkipList_Find_Exact)(&sk_condvar, &condp);
//:: }
//:: 
//:: static Bool condvar_is_initialized(Addr condp)
//:: {
//::    const struct condvar *cv = condvar_get(condp);
//:: 
//::    return cv && cv->state != CV_Dead;
//:: }
//:: 
//:: static void condvar_report(ThreadId tid, Addr condp, enum condvar_err err, const Char *action)
//:: {
//:: }
//:: 
//:: static struct condvar *condvar_check_initialized(ThreadId tid, Addr condp, const Char *action)
//:: {
//::    struct condvar *cv;
//::    vg_assert(tid != VG_INVALID_THREADID);
//::    
//::    if (!condvar_is_initialized(condp)) {
//::       condvar_report(tid, condp, CVE_NotInit, action);
//::       VG_(tm_cond_init)(tid, condp);
//::    }
//:: 
//::    cv = condvar_get(condp);
//::    vg_assert(cv != NULL);
//:: 
//::    return cv;
//:: }
//:: 
//:: /* Initialize a condition variable.  Fails if:
//::     - condp has already been initialized
//::  */
//:: void VG_(tm_cond_init)(ThreadId tid, Addr condp)
//:: {
//::    struct condvar *cv = condvar_get(condp);
//:: 
//::    if (cv == NULL) {
//::       cv = VG_(SkipNode_Alloc)(&sk_condvar);
//::       cv->condvar = condp;
//::       cv->waiters = NULL;
//::       VG_(SkipList_Insert)(&sk_condvar, cv);
//::    } else if (cv->state != CV_Dead) {
//::       condvar_report(tid, condp, CVE_ReInit, "initializing");
//::       /* ? what about existing waiters? */
//::    }
//:: 
//::    cv->state = CV_Alive;
//:: }
//:: 
//:: /* Destroy a condition variable.  Fails if:
//::     - condp has not been initialized
//::     - condp is currently being waited on
//::  */
//:: void VG_(tm_cond_destroy)(ThreadId tid, Addr condp)
//:: {
//::    struct condvar *cv = condvar_get(condp);
//:: 
//::    if (cv == NULL)
//::       condvar_report(tid, condp, CVE_NotExist, "destroying");
//::    else {
//::       if (cv->state != CV_Alive)
//:: 	 condvar_report(tid, condp, CVE_NotInit, "destroying");
//::       if (cv->waiters != NULL)
//:: 	 condvar_report(tid, condp, CVE_Busy, "destroying");
//::       cv->state = CV_Dead;
//::    }
//:: }
//:: 
//:: static struct condvar_waiter *get_waiter(const struct condvar *cv, ThreadId tid)
//:: {
//::    struct condvar_waiter *w;
//:: 
//::    for(w = cv->waiters; w; w = w->next)
//::       if (w->waiter == tid)
//:: 	 return w;
//::    return NULL;
//:: }
//:: 
//:: /* Wait for a condition, putting thread into blocked state.  Fails if:
//::     - condp has not been initialized
//::     - thread doesn't hold mutexp
//::     - thread is blocked on some other object
//::     - thread is already blocked on mutex
//::  */
//:: void VG_(tm_cond_wait)(ThreadId tid, Addr condp, Addr mutexp)
//:: {
//::    struct thread *th = thread_get(tid);
//::    struct mutex *mx;
//::    struct condvar *cv;
//::    struct condvar_waiter *waiter;
//:: 
//::    /* Condvar must exist */
//::    cv = condvar_check_initialized(tid, condp, "waiting");
//:: 
//::    /* Mutex must exist */
//::    mx = mutex_check_initialized(tid, mutexp, "waiting on condvar");
//:: 
//::    /* Thread must own mutex */
//::    if (mx->state != MX_Locked) {
//::       mutex_report(tid, mutexp, MXE_NotLocked, "waiting on condvar");
//::       VG_(tm_mutex_trylock)(tid, mutexp);
//::       VG_(tm_mutex_acquire)(tid, mutexp);
//::    } else if (mx->owner != tid) {
//::       mutex_report(tid, mutexp, MXE_NotOwner, "waiting on condvar");
//::       mx->owner = tid;
//::    }
//:: 
//::    /* Thread must not be already waiting for condvar */
//::    waiter = get_waiter(cv, tid);
//::    if (waiter != NULL)
//::       condvar_report(tid, condp, CVE_Blocked, "waiting");
//::    else {
//::       waiter = VG_(arena_malloc)(VG_AR_CORE, sizeof(*waiter));
//::       waiter->condvar = cv;
//::       waiter->mutex = mx;
//::       waiter->next = cv->waiters;
//::       cv->waiters = waiter;
//::    }
//:: 
//::    /* Thread is now blocking on condvar */
//::    do_thread_block_condvar(th, cv);
//:: 
//::    /* (half) release mutex */
//::    VG_(tm_mutex_tryunlock)(tid, mutexp);
//:: }
//:: 
//:: /* Wake from a condition, either because we've been signalled, or
//::    because of timeout.  Fails if:
//::      - thread is not waiting on condp
//::  */
//:: void VG_(tm_cond_wakeup)(ThreadId tid, Addr condp, Addr mutexp)
//:: {
//:: }
//:: 
//:: /* Signal a condition variable.  Fails if:
//::     - condp has not been initialized
//::  */
//:: void VG_(tm_cond_signal)(ThreadId tid, Addr condp)
//:: {
//:: }
//:: 
//:: /* --------------------------------------------------
//::    Error handling
//::    -------------------------------------------------- */
//:: 
//:: UInt VG_(tm_error_update_extra)(Error *err)
//:: {
//::    switch (VG_(get_error_kind)(err)) {
//::       case ThreadErr: {
//::          struct thread_error_data *errdata = VG_(get_error_extra)(err);
//::          struct thread *new_th = VG_(arena_malloc)(VG_AR_CORE, sizeof(struct thread));
//:: 
//::          VG_(memcpy)(new_th, errdata->th, sizeof(struct thread));
//:: 
//::          errdata->th = new_th;
//:: 
//::          return sizeof(struct thread_error_data);
//::       }
//:: 
//::       case MutexErr: {
//::          struct mutex_error_data *errdata = VG_(get_error_extra)(err);
//::          struct mutex *new_mx = VG_(arena_malloc)(VG_AR_CORE, sizeof(struct mutex));
//:: 
//::          VG_(memcpy)(new_mx, errdata->mx, sizeof(struct mutex));
//:: 
//::          errdata->mx = new_mx;
//:: 
//::          return sizeof(struct mutex_error_data);
//::       }
//:: 
//::       default:
//::          return 0;
//::    }
//:: }
//:: 
//:: Bool VG_(tm_error_equal)(VgRes res, Error *e1, Error *e2)
//:: {
//::    /* Guaranteed by calling function */
//::    vg_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));
//::    
//::    switch (VG_(get_error_kind)(e1)) {
//::       case ThreadErr: {
//::          struct thread_error_data *errdata1 = VG_(get_error_extra)(e1);
//::          struct thread_error_data *errdata2 = VG_(get_error_extra)(e2);
//:: 
//::          return errdata1->err == errdata2->err;
//::       }
//:: 
//::       case MutexErr: {
//::          struct mutex_error_data *errdata1 = VG_(get_error_extra)(e1);
//::          struct mutex_error_data *errdata2 = VG_(get_error_extra)(e2);
//:: 
//::          return errdata1->err == errdata2->err;
//::       }
//:: 
//::       default: 
//::          VG_(printf)("Error:\n  unknown error code %d\n",
//::                      VG_(get_error_kind)(e1));
//::          VG_(core_panic)("unknown error code in VG_(tm_error_equal)");
//::    }
//:: }
//:: 
//:: void VG_(tm_error_print)(Error *err)
//:: {
//::    switch (VG_(get_error_kind)(err)) {
//::       case ThreadErr:
//::          pp_thread_error(err);
//::          break;
//::       case MutexErr:
//::          pp_mutex_error(err);
//::          break;
//::    }
//:: }
//:: 
//:: /* --------------------------------------------------
//::    Initialisation
//::    -------------------------------------------------- */
//:: 
//:: void VG_(tm_init)()
//:: {
//::    VG_(needs_core_errors)();
//:: }

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
