
/*--------------------------------------------------------------------*/
/*--- Pthreads library modelling.                 m_pthreadmodel.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
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
   This file wraps the client's use of libpthread functions and calls
   on vg_threadmodel.c to model the state of the the client's threads.
   The intent is to 1) look for problem's in the client's use of the
   pthread API, and 2) tell tools which care about thread events (eg,
   helgrind).

   This file is intended to be implementation-independent.  It assumes
   that the client is using the same pthread.h as the one we include
   here, but makes minimal assumptions about the actual structures
   defined and so on (ie, the exact nature of pthread_t).

   (For now we assume there's a 1:1 relationship between pthread_t's
   and Valgrind-visible threads; N:M implementations will need further
   work.)

   The model is based on the pthread standard rather than any
   particular implementation, in order to encourage portable use of
   libpthread.  On the other hand, we will probably need to implement
   particular implementation extensions if they're widely used.
   
   One tricky problem we need to solve is the mapping between
   pthread_t identifiers and internal thread identifiers.
 */

#include "pub_core_basics.h"

#if 0

#define __USE_GNU
#define __USE_UNIX98
#include <pthread.h>

static const Bool debug = False;

static Bool check_wrappings(void);

#define ENTER(x)						\
   do {								\
      if (VG_(clo_trace_pthreads))				\
	 VG_(message)(Vg_DebugMsg, ">>> %d entering %s",	\
		      VG_(get_running_tid)(), #x);		\
   } while(0)

static const Char *pp_retval(enum return_type rt, Word retval)
{
   static Char buf[50];

   switch(rt) {
   case RT_RETURN:
      VG_(sprintf)(buf, "return %d 0x%x", retval, retval);
      return buf;

   case RT_LONGJMP:
      return "LONGJMPed out";

   case RT_EXIT:
      return "thread exit";
   }
   return "??";
}

#define LEAVE(x, rt, retval)						\
   do {									\
      if (VG_(clo_trace_pthreads))					\
	 VG_(message)(Vg_DebugMsg, "<<< %d leaving %s -> %s",		\
		      VG_(get_running_tid)(), #x, pp_retval(rt, retval)); \
   } while(0)

struct pthread_map
{
   pthread_t	id;

   ThreadId	tid;
};

static Int pthread_cmp(const void *v1, const void *v2)
{
   const pthread_t *a = (const pthread_t *)v1;
   const pthread_t *b = (const pthread_t *)v2;
   
   return VG_(memcmp)(a, b, sizeof(*a));
}

static SkipList sk_pthread_map = VG_SKIPLIST_INIT(struct pthread_map, id, pthread_cmp, 
					       NULL, VG_AR_CORE);

/* Find a ThreadId for a particular pthread_t; block until it becomes available */
static ThreadId get_pthread_mapping(pthread_t id)
{
   /* Nasty little spin loop; revise if this turns out to be a
      problem.  This should only spin for as long as it takes for the
      child thread to register the pthread_t. */
   for(;;) {
      struct pthread_map *m = VG_(SkipList_Find_Exact)(&sk_pthread_map, &id);

      if (m && m->tid != VG_INVALID_THREADID)
	 return m->tid;

      //VG_(printf)("find %x -> %p\n", id, m);
      VG_(vg_yield)();
   }
}

/* Create a  mapping between a ThreadId and a pthread_t */
static void pthread_id_mapping(ThreadId tid, Addr idp, UInt idsz)
{
   pthread_t id = *(pthread_t *)idp;
   struct pthread_map *m = VG_(SkipList_Find_Exact)(&sk_pthread_map, &id);

   if (debug)
      VG_(printf)("Thread %d maps to %p\n", tid, id);
   
   if (m == NULL) {
      m = VG_(SkipNode_Alloc)(&sk_pthread_map);
      m->id = id;
      m->tid = tid;
      VG_(SkipList_Insert)(&sk_pthread_map, m);
   } else {
      if (m->tid != VG_INVALID_THREADID && m->tid != tid)
	 VG_(message)(Vg_UserMsg, "Thread %d is creating duplicate mapping for pthread identifier %x; previously mapped to %d\n",
		      tid, (UInt)id, m->tid);
      m->tid = tid;
   }
}

static void check_thread_exists(ThreadId tid)
{
   if (!VG_(tm_thread_exists)(tid)) {
      if (debug)
	 VG_(printf)("creating thread %d\n", tid);
      VG_(tm_thread_create)(VG_INVALID_THREADID, tid, False);
   }
}

static Addr startfunc_wrapper = 0;

void VG_(pthread_startfunc_wrapper)(Addr wrapper)
{
   startfunc_wrapper = wrapper;
}

struct pthread_create_nonce {
   Bool detached;
   pthread_t *threadid;
};

static void *before_pthread_create(va_list va)
{
   pthread_t *threadp = va_arg(va, pthread_t *);
   const pthread_attr_t *attr = va_arg(va, const pthread_attr_t *);
   void *(*start)(void *) = va_arg(va, void *(*)(void *));
   void *arg = va_arg(va, void *);
   struct pthread_create_nonce *n;
   struct vg_pthread_newthread_data *data; 
   ThreadState *tst;

   if (!check_wrappings())
      return NULL;

   ENTER(pthread_create);

   /* Data is in the client heap and is freed by the client in the
      startfunc_wrapper. */
   vg_assert(startfunc_wrapper != 0);

   tst = VG_(get_ThreadState)(VG_(get_running_tid)());

   // XXX: why use TL_(malloc)() here?  What characteristics does this
   // allocation require?  
   // [Possible: When using a tool that replaces malloc(), we want to call
   //  the replacement version.  Otherwise, we want to use VG_(cli_malloc)().
   //  So we go via the default version of TL_(malloc)() in vg_default?]
   tl_assert2(0, "read the comment in the code about this...");

   // XXX: These three lines are going to have to change.  They relied on
   // TL_(malloc) being a weak symbol, and it just doesn't fit with the
   // VG_(tdict) approach that we've switched to.  The right way to do this
   // will be to provide a function in the core that checks if
   // VG_(tdict).malloc_malloc has been set;  if so, it should
   // call it, if not, it should call VG_(cli_malloc)().
//   VG_(tl_malloc_called_deliberately) = True;
//   data = TL_(malloc)(sizeof(*data));
//   VG_(tl_malloc_called_deliberately) = False;

   VG_TRACK(pre_mem_write, Vg_CorePThread, tst->tid, "new thread data",
	    (Addr)data, sizeof(*data));
   data->startfunc = start;
   data->arg = arg;
   VG_TRACK(post_mem_write, (Addr)data, sizeof(*data));

   /* Substitute arguments
      XXX hack: need an API to do this. */
   ((Word *)tst->arch.m_esp)[3] = startfunc_wrapper;
   ((Word *)tst->arch.m_esp)[4] = (Word)data;

   if (debug)
      VG_(printf)("starting thread at wrapper %p\n", startfunc_wrapper);

   n = VG_(arena_malloc)(VG_AR_CORE, sizeof(*n));
   n->detached = attr && !!attr->__detachstate;
   n->threadid = threadp;

   return n;
}

static void after_pthread_create(void *nonce, enum return_type rt, Word retval)
{
   struct pthread_create_nonce *n = (struct pthread_create_nonce *)nonce;
   ThreadId tid = VG_(get_running_tid)();

   if (n == NULL)
      return;

   if (rt == RT_RETURN && retval == 0) {
      if (!VG_(tm_thread_exists)(tid))
	 VG_(tm_thread_create)(tid, get_pthread_mapping(*n->threadid),
			       n->detached);
      else {
	 if (n->detached)
	    VG_(tm_thread_detach)(tid);
	 /* XXX set creator tid as well? */
      }
   }

   VG_(arena_free)(VG_AR_CORE, n);

   LEAVE(pthread_create, rt, retval);
}

static void *before_pthread_join(va_list va)
{
   pthread_t pt_joinee = va_arg(va, pthread_t);
   ThreadId joinee;

   if (!check_wrappings())
      return NULL;

   ENTER(pthread_join);

   joinee = get_pthread_mapping(pt_joinee);

   VG_(tm_thread_join)(VG_(get_running_tid)(), joinee);

   return NULL;
}

static void after_pthread_join(void *v, enum return_type rt, Word retval)
{
   /* nothing to be done? */
   if (!check_wrappings())
      return;

   LEAVE(pthread_join, rt, retval);
}

struct pthread_detach_data {
   pthread_t id;
};

static void *before_pthread_detach(va_list va)
{
   pthread_t id = va_arg(va, pthread_t);
   struct pthread_detach_data *data;

   if (!check_wrappings())
      return NULL;

   ENTER(pthread_detach);

   data = VG_(arena_malloc)(VG_AR_CORE, sizeof(*data));
   data->id = id;

   return data;
}

static void after_pthread_detach(void *nonce, enum return_type rt, Word retval)
{
   struct pthread_detach_data *data = (struct pthread_detach_data *)nonce;
   ThreadId tid;

   if (data == NULL)
      return;

   tid = get_pthread_mapping(data->id);

   VG_(arena_free)(VG_AR_CORE, data);
   
   if (rt == RT_RETURN && retval == 0)
      VG_(tm_thread_detach)(tid);
   
   LEAVE(pthread_detach, rt, retval);
}



static void *before_pthread_self(va_list va)
{
   /* If pthread_t is a structure, then this might be passed a pointer
      to the return value.  On Linux/glibc, it's a simple scalar, so it is
      returned normally. */
   if (!check_wrappings())
      return NULL;

   ENTER(pthread_self);

   check_thread_exists(VG_(get_running_tid)());
   return NULL;
}

static void after_pthread_self(void *nonce, enum return_type rt, Word retval)
{
   pthread_t ret = (pthread_t)retval;

   if (!check_wrappings())
      return;

   pthread_id_mapping(VG_(get_running_tid)(), (Addr)&ret, sizeof(ret));

   LEAVE(pthread_self, rt, retval);
}


/* If a mutex hasn't been initialized, check it against all the static
   initializers to see if it appears to have been statically
   initialized.  */
static void check_mutex_init(ThreadId tid, pthread_mutex_t *mx)
{
   static const pthread_mutex_t initializers[] = {
      PTHREAD_MUTEX_INITIALIZER,
      PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP,
      PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP,
      PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP,
   };
   Int i;
   
   if (VG_(tm_mutex_exists)((Addr)mx))
      return;

   VG_TRACK(pre_mem_read, Vg_CorePThread, tid, "pthread_mutex_t", (Addr)mx, sizeof(*mx));

   for(i = 0; i < sizeof(initializers)/sizeof(*initializers); i++)
      if (VG_(memcmp)(&initializers[i], mx, sizeof(*mx)) == 0) {
	 VG_(tm_mutex_init)(tid, (Addr)mx);
	 break;
      }
}

static void *before_pthread_mutex_init(va_list va)
{
   pthread_mutex_t *mx = va_arg(va, pthread_mutex_t *);
   const pthread_mutexattr_t *attr = va_arg(va, const pthread_mutexattr_t *);

   if (!check_wrappings())
      return NULL;

   ENTER(pthread_mutex_init);

   /* XXX look for recursive mutex */
   /* XXX look for non-process scope */
   (void)attr;

   return mx;
}

static void after_pthread_mutex_init(void *nonce, enum return_type rt, Word retval)
{
   if (!check_wrappings())
      return;

   if (rt == RT_RETURN && retval == 0)
      VG_(tm_mutex_init)(VG_(get_running_tid)(), (Addr)nonce);

   LEAVE(pthread_mutex_init, rt, retval);
}

static void *before_pthread_mutex_destroy(va_list va)
{
   pthread_mutex_t *mx = va_arg(va, pthread_mutex_t *);

   if (!check_wrappings())
      return NULL;

   ENTER(pthread_mutex_destroy);

   VG_(tm_mutex_destroy)(VG_(get_running_tid)(), (Addr)mx);

   return NULL;
}

static void after_pthread_mutex_destroy(void *nonce, enum return_type rt, Word retval)
{
   if (!check_wrappings())
      return;

   LEAVE(pthread_mutex_destroy, rt, retval);
}

static void *before_pthread_mutex_lock(va_list va)
{
   pthread_mutex_t *mx = va_arg(va, pthread_mutex_t *);

   if (!check_wrappings())
      return NULL;

   ENTER(pthread_mutex_lock);

   if (debug)
      VG_(printf)("%d locking %p\n", VG_(get_running_tid)(), mx);
   check_thread_exists(VG_(get_running_tid)());
   check_mutex_init(VG_(get_running_tid)(), mx); /* mutex might be statically initialized */
   VG_(tm_mutex_trylock)(VG_(get_running_tid)(), (Addr)mx);

   return mx;
}

static void after_pthread_mutex_lock(void *nonce, enum return_type rt, Word retval)
{
   if (!check_wrappings())
      return;

   if (rt == RT_RETURN && retval == 0)
      VG_(tm_mutex_acquire)(VG_(get_running_tid)(), (Addr)nonce);
   else {
      if (debug)
	 VG_(printf)("after mutex_lock failed: rt=%d ret=%d\n", rt, retval);
      VG_(tm_mutex_giveup)(VG_(get_running_tid)(), (Addr)nonce);
   }

   LEAVE(pthread_mutex_lock, rt, retval);
}

static void *before_pthread_mutex_trylock(va_list va)
{
   pthread_mutex_t *mx = va_arg(va, pthread_mutex_t *);

   if (!check_wrappings())
      return NULL;

   ENTER(pthread_mutex_trylock);

   if (debug)
      VG_(printf)("%d trylocking %p\n", VG_(get_running_tid)(), mx);
   check_thread_exists(VG_(get_running_tid)());
   check_mutex_init(VG_(get_running_tid)(), mx); /* mutex might be statically initialized */
   VG_(tm_mutex_trylock)(VG_(get_running_tid)(), (Addr)mx);

   return mx;
}

static void after_pthread_mutex_trylock(void *nonce, enum return_type rt, Word retval)
{
   if (nonce == NULL)
      return;
   
   if (rt == RT_RETURN && retval == 0)
      VG_(tm_mutex_acquire)(VG_(get_running_tid)(), (Addr)nonce);
   else {
      if (debug)
	 VG_(printf)("after mutex_trylock failed: rt=%d ret=%d\n", rt, retval);
      VG_(tm_mutex_giveup)(VG_(get_running_tid)(), (Addr)nonce);
   }

   LEAVE(pthread_mutex_trylock, rt, retval);
}

static void *before_pthread_mutex_unlock(va_list va)
{
   pthread_mutex_t *mx = va_arg(va, pthread_mutex_t *);

   if (!check_wrappings())
      return NULL;

   ENTER(pthread_mutex_unlock);
   
   VG_(tm_mutex_tryunlock)(VG_(get_running_tid)(), (Addr)mx);

   return mx;
}

static void after_pthread_mutex_unlock(void *nonce, enum return_type rt, Word retval)
{
   if (nonce == NULL)
      return;

   if (rt == RT_RETURN && retval == 0)
      VG_(tm_mutex_unlock)(VG_(get_running_tid)(), (Addr)nonce); /* complete unlock */
   else
      VG_(tm_mutex_acquire)(VG_(get_running_tid)(), (Addr)nonce); /* re-acquire */

   LEAVE(pthread_mutex_unlock, rt, retval);
}


static struct pt_wraps {
   const Char		*name;
   FuncWrapper		wrapper;
   const CodeRedirect	*redir;
} wraps[] = {
#define WRAP(func, extra) { #func extra, { before_##func, after_##func } }
   WRAP(pthread_create, "@@GLIBC_2.1"),	/* XXX TODO: 2.0 ABI (?) */
   WRAP(pthread_join, ""),
   WRAP(pthread_detach, ""),

   WRAP(pthread_self, ""),

   WRAP(pthread_mutex_init, ""),
   WRAP(pthread_mutex_destroy, ""),
   WRAP(pthread_mutex_lock, ""),
   WRAP(pthread_mutex_trylock, ""),
   WRAP(pthread_mutex_unlock, ""),
#undef WRAP
};

/* Check to see if all the wrappers are resolved */
static Bool check_wrappings()
{
   Int i;
   static Bool ok = True;
   static Bool checked = False;

   if (checked)
      return ok;

   for(i = 0; i < sizeof(wraps)/sizeof(*wraps); i++) {
      if (!VG_(is_resolved)(wraps[i].redir)) {
	 VG_(message)(Vg_DebugMsg, "Pthread wrapper for \"%s\" is not resolved",
		      wraps[i].name);
	 ok = False;
      }
   }

   if (startfunc_wrapper == 0) {
      VG_(message)(Vg_DebugMsg, "Pthread wrapper for thread start function is not resolved");
      ok = False;
   }

   if (!ok)
      VG_(message)(Vg_DebugMsg, "Missing intercepts; model disabled");

   checked = True;
   return ok;
}

/* 
   Set up all the wrappers for interesting functions.
 */
void VG_(pthread_init)()
{
   Int i;

   for(i = 0; i < sizeof(wraps)/sizeof(*wraps); i++) {
      //VG_(printf)("adding pthread wrapper for %s\n", wraps[i].name);
      wraps[i].redir = VG_(add_wrapper)("soname:libpthread.so.0", 
					wraps[i].name, &wraps[i].wrapper);
   }
   VG_(tm_init)();
   VG_(tm_thread_create)(VG_INVALID_THREADID, VG_(master_tid), True);
}

#else  /* !0 */
/* Stubs for now */
//:: void VG_(pthread_init)()
//:: {
//:: }
//:: 
//:: void VG_(pthread_startfunc_wrapper)(Addr wrapper)
//:: {
//:: }
#endif	/* 0 */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
