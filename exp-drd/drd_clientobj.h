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


#ifndef __DRD_CLIENTOBJ_H
#define __DRD_CLIENTOBJ_H


#include "drd_clientreq.h"   /* MutexT */
#include "drd_thread.h"      /* DrdThreadId */
#include "pub_tool_basics.h"
#include "pub_tool_oset.h"


// Forward declarations.

union drd_clientobj;


// Type definitions.

typedef enum {
  ClientMutex     = 1,
  ClientCondvar   = 2,
  ClientSemaphore = 3,
  ClientBarrier   = 4,
} ObjType;

struct any
{
  Addr    a1;
  Addr    a2;
  ObjType type;
  void    (*cleanup)(union drd_clientobj*);
};

struct mutex_info
{
  Addr        a1;
  Addr        a2;
  ObjType     type;
  void        (*cleanup)(union drd_clientobj*);
  MutexT      mutex_type;      // pthread_mutex_t or pthread_spinlock_t.
  int         recursion_count; // 0 if free, >= 1 if locked.
  DrdThreadId owner;           // owner if locked, last owner if free.
  VectorClock vc;              // vector clock associated with last unlock.
};

struct cond_info
{
  Addr    a1;
  Addr    a2;
  ObjType type;
  void    (*cleanup)(union drd_clientobj*);
  int     waiter_count;
  Addr    mutex; // Client mutex specified in pthread_cond_wait() call, and null
                 // if no client threads are currently waiting on this cond.var.
};

struct semaphore_info
{
  Addr        a1;
  Addr        a2;
  ObjType     type;
  void        (*cleanup)(union drd_clientobj*);
  UWord       value;             // Semaphore value.
  UWord       waiters;           // Number of threads inside sem_wait().
  DrdThreadId last_sem_post_tid; // Thread ID associated with last sem_post().
  VectorClock vc;                // Vector clock of last sem_post() call.
};

struct barrier_info
{
  Addr    a1;
  Addr    a2;
  ObjType type;
  void    (*cleanup)(union drd_clientobj*);
  Word     count;               // Participant count in a barrier wait.
  Word     pre_iteration;       // pthread_barrier_wait() call count modulo two.
  Word     post_iteration;      // pthread_barrier_wait() call count modulo two.
  Word     pre_waiters_left;    // number of waiters left for a complete barrier.
  Word     post_waiters_left;   // number of waiters left for a complete barrier.
  OSet*    oset;                // Thread-specific barrier information.
};

typedef union drd_clientobj
{
  struct any            any;
  struct mutex_info     mutex;
  struct cond_info      cond;
  struct semaphore_info semaphore;
  struct barrier_info   barrier;
} DrdClientobj;


// Function declarations.

void drd_clientobj_init(void);
void drd_clientobj_cleanup(void);
DrdClientobj* drd_clientobj_get(const Addr addr, const ObjType t);
Bool drd_clientobj_present(const Addr a1, const Addr a2);
DrdClientobj* drd_clientobj_add(const Addr a1, const Addr a2, const ObjType t);
Bool drd_clientobj_remove(const Addr addr, const ObjType t);
void drd_clientobj_stop_using_mem(const Addr a1, const Addr a2);
void drd_clientobj_resetiter(void);
DrdClientobj* drd_clientobj_next(const ObjType t);

#endif /* __DRD_CLIENTOBJ_H */
