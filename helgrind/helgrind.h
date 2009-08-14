/*
   ----------------------------------------------------------------

   Notice that the above BSD-style license applies to this one file
   (helgrind.h) only.  The entire rest of Valgrind is licensed under
   the terms of the GNU General Public License, version 2.  See the
   COPYING file in the source distribution for details.

   ----------------------------------------------------------------

   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2009 OpenWorks LLP
      info@open-works.co.uk

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
   (helgrind.h) only.  The entire rest of Valgrind is licensed under
   the terms of the GNU General Public License, version 2.  See the
   COPYING file in the source distribution for details.

   ---------------------------------------------------------------- 
*/

#ifndef __HELGRIND_H
#define __HELGRIND_H

#include "valgrind.h"

/* !! ABIWARNING !! ABIWARNING !! ABIWARNING !! ABIWARNING !!
   This enum comprises an ABI exported by Valgrind to programs
   which use client requests.  DO NOT CHANGE THE ORDER OF THESE
   ENTRIES, NOR DELETE ANY -- add new ones at the end. */
typedef
   enum {
      VG_USERREQ__HG_CLEAN_MEMORY = VG_USERREQ_TOOL_BASE('H','G'),

      /* The rest are for Helgrind's internal use.  Not for end-user
         use.  Do not use them unless you are a Valgrind developer. */

      /* Notify the tool what this thread's pthread_t is. */
      _VG_USERREQ__HG_SET_MY_PTHREAD_T = VG_USERREQ_TOOL_BASE('H','G') 
                                         + 256,
      _VG_USERREQ__HG_PTH_API_ERROR,              /* char*, int */
      _VG_USERREQ__HG_PTHREAD_JOIN_POST,          /* pthread_t of quitter */
      _VG_USERREQ__HG_PTHREAD_MUTEX_INIT_POST,    /* pth_mx_t*, long mbRec */
      _VG_USERREQ__HG_PTHREAD_MUTEX_DESTROY_PRE,  /* pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_PRE,   /* pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_POST,  /* pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE, /* pth_mx_t*, long isTryLock */
      _VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,    /* pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_COND_SIGNAL_PRE,    /* pth_cond_t* */
      _VG_USERREQ__HG_PTHREAD_COND_BROADCAST_PRE, /* pth_cond_t* */
      _VG_USERREQ__HG_PTHREAD_COND_WAIT_PRE,     /* pth_cond_t*, pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_COND_WAIT_POST,    /* pth_cond_t*, pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_COND_DESTROY_PRE,   /* pth_cond_t* */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_INIT_POST,   /* pth_rwlk_t* */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_DESTROY_PRE, /* pth_rwlk_t* */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,    /* pth_rwlk_t*, long isW */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,   /* pth_rwlk_t*, long isW */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_PRE,  /* pth_rwlk_t* */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_POST, /* pth_rwlk_t* */
      _VG_USERREQ__HG_POSIX_SEM_INIT_POST,        /* sem_t*, ulong value */
      _VG_USERREQ__HG_POSIX_SEM_DESTROY_PRE,      /* sem_t* */
      _VG_USERREQ__HG_POSIX_SEM_POST_PRE,         /* sem_t* */
      _VG_USERREQ__HG_POSIX_SEM_WAIT_POST,        /* sem_t* */
      _VG_USERREQ__HG_PTHREAD_BARRIER_INIT_PRE,   /* pth_bar_t*, ulong */
      _VG_USERREQ__HG_PTHREAD_BARRIER_WAIT_PRE,   /* pth_bar_t* */
      _VG_USERREQ__HG_PTHREAD_BARRIER_DESTROY_PRE, /* pth_bar_t* */
      _VG_USERREQ__HG_PTHREAD_SPIN_INIT_OR_UNLOCK_PRE,  /* pth_slk_t* */
      _VG_USERREQ__HG_PTHREAD_SPIN_INIT_OR_UNLOCK_POST, /* pth_slk_t* */
      _VG_USERREQ__HG_PTHREAD_SPIN_LOCK_PRE,      /* pth_slk_t* */
      _VG_USERREQ__HG_PTHREAD_SPIN_LOCK_POST,     /* pth_slk_t* */
      _VG_USERREQ__HG_PTHREAD_SPIN_DESTROY_PRE,   /* pth_slk_t* */
      _VG_USERREQ__HG_CLIENTREQ_UNIMP,            /* char* */
      _VG_USERREQ__HG_USERSO_SEND_PRE,        /* arbitrary UWord SO-tag */
      _VG_USERREQ__HG_USERSO_RECV_POST,       /* arbitrary UWord SO-tag */
      _VG_USERREQ__HG_RESERVED1,              /* Do not use */
      _VG_USERREQ__HG_RESERVED2               /* Do not use */

   } Vg_TCheckClientRequest;


/*----------------------------------------------------------------*/
/*--- An implementation-only request -- not for end user use   ---*/
/*----------------------------------------------------------------*/

#define _HG_CLIENTREQ_UNIMP(_qzz_str)                            \
   do {                                                          \
     unsigned long _qzz_res;                                     \
     VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                     \
                                _VG_USERREQ__HG_CLIENTREQ_UNIMP, \
                                _qzz_str, 0, 0, 0, 0);           \
     (void)0;                                                    \
   } while(0)


/*----------------------------------------------------------------*/
/*--- Misc requests                                            ---*/
/*----------------------------------------------------------------*/

/* Clean memory state.  This makes Helgrind forget everything it knew
   about the specified memory range.  Effectively this announces that
   the specified memory range now "belongs" to the calling thread, so
   that: (1) the calling thread can access it safely without
   synchronisation, and (2) all other threads must sync with this one
   to access it safely.  This is particularly useful for memory
   allocators that wish to recycle memory. */
#define VALGRIND_HG_CLEAN_MEMORY(_qzz_start, _qzz_len) \
   do {                                                \
     unsigned long _qzz_res;                           \
     VALGRIND_DO_CLIENT_REQUEST(                       \
        (_qzz_res), 0, VG_USERREQ__HG_CLEAN_MEMORY,    \
        (_qzz_start), (_qzz_len), 0, 0, 0              \
     );                                                \
     (void)0;                                          \
   } while(0)


/*----------------------------------------------------------------*/
/*--- ThreadSanitizer-compatible requests                      ---*/
/*----------------------------------------------------------------*/

/* A quite-broad set of annotations, as used in the ThreadSanitizer
   project.  This implementation aims to be a (source-level)
   compatible implementation of the macros defined in:

   http://code.google.com/p/google-perftools/source  \
                         /browse/trunk/src/base/dynamic_annotations.h

   (some of the comments below are taken from the above file)

   The implementation here is very incomplete, and intended as a
   starting point.  Many of the macros are unimplemented.  Rather than
   allowing unimplemented macros to silently do nothing, they cause an
   assertion.  Intention is to implement them on demand.

   The major use of these macros is to make visible to race detectors,
   the behaviour (effects) of user-implemented synchronisation
   primitives, that the detectors could not otherwise deduce from the
   normal observation of pthread etc calls.

   Some of the macros are no-ops in Helgrind.  That's because Helgrind
   is a pure happens-before detector, whereas ThreadSanitizer uses a
   hybrid lockset and happens-before scheme, which requires more
   accurate annotations for correct operation.

   The macros are listed in the same order as in dynamic_annotations.h
   (URL just above).

   I should point out that I am less than clear about the intended
   semantics of quite a number of them.  Comments and clarifications
   welcomed!
*/

/* ----------------------------------------------------------------
   These four allow description of user-level condition variables,
   apparently in the style of POSIX's pthread_cond_t.  Currently
   unimplemented and will assert.
   ----------------------------------------------------------------
*/
/* Report that wait on the condition variable at address CV has
   succeeded and the lock at address LOCK is now held.  CV and LOCK
   are completely arbitrary memory addresses which presumably mean
   something to the application, but are meaningless to Helgrind. */
#define ANNOTATE_CONDVAR_LOCK_WAIT(cv, lock) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_CONDVAR_LOCK_WAIT")

/* Report that wait on the condition variable at CV has succeeded.
   Variant w/o lock. */
#define ANNOTATE_CONDVAR_WAIT(cv) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_CONDVAR_WAIT")

/* Report that we are about to signal on the condition variable at
   address CV. */
#define ANNOTATE_CONDVAR_SIGNAL(cv) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_CONDVAR_SIGNAL")
  
/* Report that we are about to signal_all on the condition variable at
   CV. */
#define ANNOTATE_CONDVAR_SIGNAL_ALL(cv) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_CONDVAR_SIGNAL_ALL")


/* ----------------------------------------------------------------
   Create completely arbitrary happens-before edges between threads.
   If thread T1 does ANNOTATE_HAPPENS_BEFORE(obj) and later (w.r.t.
   some notional global clock for the computation) thread T2 does
   ANNOTATE_HAPPENS_AFTER(obj), then Helgrind will regard all memory
   accesses done by T1 before the ..BEFORE.. call as happening-before
   all memory accesses done by T2 after the ..AFTER.. call.  Hence
   Helgrind won't complain about races if T2's accesses afterwards are
   to the same locations as T1's accesses before.

   OBJ is a machine word (unsigned long, or void*), is completely
   arbitrary, and denotes the identity of some synchronisation object
   you're modelling.

   You must do the _BEFORE call just before the real sync event on the
   signaller's side, and _AFTER just after the real sync event on the
   waiter's side.

   If none of the rest of these macros make sense to you, at least
   take the time to understand these two.  They form the very essence
   of describing arbitrary inter-thread synchronisation events to
   Helgrind.  You can get a long way just with them alone.
   ----------------------------------------------------------------
*/
#define ANNOTATE_HAPPENS_BEFORE(obj) \
   do {                                                          \
     unsigned long _qzz_res;                                     \
     VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                     \
                                _VG_USERREQ__HG_USERSO_SEND_PRE, \
                                obj, 0, 0, 0, 0);                \
     (void)0;                                                    \
   } while (0)

#define ANNOTATE_HAPPENS_AFTER(obj) \
   do {                                                           \
     unsigned long _qzz_res;                                      \
     VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                      \
                                _VG_USERREQ__HG_USERSO_RECV_POST, \
                                obj, 0, 0, 0, 0);                 \
     (void)0;                                                     \
   } while (0)


/* ----------------------------------------------------------------
   Memory publishing.  The TSan sources say:

     Report that the bytes in the range [pointer, pointer+size) are about
     to be published safely. The race checker will create a happens-before
     arc from the call ANNOTATE_PUBLISH_MEMORY_RANGE(pointer, size) to
     subsequent accesses to this memory.

   I'm not sure I understand what this means exactly, nor whether it
   is relevant for a pure h-b detector.  Leaving unimplemented for
   now.
   ----------------------------------------------------------------
*/
#define ANNOTATE_PUBLISH_MEMORY_RANGE(pointer, size) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_PUBLISH_MEMORY_RANGE")


/* ----------------------------------------------------------------
   TSan sources say:
   
     Instruct the tool to create a happens-before arc between
     MU->Unlock() and MU->Lock().  This annotation may slow down the
     race detector; normally it is used only when it would be
     difficult to annotate each of the mutex's critical sections
     individually using the annotations above.

   If MU is a posix pthread_mutex_t then Helgrind will do this anyway.
   In any case, leave as unimp for now.  I'm unsure about the intended
   behaviour.
   ---------------------------------------------------------------- 
*/
#define ANNOTATE_MUTEX_IS_USED_AS_CONDVAR(mu) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_MUTEX_IS_USED_AS_CONDVAR")


/* ----------------------------------------------------------------
   TSan sources say:
   
     Annotations useful when defining memory allocators, or when
     memory that was protected in one way starts to be protected in
     another.

     Report that a new memory at "address" of size "size" has been
     allocated.  This might be used when the memory has been retrieved
     from a free list and is about to be reused, or when a the locking
     discipline for a variable changes.

   AFAICS this is the same as VALGRIND_HG_CLEAN_MEMORY.
   ---------------------------------------------------------------- 
*/
#define ANNOTATE_NEW_MEMORY(address, size) \
   VALGRIND_HG_CLEAN_MEMORY((address), (size))


/* ----------------------------------------------------------------
   TSan sources say:

     Annotations useful when defining FIFO queues that transfer data
     between threads.

   All unimplemented.  Am not claiming to understand this (yet).
   ---------------------------------------------------------------- 
*/

/* Report that the producer-consumer queue object at address PCQ has
   been created.  The ANNOTATE_PCQ_* annotations should be used only
   for FIFO queues.  For non-FIFO queues use ANNOTATE_HAPPENS_BEFORE
   (for put) and ANNOTATE_HAPPENS_AFTER (for get). */
#define ANNOTATE_PCQ_CREATE(pcq) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_PCQ_CREATE")

/* Report that the queue at address PCQ is about to be destroyed. */
#define ANNOTATE_PCQ_DESTROY(pcq) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_PCQ_DESTROY")

/* Report that we are about to put an element into a FIFO queue at
   address PCQ. */
#define ANNOTATE_PCQ_PUT(pcq) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_PCQ_PUT")

/* Report that we've just got an element from a FIFO queue at address
   PCQ. */
#define ANNOTATE_PCQ_GET(pcq) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_PCQ_GET")


/* ----------------------------------------------------------------
   Annotations that suppress errors.  It is usually better to express
   the program's synchronization using the other annotations, but
   these can be used when all else fails.

   Currently these are all unimplemented.  I can't think of a simple
   way to implement them without at least some performance overhead.
   ----------------------------------------------------------------
*/

/* Report that we may have a benign race on ADDRESS.  Insert at the
   point where ADDRESS has been allocated, preferably close to the
   point where the race happens.  See also ANNOTATE_BENIGN_RACE_STATIC.

   XXX: what's this actually supposed to do?  And what's the type of
   DESCRIPTION?  When does the annotation stop having an effect?
*/
#define ANNOTATE_BENIGN_RACE(address, description) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_BENIGN_RACE")
   

/* Request the analysis tool to ignore all reads in the current thread
   until ANNOTATE_IGNORE_READS_END is called.  Useful to ignore
   intentional racey reads, while still checking other reads and all
   writes. */
#define ANNOTATE_IGNORE_READS_BEGIN() \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_IGNORE_READS_BEGIN")

/* Stop ignoring reads. */
#define ANNOTATE_IGNORE_READS_END() \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_IGNORE_READS_END")

/* Similar to ANNOTATE_IGNORE_READS_BEGIN, but ignore writes. */
#define ANNOTATE_IGNORE_WRITES_BEGIN() \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_IGNORE_WRITES_BEGIN")

/* Stop ignoring writes. */
#define ANNOTATE_IGNORE_WRITES_END() \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_IGNORE_WRITES_END")

/* Start ignoring all memory accesses (reads and writes). */
#define ANNOTATE_IGNORE_READS_AND_WRITES_BEGIN() \
   do { \
      ANNOTATE_IGNORE_READS_BEGIN(); \
      ANNOTATE_IGNORE_WRITES_BEGIN(); \
   } while (0)

/* Stop ignoring all memory accesses. */
#define ANNOTATE_IGNORE_READS_AND_WRITES_END() \
   do { \
      ANNOTATE_IGNORE_WRITES_END(); \
      ANNOTATE_IGNORE_READS_END(); \
   } while (0)


/* ----------------------------------------------------------------
   Annotations useful for debugging.

   Again, so for unimplemented, partly for performance reasons.
   ----------------------------------------------------------------
*/

/* Request to trace every access to ADDRESS. */
#define ANNOTATE_TRACE_MEMORY(address) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_TRACE_MEMORY")

/* Report the current thread name to a race detector. */
#define ANNOTATE_THREAD_NAME(name) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_THREAD_NAME")


/* ----------------------------------------------------------------
   Annotations for describing behaviour of user-implemented lock
   primitives.  In all cases, the LOCK argument is a completely
   arbitrary machine word (unsigned long, or void*) and can be any
   value which gives a unique identity to the lock objects being
   modelled.

   We just pretend they're ordinary posix rwlocks.  That'll probably
   give some rather confusing wording in error messages, claiming that
   the arbitrary LOCK values are pthread_rwlock_t*'s, when in fact
   they are not.  Ah well.
   ----------------------------------------------------------------
*/
/* Report that a lock has just been created at address LOCK. */
#define ANNOTATE_RWLOCK_CREATE(lock) \
   do {                                                          \
     unsigned long _qzz_res;                                     \
     VALGRIND_DO_CLIENT_REQUEST(                                 \
        _qzz_res, 0, _VG_USERREQ__HG_PTHREAD_RWLOCK_INIT_POST,   \
        lock, 0, 0, 0, 0                                         \
     );                                                          \
     (void)0;                                                    \
   } while(0)
    
/* Report that the lock at address LOCK is about to be destroyed. */
#define ANNOTATE_RWLOCK_DESTROY(lock) \
   do {                                                          \
     unsigned long _qzz_res;                                     \
     VALGRIND_DO_CLIENT_REQUEST(                                 \
        _qzz_res, 0, _VG_USERREQ__HG_PTHREAD_RWLOCK_DESTROY_PRE, \
        lock, 0, 0, 0, 0                                         \
     );                                                          \
     (void)0;                                                    \
   } while(0)

/* Report that the lock at address LOCK has just been acquired.
   is_w=1 for writer lock, is_w=0 for reader lock. */
#define ANNOTATE_RWLOCK_ACQUIRED(lock, is_w) \
   do {                                                          \
     unsigned long _qzz_res;                                     \
     VALGRIND_DO_CLIENT_REQUEST(                                 \
        _qzz_res, 0, _VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,   \
        lock, is_w ? 1 : 0, 0, 0, 0                              \
     );                                                          \
     (void)0;                                                    \
   } while(0)

/* Report that the lock at address LOCK is about to be released. */
  #define ANNOTATE_RWLOCK_RELEASED(lock, is_w) \
   do {                                                          \
     unsigned long _qzz_res;                                     \
     VALGRIND_DO_CLIENT_REQUEST(                                 \
        _qzz_res, 0, _VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_PRE,  \
        lock, 0, 0, 0, 0                                         \
     );                                                          \
     (void)0;                                                    \
   } while(0)


/* ----------------------------------------------------------------
   Annotations useful for testing race detectors.
   ----------------------------------------------------------------
*/

/* Report that we expect a race on the variable at ADDRESS.  Use only
   in unit tests for a race detector. */
#define ANNOTATE_EXPECT_RACE(address, description) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_EXPECT_RACE")

/* A no-op. Insert where you like to test the interceptors. */
#define ANNOTATE_NO_OP(arg) \
   _HG_CLIENTREQ_UNIMP("ANNOTATE_NO_OP")


#endif /* __HELGRIND_H */
