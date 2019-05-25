
/*--------------------------------------------------------------------*/
/*--- Definitions for Locks and Threads.                           ---*/
/*---                                           hg_lock_n_thread.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2017 OpenWorks Ltd
      info@open-works.co.uk

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

#ifndef __HG_LOCK_N_THREAD_H
#define __HG_LOCK_N_THREAD_H


/*----------------------------------------------------------------*/
/*--- Primary data definitions                                 ---*/
/*----------------------------------------------------------------*/

/* Magic numbers, for doing assertions that structures really are of
   the right type.  Useful as some of the code can get a bit
   complex. */
#define Thread_MAGIC   0x504fc5e5
#define LockN_MAGIC    0x6545b557 /* normal nonpersistent locks */
#define LockP_MAGIC    0x755b5456 /* persistent (copied) locks */


/* These are handles for Word sets.  CONSTRAINTS: must be small ints
   numbered from zero, since 32-bit versions of them are used to
   encode lock-sets in libhb's history records (Thr_n_RCEC). */
typedef  WordSet  WordSetID;


/* Synchronisation Objects, exported abstractly by libhb. */
typedef  struct _SO  SO;

/* Thr, libhb's private thread record, exported abstractly.  Thr's are
   allocated and never deallocated (simply leaked).  Also ThrID, which
   is a small integer which uniquely identifies a Thr and which is
   used in ScalarTS because it is smaller than a Thr*.  There is a 1-1
   mapping between Thr's and ThrIDs. */
typedef  struct _Thr  Thr;
typedef  UInt         ThrID;


/* Stores information about a thread.  Addresses of these also serve
   as unique thread identifiers and so are never freed, so they should
   be as small as possible.  Freeing Thread structures makes the
   storage management just too complex, and most programs don't create
   many threads, so tolerating this leak seems like a not-bad
   tradeoff.

   Since these are never freed, the .coretid field only indicates the
   core's ThreadId associated with this Thread whilst it is alive.
   Once the thread finishes, the ThreadId is set to
   VG_INVALID_THREADID.

   The core may later re-use the same ThreadId for what is a logically
   completely different thread, which of course must have a different
   Thread structure. */
typedef
   struct _Thread {
      /* ADMIN */
      struct _Thread* admin;
      UInt            magic;
      Thr*            hbthr; /* which in turn points back here .. */
      ThreadId        coretid;  /* .. via its hgthread field */
      /* USEFUL */
      WordSetID locksetA; /* WordSet of Lock* currently held by thread */
      WordSetID locksetW; /* subset of locksetA held in w-mode */
      /* EXPOSITION */
      /* Place where parent was when this thread was created. */
      ExeContext* created_at;
      Bool        announced;
      /* != 0 if SP fixup needed for unwind : it contains a delta SP value
         to use when evh__mem_help_c(read|write)_X is called in the 
         'middle of an instruction' (e.g. in a push)
         after the SP was changed, but before the push has been completed. */
      Word        first_sp_delta;
      /* Index for generating references in error messages. */
      Int         errmsg_index;

      /* Nesting level of pthread_create(). New memory allocated is untracked
         when this value is > 0: race reporting is suppressed there. DRD does
         the same thing implicitly. This is necessary because for example
         Solaris libc caches many objects and reuses them for different threads
         and that confuses Helgrind. With libvki it would be possible to
         explicitly use VG_USERREQ__HG_CLEAN_MEMORY on such objects.
         Also mutex activity is ignored so that they do not impose false
         ordering between creator and created thread. */
      Int pthread_create_nesting_level;

      /* Nesting level of synchronization functions called by the client.
         Loads and stores are ignored when its value > 0.
         Currently this is used solely for suppressing races of primitive
         synchronization objects themselves - mutexes, condition variables,
         read-write locks and their associated sleep queues.
         See also documentation for command line option
         --ignore-thread-creation. */
      Int synchr_nesting;

#if defined(VGO_solaris)
      Int      bind_guard_flag; /* Bind flag from the runtime linker. */
#endif /* VGO_solaris */
   }
   Thread;

/* Get hg's admin_threads value, so libhb can visit all of them. */
Thread* get_admin_threads ( void );

/* Stores information about a lock's current state.  These are
   allocated and later freed (when the containing memory becomes
   NoAccess).  This gives a problem for the XError type, which
   contains Lock*s.  Solution is to copy any Lock which is to be
   incorporated into an XErrors, so as to make it independent from the
   'normal' collection of Locks, which can come and go.  When the lock
   is copied, its .magic is changed from LockN_Magic to
   LockP_Magic. */

/* Lock kinds. */
typedef
   enum {
      LK_mbRec=1001, /* normal mutex, possibly recursive */
      LK_nonRec,     /* normal mutex, definitely non recursive */
      LK_rdwr        /* reader-writer lock */
   }
   LockKind;

typedef
   struct _Lock {
      /* ADMIN */
      struct _Lock* admin_next; /* fields for a double linked */
      struct _Lock* admin_prev; /* list of these locks */
      ULong         unique; /* used for persistence-hashing */
      UInt          magic;  /* LockN_MAGIC or LockP_MAGIC */
      /* EXPOSITION */
      /* Place where lock first came to the attention of Helgrind. */
      ExeContext*   appeared_at;
      /* If the lock is held, place where the lock most recently made
         an unlocked->locked transition.  Must be sync'd with .heldBy:
         either both NULL or both non-NULL. */
      ExeContext*   acquired_at;
      /* USEFUL-STATIC */
      SO*           hbso;      /* associated SO */
      Addr          guestaddr; /* Guest address of lock */
      LockKind      kind;      /* what kind of lock this is */
      /* USEFUL-DYNAMIC */
      Bool          heldW; 
      WordBag*      heldBy; /* bag of threads that hold this lock */
      /* .heldBy is NULL: lock is unheld, and .heldW is meaningless
                          but arbitrarily set to False
         .heldBy is non-NULL:
            .heldW is True:  lock is w-held by threads in heldBy
            .heldW is False: lock is r-held by threads in heldBy
            Either way, heldBy may not validly be an empty Bag.

         for LK_nonRec, r-holdings are not allowed, and w-holdings may
         only have sizeTotal(heldBy) == 1

         for LK_mbRec, r-holdings are not allowed, and w-holdings may
         only have sizeUnique(heldBy) == 1

         for LK_rdwr, w-holdings may only have sizeTotal(heldBy) == 1 */
   }
   Lock;

#define Lock_INVALID  ((Lock*)1UL)

/*----------------------------------------------------------------*/
/*--- Sanity checking                                          ---*/
/*----------------------------------------------------------------*/

Bool HG_(is_sane_Thread)   ( Thread* thr );
Bool HG_(is_sane_LockP)    ( Lock* lock );
Bool HG_(is_sane_LockN)    ( Lock* lock );
Bool HG_(is_sane_LockNorP) ( Lock* lock );


#endif /* ! __HG_LOCK_N_THREAD_H */

/*--------------------------------------------------------------------*/
/*--- end                                       hg_lock_n_thread.h ---*/
/*--------------------------------------------------------------------*/
