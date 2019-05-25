
/*--------------------------------------------------------------------*/
/*--- LibHB: a library for implementing and checking               ---*/
/*--- the happens-before relationship in concurrent programs.      ---*/
/*---                                                 libhb_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibHB, a library for implementing and checking
   the happens-before relationship in concurrent programs.

   Copyright (C) 2008-2017 OpenWorks Ltd
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

#ifndef __LIBHB_H
#define __LIBHB_H

/* Abstract to user: thread identifiers */
/* typedef  struct _Thr  Thr; */ /* now in hg_lock_n_thread.h */

/* Abstract to user: synchronisation objects */
/* typedef  struct _SO  SO; */ /* now in hg_lock_n_thread.h */

/* Initialise library; returns Thr* for root thread.  'shadow_alloc'
   should never return NULL, instead it should simply not return if
   they encounter an out-of-memory condition. */
Thr* libhb_init (
        void        (*get_stacktrace)( Thr*, Addr*, UWord ),
        ExeContext* (*get_EC)( Thr* )
     );

/* Shut down the library, and print stats (in fact that's _all_
   this is for.) */
void libhb_shutdown ( Bool show_stats );

/* Thread creation: returns Thr* for new thread */
Thr* libhb_create ( Thr* parent );

/* Thread async exit */
void libhb_async_exit      ( Thr* exitter );
void libhb_joinedwith_done ( Thr* exitter );

/* Synchronisation objects (abstract to caller) */

/* Allocate a new one (alloc'd by library) */
SO* libhb_so_alloc ( void );

/* Dealloc one */
void libhb_so_dealloc ( SO* so );

/* Send a message via a sync object.  If strong_send is true, the
   resulting inter-thread dependency seen by a future receiver of this
   message will be a dependency on this thread only.  That is, in a
   strong send, the VC inside the SO is replaced by the clock of the
   sending thread.  For a weak send, the sender's VC is joined into
   that already in the SO, if any.  This subtlety is needed to model
   rwlocks: a strong send corresponds to releasing a rwlock that had
   been w-held (or releasing a standard mutex).  A weak send
   corresponds to releasing a rwlock that has been r-held.

   (rationale): Since in general many threads may hold a rwlock in
   r-mode, a weak send facility is necessary in order that the final
   SO reflects the join of the VCs of all the threads releasing the
   rwlock, rather than merely holding the VC of the most recent thread
   to release it. */
void libhb_so_send ( Thr* thr, SO* so, Bool strong_send );

/* Recv a message from a sync object.  If strong_recv is True, the
   resulting inter-thread dependency is considered adequate to induce
   a h-b ordering on both reads and writes.  If it is False, the
   implied h-b ordering exists only for reads, not writes.  This is
   subtlety is required in order to support reader-writer locks: a
   thread doing a write-acquire of a rwlock (or acquiring a normal
   mutex) models this by doing a strong receive.  A thread doing a
   read-acquire of a rwlock models this by doing a !strong_recv. */
void libhb_so_recv ( Thr* thr, SO* so, Bool strong_recv );

/* Has this SO ever been sent on? */
Bool libhb_so_everSent ( SO* so );

/* Memory accesses (1/2/4/8 byte size).  They report a race if one is
   found. */
#define LIBHB_CWRITE_1(_thr,_a)    zsm_sapply08_f__msmcwrite((_thr),(_a))
#define LIBHB_CWRITE_2(_thr,_a)    zsm_sapply16_f__msmcwrite((_thr),(_a))
#define LIBHB_CWRITE_4(_thr,_a)    zsm_sapply32_f__msmcwrite((_thr),(_a))
#define LIBHB_CWRITE_8(_thr,_a)    zsm_sapply64_f__msmcwrite((_thr),(_a))
#define LIBHB_CWRITE_N(_thr,_a,_n) zsm_sapplyNN_f__msmcwrite((_thr),(_a),(_n))

#define LIBHB_CREAD_1(_thr,_a)    zsm_sapply08_f__msmcread((_thr),(_a))
#define LIBHB_CREAD_2(_thr,_a)    zsm_sapply16_f__msmcread((_thr),(_a))
#define LIBHB_CREAD_4(_thr,_a)    zsm_sapply32_f__msmcread((_thr),(_a))
#define LIBHB_CREAD_8(_thr,_a)    zsm_sapply64_f__msmcread((_thr),(_a))
#define LIBHB_CREAD_N(_thr,_a,_n) zsm_sapplyNN_f__msmcread((_thr),(_a),(_n))

void zsm_sapply08_f__msmcwrite ( Thr* thr, Addr a );
void zsm_sapply16_f__msmcwrite ( Thr* thr, Addr a );
void zsm_sapply32_f__msmcwrite ( Thr* thr, Addr a );
void zsm_sapply64_f__msmcwrite ( Thr* thr, Addr a );
void zsm_sapplyNN_f__msmcwrite ( Thr* thr, Addr a, SizeT len );

void zsm_sapply08_f__msmcread ( Thr* thr, Addr a );
void zsm_sapply16_f__msmcread ( Thr* thr, Addr a );
void zsm_sapply32_f__msmcread ( Thr* thr, Addr a );
void zsm_sapply64_f__msmcread ( Thr* thr, Addr a );
void zsm_sapplyNN_f__msmcread ( Thr* thr, Addr a, SizeT len );

void libhb_Thr_resumes ( Thr* thr );

/* Set memory address ranges to new (freshly allocated), or noaccess
   (no longer accessible).  NB: "AHAE" == "Actually Has An Effect" :-) */
void libhb_srange_new      ( Thr*, Addr, SizeT );
void libhb_srange_untrack  ( Thr*, Addr, SizeT );
void libhb_srange_noaccess_NoFX ( Thr*, Addr, SizeT ); /* IS IGNORED */
void libhb_srange_noaccess_AHAE ( Thr*, Addr, SizeT ); /* IS NOT IGNORED */

/* Counts the nr of bytes addressable in the range [a, a+len[
   (so a+len excluded) and returns the nr of addressable bytes found.
   If abits /= NULL, abits must point to a block of memory of length len.
   In this array, each addressable byte will be indicated with 0xff.
   Non-addressable bytes are indicated with 0x00. */
UWord libhb_srange_get_abits (Addr a, /*OUT*/UChar *abits, SizeT len);

/* Get and set the hgthread (pointer to corresponding Thread
   structure). */
Thread* libhb_get_Thr_hgthread ( Thr* );
void    libhb_set_Thr_hgthread ( Thr*, Thread* );

/* Low level copy of shadow state from [src,src+len) to [dst,dst+len).
   Overlapping moves are checked for and asserted against. */
void libhb_copy_shadow_state ( Thr* thr, Addr src, Addr dst, SizeT len );

/* Call this periodically to give libhb the opportunity to
   garbage-collect its internal data structures. */
void libhb_maybe_GC ( void );

/* Extract info from the conflicting-access machinery. */
Bool libhb_event_map_lookup ( /*OUT*/ExeContext** resEC,
                              /*OUT*/Thr**        resThr,
                              /*OUT*/SizeT*       resSzB,
                              /*OUT*/Bool*        resIsW,
                              /*OUT*/WordSetID*   locksHeldW,
                              Thr* thr, Addr a, SizeT szB, Bool isW );

typedef void (*Access_t) (StackTrace ips, UInt n_ips,
                          Thr*  Thr_a,
                          Addr  ga,
                          SizeT SzB,
                          Bool  isW,
                          WordSetID locksHeldW );
/* Call fn for each recorded access history that overlaps with range [a, a+szB[.
   fn is first called for oldest access.*/
void libhb_event_map_access_history ( Addr a, SizeT szB, Access_t fn );

/* ------ Exported from hg_main.c ------ */
/* Yes, this is a horrible tangle.  Sigh. */

/* Get the univ_lset (universe for locksets) from hg_main.c.  Sigh. */
WordSetU* HG_(get_univ_lsets) ( void );

/* Get the the header pointer for the double linked list of locks
   (admin_locks). */
Lock* HG_(get_admin_locks) ( void );

#endif /* __LIBHB_H */

/*--------------------------------------------------------------------*/
/*--- end                                                  libhb.h ---*/
/*--------------------------------------------------------------------*/
