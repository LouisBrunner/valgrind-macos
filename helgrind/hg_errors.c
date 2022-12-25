
/*--------------------------------------------------------------------*/
/*--- Error management for Helgrind.                               ---*/
/*---                                                  hg_errors.c ---*/
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

#include "pub_tool_basics.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_stacktrace.h"
#include "pub_tool_execontext.h"
#include "pub_tool_errormgr.h"
#include "pub_tool_wordfm.h"
#include "pub_tool_xarray.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_options.h"     // VG_(clo_xml)
#include "pub_tool_aspacemgr.h"
#include "pub_tool_addrinfo.h"

#include "hg_basics.h"
#include "hg_addrdescr.h"
#include "hg_wordset.h"
#include "hg_lock_n_thread.h"
#include "libhb.h"
#include "hg_errors.h"            /* self */


/*----------------------------------------------------------------*/
/*--- Error management -- storage                              ---*/
/*----------------------------------------------------------------*/

/* maps (by value) strings to a copy of them in ARENA_TOOL */

static WordFM* string_table = NULL;

ULong HG_(stats__string_table_queries) = 0;

ULong HG_(stats__string_table_get_map_size) ( void ) {
   return string_table ? (ULong)VG_(sizeFM)(string_table) : 0;
}

static Word string_table_cmp ( UWord s1, UWord s2 ) {
   return (Word)VG_(strcmp)( (HChar*)s1, (HChar*)s2 );
}

static HChar* string_table_strdup ( const HChar* str ) {
   HChar* copy = NULL;
   HG_(stats__string_table_queries)++;
   if (!str)
      str = "(null)";
   if (!string_table) {
      string_table = VG_(newFM)( HG_(zalloc), "hg.sts.1",
                                 HG_(free), string_table_cmp );
   }
   if (VG_(lookupFM)( string_table,
                      NULL, (UWord*)&copy, (UWord)str )) {
      tl_assert(copy);
      if (0) VG_(printf)("string_table_strdup: %p -> %p\n", str, copy );
      return copy;
   } else {
      copy = HG_(strdup)("hg.sts.2", str);
      VG_(addToFM)( string_table, (UWord)copy, (UWord)copy );
      return copy;
   }
}

/* maps from Lock .unique fields to LockP*s */

static WordFM* map_LockN_to_P = NULL;

ULong HG_(stats__LockN_to_P_queries) = 0;

ULong HG_(stats__LockN_to_P_get_map_size) ( void ) {
   return map_LockN_to_P ? (ULong)VG_(sizeFM)(map_LockN_to_P) : 0;
}

static Word lock_unique_cmp ( UWord lk1W, UWord lk2W )
{
   Lock* lk1 = (Lock*)lk1W;
   Lock* lk2 = (Lock*)lk2W;
   tl_assert( HG_(is_sane_LockNorP)(lk1) );
   tl_assert( HG_(is_sane_LockNorP)(lk2) );
   if (lk1->unique < lk2->unique) return -1;
   if (lk1->unique > lk2->unique) return 1;
   return 0;
}

/* Given a normal Lock (LockN), convert it to a persistent Lock
   (LockP).  In some cases the LockN could be invalid (if it's been
   freed), so we enquire, in hg_main.c's admin_locks list, whether it
   is in fact valid.  If allowed_to_be_invalid is True, then it's OK
   for the LockN to be invalid, in which case Lock_INVALID is
   returned.  In all other cases, we insist that the LockN is a valid
   lock, and return its corresponding LockP.

   Why can LockNs sometimes be invalid?  Because they are harvested
   from locksets that are attached to the OldRef info for conflicting
   threads.  By the time we detect a race, the some of the elements of
   the lockset may have been destroyed by the client, in which case
   the corresponding Lock structures we maintain will have been freed.

   So we check that each LockN is a member of the admin_locks double
   linked list of all Lock structures.  That stops us prodding around
   in potentially freed-up Lock structures.  However, it's not quite a
   proper check: if a new Lock has been reallocated at the same
   address as one which was previously freed, we'll wind up copying
   the new one as the basis for the LockP, which is completely bogus
   because it is unrelated to the previous Lock that lived there.
   Let's hope that doesn't happen too often.
*/
static Lock* mk_LockP_from_LockN ( Lock* lkn,
                                   Bool allowed_to_be_invalid )
{
   Lock* lkp = NULL;
   HG_(stats__LockN_to_P_queries)++;

   /* First off, let's do some sanity checks.  If
      allowed_to_be_invalid is False, we _must_ be able to find 'lkn'
      in admin_locks; else we must assert.  If it is True, it's OK for
      it not to be findable, but in that case we must return
      Lock_INVALID right away. */
   Lock* lock_list = HG_(get_admin_locks)();
   while (lock_list) {
      if (lock_list == lkn)
         break;
      lock_list = lock_list->admin_next;
   }
   if (lock_list == NULL) {
      /* We didn't find it.  That possibility has to be OK'd by the
         caller. */
      tl_assert(allowed_to_be_invalid);
      return Lock_INVALID;
   }

   /* So we must be looking at a valid LockN. */
   tl_assert( HG_(is_sane_LockN)(lkn) );

   if (!map_LockN_to_P) {
      map_LockN_to_P = VG_(newFM)( HG_(zalloc), "hg.mLPfLN.1",
                                   HG_(free), lock_unique_cmp );
   }
   if (!VG_(lookupFM)( map_LockN_to_P, NULL, (UWord*)&lkp, (UWord)lkn)) {
      lkp = HG_(zalloc)( "hg.mLPfLN.2", sizeof(Lock) );
      *lkp = *lkn;
      lkp->admin_next = NULL;
      lkp->admin_prev = NULL;
      lkp->magic = LockP_MAGIC;
      /* Forget about the bag of lock holders - don't copy that.
         Also, acquired_at should be NULL whenever heldBy is, and vice
         versa.  Also forget about the associated libhb synch object. */
      lkp->heldW  = False;
      lkp->heldBy = NULL;
      lkp->acquired_at = NULL;
      lkp->hbso = NULL;
      VG_(addToFM)( map_LockN_to_P, (UWord)lkp, (UWord)lkp );
   }
   tl_assert( HG_(is_sane_LockP)(lkp) );
   return lkp;
}

static Int sort_by_guestaddr(const void* n1, const void* n2)
{
   const Lock* l1 = *(const Lock *const *)n1;
   const Lock* l2 = *(const Lock *const *)n2;

   Addr a1 = l1 == Lock_INVALID ? 0 : l1->guestaddr;
   Addr a2 = l2 == Lock_INVALID ? 0 : l2->guestaddr;
   if (a1 < a2) return -1;
   if (a1 > a2) return 1;
   return 0;
}

/* Expand a WordSet of LockN*'s into a NULL-terminated vector of
   LockP*'s.  Any LockN's that can't be converted into a LockP
   (because they have been freed, see comment on mk_LockP_from_LockN)
   are converted instead into the value Lock_INVALID.  Hence the
   returned vector is a sequence: zero or more (valid LockP* or
   LockN_INVALID), terminated by a NULL. */
static 
Lock** enumerate_WordSet_into_LockP_vector( WordSetU* univ_lsets,
                                            WordSetID lockset,
                                            Bool allowed_to_be_invalid )
{
   tl_assert(univ_lsets);
   tl_assert( HG_(plausibleWS)(univ_lsets, lockset) );
   UWord  nLocks = HG_(cardinalityWS)(univ_lsets, lockset);
   Lock** lockPs = HG_(zalloc)( "hg.eWSiLPa",
                                (nLocks+1) * sizeof(Lock*) );
   tl_assert(lockPs[nLocks] == NULL); /* pre-NULL terminated */
   UWord* lockNs  = NULL;
   UWord  nLockNs = 0;
   if (nLocks > 0)  {
      /* HG_(getPayloadWS) doesn't assign non-NULL to &lockNs if the
         lockset is empty; hence the guarding "if".  Sigh. */
      HG_(getPayloadWS)( &lockNs, &nLockNs, univ_lsets, lockset );
      tl_assert(lockNs);
   }
   UWord i;
   /* Convert to LockPs. */
   for (i = 0; i < nLockNs; i++) {
      lockPs[i] = mk_LockP_from_LockN( (Lock*)lockNs[i],
                                       allowed_to_be_invalid );
   }
   /* Sort the locks by increasing Lock::guestaddr to avoid jitters
      in the output. */
   VG_(ssort)(lockPs, nLockNs, sizeof lockPs[0], sort_by_guestaddr);

   return lockPs;
}

/* Get the number of useful elements in a vector created by
   enumerate_WordSet_into_LockP_vector.  Returns both the total number
   of elements (not including the terminating NULL) and the number of
   non-Lock_INVALID elements. */
static void count_LockP_vector ( /*OUT*/UWord* nLocks,
                                 /*OUT*/UWord* nLocksValid,
                                 Lock** vec )
{
   tl_assert(vec);
   *nLocks = *nLocksValid = 0;
   UWord n = 0;
   while (vec[n]) {
      (*nLocks)++;
      if (vec[n] != Lock_INVALID)
         (*nLocksValid)++;
      n++;
   }
}

/* Find out whether 'lk' is in 'vec'. */
static Bool elem_LockP_vector ( Lock** vec, Lock* lk )
{
   tl_assert(vec);
   tl_assert(lk);
   UWord n = 0;
   while (vec[n]) {
      if (vec[n] == lk)
         return True;
      n++;
   }
   return False;
}


/* Errors:

      race: program counter
            read or write
            data size
            previous state
            current state

      FIXME: how does state printing interact with lockset gc?
      Are the locksets in prev/curr state always valid?
      Ditto question for the threadsets
          ThreadSets - probably are always valid if Threads
          are never thrown away.
          LockSets - could at least print the lockset elements that
          correspond to actual locks at the time of printing.  Hmm.
*/

/* Error kinds */
typedef
   enum {
      XE_Race=1101,      // race
      XE_UnlockUnlocked, // unlocking a not-locked lock
      XE_UnlockForeign,  // unlocking a lock held by some other thread
      XE_UnlockBogus,    // unlocking an address not known to be a lock
      XE_PthAPIerror,    // error from the POSIX pthreads API
      XE_LockOrder,      // lock order error
      XE_Misc,           // misc other error (w/ string to describe it)
      XE_Dubious         // a bit like misc for cases where the POSIX
                         // spec is unclear on error conditons
   }
   XErrorTag;

/* Extra contexts for kinds */
typedef
   struct  {
      XErrorTag tag;
      union {
         struct {
            Addr        data_addr;
            Int         szB;
            AddrInfo    data_addrinfo;
            Bool        isWrite;
            Thread*     thr;
            Lock**      locksHeldW;
            /* h1_* and h2_* provide some description of a previously
               observed access with which we are conflicting. */
            Thread*     h1_ct; /* non-NULL means h1 info present */
            ExeContext* h1_ct_mbsegstartEC;
            ExeContext* h1_ct_mbsegendEC;
            Thread*     h2_ct; /* non-NULL means h2 info present */
            ExeContext* h2_ct_accEC;
            Int         h2_ct_accSzB;
            Bool        h2_ct_accIsW;
            Lock**      h2_ct_locksHeldW;
         } Race;
         struct {
            Thread* thr;  /* doing the unlocking */
            Lock*   lock; /* lock (that is already unlocked) */
         } UnlockUnlocked;
         struct {
            Thread* thr;    /* doing the unlocking */
            Thread* owner;  /* thread that actually holds the lock */
            Lock*   lock;   /* lock (that is held by 'owner') */
         } UnlockForeign;
         struct {
            Thread* thr;     /* doing the unlocking */
            Addr    lock_ga; /* purported address of the lock */
         } UnlockBogus;
         struct {
            Thread* thr; 
            HChar*  fnname; /* persistent, in tool-arena */
            Word    err;    /* pth error code */
            HChar*  errstr; /* persistent, in tool-arena */
         } PthAPIerror;
         struct {
            Thread*     thr;
            /* The first 4 fields describe the previously observed
               (should-be) ordering. */
            Lock*       shouldbe_earlier_lk;
            Lock*       shouldbe_later_lk;
            ExeContext* shouldbe_earlier_ec;
            ExeContext* shouldbe_later_ec;
            /* In principle we need to record two more stacks, from
               this thread, when acquiring the locks in the "wrong"
               order.  In fact the wallclock-later acquisition by this
               thread is recorded in the main stack for this error.
               So we only need a stack for the earlier acquisition by
               this thread. */
            ExeContext* actual_earlier_ec;
         } LockOrder;
         struct {
            Thread*     thr;
            HChar*      errstr; /* persistent, in tool-arena */
            HChar*      auxstr; /* optional, persistent, in tool-arena */
            ExeContext* auxctx; /* optional */
         } Misc;
      } XE;
   }
   XError;

static void init_XError ( XError* xe ) {
   VG_(memset)(xe, 0, sizeof(*xe) );
   xe->tag = XE_Race-1; /* bogus */
}


/* Extensions of suppressions */
typedef
   enum {
      XS_Race=1201, /* race */
      XS_FreeMemLock,
      XS_UnlockUnlocked,
      XS_UnlockForeign,
      XS_UnlockBogus,
      XS_PthAPIerror,
      XS_LockOrder,
      XS_Misc,
      XS_Dubious
   }
   XSuppTag;


/* Updates the copy with address info if necessary. */
UInt HG_(update_extra) ( const Error* err )
{
   XError* xe = (XError*)VG_(get_error_extra)(err);
   tl_assert(xe);
   //if (extra != NULL && Undescribed == extra->addrinfo.akind) {
   //   describe_addr ( VG_(get_error_address)(err), &(extra->addrinfo) );
   //}

   if (xe->tag == XE_Race) {

      /* Note the set of locks that the thread is (w-)holding.
         Convert the WordSetID of LockN*'s into a NULL-terminated
         vector of LockP*'s.  We don't expect to encounter any invalid
         LockNs in this conversion. */
      tl_assert(xe->XE.Race.thr);
      xe->XE.Race.locksHeldW
         = enumerate_WordSet_into_LockP_vector(
              HG_(get_univ_lsets)(),
              xe->XE.Race.thr->locksetW,
              False/*!allowed_to_be_invalid*/
           );

      /* See if we can come up with a source level description of the
         raced-upon address.  This is potentially expensive, which is
         why it's only done at the update_extra point, not when the
         error is initially created. */
      static Int xxx = 0;
      xxx++;
      if (0)
         VG_(printf)("HG_(update_extra): "
                     "%d conflicting-event queries\n", xxx);

      HG_(describe_addr) (VG_(get_ExeContext_epoch)(VG_(get_error_where)(err)),
                          xe->XE.Race.data_addr, &xe->XE.Race.data_addrinfo);

      /* And poke around in the conflicting-event map, to see if we
         can rustle up a plausible-looking conflicting memory access
         to show. */
      if (HG_(clo_history_level) >= 2) { 
         Thr*        thrp            = NULL;
         ExeContext* wherep          = NULL;
         Addr        acc_addr        = xe->XE.Race.data_addr;
         Int         acc_szB         = xe->XE.Race.szB;
         Thr*        acc_thr         = xe->XE.Race.thr->hbthr;
         Bool        acc_isW         = xe->XE.Race.isWrite;
         SizeT       conf_szB        = 0;
         Bool        conf_isW        = False;
         WordSetID   conf_locksHeldW = 0;
         tl_assert(!xe->XE.Race.h2_ct_accEC);
         tl_assert(!xe->XE.Race.h2_ct);
         if (libhb_event_map_lookup(
                &wherep, &thrp, &conf_szB, &conf_isW, &conf_locksHeldW,
                acc_thr, acc_addr, acc_szB, acc_isW )) {
            Thread* threadp;
            tl_assert(wherep);
            tl_assert(thrp);
            threadp = libhb_get_Thr_hgthread( thrp );
            tl_assert(threadp);
            xe->XE.Race.h2_ct_accEC  = wherep;
            xe->XE.Race.h2_ct        = threadp;
            xe->XE.Race.h2_ct_accSzB = (Int)conf_szB;
            xe->XE.Race.h2_ct_accIsW = conf_isW;
            xe->XE.Race.h2_ct_locksHeldW
               = enumerate_WordSet_into_LockP_vector(
                    HG_(get_univ_lsets)(),
                    conf_locksHeldW,
                    True/*allowed_to_be_invalid*/
                 );
        }
      }

      // both NULL or both non-NULL
      tl_assert( (!!xe->XE.Race.h2_ct) == (!!xe->XE.Race.h2_ct_accEC) );
   }

   return sizeof(XError);
}

void HG_(record_error_Race) ( Thread* thr, 
                              Addr data_addr, Int szB, Bool isWrite,
                              Thread* h1_ct,
                              ExeContext* h1_ct_segstart,
                              ExeContext* h1_ct_mbsegendEC )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );

#  if defined(VGO_linux) || defined(VGO_freebsd)
   /* Skip any races on locations apparently in GOTPLT sections.  This
      is said to be caused by ld.so poking PLT table entries (or
      whatever) when it writes the resolved address of a dynamically
      linked routine, into the table (or whatever) when it is called
      for the first time. */
   {
     VgSectKind sect = VG_(DebugInfo_sect_kind)( NULL, data_addr );
     if (0) VG_(printf)("XXXXXXXXX RACE on %#lx %s\n",
                        data_addr, VG_(pp_SectKind)(sect));
     /* SectPLT is required on ???-linux */
     if (sect == Vg_SectGOTPLT) return;
     /* SectPLT is required on ppc32/64-linux */
     if (sect == Vg_SectPLT) return;
     /* SectGOT is required on arm-linux */
     if (sect == Vg_SectGOT) return;
   }
#  endif

   init_XError(&xe);
   xe.tag = XE_Race;
   xe.XE.Race.data_addr   = data_addr;
   xe.XE.Race.szB         = szB;
   xe.XE.Race.isWrite     = isWrite;
   xe.XE.Race.thr         = thr;
   tl_assert(isWrite == False || isWrite == True);
   tl_assert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   /* Skip on the detailed description of the raced-on address at this
      point; it's expensive.  Leave it for the update_extra function
      if we ever make it that far. */
   xe.XE.Race.data_addrinfo.tag = Addr_Undescribed;
   // FIXME: tid vs thr
   // Skip on any of the conflicting-access info at this point.
   // It's expensive to obtain, and this error is more likely than
   // not to be discarded.  We'll fill these fields in in 
   // HG_(update_extra) just above, assuming the error ever makes
   // it that far (unlikely).
   xe.XE.Race.h2_ct_accSzB = 0;
   xe.XE.Race.h2_ct_accIsW = False;
   xe.XE.Race.h2_ct_accEC  = NULL;
   xe.XE.Race.h2_ct        = NULL;
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );

   xe.XE.Race.h1_ct              = h1_ct;
   xe.XE.Race.h1_ct_mbsegstartEC = h1_ct_segstart;
   xe.XE.Race.h1_ct_mbsegendEC   = h1_ct_mbsegendEC;

   VG_(maybe_record_error)( thr->coretid,
                            XE_Race, data_addr, NULL, &xe );
}

void HG_(record_error_UnlockUnlocked) ( Thread* thr, Lock* lk )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert( HG_(is_sane_LockN)(lk) );
   init_XError(&xe);
   xe.tag = XE_UnlockUnlocked;
   xe.XE.UnlockUnlocked.thr
      = thr;
   xe.XE.UnlockUnlocked.lock
      = mk_LockP_from_LockN(lk, False/*!allowed_to_be_invalid*/);
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_UnlockUnlocked, 0, NULL, &xe );
}

void HG_(record_error_UnlockForeign) ( Thread* thr,
                                       Thread* owner, Lock* lk )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert( HG_(is_sane_Thread)(owner) );
   tl_assert( HG_(is_sane_LockN)(lk) );
   init_XError(&xe);
   xe.tag = XE_UnlockForeign;
   xe.XE.UnlockForeign.thr   = thr;
   xe.XE.UnlockForeign.owner = owner;
   xe.XE.UnlockForeign.lock
      = mk_LockP_from_LockN(lk, False/*!allowed_to_be_invalid*/);
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_UnlockForeign, 0, NULL, &xe );
}

void HG_(record_error_UnlockBogus) ( Thread* thr, Addr lock_ga )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   init_XError(&xe);
   xe.tag = XE_UnlockBogus;
   xe.XE.UnlockBogus.thr     = thr;
   xe.XE.UnlockBogus.lock_ga = lock_ga;
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_UnlockBogus, 0, NULL, &xe );
}

void HG_(record_error_LockOrder)(
        Thread*     thr, 
        Lock*       shouldbe_earlier_lk,
        Lock*       shouldbe_later_lk,
        ExeContext* shouldbe_earlier_ec,
        ExeContext* shouldbe_later_ec,
        ExeContext* actual_earlier_ec
     )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert(HG_(clo_track_lockorders));
   init_XError(&xe);
   xe.tag = XE_LockOrder;
   xe.XE.LockOrder.thr       = thr;
   xe.XE.LockOrder.shouldbe_earlier_lk 
      = mk_LockP_from_LockN(shouldbe_earlier_lk, 
                            False/*!allowed_to_be_invalid*/);
   xe.XE.LockOrder.shouldbe_earlier_ec = shouldbe_earlier_ec;
   xe.XE.LockOrder.shouldbe_later_lk   
      = mk_LockP_from_LockN(shouldbe_later_lk, 
                            False/*!allowed_to_be_invalid*/);
   xe.XE.LockOrder.shouldbe_later_ec   = shouldbe_later_ec;
   xe.XE.LockOrder.actual_earlier_ec   = actual_earlier_ec;
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_LockOrder, 0, NULL, &xe );
}

void HG_(record_error_PthAPIerror) ( Thread* thr, const HChar* fnname, 
                                     Word err, const HChar* errstr )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert(fnname);
   tl_assert(errstr);
   init_XError(&xe);
   xe.tag = XE_PthAPIerror;
   xe.XE.PthAPIerror.thr    = thr;
   xe.XE.PthAPIerror.fnname = string_table_strdup(fnname);
   xe.XE.PthAPIerror.err    = err;
   xe.XE.PthAPIerror.errstr = string_table_strdup(errstr);
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_PthAPIerror, 0, NULL, &xe );
}

void HG_(record_error_Misc_w_aux) ( Thread* thr, const HChar* errstr,
                                    const HChar* auxstr, ExeContext* auxctx )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert(errstr);
   init_XError(&xe);
   xe.tag = XE_Misc;
   xe.XE.Misc.thr    = thr;
   xe.XE.Misc.errstr = string_table_strdup(errstr);
   xe.XE.Misc.auxstr = auxstr ? string_table_strdup(auxstr) : NULL;
   xe.XE.Misc.auxctx = auxctx;
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_Misc, 0, NULL, &xe );
}

void HG_(record_error_Misc) ( Thread* thr, const HChar* errstr )
{
   HG_(record_error_Misc_w_aux)(thr, errstr, NULL, NULL);
}

void HG_(record_error_Dubious_w_aux) ( Thread* thr, const HChar* errstr,
                                    const HChar* auxstr, ExeContext* auxctx )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert(errstr);
   init_XError(&xe);
   xe.tag = XE_Dubious;
   xe.XE.Misc.thr    = thr;
   xe.XE.Misc.errstr = string_table_strdup(errstr);
   xe.XE.Misc.auxstr = auxstr ? string_table_strdup(auxstr) : NULL;
   xe.XE.Misc.auxctx = auxctx;
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_Dubious, 0, NULL, &xe );
}

void HG_(record_error_Dubious) ( Thread* thr, const HChar* errstr )
{
   HG_(record_error_Dubious_w_aux)(thr, errstr, NULL, NULL);
}

Bool HG_(eq_Error) ( VgRes not_used, const Error* e1, const Error* e2 )
{
   XError *xe1, *xe2;

   tl_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));

   xe1 = (XError*)VG_(get_error_extra)(e1);
   xe2 = (XError*)VG_(get_error_extra)(e2);
   tl_assert(xe1);
   tl_assert(xe2);

   switch (VG_(get_error_kind)(e1)) {
      case XE_Race:
         return xe1->XE.Race.szB == xe2->XE.Race.szB
                && xe1->XE.Race.isWrite == xe2->XE.Race.isWrite
                && (HG_(clo_cmp_race_err_addrs)
                       ? xe1->XE.Race.data_addr == xe2->XE.Race.data_addr
                       : True);
      case XE_UnlockUnlocked:
         return xe1->XE.UnlockUnlocked.thr == xe2->XE.UnlockUnlocked.thr
                && xe1->XE.UnlockUnlocked.lock == xe2->XE.UnlockUnlocked.lock;
      case XE_UnlockForeign:
         return xe1->XE.UnlockForeign.thr == xe2->XE.UnlockForeign.thr
                && xe1->XE.UnlockForeign.owner == xe2->XE.UnlockForeign.owner
                && xe1->XE.UnlockForeign.lock == xe2->XE.UnlockForeign.lock;
      case XE_UnlockBogus:
         return xe1->XE.UnlockBogus.thr == xe2->XE.UnlockBogus.thr
                && xe1->XE.UnlockBogus.lock_ga == xe2->XE.UnlockBogus.lock_ga;
      case XE_PthAPIerror:
         return xe1->XE.PthAPIerror.thr == xe2->XE.PthAPIerror.thr
                && 0==VG_(strcmp)(xe1->XE.PthAPIerror.fnname,
                                  xe2->XE.PthAPIerror.fnname)
                && xe1->XE.PthAPIerror.err == xe2->XE.PthAPIerror.err;
      case XE_LockOrder:
         return xe1->XE.LockOrder.thr == xe2->XE.LockOrder.thr;
      case XE_Misc:
         return xe1->XE.Misc.thr == xe2->XE.Misc.thr
                && 0==VG_(strcmp)(xe1->XE.Misc.errstr, xe2->XE.Misc.errstr);
      case XE_Dubious:
      return xe1->XE.Misc.thr == xe2->XE.Misc.thr
             && 0==VG_(strcmp)(xe1->XE.Misc.errstr, xe2->XE.Misc.errstr);
      default:
         tl_assert(0);
   }

   /*NOTREACHED*/
   tl_assert(0);
}


/*----------------------------------------------------------------*/
/*--- Error management -- printing                             ---*/
/*----------------------------------------------------------------*/

/* Do a printf-style operation on either the XML or normal output
   channel, depending on the setting of VG_(clo_xml).
*/
static void emit_WRK ( const HChar* format, va_list vargs )
{
   if (VG_(clo_xml)) {
      VG_(vprintf_xml)(format, vargs);
   } else {
      VG_(vmessage)(Vg_UserMsg, format, vargs);
   }
}
static void emit ( const HChar* format, ... ) PRINTF_CHECK(1, 2);
static void emit ( const HChar* format, ... )
{
   va_list vargs;
   va_start(vargs, format);
   emit_WRK(format, vargs);
   va_end(vargs);
}


/* Announce (that is, print the point-of-creation) of 'thr'.  Only do
   this once, as we only want to see these announcements once per
   thread.  Returned Bool indicates whether or not an announcement was
   made.
*/
static Bool announce_one_thread ( Thread* thr ) 
{
   tl_assert(HG_(is_sane_Thread)(thr));
   tl_assert(thr->errmsg_index >= 1);
   if (thr->announced)
      return False;

   if (VG_(clo_xml)) {

      VG_(printf_xml)("<announcethread>\n");
      VG_(printf_xml)("  <hthreadid>%d</hthreadid>\n", thr->errmsg_index);
      if (thr->errmsg_index == 1) {
         tl_assert(thr->created_at == NULL);
         VG_(printf_xml)("  <isrootthread></isrootthread>\n");
      } else {
         tl_assert(thr->created_at != NULL);
         VG_(pp_ExeContext)( thr->created_at );
      }
      VG_(printf_xml)("</announcethread>\n\n");

   } else {

      VG_(umsg)("---Thread-Announcement----------"
                "--------------------------------" "\n");
      VG_(umsg)("\n");

      if (thr->errmsg_index == 1) {
         tl_assert(thr->created_at == NULL);
         VG_(message)(Vg_UserMsg, 
                      "Thread #%d is the program's root thread\n",
                       thr->errmsg_index);
      } else {
         tl_assert(thr->created_at != NULL);
         VG_(message)(Vg_UserMsg, "Thread #%d was created\n",
                                  thr->errmsg_index);
         VG_(pp_ExeContext)( thr->created_at );
      }
      VG_(message)(Vg_UserMsg, "\n");

   }

   thr->announced = True;
   return True;
}

/* Announce 'lk'. */
static void announce_LockP ( Lock* lk )
{
   tl_assert(lk);
   if (lk == Lock_INVALID)
      return; /* Can't be announced -- we know nothing about it. */
   tl_assert(lk->magic == LockP_MAGIC);

   if (VG_(clo_xml)) {
      if (lk->appeared_at) {
         emit( "  <auxwhat>Lock at %p was first observed</auxwhat>\n",
               (void*)lk );
         VG_(pp_ExeContext)( lk->appeared_at );
      }

   } else {
      if (lk->appeared_at) {
         VG_(umsg)( " Lock at %p was first observed\n",
                    (void*)lk->guestaddr );
         VG_(pp_ExeContext)( lk->appeared_at );
      } else {
         VG_(umsg)( " Lock at %p : no stacktrace for first observation\n",
                    (void*)lk->guestaddr );
      }
      HG_(get_and_pp_addrdescr)
         (lk->appeared_at
          ? VG_(get_ExeContext_epoch)(lk->appeared_at)
          : VG_(current_DiEpoch)(),
          lk->guestaddr);
      VG_(umsg)("\n");
   }
}

/* Announce (that is, print point-of-first-observation) for the
   locks in 'lockvec' and, if non-NULL, 'lockvec2'. */
static void announce_combined_LockP_vecs ( Lock** lockvec,
                                           Lock** lockvec2 )
{
   UWord i;
   tl_assert(lockvec);
   for (i = 0; lockvec[i]; i++) {
      announce_LockP(lockvec[i]);
   }
   if (lockvec2) {
      for (i = 0; lockvec2[i]; i++) {
         Lock* lk = lockvec2[i];
         if (!elem_LockP_vector(lockvec, lk))
            announce_LockP(lk);
      }
   }
}


static void show_LockP_summary_textmode ( Lock** locks, const HChar* pre )
{
   tl_assert(locks);
   UWord i;
   UWord nLocks = 0, nLocksValid = 0;
   count_LockP_vector(&nLocks, &nLocksValid, locks);
   tl_assert(nLocksValid <= nLocks);

   if (nLocks == 0) {
      VG_(umsg)( "%sLocks held: none", pre );
   } else {
      VG_(umsg)( "%sLocks held: %lu, at address%s ",
                 pre, nLocks, nLocksValid == 1 ? "" : "es" );
   }

   if (nLocks > 0) {
      for (i = 0; i < nLocks; i++) {
         if (locks[i] == Lock_INVALID)
            continue;
         VG_(umsg)( "%p", (void*)locks[i]->guestaddr);
         if (locks[i+1] != NULL)
            VG_(umsg)(" ");
      }
      if (nLocksValid < nLocks)
         VG_(umsg)(" (and %lu that can't be shown)", nLocks - nLocksValid);
   }
   VG_(umsg)("\n");
}


/* This is the "this error is due to be printed shortly; so have a
   look at it any print any preamble you want" function.  We use it to
   announce any previously un-announced threads in the upcoming error
   message.
*/
void HG_(before_pp_Error) ( const Error* err )
{
   XError* xe;
   tl_assert(err);
   xe = (XError*)VG_(get_error_extra)(err);
   tl_assert(xe);

   switch (VG_(get_error_kind)(err)) {
      case XE_Dubious:
         announce_one_thread( xe->XE.Misc.thr );
         break;
      case XE_Misc:
         announce_one_thread( xe->XE.Misc.thr );
         break;
      case XE_LockOrder:
         announce_one_thread( xe->XE.LockOrder.thr );
         break;
      case XE_PthAPIerror:
         announce_one_thread( xe->XE.PthAPIerror.thr );
         break;
      case XE_UnlockBogus:
         announce_one_thread( xe->XE.UnlockBogus.thr );
         break;
      case XE_UnlockForeign:
         announce_one_thread( xe->XE.UnlockForeign.thr );
         announce_one_thread( xe->XE.UnlockForeign.owner );
         break;
      case XE_UnlockUnlocked:
         announce_one_thread( xe->XE.UnlockUnlocked.thr );
         break;
      case XE_Race:
         announce_one_thread( xe->XE.Race.thr );
         if (xe->XE.Race.h2_ct)
            announce_one_thread( xe->XE.Race.h2_ct );
         if (xe->XE.Race.h1_ct)
            announce_one_thread( xe->XE.Race.h1_ct );
         if (xe->XE.Race.data_addrinfo.Addr.Block.alloc_tinfo.tnr) {
            Thread* thr = get_admin_threads();
            while (thr) {
               if (thr->errmsg_index 
                   == xe->XE.Race.data_addrinfo.Addr.Block.alloc_tinfo.tnr) {
                  announce_one_thread (thr);
                  break;
               }
               thr = thr->admin;
            }
         }
         break;
      default:
         tl_assert(0);
   }
}

void HG_(pp_Error) ( const Error* err )
{
   const Bool xml = VG_(clo_xml); /* a shorthand, that's all */

   if (!xml) {
      VG_(umsg)("--------------------------------"
                "--------------------------------" "\n");
      VG_(umsg)("\n");
   }

   XError *xe = (XError*)VG_(get_error_extra)(err);
   tl_assert(xe);

   if (xml)
      emit( "  <kind>%s</kind>\n", HG_(get_error_name)(err));

   switch (VG_(get_error_kind)(err)) {
   case XE_Dubious: {
      tl_assert( HG_(is_sane_Thread)( xe->XE.Misc.thr ) );

      if (xml) {

         emit( "  <xwhat>\n" );
         emit( "    <text>Thread #%d: %s</text>\n",
               (Int)xe->XE.Misc.thr->errmsg_index,
               xe->XE.Misc.errstr );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.Misc.thr->errmsg_index );
         emit( "  </xwhat>\n" );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         if (xe->XE.Misc.auxstr) {
            emit("  <auxwhat>%s</auxwhat>\n", xe->XE.Misc.auxstr);
            if (xe->XE.Misc.auxctx)
               VG_(pp_ExeContext)( xe->XE.Misc.auxctx );
         }

      } else {

         emit( "Thread #%d: %s\n",
               (Int)xe->XE.Misc.thr->errmsg_index,
               xe->XE.Misc.errstr );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         if (xe->XE.Misc.auxstr) {
            emit(" %s\n", xe->XE.Misc.auxstr);
            if (xe->XE.Misc.auxctx)
               VG_(pp_ExeContext)( xe->XE.Misc.auxctx );
         }

      }
      break;
   }

   case XE_Misc: {
      tl_assert( HG_(is_sane_Thread)( xe->XE.Misc.thr ) );

      if (xml) {

         emit( "  <xwhat>\n" );
         emit( "    <text>Thread #%d: %s</text>\n",
               (Int)xe->XE.Misc.thr->errmsg_index,
               xe->XE.Misc.errstr );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.Misc.thr->errmsg_index );
         emit( "  </xwhat>\n" );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         if (xe->XE.Misc.auxstr) {
            emit("  <auxwhat>%s</auxwhat>\n", xe->XE.Misc.auxstr);
            if (xe->XE.Misc.auxctx)
               VG_(pp_ExeContext)( xe->XE.Misc.auxctx );
         }

      } else {

         emit( "Thread #%d: %s\n",
               (Int)xe->XE.Misc.thr->errmsg_index,
               xe->XE.Misc.errstr );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         if (xe->XE.Misc.auxstr) {
            emit(" %s\n", xe->XE.Misc.auxstr);
            if (xe->XE.Misc.auxctx)
               VG_(pp_ExeContext)( xe->XE.Misc.auxctx );
         }

      }
      break;
   }

   case XE_LockOrder: {
      tl_assert( HG_(is_sane_Thread)( xe->XE.LockOrder.thr ) );

      if (xml) {

         emit( "  <xwhat>\n" );
         emit( "    <text>Thread #%d: lock order \"%p before %p\" "
                    "violated</text>\n",
               (Int)xe->XE.LockOrder.thr->errmsg_index,
               (void*)xe->XE.LockOrder.shouldbe_earlier_lk->guestaddr,
               (void*)xe->XE.LockOrder.shouldbe_later_lk->guestaddr );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.LockOrder.thr->errmsg_index );
         emit( "  </xwhat>\n" );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         if (xe->XE.LockOrder.shouldbe_earlier_ec
             && xe->XE.LockOrder.shouldbe_later_ec) {
            emit( "  <auxwhat>Required order was established by "
                  "acquisition of lock at %p</auxwhat>\n",
                  (void*)xe->XE.LockOrder.shouldbe_earlier_lk->guestaddr );
            VG_(pp_ExeContext)( xe->XE.LockOrder.shouldbe_earlier_ec );
            emit( "  <auxwhat>followed by a later acquisition "
                  "of lock at %p</auxwhat>\n",
                  (void*)xe->XE.LockOrder.shouldbe_later_lk->guestaddr );
            VG_(pp_ExeContext)( xe->XE.LockOrder.shouldbe_later_ec );
         }
         announce_LockP ( xe->XE.LockOrder.shouldbe_earlier_lk );
         announce_LockP ( xe->XE.LockOrder.shouldbe_later_lk );

      } else {

         emit( "Thread #%d: lock order \"%p before %p\" violated\n",
               (Int)xe->XE.LockOrder.thr->errmsg_index,
               (void*)xe->XE.LockOrder.shouldbe_earlier_lk->guestaddr,
               (void*)xe->XE.LockOrder.shouldbe_later_lk->guestaddr );
         emit( "\n" );
         emit( "Observed (incorrect) order is: "
               "acquisition of lock at %p\n",
               (void*)xe->XE.LockOrder.shouldbe_later_lk->guestaddr);
         if (xe->XE.LockOrder.actual_earlier_ec) {
             VG_(pp_ExeContext)(xe->XE.LockOrder.actual_earlier_ec);
         } else {
            emit("   (stack unavailable)\n");
         }
         emit( "\n" );
         emit(" followed by a later acquisition of lock at %p\n",
              (void*)xe->XE.LockOrder.shouldbe_earlier_lk->guestaddr);
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         if (xe->XE.LockOrder.shouldbe_earlier_ec
             && xe->XE.LockOrder.shouldbe_later_ec) {
            emit("\n");
            emit( "Required order was established by "
                  "acquisition of lock at %p\n",
                  (void*)xe->XE.LockOrder.shouldbe_earlier_lk->guestaddr );
            VG_(pp_ExeContext)( xe->XE.LockOrder.shouldbe_earlier_ec );
            emit( "\n" );
            emit( " followed by a later acquisition of lock at %p\n",
                  (void*)xe->XE.LockOrder.shouldbe_later_lk->guestaddr );
            VG_(pp_ExeContext)( xe->XE.LockOrder.shouldbe_later_ec );
         }
         emit("\n");
         announce_LockP ( xe->XE.LockOrder.shouldbe_earlier_lk );
         announce_LockP ( xe->XE.LockOrder.shouldbe_later_lk );

      }

      break;
   }

   case XE_PthAPIerror: {
      tl_assert( HG_(is_sane_Thread)( xe->XE.PthAPIerror.thr ) );

      if (xml) {

         emit( "  <xwhat>\n" );
         emit(
            "    <text>Thread #%d's call to %pS failed</text>\n",
            (Int)xe->XE.PthAPIerror.thr->errmsg_index,
            xe->XE.PthAPIerror.fnname );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.PthAPIerror.thr->errmsg_index );
         emit( "  </xwhat>\n" );
         emit( "  <what>with error code %ld (%s)</what>\n",
               xe->XE.PthAPIerror.err, xe->XE.PthAPIerror.errstr );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );

      } else {

         emit( "Thread #%d's call to %pS failed\n",
                      (Int)xe->XE.PthAPIerror.thr->errmsg_index,
                      xe->XE.PthAPIerror.fnname );
         emit( "   with error code %ld (%s)\n",
               xe->XE.PthAPIerror.err, xe->XE.PthAPIerror.errstr );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );

      }

      break;
   }

   case XE_UnlockBogus: {
      tl_assert( HG_(is_sane_Thread)( xe->XE.UnlockBogus.thr ) );

      if (xml) {

         emit( "  <xwhat>\n" );
         emit( "    <text>Thread #%d unlocked an invalid "
                    "lock at %p</text>\n",
               (Int)xe->XE.UnlockBogus.thr->errmsg_index,
               (void*)xe->XE.UnlockBogus.lock_ga );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.UnlockBogus.thr->errmsg_index );
         emit( "  </xwhat>\n" );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );

      } else {

         emit( "Thread #%d unlocked an invalid lock at %p\n",
               (Int)xe->XE.UnlockBogus.thr->errmsg_index,
               (void*)xe->XE.UnlockBogus.lock_ga );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );

      }

      break;
   }

   case XE_UnlockForeign: {
      tl_assert( HG_(is_sane_LockP)( xe->XE.UnlockForeign.lock ) );
      tl_assert( HG_(is_sane_Thread)( xe->XE.UnlockForeign.owner ) );
      tl_assert( HG_(is_sane_Thread)( xe->XE.UnlockForeign.thr ) );

      if (xml) {

         emit( "  <xwhat>\n" );
         emit( "    <text>Thread #%d unlocked lock at %p "
                    "currently held by thread #%d</text>\n",
               (Int)xe->XE.UnlockForeign.thr->errmsg_index,
               (void*)xe->XE.UnlockForeign.lock->guestaddr,
               (Int)xe->XE.UnlockForeign.owner->errmsg_index );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.UnlockForeign.thr->errmsg_index );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.UnlockForeign.owner->errmsg_index );
         emit( "  </xwhat>\n" );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         announce_LockP ( xe->XE.UnlockForeign.lock );

      } else {

         emit( "Thread #%d unlocked lock at %p "
               "currently held by thread #%d\n",
               (Int)xe->XE.UnlockForeign.thr->errmsg_index,
               (void*)xe->XE.UnlockForeign.lock->guestaddr,
               (Int)xe->XE.UnlockForeign.owner->errmsg_index );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         announce_LockP ( xe->XE.UnlockForeign.lock );

      }

      break;
   }

   case XE_UnlockUnlocked: {
      tl_assert( HG_(is_sane_LockP)( xe->XE.UnlockUnlocked.lock ) );
      tl_assert( HG_(is_sane_Thread)( xe->XE.UnlockUnlocked.thr ) );

      if (xml) {

         emit( "  <xwhat>\n" );
         emit( "    <text>Thread #%d unlocked a "
                    "not-locked lock at %p</text>\n",
               (Int)xe->XE.UnlockUnlocked.thr->errmsg_index,
               (void*)xe->XE.UnlockUnlocked.lock->guestaddr );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.UnlockUnlocked.thr->errmsg_index );
         emit( "  </xwhat>\n" );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         announce_LockP ( xe->XE.UnlockUnlocked.lock);

      } else {

         emit( "Thread #%d unlocked a not-locked lock at %p\n",
               (Int)xe->XE.UnlockUnlocked.thr->errmsg_index,
               (void*)xe->XE.UnlockUnlocked.lock->guestaddr );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         announce_LockP ( xe->XE.UnlockUnlocked.lock);

      }

      break;
   }

   case XE_Race: {
      Addr      err_ga;
      const HChar* what;
      Int       szB;
      what      = xe->XE.Race.isWrite ? "write" : "read";
      szB       = xe->XE.Race.szB;
      err_ga = VG_(get_error_address)(err);

      tl_assert( HG_(is_sane_Thread)( xe->XE.Race.thr ));
      if (xe->XE.Race.h2_ct)
         tl_assert( HG_(is_sane_Thread)( xe->XE.Race.h2_ct ));

      if (xml) {

         /* ------ XML ------ */
         emit( "  <xwhat>\n" );
         emit( "    <text>Possible data race during %s of size %d "
                    "at %p by thread #%d</text>\n",
               what, szB, (void*)err_ga, (Int)xe->XE.Race.thr->errmsg_index );
         emit( "    <hthreadid>%d</hthreadid>\n",
               (Int)xe->XE.Race.thr->errmsg_index );
         emit( "  </xwhat>\n" );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );

         if (xe->XE.Race.h2_ct) {
            tl_assert(xe->XE.Race.h2_ct_accEC); // assured by update_extra
            emit( "  <xauxwhat>\n");
            emit( "    <text>This conflicts with a previous %s of size %d "
                            "by thread #%d</text>\n",
                  xe->XE.Race.h2_ct_accIsW ? "write" : "read",
                  xe->XE.Race.h2_ct_accSzB,
                  xe->XE.Race.h2_ct->errmsg_index );
            emit( "    <hthreadid>%d</hthreadid>\n", 
                  xe->XE.Race.h2_ct->errmsg_index);
            emit("  </xauxwhat>\n");
            VG_(pp_ExeContext)( xe->XE.Race.h2_ct_accEC );
         }

         if (xe->XE.Race.h1_ct) {
            emit( "  <xauxwhat>\n");
            emit( "    <text>This conflicts with a previous access "
                  "by thread #%d, after</text>\n",
                  xe->XE.Race.h1_ct->errmsg_index );
            emit( "    <hthreadid>%d</hthreadid>\n", 
                  xe->XE.Race.h1_ct->errmsg_index );
            emit("  </xauxwhat>\n");
            if (xe->XE.Race.h1_ct_mbsegstartEC) {
               VG_(pp_ExeContext)( xe->XE.Race.h1_ct_mbsegstartEC );
            } else {
               emit( "  <auxwhat>(the start of the thread)</auxwhat>\n" );
            }
            emit( "  <auxwhat>but before</auxwhat>\n" );
            if (xe->XE.Race.h1_ct_mbsegendEC) {
               VG_(pp_ExeContext)( xe->XE.Race.h1_ct_mbsegendEC );
            } else {
               emit( "  <auxwhat>(the end of the thread)</auxwhat>\n" );
            }
         }

      } else {

         /* ------ Text ------ */
         announce_combined_LockP_vecs( xe->XE.Race.locksHeldW,
                                       xe->XE.Race.h2_ct_locksHeldW );

         emit( "Possible data race during %s of size %d "
               "at %p by thread #%d\n",
               what, szB, (void*)err_ga, (Int)xe->XE.Race.thr->errmsg_index );

         tl_assert(xe->XE.Race.locksHeldW);
         show_LockP_summary_textmode( xe->XE.Race.locksHeldW, "" );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );

         if (xe->XE.Race.h2_ct) {
            tl_assert(xe->XE.Race.h2_ct_accEC); // assured by update_extra
            tl_assert(xe->XE.Race.h2_ct_locksHeldW);
            emit( "\n" );
            emit( "This conflicts with a previous %s of size %d "
                  "by thread #%d\n",
                  xe->XE.Race.h2_ct_accIsW ? "write" : "read",
                  xe->XE.Race.h2_ct_accSzB,
                  xe->XE.Race.h2_ct->errmsg_index );
            show_LockP_summary_textmode( xe->XE.Race.h2_ct_locksHeldW, "" );
            VG_(pp_ExeContext)( xe->XE.Race.h2_ct_accEC );
         }

         if (xe->XE.Race.h1_ct) {
            emit( " This conflicts with a previous access by thread #%d, "
                  "after\n",
                  xe->XE.Race.h1_ct->errmsg_index );
            if (xe->XE.Race.h1_ct_mbsegstartEC) {
               VG_(pp_ExeContext)( xe->XE.Race.h1_ct_mbsegstartEC );
            } else {
               emit( "   (the start of the thread)\n" );
            }
            emit( " but before\n" );
            if (xe->XE.Race.h1_ct_mbsegendEC) {
               VG_(pp_ExeContext)( xe->XE.Race.h1_ct_mbsegendEC );
            } else {
               emit( "   (the end of the thread)\n" );
            }
         }

      }
      VG_(pp_addrinfo) (err_ga, &xe->XE.Race.data_addrinfo);
      break; /* case XE_Race */
   } /* case XE_Race */

   default:
      tl_assert(0);
   } /* switch (VG_(get_error_kind)(err)) */
}

void HG_(print_access) (StackTrace ips, UInt n_ips,
                        Thr* thr_a,
                        Addr  ga,
                        SizeT SzB,
                        Bool  isW,
                        WordSetID locksHeldW )
{
   Thread* threadp;

   threadp = libhb_get_Thr_hgthread( thr_a );
   tl_assert(threadp);
   if (!threadp->announced) {
      /* This is for interactive use. We announce the thread if needed,
         but reset it to not announced afterwards, because we want
         the thread to be announced on the error output/log if needed. */
      announce_one_thread (threadp);
      threadp->announced = False;
   }

   announce_one_thread (threadp);
   VG_(printf) ("%s of size %d at %p by thread #%d",
                isW ? "write" : "read",
                (int)SzB, (void*)ga, threadp->errmsg_index);
   if (threadp->coretid == VG_INVALID_THREADID) 
      VG_(printf)(" tid (exited)\n");
   else
      VG_(printf)(" tid %u\n", threadp->coretid);
   {
      Lock** locksHeldW_P;
      locksHeldW_P = enumerate_WordSet_into_LockP_vector(
                       HG_(get_univ_lsets)(),
                       locksHeldW,
                       True/*allowed_to_be_invalid*/
                    );
      show_LockP_summary_textmode( locksHeldW_P, "" );
      HG_(free) (locksHeldW_P);
   }
   // FIXME PW EPOCH : need the real ips epoch.
   VG_(pp_StackTrace)( VG_(current_DiEpoch)(), ips, n_ips );
   VG_(printf) ("\n");
}

const HChar* HG_(get_error_name) ( const Error* err )
{
   switch (VG_(get_error_kind)(err)) {
      case XE_Race:           return "Race";
      case XE_UnlockUnlocked: return "UnlockUnlocked";
      case XE_UnlockForeign:  return "UnlockForeign";
      case XE_UnlockBogus:    return "UnlockBogus";
      case XE_PthAPIerror:    return "PthAPIerror";
      case XE_LockOrder:      return "LockOrder";
      case XE_Misc:           return "Misc";
      case XE_Dubious:        return "Dubious";
      default: tl_assert(0); /* fill in missing case */
   }
}

Bool HG_(recognised_suppression) ( const HChar* name, Supp *su )
{
#  define TRY(_name,_xskind)                   \
      if (0 == VG_(strcmp)(name, (_name))) {   \
         VG_(set_supp_kind)(su, (_xskind));    \
         return True;                          \
      }
   TRY("Race",           XS_Race);
   TRY("FreeMemLock",    XS_FreeMemLock);
   TRY("UnlockUnlocked", XS_UnlockUnlocked);
   TRY("UnlockForeign",  XS_UnlockForeign);
   TRY("UnlockBogus",    XS_UnlockBogus);
   TRY("PthAPIerror",    XS_PthAPIerror);
   TRY("LockOrder",      XS_LockOrder);
   TRY("Misc",           XS_Misc);
   TRY("Dubious",        XS_Dubious);
   return False;
#  undef TRY
}

Bool HG_(read_extra_suppression_info) ( Int fd, HChar** bufpp, SizeT* nBufp,
                                        Int* lineno, Supp* su )
{
   /* do nothing -- no extra suppression info present.  Return True to
      indicate nothing bad happened. */
   return True;
}

Bool HG_(error_matches_suppression) ( const Error* err, const Supp* su )
{
   switch (VG_(get_supp_kind)(su)) {
   case XS_Race:           return VG_(get_error_kind)(err) == XE_Race;
   case XS_UnlockUnlocked: return VG_(get_error_kind)(err) == XE_UnlockUnlocked;
   case XS_UnlockForeign:  return VG_(get_error_kind)(err) == XE_UnlockForeign;
   case XS_UnlockBogus:    return VG_(get_error_kind)(err) == XE_UnlockBogus;
   case XS_PthAPIerror:    return VG_(get_error_kind)(err) == XE_PthAPIerror;
   case XS_LockOrder:      return VG_(get_error_kind)(err) == XE_LockOrder;
   case XS_Misc:           return VG_(get_error_kind)(err) == XE_Misc;
   case XS_Dubious:        return VG_(get_error_kind)(err) == XE_Dubious;
   //case XS_: return VG_(get_error_kind)(err) == XE_;
   default: tl_assert(0); /* fill in missing cases */
   }
}

SizeT HG_(get_extra_suppression_info) ( const Error* err,
                                       /*OUT*/HChar* buf, Int nBuf )
{
   tl_assert(nBuf >= 1);
   /* Do nothing */
   buf[0] = '\0';
   return 0;
}

SizeT HG_(print_extra_suppression_use) ( const Supp* su,
                                        /*OUT*/HChar* buf, Int nBuf )
{
   tl_assert(nBuf >= 1);
   /* Do nothing */
   buf[0] = '\0';
   return 0;
}

void HG_(update_extra_suppression_use) ( const Error* err, const Supp* su )
{
   /* Do nothing */
   return;
}


/*--------------------------------------------------------------------*/
/*--- end                                              hg_errors.c ---*/
/*--------------------------------------------------------------------*/
