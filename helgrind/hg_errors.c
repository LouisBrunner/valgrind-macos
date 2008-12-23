
/*--------------------------------------------------------------------*/
/*--- Error management for Helgrind.                               ---*/
/*---                                                  hg_errors.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2008 OpenWorks Ltd
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_tool_basics.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_execontext.h"
#include "pub_tool_errormgr.h"
#include "pub_tool_wordfm.h"
#include "pub_tool_xarray.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_threadstate.h"

#include "hg_basics.h"
#include "hg_wordset.h"
#include "hg_lock_n_thread.h"
#include "libhb.h"
#include "hg_errors.h"            /* self */


/*----------------------------------------------------------------*/
/*---                                                          ---*/
/*----------------------------------------------------------------*/

/* This has to do with printing error messages.  See comments on
   announce_threadset() and summarise_threadset().  Perhaps it
   should be a command line option. */
#define N_THREADS_TO_ANNOUNCE 5


/*----------------------------------------------------------------*/
/*--- Error management                                         ---*/
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

static HChar* string_table_strdup ( HChar* str ) {
   HChar* copy = NULL;
   HG_(stats__string_table_queries)++;
   if (!str)
      str = "(null)";
   if (!string_table) {
      string_table = VG_(newFM)( HG_(zalloc), "hg.sts.1",
                                 HG_(free), string_table_cmp );
      tl_assert(string_table);
   }
   if (VG_(lookupFM)( string_table,
                      NULL, (Word*)&copy, (Word)str )) {
      tl_assert(copy);
      if (0) VG_(printf)("string_table_strdup: %p -> %p\n", str, copy );
      return copy;
   } else {
      copy = HG_(strdup)("hg.sts.2", str);
      tl_assert(copy);
      VG_(addToFM)( string_table, (Word)copy, (Word)copy );
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

static Lock* mk_LockP_from_LockN ( Lock* lkn )
{
   Lock* lkp = NULL;
   HG_(stats__LockN_to_P_queries)++;
   tl_assert( HG_(is_sane_LockN)(lkn) );
   if (!map_LockN_to_P) {
      map_LockN_to_P = VG_(newFM)( HG_(zalloc), "hg.mLPfLN.1",
                                   HG_(free), lock_unique_cmp );
      tl_assert(map_LockN_to_P);
   }
   if (!VG_(lookupFM)( map_LockN_to_P, NULL, (Word*)&lkp, (Word)lkn)) {
      lkp = HG_(zalloc)( "hg.mLPfLN.2", sizeof(Lock) );
      *lkp = *lkn;
      lkp->admin = NULL;
      lkp->magic = LockP_MAGIC;
      /* Forget about the bag of lock holders - don't copy that.
         Also, acquired_at should be NULL whenever heldBy is, and vice
         versa.  Also forget about the associated libhb synch object. */
      lkp->heldW  = False;
      lkp->heldBy = NULL;
      lkp->acquired_at = NULL;
      lkp->hbso = NULL;
      VG_(addToFM)( map_LockN_to_P, (Word)lkp, (Word)lkp );
   }
   tl_assert( HG_(is_sane_LockP)(lkp) );
   return lkp;
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
      XE_FreeMemLock,    // freeing memory containing a locked lock
      XE_UnlockUnlocked, // unlocking a not-locked lock
      XE_UnlockForeign,  // unlocking a lock held by some other thread
      XE_UnlockBogus,    // unlocking an address not known to be a lock
      XE_PthAPIerror,    // error from the POSIX pthreads API
      XE_LockOrder,      // lock order error
      XE_Misc            // misc other error (w/ string to describe it)
   }
   XErrorTag;

/* Extra contexts for kinds */
typedef
   struct  {
      XErrorTag tag;
      union {
         struct {
            Addr  data_addr;
            Int   szB;
            Bool  isWrite;
            ExeContext* mb_lastlock;
            ExeContext* mb_confacc;
            Thread* thr;
            Thread* mb_confaccthr;
            Int   mb_confaccSzB;
            Bool  mb_confaccIsW;
            Char  descr1[96];
            Char  descr2[96];
         } Race;
         struct {
            Thread* thr;  /* doing the freeing */
            Lock*   lock; /* lock which is locked */
         } FreeMemLock;
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
            Addr        before_ga; /* always locked first in prog. history */
            Addr        after_ga;
            ExeContext* before_ec;
            ExeContext* after_ec;
         } LockOrder;
         struct {
            Thread* thr;
            HChar*  errstr; /* persistent, in tool-arena */
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
      XS_Misc
   }
   XSuppTag;


/* Updates the copy with address info if necessary. */
UInt HG_(update_extra) ( Error* err )
{
   XError* xe = (XError*)VG_(get_error_extra)(err);
   tl_assert(xe);
   //if (extra != NULL && Undescribed == extra->addrinfo.akind) {
   //   describe_addr ( VG_(get_error_address)(err), &(extra->addrinfo) );
   //}

   if (xe->tag == XE_Race) {
      /* See if we can come up with a source level description of the
         raced-upon address.  This is potentially expensive, which is
         why it's only done at the update_extra point, not when the
         error is initially created. */
      static Int xxx = 0;
      xxx++;
      if (0)
         VG_(printf)("HG_(update_extra): "
                     "%d conflicting-event queries\n", xxx);
      tl_assert(sizeof(xe->XE.Race.descr1) == sizeof(xe->XE.Race.descr2));
      if (VG_(get_data_description)(
                &xe->XE.Race.descr1[0],
                &xe->XE.Race.descr2[0],
                sizeof(xe->XE.Race.descr1)-1,
                xe->XE.Race.data_addr )) {
         tl_assert( xe->XE.Race.descr1
                       [ sizeof(xe->XE.Race.descr1)-1 ] == 0);
         tl_assert( xe->XE.Race.descr2
                       [ sizeof(xe->XE.Race.descr2)-1 ] == 0);
      }
      { Thr* thrp = NULL;
        ExeContext* wherep = NULL;
        Addr  acc_addr = xe->XE.Race.data_addr;
        Int   acc_szB  = xe->XE.Race.szB;
        Thr*  acc_thr  = xe->XE.Race.thr->hbthr;
        Bool  acc_isW  = xe->XE.Race.isWrite;
        SizeT conf_szB = 0;
        Bool  conf_isW = False;
        tl_assert(!xe->XE.Race.mb_confacc);
        tl_assert(!xe->XE.Race.mb_confaccthr);
        if (libhb_event_map_lookup(
               &wherep, &thrp, &conf_szB, &conf_isW,
               acc_thr, acc_addr, acc_szB, acc_isW )) {
           Thread* threadp;
           tl_assert(wherep);
           tl_assert(thrp);
           threadp = libhb_get_Thr_opaque( thrp );
           tl_assert(threadp);
           xe->XE.Race.mb_confacc = wherep;
           xe->XE.Race.mb_confaccthr = threadp;
           xe->XE.Race.mb_confaccSzB = (Int)conf_szB;
           xe->XE.Race.mb_confaccIsW = conf_isW;
        }
      }
   }

   return sizeof(XError);
}

void HG_(record_error_Race) ( Thread* thr, 
                              Addr data_addr, Int szB, Bool isWrite,
                              ExeContext* mb_lastlock )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );

#  if defined(VGO_linux)
   /* Skip any races on locations apparently in GOTPLT sections.  This
      is said to be caused by ld.so poking PLT table entries (or
      whatever) when it writes the resolved address of a dynamically
      linked routine, into the table (or whatever) when it is called
      for the first time. */
   {
     VgSectKind sect = VG_(seginfo_sect_kind)( NULL, 0, data_addr );
     if (0) VG_(printf)("XXXXXXXXX RACE on %#lx %s\n",
                        data_addr, VG_(pp_SectKind)(sect));
     /* SectPLT is required on ???-linux */
     if (sect == Vg_SectGOTPLT) return;
     /* SectPLT is required on ppc32/64-linux */
     if (sect == Vg_SectPLT) return;
   }
#  endif

   init_XError(&xe);
   xe.tag = XE_Race;
   xe.XE.Race.data_addr   = data_addr;
   xe.XE.Race.szB         = szB;
   xe.XE.Race.isWrite     = isWrite;
   xe.XE.Race.mb_lastlock = mb_lastlock;
   xe.XE.Race.thr         = thr;
   tl_assert(isWrite == False || isWrite == True);
   tl_assert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   xe.XE.Race.descr1[0] = xe.XE.Race.descr2[0] = 0;
   // FIXME: tid vs thr
   // Skip on any of the conflicting-access info at this point.
   // It's expensive to obtain, and this error is more likely than
   // not to be discarded.  We'll fill these fields in in 
   // HG_(update_extra) just above, assuming the error ever makes
   // it that far (unlikely).
   xe.XE.Race.mb_confaccSzB = 0;
   xe.XE.Race.mb_confaccIsW = False;
   xe.XE.Race.mb_confacc    = NULL;
   xe.XE.Race.mb_confaccthr = NULL;
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_Race, data_addr, NULL, &xe );
}

void HG_(record_error_FreeMemLock) ( Thread* thr, Lock* lk )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert( HG_(is_sane_LockN)(lk) );
   init_XError(&xe);
   xe.tag = XE_FreeMemLock;
   xe.XE.FreeMemLock.thr  = thr;
   xe.XE.FreeMemLock.lock = mk_LockP_from_LockN(lk);
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_FreeMemLock, 0, NULL, &xe );
}

void HG_(record_error_UnlockUnlocked) ( Thread* thr, Lock* lk )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert( HG_(is_sane_LockN)(lk) );
   init_XError(&xe);
   xe.tag = XE_UnlockUnlocked;
   xe.XE.UnlockUnlocked.thr  = thr;
   xe.XE.UnlockUnlocked.lock = mk_LockP_from_LockN(lk);
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
   xe.XE.UnlockForeign.lock  = mk_LockP_from_LockN(lk);
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
        Thread* thr, Addr before_ga, Addr after_ga,
        ExeContext* before_ec, ExeContext* after_ec 
     )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   if (!HG_(clo_track_lockorders))
      return;
   init_XError(&xe);
   xe.tag = XE_LockOrder;
   xe.XE.LockOrder.thr       = thr;
   xe.XE.LockOrder.before_ga = before_ga;
   xe.XE.LockOrder.before_ec = before_ec;
   xe.XE.LockOrder.after_ga  = after_ga;
   xe.XE.LockOrder.after_ec  = after_ec;
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_LockOrder, 0, NULL, &xe );
}

void HG_(record_error_PthAPIerror) ( Thread* thr, HChar* fnname, 
                                     Word err, HChar* errstr )
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

void HG_(record_error_Misc) ( Thread* thr, HChar* errstr )
{
   XError xe;
   tl_assert( HG_(is_sane_Thread)(thr) );
   tl_assert(errstr);
   init_XError(&xe);
   xe.tag = XE_Misc;
   xe.XE.Misc.thr    = thr;
   xe.XE.Misc.errstr = string_table_strdup(errstr);
   // FIXME: tid vs thr
   tl_assert( HG_(is_sane_ThreadId)(thr->coretid) );
   tl_assert( thr->coretid != VG_INVALID_THREADID );
   VG_(maybe_record_error)( thr->coretid,
                            XE_Misc, 0, NULL, &xe );
}

Bool HG_(eq_Error) ( VgRes not_used, Error* e1, Error* e2 )
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
      case XE_FreeMemLock:
         return xe1->XE.FreeMemLock.thr == xe2->XE.FreeMemLock.thr
                && xe1->XE.FreeMemLock.lock == xe2->XE.FreeMemLock.lock;
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
      default:
         tl_assert(0);
   }

   /*NOTREACHED*/
   tl_assert(0);
}


/* Announce (that is, print the point-of-creation) of 'thr'.  Only do
   this once, as we only want to see these announcements once per
   thread. */
static void announce_one_thread ( Thread* thr ) 
{
   tl_assert(HG_(is_sane_Thread)(thr));
   tl_assert(thr->errmsg_index >= 1);
   if (!thr->announced) {
      if (thr->errmsg_index == 1) {
         tl_assert(thr->created_at == NULL);
         VG_(message)(Vg_UserMsg, "Thread #%d is the program's root thread",
                                  thr->errmsg_index);
      } else {
         tl_assert(thr->created_at != NULL);
         VG_(message)(Vg_UserMsg, "Thread #%d was created",
                                  thr->errmsg_index);
         VG_(pp_ExeContext)( thr->created_at );
      }
      VG_(message)(Vg_UserMsg, "");
      thr->announced = True;
   }
}


void HG_(pp_Error) ( Error* err )
{
   XError *xe = (XError*)VG_(get_error_extra)(err);

   switch (VG_(get_error_kind)(err)) {

   case XE_Misc: {
      tl_assert(xe);
      tl_assert( HG_(is_sane_Thread)( xe->XE.Misc.thr ) );
      announce_one_thread( xe->XE.Misc.thr );
      VG_(message)(Vg_UserMsg,
                  "Thread #%d: %s",
                  (Int)xe->XE.Misc.thr->errmsg_index,
                  xe->XE.Misc.errstr);
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      break;
   }

   case XE_LockOrder: {
      tl_assert(xe);
      tl_assert( HG_(is_sane_Thread)( xe->XE.LockOrder.thr ) );
      announce_one_thread( xe->XE.LockOrder.thr );
      VG_(message)(Vg_UserMsg,
                  "Thread #%d: lock order \"%p before %p\" violated",
                  (Int)xe->XE.LockOrder.thr->errmsg_index,
                  (void*)xe->XE.LockOrder.before_ga,
                  (void*)xe->XE.LockOrder.after_ga);
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      if (xe->XE.LockOrder.before_ec && xe->XE.LockOrder.after_ec) {
         VG_(message)(Vg_UserMsg,
            "  Required order was established by acquisition of lock at %p",
            (void*)xe->XE.LockOrder.before_ga);
         VG_(pp_ExeContext)( xe->XE.LockOrder.before_ec );
         VG_(message)(Vg_UserMsg,
            "  followed by a later acquisition of lock at %p", 
            (void*)xe->XE.LockOrder.after_ga);
         VG_(pp_ExeContext)( xe->XE.LockOrder.after_ec );
      }
      break;
   }

   case XE_PthAPIerror: {
      tl_assert(xe);
      tl_assert( HG_(is_sane_Thread)( xe->XE.PthAPIerror.thr ) );
      announce_one_thread( xe->XE.PthAPIerror.thr );
      VG_(message)(Vg_UserMsg,
                  "Thread #%d's call to %s failed",
                  (Int)xe->XE.PthAPIerror.thr->errmsg_index,
                  xe->XE.PthAPIerror.fnname);
      VG_(message)(Vg_UserMsg,
                  "   with error code %ld (%s)",
                  xe->XE.PthAPIerror.err,
                  xe->XE.PthAPIerror.errstr);
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      break;
   }

   case XE_UnlockBogus: {
      tl_assert(xe);
      tl_assert( HG_(is_sane_Thread)( xe->XE.UnlockBogus.thr ) );
      announce_one_thread( xe->XE.UnlockBogus.thr );
      VG_(message)(Vg_UserMsg,
                   "Thread #%d unlocked an invalid lock at %p ",
                   (Int)xe->XE.UnlockBogus.thr->errmsg_index,
                   (void*)xe->XE.UnlockBogus.lock_ga);
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      break;
   }

   case XE_UnlockForeign: {
      tl_assert(xe);
      tl_assert( HG_(is_sane_LockP)( xe->XE.UnlockForeign.lock ) );
      tl_assert( HG_(is_sane_Thread)( xe->XE.UnlockForeign.owner ) );
      tl_assert( HG_(is_sane_Thread)( xe->XE.UnlockForeign.thr ) );
      announce_one_thread( xe->XE.UnlockForeign.thr );
      announce_one_thread( xe->XE.UnlockForeign.owner );
      VG_(message)(Vg_UserMsg,
                   "Thread #%d unlocked lock at %p "
                   "currently held by thread #%d",
                   (Int)xe->XE.UnlockForeign.thr->errmsg_index,
                   (void*)xe->XE.UnlockForeign.lock->guestaddr,
                   (Int)xe->XE.UnlockForeign.owner->errmsg_index );
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      if (xe->XE.UnlockForeign.lock->appeared_at) {
         VG_(message)(Vg_UserMsg,
                      "  Lock at %p was first observed",
                      (void*)xe->XE.UnlockForeign.lock->guestaddr);
         VG_(pp_ExeContext)( xe->XE.UnlockForeign.lock->appeared_at );
      }
      break;
   }

   case XE_UnlockUnlocked: {
      tl_assert(xe);
      tl_assert( HG_(is_sane_LockP)( xe->XE.UnlockUnlocked.lock ) );
      tl_assert( HG_(is_sane_Thread)( xe->XE.UnlockUnlocked.thr ) );
      announce_one_thread( xe->XE.UnlockUnlocked.thr );
      VG_(message)(Vg_UserMsg,
                   "Thread #%d unlocked a not-locked lock at %p ",
                   (Int)xe->XE.UnlockUnlocked.thr->errmsg_index,
                   (void*)xe->XE.UnlockUnlocked.lock->guestaddr);
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      if (xe->XE.UnlockUnlocked.lock->appeared_at) {
         VG_(message)(Vg_UserMsg,
                      "  Lock at %p was first observed",
                      (void*)xe->XE.UnlockUnlocked.lock->guestaddr);
         VG_(pp_ExeContext)( xe->XE.UnlockUnlocked.lock->appeared_at );
      }
      break;
   }

   case XE_FreeMemLock: {
      tl_assert(xe);
      tl_assert( HG_(is_sane_LockP)( xe->XE.FreeMemLock.lock ) );
      tl_assert( HG_(is_sane_Thread)( xe->XE.FreeMemLock.thr ) );
      announce_one_thread( xe->XE.FreeMemLock.thr );
      VG_(message)(Vg_UserMsg,
                   "Thread #%d deallocated location %p "
                   "containing a locked lock",
                   (Int)xe->XE.FreeMemLock.thr->errmsg_index,
                   (void*)xe->XE.FreeMemLock.lock->guestaddr);
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      if (xe->XE.FreeMemLock.lock->appeared_at) {
         VG_(message)(Vg_UserMsg,
                      "  Lock at %p was first observed",
                      (void*)xe->XE.FreeMemLock.lock->guestaddr);
         VG_(pp_ExeContext)( xe->XE.FreeMemLock.lock->appeared_at );
      }
      break;
   }

   case XE_Race: {
      Addr      err_ga;
      HChar*    what;
      Int       szB;
      what      = xe->XE.Race.isWrite ? "write" : "read";
      szB       = xe->XE.Race.szB;
      err_ga = VG_(get_error_address)(err);

      announce_one_thread( xe->XE.Race.thr );
      if (xe->XE.Race.mb_confaccthr)
         announce_one_thread( xe->XE.Race.mb_confaccthr );
      VG_(message)(Vg_UserMsg,
         "Possible data race during %s of size %d at %#lx by thread #%d",
         what, szB, err_ga, (Int)xe->XE.Race.thr->errmsg_index
      );
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      if (xe->XE.Race.mb_confacc) {
         if (xe->XE.Race.mb_confaccthr) {
            VG_(message)(Vg_UserMsg,
               " This conflicts with a previous %s of size %d by thread #%d",
               xe->XE.Race.mb_confaccIsW ? "write" : "read",
               xe->XE.Race.mb_confaccSzB,
               xe->XE.Race.mb_confaccthr->errmsg_index
            );
         } else {
            // FIXME: can this ever happen?
            VG_(message)(Vg_UserMsg,
               " This conflicts with a previous %s of size %d",
               xe->XE.Race.mb_confaccIsW ? "write" : "read",
               xe->XE.Race.mb_confaccSzB
            );
         }
         VG_(pp_ExeContext)( xe->XE.Race.mb_confacc );
      }


      /* If we have a better description of the address, show it. */
      if (xe->XE.Race.descr1[0] != 0)
         VG_(message)(Vg_UserMsg, " %s", &xe->XE.Race.descr1[0]);
      if (xe->XE.Race.descr2[0] != 0)
         VG_(message)(Vg_UserMsg, " %s", &xe->XE.Race.descr2[0]);

      break; /* case XE_Race */
   } /* case XE_Race */

   default:
      tl_assert(0);
   } /* switch (VG_(get_error_kind)(err)) */
}

Char* HG_(get_error_name) ( Error* err )
{
   switch (VG_(get_error_kind)(err)) {
      case XE_Race:           return "Race";
      case XE_FreeMemLock:    return "FreeMemLock";
      case XE_UnlockUnlocked: return "UnlockUnlocked";
      case XE_UnlockForeign:  return "UnlockForeign";
      case XE_UnlockBogus:    return "UnlockBogus";
      case XE_PthAPIerror:    return "PthAPIerror";
      case XE_LockOrder:      return "LockOrder";
      case XE_Misc:           return "Misc";
      default: tl_assert(0); /* fill in missing case */
   }
}

Bool HG_(recognised_suppression) ( Char* name, Supp *su )
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
   return False;
#  undef TRY
}

Bool HG_(read_extra_suppression_info) ( Int fd, Char* buf, Int nBuf,
                                        Supp* su )
{
   /* do nothing -- no extra suppression info present.  Return True to
      indicate nothing bad happened. */
   return True;
}

Bool HG_(error_matches_suppression) ( Error* err, Supp* su )
{
   switch (VG_(get_supp_kind)(su)) {
   case XS_Race:           return VG_(get_error_kind)(err) == XE_Race;
   case XS_FreeMemLock:    return VG_(get_error_kind)(err) == XE_FreeMemLock;
   case XS_UnlockUnlocked: return VG_(get_error_kind)(err) == XE_UnlockUnlocked;
   case XS_UnlockForeign:  return VG_(get_error_kind)(err) == XE_UnlockForeign;
   case XS_UnlockBogus:    return VG_(get_error_kind)(err) == XE_UnlockBogus;
   case XS_PthAPIerror:    return VG_(get_error_kind)(err) == XE_PthAPIerror;
   case XS_LockOrder:      return VG_(get_error_kind)(err) == XE_LockOrder;
   case XS_Misc:           return VG_(get_error_kind)(err) == XE_Misc;
   //case XS_: return VG_(get_error_kind)(err) == XE_;
   default: tl_assert(0); /* fill in missing cases */
   }
}

void HG_(print_extra_suppression_info) ( Error* err )
{
   /* Do nothing */
}


/*--------------------------------------------------------------------*/
/*--- end                                              hg_errors.c ---*/
/*--------------------------------------------------------------------*/
