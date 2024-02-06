
/*--------------------------------------------------------------------*/
/*--- Store and compare stack backtraces            m_execontext.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
      jseward@acm.org

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

#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"     // For VG_(message)()
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_stacktrace.h"
#include "pub_core_machine.h"       // VG_(get_IP)
#include "pub_core_threadstate.h"   // VG_(is_valid_tid)
#include "pub_core_execontext.h"    // self

/*------------------------------------------------------------*/
/*--- Low-level ExeContext storage.                        ---*/
/*------------------------------------------------------------*/

/* Depending on VgRes, the first 2, 4 or all IP values are used in
   comparisons to remove duplicate errors, and for comparing against
   suppression specifications.  If not used in comparison, the rest
   are purely informational (but often important).

   The contexts are stored in a traditional chained hash table, so as
   to allow quick determination of whether a new context already
   exists.  The hash table starts small and expands dynamically, so as
   to keep the load factor below 1.0.

   The idea is only to ever store any one context once, so as to save
   space and make exact comparisons faster. */


/* Primes for the hash table */

#define N_EC_PRIMES 18

static SizeT ec_primes[N_EC_PRIMES] = {
         769UL,         1543UL,         3079UL,          6151UL,
       12289UL,        24593UL,        49157UL,         98317UL,
      196613UL,       393241UL,       786433UL,       1572869UL,
     3145739UL,      6291469UL,     12582917UL,      25165843UL,
    50331653UL,    100663319UL
};


/* Each element is present in a hash chain, and also contains a
   variable length array of guest code addresses (the useful part). */

struct _ExeContext {
   struct _ExeContext* chain;
   /* A 32-bit unsigned integer that uniquely identifies this
      ExeContext.  Memcheck uses these for origin tracking.  Values
      must be nonzero (else Memcheck's origin tracking is hosed), must
      be a multiple of four, and must be unique.  Hence they start at
      4. */
   UInt ecu;
   /* epoch in which the ExeContext can be symbolised. In other words, epoch
      identifies the set of debug info to use to symbolise the Addr in ips
      using e.g. VG_(get_filename), VG_(get_fnname), ...
      Note 1: 2 ExeContexts are equal when their ips array are equal and
      their epoch are equal.
      Note 2: a freshly created ExeContext has a DiEpoch_INVALID epoch.
      DiEpoch_INVALID is used as a special value to indicate that ExeContext
      is valid in the current epoch. VG_(get_ExeContext_epoch) translates
      this invalid value in the real current epoch.
      When a debug info is archived, the set of ExeContext is scanned :
      If an ExeContext with epoch == DiEpoch_INVALID has one or more
      ips Addr corresponding to the just archived debug info, the ExeContext
      epoch is changed to the last epoch identifying the set containing the
      archived debug info. */
   DiEpoch epoch;
   /* Variable-length array.  The size is 'n_ips'; at
      least 1, at most VG_DEEPEST_BACKTRACE.  [0] is the current IP,
      [1] is its caller, [2] is the caller of [1], etc. */
   UInt n_ips;
   Addr ips[0];
};


/* This is the dynamically expanding hash table. */
static ExeContext** ec_htab; /* array [ec_htab_size] of ExeContext* */
static SizeT        ec_htab_size;     /* one of the values in ec_primes */
static SizeT        ec_htab_size_idx; /* 0 .. N_EC_PRIMES-1 */

/* ECU serial number */
static UInt ec_next_ecu = 4; /* We must never issue zero */

static ExeContext* null_ExeContext;

/* Stats only: the number of times the system was searched to locate a
   context. */
static ULong ec_searchreqs;

/* Stats only: the number of full context comparisons done. */
static ULong ec_searchcmps;

/* Stats only: total number of stored contexts. */
static ULong ec_totstored;

/* Number of 2, 4 and (fast) full cmps done. */
static ULong ec_cmp2s;
static ULong ec_cmp4s;
static ULong ec_cmpAlls;


/*------------------------------------------------------------*/
/*--- ExeContext functions.                                ---*/
/*------------------------------------------------------------*/

static ExeContext* record_ExeContext_wrk2 ( const Addr* ips, UInt n_ips );

/* Initialise this subsystem. */
static void init_ExeContext_storage ( void )
{
   Int i;
   static Bool init_done = False;
   if (LIKELY(init_done))
      return;
   ec_searchreqs = 0;
   ec_searchcmps = 0;
   ec_totstored = 0;
   ec_cmp2s = 0;
   ec_cmp4s = 0;
   ec_cmpAlls = 0;

   ec_htab_size_idx = 0;
   ec_htab_size = ec_primes[ec_htab_size_idx];
   ec_htab = VG_(malloc)("execontext.iEs1",
                         sizeof(ExeContext*) * ec_htab_size);
   for (i = 0; i < ec_htab_size; i++)
      ec_htab[i] = NULL;

   {
      Addr ips[1];
      ips[0] = 0;
      null_ExeContext = record_ExeContext_wrk2(ips, 1);
      // null execontext must be the first one created and get ecu 4.
      vg_assert(null_ExeContext->ecu == 4);
   }

   init_done = True;
}

DiEpoch VG_(get_ExeContext_epoch)( const ExeContext* e )
{
   if (is_DiEpoch_INVALID (e->epoch))
      return VG_(current_DiEpoch)();
   else
      return e->epoch;
}

/* Print stats. */
void VG_(print_ExeContext_stats) ( Bool with_stacktraces )
{
   Int i;
   ULong total_n_ips;
   ExeContext* ec;

   init_ExeContext_storage();

   if (with_stacktraces) {
      VG_(message)(Vg_DebugMsg, "   exectx: Printing contexts stacktraces\n");
      for (i = 0; i < ec_htab_size; i++) {
         for (ec = ec_htab[i]; ec; ec = ec->chain) {
            VG_(message)(Vg_DebugMsg,
                         "   exectx: stacktrace ecu %u epoch %u n_ips %u\n",
                         ec->ecu, ec->epoch.n, ec->n_ips);
            VG_(pp_StackTrace)( VG_(get_ExeContext_epoch)(ec),
                                ec->ips, ec->n_ips );
         }
      }
      VG_(message)(Vg_DebugMsg, 
                   "   exectx: Printed %'llu contexts stacktraces\n",
                   ec_totstored);
   }
   
   total_n_ips = 0;
   for (i = 0; i < ec_htab_size; i++) {
      for (ec = ec_htab[i]; ec; ec = ec->chain)
         total_n_ips += ec->n_ips;
   }
   VG_(message)(Vg_DebugMsg, 
      "   exectx: %'lu lists, %'llu contexts (avg %3.2f per list)"
      " (avg %3.2f IP per context)\n",
      ec_htab_size, ec_totstored, (Double)ec_totstored / (Double)ec_htab_size,
      (Double)total_n_ips / (Double)ec_totstored
   );
   VG_(message)(Vg_DebugMsg, 
      "   exectx: %'llu searches, %'llu full compares (%'llu per 1000)\n",
      ec_searchreqs, ec_searchcmps, 
      ec_searchreqs == 0 
         ? 0ULL 
         : ( (ec_searchcmps * 1000ULL) / ec_searchreqs ) 
   );
   VG_(message)(Vg_DebugMsg, 
      "   exectx: %'llu cmp2, %'llu cmp4, %'llu cmpAll\n",
      ec_cmp2s, ec_cmp4s, ec_cmpAlls 
   );
}

/* Print an ExeContext. */
void VG_(pp_ExeContext) ( ExeContext* ec )
{
   VG_(pp_StackTrace)( VG_(get_ExeContext_epoch)(ec), ec->ips, ec->n_ips );
}

void VG_(apply_ExeContext)(
   void(*action)(UInt n, DiEpoch ep, Addr ip, void* opaque),
   void* opaque, ExeContext* ec)
{
   VG_(apply_StackTrace)(action, opaque, VG_(get_ExeContext_epoch)(ec),
                         ec->ips, ec->n_ips);
}

void VG_(archive_ExeContext_in_range) (DiEpoch last_epoch,
                                       Addr text_avma, SizeT length )
{
   Int i;
   ExeContext* ec;
   ULong n_archived = 0;
   const Addr text_avma_end = text_avma + length - 1;

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, "Scanning and archiving ExeContexts ...\n");
   for (i = 0; i < ec_htab_size; i++) {
      for (ec = ec_htab[i]; ec; ec = ec->chain) {
         if (is_DiEpoch_INVALID (ec->epoch))
            for (UInt j = 0; j < ec->n_ips; j++) {
               if (UNLIKELY(ec->ips[j] >= text_avma
                            && ec->ips[j] <= text_avma_end)) {
                  ec->epoch = last_epoch;
                  n_archived++;
                  break;
               }
            }
      }
   }
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg,
                   "Scanned %'llu ExeContexts, archived %'llu ExeContexts\n",
                   ec_totstored, n_archived);
}

/* Compare two ExeContexts.  Number of callers considered depends on res. */
Bool VG_(eq_ExeContext) ( VgRes res, const ExeContext* e1,
                          const ExeContext* e2 )
{
   Int i;

   if (e1 == NULL || e2 == NULL) 
      return False;

   // Must be at least one address in each trace.
   vg_assert(e1->n_ips >= 1 && e2->n_ips >= 1);

   // Note: we compare the epoch in the case below, and not here
   // to have the ec_cmp* stats correct.

   switch (res) {
   case Vg_LowRes:
      /* Just compare the top two callers. */
      ec_cmp2s++;
      for (i = 0; i < 2; i++) {
         if ( (e1->n_ips <= i) &&  (e2->n_ips <= i)) return True;
         if ( (e1->n_ips <= i) && !(e2->n_ips <= i)) return False;
         if (!(e1->n_ips <= i) &&  (e2->n_ips <= i)) return False;
         if (e1->ips[i] != e2->ips[i])               return False;
      }
      return e1->epoch.n == e2->epoch.n;

   case Vg_MedRes:
      /* Just compare the top four callers. */
      ec_cmp4s++;
      for (i = 0; i < 4; i++) {
         if ( (e1->n_ips <= i) &&  (e2->n_ips <= i)) return True;
         if ( (e1->n_ips <= i) && !(e2->n_ips <= i)) return False;
         if (!(e1->n_ips <= i) &&  (e2->n_ips <= i)) return False;
         if (e1->ips[i] != e2->ips[i])               return False;
      }
      return e1->epoch.n == e2->epoch.n;

   case Vg_HighRes:
      ec_cmpAlls++;
      /* Compare them all -- just do pointer comparison. */
      if (e1 != e2) return False;
      return True;

   default:
      VG_(core_panic)("VG_(eq_ExeContext): unrecognised VgRes");
   }
}

/* VG_(record_ExeContext) is the head honcho here.  Take a snapshot of
   the client's stack.  Search our collection of ExeContexts to see if
   we already have it, and if not, allocate a new one.  Either way,
   return a pointer to the context.  If there is a matching context we
   guarantee to not allocate a new one.  Thus we never store
   duplicates, and so exact equality can be quickly done as equality
   on the returned ExeContext* values themselves.  Inspired by Hugs's
   Text type.

   Also checks whether the hash table needs expanding, and expands it
   if so. */

static inline UWord ROLW ( UWord w, Int n )
{
   Int bpw = 8 * sizeof(UWord);
   w = (w << n) | (w >> (bpw-n));
   return w;
}

static UWord calc_hash ( const Addr* ips, UInt n_ips, UWord htab_sz )
{
   UInt  i;
   UWord hash = 0;
   vg_assert(htab_sz > 0);
   for (i = 0; i < n_ips; i++) {
      hash ^= ips[i];
      hash = ROLW(hash, 19);
   }
   return hash % htab_sz;
}

static void resize_ec_htab ( void )
{
   SizeT        i;
   SizeT        new_size;
   ExeContext** new_ec_htab;

   vg_assert(ec_htab_size_idx < N_EC_PRIMES);
   if (ec_htab_size_idx == N_EC_PRIMES-1)
      return; /* out of primes - can't resize further */

   new_size = ec_primes[ec_htab_size_idx + 1];
   new_ec_htab = VG_(malloc)("execontext.reh1",
                             sizeof(ExeContext*) * new_size);

   VG_(debugLog)(
      1, "execontext",
         "resizing htab from size %lu to %lu (idx %lu)  Total#ECs=%llu\n",
         ec_htab_size, new_size, ec_htab_size_idx + 1, ec_totstored);

   for (i = 0; i < new_size; i++)
      new_ec_htab[i] = NULL;

   for (i = 0; i < ec_htab_size; i++) {
      ExeContext* cur = ec_htab[i];
      while (cur) {
         ExeContext* next = cur->chain;
         UWord hash = calc_hash(cur->ips, cur->n_ips, new_size);
         vg_assert(hash < new_size);
         cur->chain = new_ec_htab[hash];
         new_ec_htab[hash] = cur;
         cur = next;
      }
   }

   VG_(free)(ec_htab);
   ec_htab      = new_ec_htab;
   ec_htab_size = new_size;
   ec_htab_size_idx++;
}

/* Used by the outer as a marker to separate the frames of the inner valgrind
   from the frames of the inner guest frames. */
static void _______VVVVVVVV_appended_inner_guest_stack_VVVVVVVV_______ (void)
{
}

/* Do the first part of getting a stack trace: actually unwind the
   stack, and hand the results off to the duplicate-trace-finder
   (_wrk2). */
static ExeContext* record_ExeContext_wrk ( ThreadId tid, Word first_ip_delta,
                                           Bool first_ip_only )
{
   Addr ips[VG_(clo_backtrace_size)];
   UInt n_ips;

   init_ExeContext_storage();

   vg_assert(sizeof(void*) == sizeof(UWord));
   vg_assert(sizeof(void*) == sizeof(Addr));

   vg_assert(VG_(is_valid_tid)(tid));

   if (first_ip_only) {
      n_ips = 1;
      ips[0] = VG_(get_IP)(tid) + first_ip_delta;
   } else {
      n_ips = VG_(get_StackTrace)( tid, ips, VG_(clo_backtrace_size),
                                   NULL/*array to dump SP values in*/,
                                   NULL/*array to dump FP values in*/,
                                   first_ip_delta );
      if (VG_(inner_threads) != NULL
          && n_ips + 1 < VG_(clo_backtrace_size)) {
         /* An inner V has informed us (the outer) of its thread array.
            Append the inner guest stack trace, if we still have some
            room in the ips array for the separator and (some) inner
            guest IPs. */
         UInt inner_tid;

         for (inner_tid = 1; inner_tid < VG_N_THREADS; inner_tid++) {
            if (VG_(threads)[tid].os_state.lwpid 
                == VG_(inner_threads)[inner_tid].os_state.lwpid) {
               ThreadState* save_outer_vg_threads = VG_(threads);
               UInt n_ips_inner_guest;

               /* Append the separator + the inner guest stack trace. */
               ips[n_ips] = (Addr)
                  _______VVVVVVVV_appended_inner_guest_stack_VVVVVVVV_______;
               n_ips++;
               VG_(threads) = VG_(inner_threads);
               n_ips_inner_guest 
                  = VG_(get_StackTrace)( inner_tid,
                                         ips + n_ips,
                                         VG_(clo_backtrace_size) - n_ips,
                                         NULL/*array to dump SP values in*/,
                                         NULL/*array to dump FP values in*/,
                                         first_ip_delta );
               n_ips += n_ips_inner_guest;
               VG_(threads) = save_outer_vg_threads;
               break;
            }
         }
      }
   }

   return record_ExeContext_wrk2 ( ips, n_ips );
}

/* Do the second part of getting a stack trace: ips[0 .. n_ips-1]
   holds a proposed trace.  Find or allocate a suitable ExeContext.
   Note that callers must have done init_ExeContext_storage() before
   getting to this point. */
static ExeContext* record_ExeContext_wrk2 ( const Addr* ips, UInt n_ips )
{
   Int         i;
   Bool        same;
   UWord       hash;
   ExeContext* new_ec;
   ExeContext* list;
   ExeContext  *prev2, *prev;

   static UInt ctr = 0;

   vg_assert(n_ips >= 1 && n_ips <= VG_(clo_backtrace_size));

   /* Now figure out if we've seen this one before.  First hash it so
      as to determine the list number. */
   hash = calc_hash( ips, n_ips, ec_htab_size );

   /* And (the expensive bit) look a for matching entry in the list. */

   ec_searchreqs++;

   prev2 = NULL;
   prev  = NULL;
   list  = ec_htab[hash];

   while (True) {
      if (list == NULL) break;
      ec_searchcmps++;
      same = list->n_ips == n_ips && is_DiEpoch_INVALID (list->epoch);
      for (i = 0; i < n_ips && same ; i++) {
         same = list->ips[i] == ips[i];
      }
      if (same) break;
      prev2 = prev;
      prev  = list;
      list  = list->chain;
   }

   if (list != NULL) {
      /* Yay!  We found it.  Once every 8 searches, move it one step
         closer to the start of the list to make future searches
         cheaper. */
      if (0 == ((ctr++) & 7)) {
         if (prev2 != NULL && prev != NULL) {
            /* Found at 3rd or later position in the chain. */
            vg_assert(prev2->chain == prev);
            vg_assert(prev->chain  == list);
            prev2->chain = list;
            prev->chain  = list->chain;
            list->chain  = prev;
         }
         else if (prev2 == NULL && prev != NULL) {
            /* Found at 2nd position in the chain. */
            vg_assert(ec_htab[hash] == prev);
            vg_assert(prev->chain == list);
            prev->chain = list->chain;
            list->chain = prev;
            ec_htab[hash] = list;
         }
      }
      return list;
   }

   /* Bummer.  We have to allocate a new context record. */
   ec_totstored++;

   new_ec = VG_(perm_malloc)( sizeof(struct _ExeContext) 
                              + n_ips * sizeof(Addr),
                              vg_alignof(struct _ExeContext));

   for (i = 0; i < n_ips; i++)
      new_ec->ips[i] = ips[i];

   vg_assert(VG_(is_plausible_ECU)(ec_next_ecu));
   new_ec->ecu = ec_next_ecu;
   ec_next_ecu += 4;
   if (ec_next_ecu == 0) {
      /* Urr.  Now we're hosed; we emitted 2^30 ExeContexts already
         and have run out of numbers.  Not sure what to do. */
      VG_(core_panic)("m_execontext: more than 2^30 ExeContexts created");
   }

   new_ec->n_ips = n_ips;
   new_ec->chain = ec_htab[hash];
   new_ec->epoch = DiEpoch_INVALID();
   ec_htab[hash] = new_ec;

   /* Resize the hash table, maybe? */
   if ( ((ULong)ec_totstored) > ((ULong)ec_htab_size) ) {
      vg_assert(ec_htab_size_idx < N_EC_PRIMES);
      if (ec_htab_size_idx < N_EC_PRIMES-1)
         resize_ec_htab();
   }

   return new_ec;
}

ExeContext* VG_(record_ExeContext)( ThreadId tid, Word first_ip_delta ) {
   return record_ExeContext_wrk( tid, first_ip_delta,
                                      False/*!first_ip_only*/ );
}

ExeContext* VG_(record_depth_1_ExeContext)( ThreadId tid, Word first_ip_delta )
{
   return record_ExeContext_wrk( tid, first_ip_delta,
                                      True/*first_ip_only*/ );
}

ExeContext* VG_(make_depth_1_ExeContext_from_Addr)( Addr a ) {
   init_ExeContext_storage();
   return record_ExeContext_wrk2( &a, 1 );
}

StackTrace VG_(get_ExeContext_StackTrace) ( ExeContext* e ) {
   return e->ips;
}  

UInt VG_(get_ECU_from_ExeContext)( const ExeContext* e ) {
   vg_assert(VG_(is_plausible_ECU)(e->ecu));
   return e->ecu;
}

Int VG_(get_ExeContext_n_ips)( const ExeContext* e ) {
   vg_assert(e->n_ips >= 1);
   return e->n_ips;
}

ExeContext* VG_(get_ExeContext_from_ECU)( UInt ecu )
{
   UWord i;
   ExeContext* ec;
   vg_assert(VG_(is_plausible_ECU)(ecu));
   vg_assert(ec_htab_size > 0);
   for (i = 0; i < ec_htab_size; i++) {
      for (ec = ec_htab[i]; ec; ec = ec->chain) {
         if (ec->ecu == ecu)
            return ec;
      }
   }
   return NULL;
}

ExeContext* VG_(make_ExeContext_from_StackTrace)( const Addr* ips, UInt n_ips )
{
   init_ExeContext_storage();
   return record_ExeContext_wrk2(ips, n_ips);
}

ExeContext* VG_(null_ExeContext) (void)
{
   init_ExeContext_storage();
   return null_ExeContext;
}

/*--------------------------------------------------------------------*/
/*--- end                                           m_execontext.c ---*/
/*--------------------------------------------------------------------*/
