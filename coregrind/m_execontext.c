
/*--------------------------------------------------------------------*/
/*--- Store and compare stack backtraces            m_execontext.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward 
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

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
#include "pub_core_vki.h"           // To keep pub_core_threadstate.h happy
#include "pub_core_libcsetjmp.h"    // Ditto
#include "pub_core_threadstate.h"   // VG_(is_valid_tid)
#include "pub_core_execontext.h"    // self

/*------------------------------------------------------------*/
/*--- Low-level ExeContext storage.                        ---*/
/*------------------------------------------------------------*/

/* The first 4 IP values are used in comparisons to remove duplicate
   errors, and for comparing against suppression specifications.  The
   rest are purely informational (but often important).

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
/*--- Exported functions.                                  ---*/
/*------------------------------------------------------------*/


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
   ec_htab = VG_(arena_malloc)(VG_AR_EXECTXT, "execontext.iEs1",
                               sizeof(ExeContext*) * ec_htab_size);
   for (i = 0; i < ec_htab_size; i++)
      ec_htab[i] = NULL;

   init_done = True;
}


/* Print stats. */
void VG_(print_ExeContext_stats) ( void )
{
   init_ExeContext_storage();
   VG_(message)(Vg_DebugMsg, 
      "   exectx: %'lu lists, %'llu contexts (avg %'llu per list)\n",
      ec_htab_size, ec_totstored, ec_totstored / (ULong)ec_htab_size
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
   VG_(pp_StackTrace)( ec->ips, ec->n_ips );
}


/* Compare two ExeContexts.  Number of callers considered depends on res. */
Bool VG_(eq_ExeContext) ( VgRes res, ExeContext* e1, ExeContext* e2 )
{
   Int i;

   if (e1 == NULL || e2 == NULL) 
      return False;

   // Must be at least one address in each trace.
   tl_assert(e1->n_ips >= 1 && e2->n_ips >= 1);

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
      return True;

   case Vg_MedRes:
      /* Just compare the top four callers. */
      ec_cmp4s++;
      for (i = 0; i < 4; i++) {
         if ( (e1->n_ips <= i) &&  (e2->n_ips <= i)) return True;
         if ( (e1->n_ips <= i) && !(e2->n_ips <= i)) return False;
         if (!(e1->n_ips <= i) &&  (e2->n_ips <= i)) return False;
         if (e1->ips[i] != e2->ips[i])               return False;
      }
      return True;

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

static UWord calc_hash ( Addr* ips, UInt n_ips, UWord htab_sz )
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

   vg_assert(ec_htab_size_idx >= 0 && ec_htab_size_idx < N_EC_PRIMES);
   if (ec_htab_size_idx == N_EC_PRIMES-1)
      return; /* out of primes - can't resize further */

   new_size = ec_primes[ec_htab_size_idx + 1];
   new_ec_htab = VG_(arena_malloc)(VG_AR_EXECTXT, "execontext.reh1",
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

   VG_(arena_free)(VG_AR_EXECTXT, ec_htab);
   ec_htab      = new_ec_htab;
   ec_htab_size = new_size;
   ec_htab_size_idx++;
}

/* Do the first part of getting a stack trace: actually unwind the
   stack, and hand the results off to the duplicate-trace-finder
   (_wrk2). */
static ExeContext* record_ExeContext_wrk2 ( Addr* ips, UInt n_ips ); /*fwds*/
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
   }

   return record_ExeContext_wrk2 ( ips, n_ips );
}

/* Do the second part of getting a stack trace: ips[0 .. n_ips-1]
   holds a proposed trace.  Find or allocate a suitable ExeContext.
   Note that callers must have done init_ExeContext_storage() before
   getting to this point. */
static ExeContext* record_ExeContext_wrk2 ( Addr* ips, UInt n_ips )
{
   Int         i;
   Bool        same;
   UWord       hash;
   ExeContext* new_ec;
   ExeContext* list;
   ExeContext  *prev2, *prev;

   static UInt ctr = 0;

   tl_assert(n_ips >= 1 && n_ips <= VG_(clo_backtrace_size));

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
      same = True;
      for (i = 0; i < n_ips; i++) {
         if (list->ips[i] != ips[i]) {
            same = False;
            break; 
         }
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

   new_ec = VG_(arena_malloc)( VG_AR_EXECTXT, "execontext.rEw2.2",
                               sizeof(struct _ExeContext) 
                               + n_ips * sizeof(Addr) );

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
   ec_htab[hash] = new_ec;

   /* Resize the hash table, maybe? */
   if ( ((ULong)ec_totstored) > ((ULong)ec_htab_size) ) {
      vg_assert(ec_htab_size_idx >= 0 && ec_htab_size_idx < N_EC_PRIMES);
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

UInt VG_(get_ECU_from_ExeContext)( ExeContext* e ) {
   vg_assert(VG_(is_plausible_ECU)(e->ecu));
   return e->ecu;
}

Int VG_(get_ExeContext_n_ips)( ExeContext* e ) {
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

ExeContext* VG_(make_ExeContext_from_StackTrace)( Addr* ips, UInt n_ips )
{
   return record_ExeContext_wrk2(ips, n_ips);
}

/*--------------------------------------------------------------------*/
/*--- end                                           m_execontext.c ---*/
/*--------------------------------------------------------------------*/
