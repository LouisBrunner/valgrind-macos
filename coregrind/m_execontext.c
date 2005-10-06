
/*--------------------------------------------------------------------*/
/*--- Store and compare stack backtraces            m_execontext.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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
#include "pub_core_execontext.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"     // For VG_(message)()
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_profile.h"
#include "pub_core_stacktrace.h"

/*------------------------------------------------------------*/
/*--- Low-level ExeContext storage.                        ---*/
/*------------------------------------------------------------*/

/* The first 4 IP values are used in comparisons to remove duplicate errors,
   and for comparing against suppression specifications.  The rest are
   purely informational (but often important). */

struct _ExeContext {
   struct _ExeContext * next;
   /* Variable-length array.  The size is VG_(clo_backtrace_size); at
      least 1, at most VG_DEEPEST_BACKTRACE.  [0] is the current IP,
      [1] is its caller, [2] is the caller of [1], etc. */
   Addr ips[0];
};

/* Number of lists in which we keep track of ExeContexts.  Should be
   prime. */
#define N_EC_LISTS 30011 /*4999*/ /* a prime number */

/* The idea is only to ever store any one context once, so as to save
   space and make exact comparisons faster. */

static ExeContext* ec_list[N_EC_LISTS];

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
   if (init_done)
      return;
   ec_searchreqs = 0;
   ec_searchcmps = 0;
   ec_totstored = 0;
   ec_cmp2s = 0;
   ec_cmp4s = 0;
   ec_cmpAlls = 0;
   for (i = 0; i < N_EC_LISTS; i++)
      ec_list[i] = NULL;
   init_done = True;
}


/* Print stats. */
void VG_(print_ExeContext_stats) ( void )
{
   init_ExeContext_storage();
   VG_(message)(Vg_DebugMsg, 
      "   exectx: %,lu lists, %,llu contexts (avg %,llu per list)",
      N_EC_LISTS, ec_totstored, ec_totstored / N_EC_LISTS 
   );
   VG_(message)(Vg_DebugMsg, 
      "   exectx: %,llu searches, %,llu full compares (%,llu per 1000)",
      ec_searchreqs, ec_searchcmps, 
      ec_searchreqs == 0 
         ? 0L 
         : ( (ec_searchcmps * 1000) / ec_searchreqs ) 
   );
   VG_(message)(Vg_DebugMsg, 
      "   exectx: %,llu cmp2, %,llu cmp4, %,llu cmpAll",
      ec_cmp2s, ec_cmp4s, ec_cmpAlls 
   );
}


/* Print an ExeContext. */
void VG_(pp_ExeContext) ( ExeContext* ec )
{
   VG_(pp_StackTrace)( ec->ips, VG_(clo_backtrace_size) );
}


/* Compare two ExeContexts, comparing all callers. */
Bool VG_(eq_ExeContext) ( VgRes res, ExeContext* e1, ExeContext* e2 )
{
   if (e1 == NULL || e2 == NULL) 
      return False;
   switch (res) {
   case Vg_LowRes:
      /* Just compare the top two callers. */
      ec_cmp2s++;
      if (e1->ips[0] != e2->ips[0]) return False;

      if (VG_(clo_backtrace_size) < 2) return True;
      if (e1->ips[1] != e2->ips[1]) return False;
      return True;

   case Vg_MedRes:
      /* Just compare the top four callers. */
      ec_cmp4s++;
      if (e1->ips[0] != e2->ips[0]) return False;

      if (VG_(clo_backtrace_size) < 2) return True;
      if (e1->ips[1] != e2->ips[1]) return False;

      if (VG_(clo_backtrace_size) < 3) return True;
      if (e1->ips[2] != e2->ips[2]) return False;

      if (VG_(clo_backtrace_size) < 4) return True;
      if (e1->ips[3] != e2->ips[3]) return False;
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

/* This guy is the head honcho here.  Take a snapshot of the client's
   stack.  Search our collection of ExeContexts to see if we already
   have it, and if not, allocate a new one.  Either way, return a
   pointer to the context.  If there is a matching context we
   guarantee to not allocate a new one.  Thus we never store
   duplicates, and so exact equality can be quickly done as equality
   on the returned ExeContext* values themselves.  Inspired by Hugs's
   Text type.  
*/
ExeContext* VG_(record_ExeContext) ( ThreadId tid )
{
   Int         i;
   Addr        ips[VG_DEEPEST_BACKTRACE];
   Bool        same;
   UWord       hash;
   ExeContext* new_ec;
   ExeContext* list;

   VGP_PUSHCC(VgpExeContext);

   init_ExeContext_storage();
   vg_assert(VG_(clo_backtrace_size) >= 1 
             && VG_(clo_backtrace_size) <= VG_DEEPEST_BACKTRACE);

   VG_(get_StackTrace)( tid, ips, VG_(clo_backtrace_size) );

   /* Now figure out if we've seen this one before.  First hash it so
      as to determine the list number. */

   hash = 0;
   for (i = 0; i < VG_(clo_backtrace_size); i++) {
      hash ^= ips[i];
      hash = (hash << 29) | (hash >> 3);
   }
   hash = hash % N_EC_LISTS;

   /* And (the expensive bit) look a matching entry in the list. */

   ec_searchreqs++;

   list = ec_list[hash];

   while (True) {
      if (list == NULL) break;
      ec_searchcmps++;
      same = True;
      for (i = 0; i < VG_(clo_backtrace_size); i++) {
         if (list->ips[i] != ips[i]) {
            same = False;
            break; 
         }
      }
      if (same) break;
      list = list->next;
   }

   if (list != NULL) {
      /* Yay!  We found it.  */
      VGP_POPCC(VgpExeContext);
      return list;
   }

   /* Bummer.  We have to allocate a new context record. */
   ec_totstored++;

   new_ec = VG_(arena_malloc)( VG_AR_EXECTXT, 
                               sizeof(struct _ExeContext *) 
                               + VG_(clo_backtrace_size) * sizeof(Addr) );

   for (i = 0; i < VG_(clo_backtrace_size); i++)
      new_ec->ips[i] = ips[i];

   new_ec->next = ec_list[hash];
   ec_list[hash] = new_ec;

   VGP_POPCC(VgpExeContext);
   return new_ec;
}

StackTrace VG_(extract_StackTrace) ( ExeContext* e )
{                                  
   return e->ips;
}  

/*--------------------------------------------------------------------*/
/*--- end                                           m_execontext.c ---*/
/*--------------------------------------------------------------------*/
