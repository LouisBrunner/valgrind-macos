
/*--------------------------------------------------------------------*/
/*--- Storage, and equality on, execution contexts (backtraces).   ---*/
/*---                                              vg_execontext.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org
      Julian_Seward@muraroa.demon.co.uk

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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"
#include "vg_constants.h"


/*------------------------------------------------------------*/
/*--- Low-level ExeContext storage.                        ---*/
/*------------------------------------------------------------*/

/* The idea is only to ever store any one context once, so as to save
   space and make exact comparisons faster. */

static ExeContext* vg_ec_list[VG_N_EC_LISTS];

/* Stats only: the number of times the system was searched to locate a
   context. */
static UInt vg_ec_searchreqs;

/* Stats only: the number of full context comparisons done. */
static UInt vg_ec_searchcmps;

/* Stats only: total number of stored contexts. */
static UInt vg_ec_totstored;

/* Number of 2, 4 and (fast) full cmps done. */
static UInt vg_ec_cmp2s;
static UInt vg_ec_cmp4s;
static UInt vg_ec_cmpAlls;


/*------------------------------------------------------------*/
/*--- Exported functions.                                  ---*/
/*------------------------------------------------------------*/


/* Initialise this subsystem. */
void VG_(init_ExeContext_storage) ( void )
{
   Int i;
   vg_ec_searchreqs = 0;
   vg_ec_searchcmps = 0;
   vg_ec_totstored = 0;
   vg_ec_cmp2s = 0;
   vg_ec_cmp4s = 0;
   vg_ec_cmpAlls = 0;
   for (i = 0; i < VG_N_EC_LISTS; i++)
      vg_ec_list[i] = NULL;
}


/* Show stats. */
void VG_(show_ExeContext_stats) ( void )
{
   VG_(message)(Vg_DebugMsg, 
      "exectx: %d lists, %d contexts (avg %d per list)",
      VG_N_EC_LISTS, vg_ec_totstored, 
      vg_ec_totstored / VG_N_EC_LISTS 
   );
   VG_(message)(Vg_DebugMsg, 
      "exectx: %d searches, %d full compares (%d per 1000)",
      vg_ec_searchreqs, vg_ec_searchcmps, 
      vg_ec_searchreqs == 0 
         ? 0 
         : (UInt)( (((ULong)vg_ec_searchcmps) * 1000) 
           / ((ULong)vg_ec_searchreqs )) 
   );
   VG_(message)(Vg_DebugMsg, 
      "exectx: %d cmp2, %d cmp4, %d cmpAll",
      vg_ec_cmp2s, vg_ec_cmp4s, vg_ec_cmpAlls 
   );
}


/* Print an ExeContext. */
void VG_(pp_ExeContext) ( ExeContext* e )
{
   VG_(mini_stack_dump) ( e );
}


/* Compare two ExeContexts, comparing all callers. */
Bool VG_(eq_ExeContext_all) ( ExeContext* e1, ExeContext* e2 )
{
   vg_ec_cmpAlls++;
   /* Just do pointer comparison. */
   if (e1 != e2) return False;
   return True;
}


/* Compare two ExeContexts, just comparing the top two callers. */
Bool VG_(eq_ExeContext_top2) ( ExeContext* e1, ExeContext* e2 )
{
   vg_ec_cmp2s++;
   if (e1->eips[0] != e2->eips[0]
       || e1->eips[1] != e2->eips[1]) return False;
   return True;
}


/* Compare two ExeContexts, just comparing the top four callers. */
Bool VG_(eq_ExeContext_top4) ( ExeContext* e1, ExeContext* e2 )
{
   vg_ec_cmp4s++;
   if (e1->eips[0] != e2->eips[0]
       || e1->eips[1] != e2->eips[1]) return False;

   if (VG_(clo_backtrace_size) < 3) return True;
   if (e1->eips[2] != e2->eips[2]) return False;

   if (VG_(clo_backtrace_size) < 4) return True;
   if (e1->eips[3] != e2->eips[3]) return False;

   return True;
}


/* This guy is the head honcho here.  Take a snapshot of the client's
   stack.  Search our collection of ExeContexts to see if we already
   have it, and if not, allocate a new one.  Either way, return a
   pointer to the context.  If there is a matching context we
   guarantee to not allocate a new one.  Thus we never store
   duplicates, and so exact equality can be quickly done as equality
   on the returned ExeContext* values themselves.  Inspired by Hugs's
   Text type.  

   In order to be thread-safe, we pass in the thread's %EIP and %EBP.
*/
ExeContext* VG_(get_ExeContext) ( Bool skip_top_frame,
                                  Addr eip, Addr ebp )
{
   Int         i;
   Addr        eips[VG_DEEPEST_BACKTRACE];
   Bool        same;
   UInt        hash;
   ExeContext* new_ec;
   ExeContext* list;

   VGP_PUSHCC(VgpExeContext);

   vg_assert(VG_(clo_backtrace_size) >= 2 
             && VG_(clo_backtrace_size) <= VG_DEEPEST_BACKTRACE);

   /* First snaffle %EIPs from the client's stack into eips[0
      .. VG_(clo_backtrace_size)-1], putting zeroes in when the trail
      goes cold. */

   for (i = 0; i < VG_(clo_backtrace_size); i++)
      eips[i] = 0;
   
#  define GET_CALLER(lval)                                        \
   if (ebp != 0 && VGM_(check_readable)(ebp, 8, NULL)) {          \
      lval = ((UInt*)ebp)[1];  /* ret addr */                     \
      ebp  = ((UInt*)ebp)[0];  /* old ebp */                      \
   } else {                                                       \
      lval = ebp = 0;                                             \
   }

   if (skip_top_frame) {
      for (i = 0; i < VG_(clo_backtrace_size); i++)
         GET_CALLER(eips[i]);
   } else {
      eips[0] = eip;
      for (i = 1; i < VG_(clo_backtrace_size); i++)
         GET_CALLER(eips[i]);
   }
#  undef GET_CALLER

   /* Now figure out if we've seen this one before.  First hash it so
      as to determine the list number. */

   hash = 0;
   for (i = 0; i < VG_(clo_backtrace_size); i++) {
      hash ^= (UInt)eips[i];
      hash = (hash << 29) | (hash >> 3);
   }
   hash = hash % VG_N_EC_LISTS;

   /* And (the expensive bit) look a matching entry in the list. */

   vg_ec_searchreqs++;

   list = vg_ec_list[hash];

   while (True) {
      if (list == NULL) break;
      vg_ec_searchcmps++;
      same = True;
      for (i = 0; i < VG_(clo_backtrace_size); i++) {
         if (list->eips[i] != eips[i]) {
            same = False;
            break; 
         }
      }
      if (same) break;
      list = list->next;
   }

   if (list != NULL) {
      /* Yay!  We found it.  */
      VGP_POPCC;
      return list;
   }

   /* Bummer.  We have to allocate a new context record. */
   vg_ec_totstored++;

   new_ec 
      = VG_(malloc)( 
           VG_AR_EXECTXT, 
           sizeof(struct _ExeContextRec *) 
              + VG_(clo_backtrace_size) * sizeof(Addr) 
        );

   for (i = 0; i < VG_(clo_backtrace_size); i++)
      new_ec->eips[i] = eips[i];

   new_ec->next = vg_ec_list[hash];
   vg_ec_list[hash] = new_ec;

   VGP_POPCC;
   return new_ec;
}


/*--------------------------------------------------------------------*/
/*--- end                                          vg_execontext.c ---*/
/*--------------------------------------------------------------------*/
