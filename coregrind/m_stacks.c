
/*--------------------------------------------------------------------*/
/*--- Stack management.                                 m_stacks.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_stacks.h"
#include "pub_core_tooliface.h"

/*
   The stack
   ~~~~~~~~~
   The stack's segment seems to be dynamically extended downwards by
   the kernel as the stack pointer moves down.  Initially, a 1-page
   (4k) stack is allocated.  When SP moves below that for the first
   time, presumably a page fault occurs.  The kernel detects that the
   faulting address is in the range from SP - VG_STACK_REDZONE_SZB
   upwards to the current valid stack.  It then extends the stack
   segment downwards for enough to cover the faulting address, and
   resumes the process (invisibly).  The process is unaware of any of
   this.

   That means that Valgrind can't spot when the stack segment is being
   extended.  Fortunately, we want to precisely and continuously
   update stack permissions around SP, so we need to spot all writes
   to SP anyway.

   The deal is: when SP is assigned a lower value, the stack is being
   extended.  Create suitably-permissioned pages to fill in any holes
   between the old stack ptr and this one, if necessary.  Then mark
   all bytes in the area just "uncovered" by this SP change as
   write-only.

   When SP goes back up, mark the area receded over as unreadable and
   unwritable.

   Just to record the SP boundary conditions somewhere convenient: 
   SP - VG_STACK_REDZONE_SZB always points to the lowest live byte in
   the stack.  All addresses below SP - VG_STACK_REDZONE_SZB are not
   live; those at and above it are.

   We do not concern ourselves here with the VG_STACK_REDZONE_SZB
   bias; that is handled by new_mem_stack/die_mem_stack.
*/

/*
 * This structure holds information about the start and end addresses of
 * registered stacks.  There's always at least one stack registered:
 * the main process stack.  It will be the first stack registered and
 * so will have a stack id of 0.  The user does not need to register
 * this stack: Valgrind does it automatically right before it starts
 * running the client.  No other stacks are automatically registered by
 * Valgrind, however.
 */
typedef struct _Stack {
   UWord id;
   Addr start;
   Addr end;
   struct _Stack *next;
} Stack;

static Stack *stacks;
static UWord next_id;  /* Next id we hand out to a newly registered stack */

/*
 * These are the id, start and end values of the current stack.  If the
 * stack pointer falls outside the range of the current stack, we search
 * the stacks list above for a matching stack.
 */
static Stack *current_stack;

/* Find 'st' in the stacks_list and move it one step closer the the
   front of the list, so as to make subsequent searches for it
   cheaper. */
static void move_Stack_one_step_forward ( Stack* st )
{
   Stack *st0, *st1, *st2;
   if (st == stacks)
      return; /* already at head of list */
   vg_assert(st != NULL);
   st0 = stacks;
   st1 = NULL;
   st2 = NULL;
   while (True) {
      if (st0 == NULL || st0 == st) break;
      st2 = st1;
      st1 = st0;
      st0 = st0->next;
   }
   vg_assert(st0 == st);
   if (st0 != NULL && st1 != NULL && st2 != NULL) {
      Stack* tmp;
      /* st0 points to st, st1 to its predecessor, and st2 to st1's
         predecessor.  Swap st0 and st1, that is, move st0 one step
         closer to the start of the list. */
      vg_assert(st2->next == st1);
      vg_assert(st1->next == st0);
      tmp = st0->next;
      st2->next = st0;
      st0->next = st1;
      st1->next = tmp;
   }
   else
   if (st0 != NULL && st1 != NULL && st2 == NULL) {
      /* it's second in the list. */
      vg_assert(stacks == st1);
      vg_assert(st1->next == st0);
      st1->next = st0->next;
      st0->next = st1;
      stacks = st0;
   }
}

/* Find what stack an address falls into. */
static Stack* find_stack_by_addr(Addr sp)
{
   static UWord n_fails = 0;
   static UWord n_searches = 0;
   static UWord n_steps = 0;
   Stack *i = stacks;
   n_searches++;
   if (0 && 0 == (n_searches % 10000))
      VG_(printf)("(hgdev) %lu searches, %lu steps, %lu fails\n",
                  n_searches, n_steps+1, n_fails);
   /* fast track common case */
   if (i && sp >= i->start && sp <= i->end)
      return i;
   /* else search the list */
   while (i) {
      n_steps++;
      if (sp >= i->start && sp <= i->end) {
         if (1 && (n_searches & 0x3F) == 0) {
            move_Stack_one_step_forward( i );
         }
         return i;
      }
      i = i->next;
   }
   n_fails++;
   return NULL;
}

/*
 * Register a new stack from start - end.  This is invoked from the
 * VALGRIND_STACK_REGISTER client request, and is also called just before
 * we start the client running, to register the main process stack.
 */
UWord VG_(register_stack)(Addr start, Addr end)
{
   Stack *i;

   if (start > end) {
      Addr t = end;
      end = start;
      start = t;
   }

   i = (Stack *)VG_(arena_malloc)(VG_AR_CORE, "stacks.rs.1", sizeof(Stack));
   i->start = start;
   i->end = end;
   i->id = next_id++;
   i->next = stacks;
   stacks = i;

   if (i->id == 0) {
      current_stack = i;
   }

   VG_(debugLog)(2, "stacks", "register %p-%p as stack %lu\n",
                    (void*)start, (void*)end, i->id);

   return i->id;
}

/*
 * Deregister a stack.  This is invoked from the VALGRIND_STACK_DEREGISTER
 * client request.
 */
void VG_(deregister_stack)(UWord id)
{
   Stack *i = stacks;
   Stack *prev = NULL;

   VG_(debugLog)(2, "stacks", "deregister stack %lu\n", id);

   if (current_stack && current_stack->id == id) { 
      current_stack = NULL;
   }

   while(i) {
      if (i->id == id) {
         if(prev == NULL) {
            stacks = i->next;
         } else {
            prev->next = i->next;
         }
         VG_(arena_free)(VG_AR_CORE, i);
         return;
      }
      prev = i;
      i = i->next;
   }
}

/*
 * Change a stack.  This is invoked from the VALGRIND_STACK_CHANGE client
 * request and from the stack growth stuff the signals module when
 * extending the main process stack.
 */
void VG_(change_stack)(UWord id, Addr start, Addr end)
{
   Stack *i = stacks;

   while (i) {
      if (i->id == id) {
         VG_(debugLog)(2, "stacks", "change stack %lu from %p-%p to %p-%p\n",
                       id, (void*)i->start, (void*)i->end,
                           (void*)start,    (void*)end);
         i->start = start;
         i->end = end;
         return;
      }
      i = i->next;
   }
}

/*
 * Find the bounds of the stack (if any) which includes the
 * specified stack pointer.
 */
void VG_(stack_limits)(Addr SP, Addr *start, Addr *end )
{
   Stack* stack = find_stack_by_addr(SP);

   if (stack) {
      *start = stack->start;
      *end = stack->end;
   }
}

/* This function gets called if new_mem_stack and/or die_mem_stack are
   tracked by the tool, and one of the specialised cases
   (eg. new_mem_stack_4) isn't used in preference.  
*/
VG_REGPARM(3)
void VG_(unknown_SP_update)( Addr old_SP, Addr new_SP, UInt ecu )
{
   static Int moans = 3;
   Word delta  = (Word)new_SP - (Word)old_SP;

   /* Check if the stack pointer is still in the same stack as before. */
   if (current_stack == NULL ||
       new_SP < current_stack->start || new_SP > current_stack->end) {
      Stack* new_stack = find_stack_by_addr(new_SP);
      if (new_stack 
          && (current_stack == NULL || new_stack->id != current_stack->id)) { 
         /* The stack pointer is now in another stack.  Update the current
            stack information and return without doing anything else. */
         current_stack = new_stack;
         return;
      }
   }

   if (delta < -VG_(clo_max_stackframe) || VG_(clo_max_stackframe) < delta) {
      /* SP has changed by more than some threshold amount (by
         default, 2MB).  We take this to mean that the application is
         switching to a new stack, for whatever reason.
       
         JRS 20021001: following discussions with John Regehr, if a stack
         switch happens, it seems best not to mess at all with memory
         permissions.  Seems to work well with Netscape 4.X.  Really the
         only remaining difficulty is knowing exactly when a stack switch is
         happening. */
      if (VG_(clo_verbosity) > 0 && moans > 0 && !VG_(clo_xml)) {
         moans--;
         VG_(message)(Vg_UserMsg,
            "Warning: client switching stacks?  "
            "SP change: 0x%lx --> 0x%lx\n", old_SP, new_SP);
         VG_(message)(Vg_UserMsg,
            "         to suppress, use: --max-stackframe=%ld or greater\n",
            (delta < 0 ? -delta : delta));
         if (moans == 0)
            VG_(message)(Vg_UserMsg,
                "         further instances of this message "
                "will not be shown.\n");
      }
   } else if (delta < 0) {
      VG_TRACK( new_mem_stack_w_ECU, new_SP, -delta, ecu );
      VG_TRACK( new_mem_stack,       new_SP, -delta );

   } else if (delta > 0) {
      VG_TRACK( die_mem_stack, old_SP,  delta );
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

