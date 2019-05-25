
/*--------------------------------------------------------------------*/
/*--- Stack management.                                 m_stacks.c ---*/
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
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_options.h"
#include "pub_core_stacks.h"
#include "pub_core_tooliface.h"
#include "pub_core_inner.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
#include "pub_core_clreq.h"
#endif 

// For expensive debugging
#define EDEBUG(fmt, args...) //VG_(debugLog)(2, "stacks", fmt, ## args)

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
   Addr start; // Lowest stack byte, included.
   Addr end;   // Highest stack byte, included.
   struct _Stack *next;
   UWord outer_id; /* For an inner valgrind, stack id registered in outer
                      valgrind. */
} Stack;

static Stack *stacks;
static UWord next_id;  /* Next id we hand out to a newly registered stack */

/*
 * These are the id, start and end values of the current stack.  If the
 * stack pointer falls outside the range of the current stack, we search
 * the stacks list above for a matching stack.
 */
static Stack *current_stack;

/* Find 'st' in the stacks_list and move it one step closer to the
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
      /* If caller provides addresses in reverse order, swap them.
         Ugly but not doing that breaks backward compatibility with
         (user) code registering stacks with start/end inverted . */
      Addr t = end;
      end = start;
      start = t;
   }

   i = VG_(malloc)("stacks.rs.1", sizeof(Stack));
   i->start = start;
   i->end = end;
   i->id = next_id++;
   i->next = stacks;
   stacks = i;

   if (i->id == 0) {
      current_stack = i;
   }

   VG_(debugLog)(2, "stacks", "register [start-end] [%p-%p] as stack %lu\n",
                    (void*)start, (void*)end, i->id);
   INNER_REQUEST(i->outer_id = VALGRIND_STACK_REGISTER(start, end));
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
         INNER_REQUEST(VALGRIND_STACK_DEREGISTER(i->outer_id));
         VG_(free)(i);
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
         VG_(debugLog)(2, "stacks", 
                       "change stack %lu from [%p-%p] to [%p-%p]\n",
                       id, (void*)i->start, (void*)i->end,
                           (void*)start,    (void*)end);
         /* FIXME : swap start/end like VG_(register_stack) ??? */
         i->start = start;
         i->end = end;
         INNER_REQUEST(VALGRIND_STACK_CHANGE(i->outer_id, start, end));
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
   NSegment const *stackseg = VG_(am_find_nsegment) (SP);

   if (LIKELY(stack)) {
      *start = stack->start;
      *end = stack->end;
   }

   /* SP is assumed to be in a RW segment or in the SkResvn segment of an
      extensible stack (normally, only the main thread has an extensible
      stack segment).
      If no such segment is found, assume we have no valid
      stack for SP, and set *start and *end to 0.
      Otherwise, possibly reduce the stack limits using the boundaries of
      the RW segment/SkResvn segments containing SP. */
   if (UNLIKELY(stackseg == NULL)) {
      VG_(debugLog)(2, "stacks", 
                    "no addressable segment for SP %p\n", 
                    (void*)SP);
      *start = 0;
      *end = 0;
      return;
   } 

   if (UNLIKELY((!stackseg->hasR || !stackseg->hasW)
                && (stackseg->kind != SkResvn || stackseg->smode != SmUpper))) {
      VG_(debugLog)(2, "stacks", 
                    "segment for SP %p is not RW or not a SmUpper Resvn\n",
                    (void*)SP);
      *start = 0;
      *end = 0;
      return;
   } 

   /* SP is in a RW segment, or in the SkResvn of an extensible stack.
      We can use the seg start as the stack start limit. */
   if (UNLIKELY(*start < stackseg->start)) {
      VG_(debugLog)(2, "stacks", 
                    "segment for SP %p changed stack start limit"
                    " from %p to %p\n",
                    (void*)SP, (void*)*start, (void*)stackseg->start);
      *start = stackseg->start;
   }

   /* Now, determine the stack end limit. If the stackseg is SkResvn,
      we need to get the neighbour segment (towards higher addresses).
      This segment must be anonymous and RW. */
   if (UNLIKELY(stackseg->kind == SkResvn)) {
      stackseg = VG_(am_next_nsegment)(stackseg, /*forward*/ True);
      if (!stackseg || !stackseg->hasR || !stackseg->hasW
          || stackseg->kind != SkAnonC) {
         VG_(debugLog)(2, "stacks", 
                       "Next forward segment for SP %p Resvn segment"
                       " is not RW or not AnonC\n",
                       (void*)SP);
         *start = 0;
         *end = 0;
         return;
      }
   }

   /* Limit the stack end limit, using the found segment. */
   if (UNLIKELY(*end > stackseg->end)) {
      VG_(debugLog)(2, "stacks", 
                    "segment for SP %p changed stack end limit"
                    " from %p to %p\n",
                    (void*)SP, (void*)*end, (void*)stackseg->end);
      *end = stackseg->end;
   }

   /* If reducing start and/or end to the SP segment gives an
      empty range, return 'empty' limits */
   if (UNLIKELY(*start > *end)) {
      VG_(debugLog)(2, "stacks", 
                    "stack for SP %p start %p after end %p\n",
                    (void*)SP, (void*)*start, (void*)end);
      *start = 0;
      *end = 0;
   }
}

/* complaints_stack_switch reports that SP has changed by more than some
   threshold amount (by default, 2MB).  We take this to mean that the
   application is switching to a new stack, for whatever reason.
   
   JRS 20021001: following discussions with John Regehr, if a stack
   switch happens, it seems best not to mess at all with memory
   permissions.  Seems to work well with Netscape 4.X.  Really the
   only remaining difficulty is knowing exactly when a stack switch is
   happening. */
__attribute__((noinline))
static void complaints_stack_switch (Addr old_SP, Addr new_SP)
{
   static Int complaints = 3;
   if (VG_(clo_verbosity) > 0 && complaints > 0 && !VG_(clo_xml)) {
      Word delta  = (Word)new_SP - (Word)old_SP;
      complaints--;
      VG_(message)(Vg_UserMsg,
                   "Warning: client switching stacks?  "
                   "SP change: 0x%lx --> 0x%lx\n", old_SP, new_SP);
      VG_(message)(Vg_UserMsg,
                   "         to suppress, use: --max-stackframe=%ld "
                   "or greater\n",
                   (delta < 0 ? -delta : delta));
      if (complaints == 0)
         VG_(message)(Vg_UserMsg,
                      "         further instances of this message "
                      "will not be shown.\n");
   }
}

/* The functions VG_(unknown_SP_update) and VG_(unknown_SP_update_w_ECU)
   get called if new_mem_stack and/or die_mem_stack are
   tracked by the tool, and one of the specialised cases
   (eg. new_mem_stack_4) isn't used in preference.  

   These functions are performance critical, so are built with macros. */

// preamble + check if stack has switched.
#define IF_STACK_SWITCH_SET_current_stack_AND_RETURN                    \
   Word delta  = (Word)new_SP - (Word)old_SP;                           \
                                                                        \
   EDEBUG("current_stack  %p-%p %lu new_SP %p old_SP %p\n",             \
          (void *) (current_stack ? current_stack->start : 0x0),        \
          (void *) (current_stack ? current_stack->end : 0x0),          \
          current_stack ? current_stack->id : 0,                        \
          (void *)new_SP, (void *)old_SP);                              \
                                                                        \
   /* Check if the stack pointer is still in the same stack as before. */ \
   if (UNLIKELY(current_stack == NULL ||                                \
      new_SP < current_stack->start || new_SP > current_stack->end)) {  \
      Stack* new_stack = find_stack_by_addr(new_SP);                    \
      if (new_stack                                                     \
          && (current_stack == NULL || new_stack->id != current_stack->id)) { \
         /* The stack pointer is now in another stack.  Update the current */ \
         /* stack information and return without doing anything else. */ \
         current_stack = new_stack;                                     \
         EDEBUG("new current_stack  %p-%p %lu \n",                      \
                (void *) current_stack->start,                          \
                (void *) current_stack->end,                            \
                current_stack->id);                                     \
         return;                                                        \
      } else {                                                          \
         EDEBUG("new current_stack not found\n");                       \
      }                                                                 \
   }

#define IF_BIG_DELTA_complaints_AND_RETURN                              \
   if (UNLIKELY(delta < -VG_(clo_max_stackframe)                        \
                || VG_(clo_max_stackframe) < delta)) {                  \
      complaints_stack_switch(old_SP, new_SP);                          \
      return;                                                           \
   }

#define IF_SMALLER_STACK_die_mem_stack_AND_RETURN                       \
   if (delta > 0) {                                                     \
      VG_TRACK( die_mem_stack, old_SP,  delta );                        \
      return;                                                           \
   }

  
VG_REGPARM(3)
void VG_(unknown_SP_update_w_ECU)( Addr old_SP, Addr new_SP, UInt ecu ) {
   IF_STACK_SWITCH_SET_current_stack_AND_RETURN;
   IF_BIG_DELTA_complaints_AND_RETURN;
   IF_SMALLER_STACK_die_mem_stack_AND_RETURN;
   if (delta < 0) { // IF_BIGGER_STACK
      VG_TRACK( new_mem_stack_w_ECU, new_SP, -delta, ecu );
      return;
   }
   // SAME_STACK. nothing to do.
}

VG_REGPARM(2)
void VG_(unknown_SP_update)( Addr old_SP, Addr new_SP ) {
   IF_STACK_SWITCH_SET_current_stack_AND_RETURN;
   IF_BIG_DELTA_complaints_AND_RETURN;
   IF_SMALLER_STACK_die_mem_stack_AND_RETURN;
   if (delta < 0) { // IF_BIGGER_STACK
      VG_TRACK( new_mem_stack,      new_SP, -delta );
      return;
   }
   // SAME_STACK. nothing to do.
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

