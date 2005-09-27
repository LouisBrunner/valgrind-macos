
/*--------------------------------------------------------------------*/
/*--- Stack management.                                 m_stacks.c ---*/
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
static Stack current_stack;

/* Find what stack an address falls into. */
static Stack* find_stack_by_addr(Addr sp)
{
   Stack *i = stacks;
   while (i) {
      if (sp >= i->start && sp <= i->end) {
         return i;
      }
      i = i->next;
   }
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

   if (0) VG_(printf)("REGISTER STACK %p %p\n", start,end);

   if (start > end) {
      Addr t = end;
      end = start;
      start = t;
   }

   i = (Stack *)VG_(arena_malloc)(VG_AR_CORE, sizeof(Stack));
   i->start = start;
   i->end = end;
   i->id = next_id++;
   i->next = stacks;
   stacks = i;

   if (i->id == 0) {
      current_stack = *i;
   }

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

   if (current_stack.id == id) {
      return;
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

   if (id == current_stack.id) {
      current_stack.start = start;
      current_stack.end = end;
   }

   while (i) {
      if (i->id == id) {
         i->start = start;
         i->end = end;
         return;
      }
      i = i->next;
   }
}

/* This function gets called if new_mem_stack and/or die_mem_stack are
   tracked by the tool, and one of the specialised cases
   (eg. new_mem_stack_4) isn't used in preference.  
*/
VG_REGPARM(2)
void VG_(unknown_SP_update)( Addr old_SP, Addr new_SP )
{
   static Int moans = 3;
   Word delta  = (Word)new_SP - (Word)old_SP;

   /* Check if the stack pointer is still in the same stack as before. */
   if (new_SP < current_stack.start || new_SP > current_stack.end) {
      Stack* new_stack = find_stack_by_addr(new_SP);
      if (new_stack && new_stack->id != current_stack.id) {
         /* The stack pointer is now in another stack.  Update the current
            stack information and return without doing anything else. */
         current_stack = *new_stack;
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
      if (VG_(clo_verbosity) > 0 && moans > 0) {
         moans--;
         VG_(message)(Vg_UserMsg,
            "Warning: client switching stacks?  "
            "SP change: %p --> %p", old_SP, new_SP);
         VG_(message)(Vg_UserMsg,
            "         to suppress, use: --max-stackframe=%d or greater",
            (delta < 0 ? -delta : delta));
         if (moans == 0)
            VG_(message)(Vg_UserMsg,
                "         further instances of this message "
                "will not be shown.");
      }
   } else if (delta < 0) {
      VG_TRACK( new_mem_stack, new_SP, -delta );

   } else if (delta > 0) {
      VG_TRACK( die_mem_stack, old_SP,  delta );
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

