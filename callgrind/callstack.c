/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                               ct_callstack.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2010, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

#include "global.h"

/*------------------------------------------------------------*/
/*--- Call stack, operations                               ---*/
/*------------------------------------------------------------*/

/* Stack of current thread. Gets initialized when switching to 1st thread.
 *
 * The artificial call stack is an array of call_entry's, representing
 * stack frames of the executing program. 
 * Array call_stack and call_stack_esp have same size and grow on demand.
 * Array call_stack_esp holds SPs of corresponding stack frames.
 *
 */

#define N_CALL_STACK_INITIAL_ENTRIES 500

call_stack CLG_(current_call_stack);

void CLG_(init_call_stack)(call_stack* s)
{
  Int i;

  CLG_ASSERT(s != 0);

  s->size = N_CALL_STACK_INITIAL_ENTRIES;   
  s->entry = (call_entry*) CLG_MALLOC("cl.callstack.ics.1",
                                      s->size * sizeof(call_entry));
  s->sp = 0;
  s->entry[0].cxt = 0; /* for assertion in push_cxt() */

  for(i=0; i<s->size; i++) s->entry[i].enter_cost = 0;
}

call_entry* CLG_(get_call_entry)(Int sp)
{
  CLG_ASSERT(sp <= CLG_(current_call_stack).sp);
  return &(CLG_(current_call_stack).entry[sp]);
}

void CLG_(copy_current_call_stack)(call_stack* dst)
{
  CLG_ASSERT(dst != 0);

  dst->size  = CLG_(current_call_stack).size;
  dst->entry = CLG_(current_call_stack).entry;
  dst->sp    = CLG_(current_call_stack).sp;
}

void CLG_(set_current_call_stack)(call_stack* s)
{
  CLG_ASSERT(s != 0);

  CLG_(current_call_stack).size  = s->size;
  CLG_(current_call_stack).entry = s->entry;
  CLG_(current_call_stack).sp    = s->sp;
}


static __inline__
void ensure_stack_size(Int i)
{
  Int oldsize;
  call_stack *cs = &CLG_(current_call_stack);

  if (i < cs->size) return;

  oldsize = cs->size;
  cs->size *= 2;
  while (i > cs->size) cs->size *= 2;

  cs->entry = (call_entry*) VG_(realloc)("cl.callstack.ess.1",
                                         cs->entry,
					 cs->size * sizeof(call_entry));

  for(i=oldsize; i<cs->size; i++)
    cs->entry[i].enter_cost = 0;

  CLG_(stat).call_stack_resizes++;
 
  CLG_DEBUGIF(2)
    VG_(printf)("        call stack enlarged to %d entries\n",
		CLG_(current_call_stack).size);
}



/* Called when function entered nonrecursive */
static void function_entered(fn_node* fn)
{
  CLG_ASSERT(fn != 0);

#if CLG_ENABLE_DEBUG
  if (fn->verbosity >=0) {
    Int old = CLG_(clo).verbose;
    CLG_(clo).verbose = fn->verbosity;
    fn->verbosity = old;
    VG_(message)(Vg_DebugMsg, 
		 "Entering %s: Verbosity set to %d\n",
		 fn->name, CLG_(clo).verbose);
  }
#endif		
	    
  if (fn->dump_before) {
    Char trigger[FN_NAME_LEN];
    VG_(sprintf)(trigger, "--dump-before=%s", fn->name);
    CLG_(dump_profile)(trigger, True);
  }
  else if (fn->zero_before) {
    CLG_(zero_all_cost)(True);
  }

  if (fn->toggle_collect) {
    CLG_(current_state).collect = !CLG_(current_state).collect;
    CLG_DEBUG(2,"   entering %s: toggled collection state to %s\n",
	     fn->name,
	     CLG_(current_state).collect ? "ON" : "OFF");
  }
}	

/* Called when function left (no recursive level active) */
static void function_left(fn_node* fn)
{
  CLG_ASSERT(fn != 0);

  if (fn->dump_after) {
    Char trigger[FN_NAME_LEN];
    VG_(sprintf)(trigger, "--dump-after=%s", fn->name);
    CLG_(dump_profile)(trigger, True);
  }
  if (fn->toggle_collect) {
    CLG_(current_state).collect = !CLG_(current_state).collect;
    CLG_DEBUG(2,"   leaving %s: toggled collection state to %s\n",
	     fn->name,
	     CLG_(current_state).collect ? "ON" : "OFF");
  }

#if CLG_ENABLE_DEBUG
  if (fn->verbosity >=0) {
    Int old = CLG_(clo).verbose;
    CLG_(clo).verbose = fn->verbosity;
    fn->verbosity = old;
    VG_(message)(Vg_DebugMsg, 
		 "Leaving %s: Verbosity set back to %d\n",
		 fn->name, CLG_(clo).verbose);
  }
#endif		
}


/* Push call on call stack.
 *
 * Increment the usage count for the function called.
 * A jump from <from> to <to>, with <sp>.
 * If <skip> is true, this is a call to a function to be skipped;
 * for this, we set jcc = 0.
 */
void CLG_(push_call_stack)(BBCC* from, UInt jmp, BBCC* to, Addr sp, Bool skip)
{
    jCC* jcc;
    UInt* pdepth;
    call_entry* current_entry;
    Addr ret_addr;

    /* Ensure a call stack of size <current_sp>+1.
     * The +1 is needed as push_cxt will store the
     * context at [current_sp]
     */
    ensure_stack_size(CLG_(current_call_stack).sp +1);
    current_entry = &(CLG_(current_call_stack).entry[CLG_(current_call_stack).sp]);

    if (skip) {
	jcc = 0;
    }
    else {
	fn_node* to_fn = to->cxt->fn[0];

	if (CLG_(current_state).nonskipped) {
	    /* this is a jmp from skipped to nonskipped */
	    CLG_ASSERT(CLG_(current_state).nonskipped == from);
	}

	/* As push_cxt() has to be called before push_call_stack if not
	 * skipping, the old context should already be saved on the stack */
	CLG_ASSERT(current_entry->cxt != 0);
	CLG_(copy_cost_lz)( CLG_(sets).full, &(current_entry->enter_cost),
			   CLG_(current_state).cost );

	jcc = CLG_(get_jcc)(from, jmp, to);
	CLG_ASSERT(jcc != 0);

	pdepth = CLG_(get_fn_entry)(to_fn->number);
	if (CLG_(clo).skip_direct_recursion) {
	    /* only increment depth if another function is called */
	  if (jcc->from->cxt->fn[0] != to_fn) (*pdepth)++;
	}
	else (*pdepth)++;

	if (*pdepth>1)
	  CLG_(stat).rec_call_counter++;
	
	jcc->call_counter++;
	CLG_(stat).call_counter++;

	if (*pdepth == 1) function_entered(to_fn);
    }

    /* return address is only is useful with a real call;
     * used to detect RET w/o CALL */
    ret_addr = (from->bb->jmpkind == Ijk_Call) ?
	bb_addr(from->bb) + from->bb->instr_len : 0;

    /* put jcc on call stack */
    current_entry->jcc = jcc;
    current_entry->sp = sp;
    current_entry->ret_addr = ret_addr;
    current_entry->nonskipped = CLG_(current_state).nonskipped;

    CLG_(current_call_stack).sp++;

    /* To allow for above assertion we set context of next frame to 0 */
    CLG_ASSERT(CLG_(current_call_stack).sp < CLG_(current_call_stack).size);
    current_entry++;
    current_entry->cxt = 0;

    if (!skip)
	CLG_(current_state).nonskipped = 0;
    else if (!CLG_(current_state).nonskipped) {
	/* a call from nonskipped to skipped */
	CLG_(current_state).nonskipped = from;
	if (!CLG_(current_state).nonskipped->skipped) {
	  CLG_(init_cost_lz)( CLG_(sets).full,
			     &CLG_(current_state).nonskipped->skipped);
	  CLG_(stat).distinct_skips++;
	}
    }

#if CLG_ENABLE_DEBUG
    CLG_DEBUGIF(0) {
	if (CLG_(clo).verbose<2) {
	  if (jcc && jcc->to && jcc->to->bb) {
	    char spaces[][41] = { "   .   .   .   .   .   .   .   .   .   .",
				  "  .   .   .   .   .   .   .   .   .   . ",
				  " .   .   .   .   .   .   .   .   .   .  ",
				  ".   .   .   .   .   .   .   .   .   .   " };

	    int s = CLG_(current_call_stack).sp;
	    Int* pars = (Int*) sp;

	    BB* bb = jcc->to->bb;
	    if (s>40) s=40;
	    VG_(printf)("%s> %s(0x%x, 0x%x, ...) [%s / %#lx]\n", spaces[s%4]+40-s, bb->fn->name,
                        pars ? pars[1]:0,
			pars ? pars[2]:0,
			bb->obj->name + bb->obj->last_slash_pos,
			bb->offset);
	  }
	}
	else if (CLG_(clo).verbose<4) {
	    VG_(printf)("+ %2d ", CLG_(current_call_stack).sp);
	    CLG_(print_short_jcc)(jcc);
	    VG_(printf)(", SP %#lx, RA %#lx\n", sp, ret_addr);
	}
	else {
	    VG_(printf)("  Pushed ");
	    CLG_(print_stackentry)(3, CLG_(current_call_stack).sp-1);
	}
    }
#endif

}


/* Pop call stack and update inclusive sums.
 * Returns modified fcc.
 *
 * If the JCC becomes inactive, call entries are freed if possible
 */
void CLG_(pop_call_stack)()
{
    jCC* jcc;
    Int depth = 0;
    call_entry* lower_entry;

    if (CLG_(current_state).sig >0) {
	/* Check if we leave a signal handler; this can happen when
	 * calling longjmp() in the handler */
	CLG_(run_post_signal_on_call_stack_bottom)();
    }

    lower_entry =
	&(CLG_(current_call_stack).entry[CLG_(current_call_stack).sp-1]);

    CLG_DEBUG(4,"+ pop_call_stack: frame %d, jcc %p\n", 
		CLG_(current_call_stack).sp, lower_entry->jcc);

    /* jCC item not any more on real stack: pop */
    jcc = lower_entry->jcc;
    CLG_(current_state).nonskipped = lower_entry->nonskipped;

    if (jcc) {
	fn_node* to_fn  = jcc->to->cxt->fn[0];
	UInt* pdepth =  CLG_(get_fn_entry)(to_fn->number);
	if (CLG_(clo).skip_direct_recursion) {
	    /* only decrement depth if another function was called */
	  if (jcc->from->cxt->fn[0] != to_fn) (*pdepth)--;
	}
	else (*pdepth)--;
	depth = *pdepth;

	/* add cost difference to sum */
	if ( CLG_(add_diff_cost_lz)( CLG_(sets).full, &(jcc->cost),
				    lower_entry->enter_cost,
				    CLG_(current_state).cost) ) {
	    
	  /* only count this call if it attributed some cost.
	   * the ret_counter is used to check if a BBCC dump is needed.
	   */
	  jcc->from->ret_counter++;
	}
	CLG_(stat).ret_counter++;

	/* restore context */
	CLG_(current_state).cxt  = lower_entry->cxt;
	CLG_(current_fn_stack).top =
	  CLG_(current_fn_stack).bottom + lower_entry->fn_sp;
	CLG_ASSERT(CLG_(current_state).cxt != 0);

	if (depth == 0) function_left(to_fn);
    }

    /* To allow for an assertion in push_call_stack() */
    lower_entry->cxt = 0;

    CLG_(current_call_stack).sp--;

#if CLG_ENABLE_DEBUG
    CLG_DEBUGIF(1) {
	if (CLG_(clo).verbose<4) {
	    if (jcc) {
		/* popped JCC target first */
		VG_(printf)("- %2d %#lx => ",
			    CLG_(current_call_stack).sp,
			    bb_addr(jcc->to->bb));
		CLG_(print_addr)(bb_jmpaddr(jcc->from->bb));
		VG_(printf)(", SP %#lx\n",
			    CLG_(current_call_stack).entry[CLG_(current_call_stack).sp].sp);
		CLG_(print_cost)(10, CLG_(sets).full, jcc->cost);
	    }
	    else
		VG_(printf)("- %2d [Skipped JCC], SP %#lx\n",
			    CLG_(current_call_stack).sp,
			    CLG_(current_call_stack).entry[CLG_(current_call_stack).sp].sp);
	}
	else {
	    VG_(printf)("  Popped ");
	    CLG_(print_stackentry)(7, CLG_(current_call_stack).sp);
	    if (jcc) {
		VG_(printf)("       returned to ");
		CLG_(print_addr_ln)(bb_jmpaddr(jcc->from->bb));
	    }
	}
    }
#endif

}


/* Unwind enough CallStack items to sync with current stack pointer.
 * Returns the number of stack frames unwinded.
 */
Int CLG_(unwind_call_stack)(Addr sp, Int minpops)
{
    Int csp;
    Int unwind_count = 0;
    CLG_DEBUG(4,"+ unwind_call_stack(sp %#lx, minpops %d): frame %d\n",
	      sp, minpops, CLG_(current_call_stack).sp);

    /* We pop old stack frames.
     * For a call, be p the stack address with return address.
     *  - call_stack_esp[] has SP after the CALL: p-4
     *  - current sp is after a RET: >= p
     */
    
    while( (csp=CLG_(current_call_stack).sp) >0) {
	call_entry* top_ce = &(CLG_(current_call_stack).entry[csp-1]);

	if ((top_ce->sp < sp) ||
	    ((top_ce->sp == sp) && minpops>0)) {

	    minpops--;
	    unwind_count++;
	    CLG_(pop_call_stack)();
	    csp=CLG_(current_call_stack).sp;
	    continue;
	}
	break;
    }

    CLG_DEBUG(4,"- unwind_call_stack\n");
    return unwind_count;
}
