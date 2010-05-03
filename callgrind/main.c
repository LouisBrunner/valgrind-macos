
/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                       main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call graph
   profiling programs.

   Copyright (C) 2002-2010, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

   This tool is derived from and contains code from Cachegrind
   Copyright (C) 2002-2010 Nicholas Nethercote (njn@valgrind.org)

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

#include "config.h"
#include "callgrind.h"
#include "global.h"

#include <pub_tool_threadstate.h>

/*------------------------------------------------------------*/
/*--- Global variables                                     ---*/
/*------------------------------------------------------------*/

/* for all threads */
CommandLineOptions CLG_(clo);
Statistics CLG_(stat);
Bool CLG_(instrument_state) = True; /* Instrumentation on ? */

/* thread and signal handler specific */
exec_state CLG_(current_state);


/*------------------------------------------------------------*/
/*--- Statistics                                           ---*/
/*------------------------------------------------------------*/

static void CLG_(init_statistics)(Statistics* s)
{
  s->call_counter        = 0;
  s->jcnd_counter        = 0;
  s->jump_counter        = 0;
  s->rec_call_counter    = 0;
  s->ret_counter         = 0;
  s->bb_executions       = 0;

  s->context_counter     = 0;
  s->bb_retranslations   = 0;

  s->distinct_objs       = 0;
  s->distinct_files      = 0;
  s->distinct_fns        = 0;
  s->distinct_contexts   = 0;
  s->distinct_bbs        = 0;
  s->distinct_bbccs      = 0;
  s->distinct_instrs     = 0;
  s->distinct_skips      = 0;

  s->bb_hash_resizes     = 0;
  s->bbcc_hash_resizes   = 0;
  s->jcc_hash_resizes    = 0;
  s->cxt_hash_resizes    = 0;
  s->fn_array_resizes    = 0;
  s->call_stack_resizes  = 0;
  s->fn_stack_resizes    = 0;

  s->full_debug_BBs      = 0;
  s->file_line_debug_BBs = 0;
  s->fn_name_debug_BBs   = 0;
  s->no_debug_BBs        = 0;
  s->bbcc_lru_misses     = 0;
  s->jcc_lru_misses      = 0;
  s->cxt_lru_misses      = 0;
  s->bbcc_clones         = 0;
}


/*------------------------------------------------------------*/
/*--- Instrumentation structures and event queue handling  ---*/
/*------------------------------------------------------------*/

/* Maintain an ordered list of memory events which are outstanding, in
   the sense that no IR has yet been generated to do the relevant
   helper calls.  The BB is scanned top to bottom and memory events
   are added to the end of the list, merging with the most recent
   notified event where possible (Dw immediately following Dr and
   having the same size and EA can be merged).

   This merging is done so that for architectures which have
   load-op-store instructions (x86, amd64), the insn is treated as if
   it makes just one memory reference (a modify), rather than two (a
   read followed by a write at the same address).

   At various points the list will need to be flushed, that is, IR
   generated from it.  That must happen before any possible exit from
   the block (the end, or an IRStmt_Exit).  Flushing also takes place
   when there is no space to add a new event.

   If we require the simulation statistics to be up to date with
   respect to possible memory exceptions, then the list would have to
   be flushed before each memory reference.  That would however lose
   performance by inhibiting event-merging during flushing.

   Flushing the list consists of walking it start to end and emitting
   instrumentation IR for each event, in the order in which they
   appear.  It may be possible to emit a single call for two adjacent
   events in order to reduce the number of helper function calls made.
   For example, it could well be profitable to handle two adjacent Ir
   events with a single helper call.  */

typedef
   IRExpr
   IRAtom;

typedef
   enum {
      Ev_Ir,  // Instruction read
      Ev_Dr,  // Data read
      Ev_Dw,  // Data write
      Ev_Dm,  // Data modify (read then write)
   }
   EventTag;

typedef
   struct {
      EventTag   tag;
      InstrInfo* inode;
      union {
	 struct {
	 } Ir;
	 struct {
	    IRAtom* ea;
	    Int     szB;
	 } Dr;
	 struct {
	    IRAtom* ea;
	    Int     szB;
	 } Dw;
	 struct {
	    IRAtom* ea;
	    Int     szB;
	 } Dm;
      } Ev;
   }
   Event;

static void init_Event ( Event* ev ) {
   VG_(memset)(ev, 0, sizeof(Event));
}

static IRAtom* get_Event_dea ( Event* ev ) {
   switch (ev->tag) {
      case Ev_Dr: return ev->Ev.Dr.ea;
      case Ev_Dw: return ev->Ev.Dw.ea;
      case Ev_Dm: return ev->Ev.Dm.ea;
      default:    tl_assert(0);
   }
}

static Int get_Event_dszB ( Event* ev ) {
   switch (ev->tag) {
      case Ev_Dr: return ev->Ev.Dr.szB;
      case Ev_Dw: return ev->Ev.Dw.szB;
      case Ev_Dm: return ev->Ev.Dm.szB;
      default:    tl_assert(0);
   }
}


/* Up to this many unnotified events are allowed.  Number is
   arbitrary.  Larger numbers allow more event merging to occur, but
   potentially induce more spilling due to extending live ranges of
   address temporaries. */
#define N_EVENTS 16


/* A struct which holds all the running state during instrumentation.
   Mostly to avoid passing loads of parameters everywhere. */
typedef struct {
    /* The current outstanding-memory-event list. */
    Event events[N_EVENTS];
    Int   events_used;

    /* The array of InstrInfo's is part of BB struct. */
    BB* bb;

    /* BB seen before (ie. re-instrumentation) */
    Bool seen_before;

    /* Number InstrInfo bins 'used' so far. */
    UInt ii_index;

    // current offset of guest instructions from BB start
    UInt instr_offset;

    /* The output SB being constructed. */
    IRSB* sbOut;
} ClgState;


static void showEvent ( Event* ev )
{
   switch (ev->tag) {
      case Ev_Ir:
	 VG_(printf)("Ir (InstrInfo %p) at +%d\n",
		     ev->inode, ev->inode->instr_offset);
	 break;
      case Ev_Dr:
	 VG_(printf)("Dr (InstrInfo %p) at +%d %d EA=",
		     ev->inode, ev->inode->instr_offset, ev->Ev.Dr.szB);
	 ppIRExpr(ev->Ev.Dr.ea);
	 VG_(printf)("\n");
	 break;
      case Ev_Dw:
	 VG_(printf)("Dw (InstrInfo %p) at +%d %d EA=",
		     ev->inode, ev->inode->instr_offset, ev->Ev.Dw.szB);
	 ppIRExpr(ev->Ev.Dw.ea);
	 VG_(printf)("\n");
	 break;
      case Ev_Dm:
	 VG_(printf)("Dm (InstrInfo %p) at +%d %d EA=",
		     ev->inode, ev->inode->instr_offset, ev->Ev.Dm.szB);
	 ppIRExpr(ev->Ev.Dm.ea);
	 VG_(printf)("\n");
	 break;
      default:
	 tl_assert(0);
	 break;
   }
}

/* Generate code for all outstanding memory events, and mark the queue
   empty.  Code is generated into cgs->sbOut, and this activity
   'consumes' slots in cgs->bb. */

static void flushEvents ( ClgState* clgs )
{
   Int        i, regparms, inew;
   Char*      helperName;
   void*      helperAddr;
   IRExpr**   argv;
   IRExpr*    i_node_expr;
   IRDirty*   di;
   Event*     ev;
   Event*     ev2;
   Event*     ev3;

   if (!clgs->seen_before) {
       // extend event sets as needed
       // available sets: D0 Dr
       for(i=0; i<clgs->events_used; i++) {
	   ev  = &clgs->events[i];
	   switch(ev->tag) {
	   case Ev_Ir:
	       // Ir event always is first for a guest instruction
	       CLG_ASSERT(ev->inode->eventset == 0);
	       ev->inode->eventset = CLG_(sets).UIr;
	       break;
	   case Ev_Dr:
	       // extend event set by Dr counter
	       if ((ev->inode->eventset == CLG_(sets).UIrDr)   ||
		   (ev->inode->eventset == CLG_(sets).UIrDrDw) ||
		   (ev->inode->eventset == CLG_(sets).UIrDwDr))
		   break;
	       if (ev->inode->eventset == CLG_(sets).UIrDw) {
		   ev->inode->eventset = CLG_(sets).UIrDwDr;
		   break;
	       }
	       CLG_ASSERT(ev->inode->eventset == CLG_(sets).UIr);
	       ev->inode->eventset = CLG_(sets).UIrDr;
	       break;
	   case Ev_Dw:
	   case Ev_Dm:
	       // extend event set by Dw counter
	       if ((ev->inode->eventset == CLG_(sets).UIrDw)   ||
		   (ev->inode->eventset == CLG_(sets).UIrDwDr) ||
		   (ev->inode->eventset == CLG_(sets).UIrDrDw))
		   break;
	       if (ev->inode->eventset == CLG_(sets).UIrDr) {
		   ev->inode->eventset = CLG_(sets).UIrDrDw;
		   break;
	       }
	       CLG_ASSERT(ev->inode->eventset == CLG_(sets).UIr);
	       ev->inode->eventset = CLG_(sets).UIrDw;
	       break;
	   default:
	       tl_assert(0);
	   }
       }
   }

   for(i = 0; i < clgs->events_used; i = inew) {

      helperName = NULL;
      helperAddr = NULL;
      argv       = NULL;
      regparms   = 0;

      /* generate IR to notify event i and possibly the ones
	 immediately following it. */
      tl_assert(i >= 0 && i < clgs->events_used);

      ev  = &clgs->events[i];
      ev2 = ( i < clgs->events_used-1 ? &clgs->events[i+1] : NULL );
      ev3 = ( i < clgs->events_used-2 ? &clgs->events[i+2] : NULL );

      CLG_DEBUGIF(5) {
	 VG_(printf)("   flush ");
	 showEvent( ev );
      }

      i_node_expr = mkIRExpr_HWord( (HWord)ev->inode );

      /* Decide on helper fn to call and args to pass it, and advance
	 i appropriately.
	 Dm events have same effect as Dw events */
      switch (ev->tag) {
	 case Ev_Ir:
	    /* Merge an Ir with a following Dr. */
	    if (ev2 && ev2->tag == Ev_Dr) {
	       /* Why is this true?  It's because we're merging an Ir
		  with a following Dr.  The Ir derives from the
		  instruction's IMark and the Dr from data
		  references which follow it.  In short it holds
		  because each insn starts with an IMark, hence an
		  Ev_Ir, and so these Dr must pertain to the
		  immediately preceding Ir.  Same applies to analogous
		  assertions in the subsequent cases. */
	       tl_assert(ev2->inode == ev->inode);
	       helperName = CLG_(cachesim).log_1I1Dr_name;
	       helperAddr = CLG_(cachesim).log_1I1Dr;
	       argv = mkIRExprVec_3( i_node_expr,
				     get_Event_dea(ev2),
				     mkIRExpr_HWord( get_Event_dszB(ev2) ) );
	       regparms = 3;
	       inew = i+2;
	    }
	    /* Merge an Ir with a following Dw/Dm. */
	    else
	    if (ev2 && (ev2->tag == Ev_Dw || ev2->tag == Ev_Dm)) {
	       tl_assert(ev2->inode == ev->inode);
	       helperName = CLG_(cachesim).log_1I1Dw_name;
	       helperAddr = CLG_(cachesim).log_1I1Dw;
	       argv = mkIRExprVec_3( i_node_expr,
				     get_Event_dea(ev2),
				     mkIRExpr_HWord( get_Event_dszB(ev2) ) );
	       regparms = 3;
	       inew = i+2;
	    }
	    /* Merge an Ir with two following Irs. */
	    else
	    if (ev2 && ev3 && ev2->tag == Ev_Ir && ev3->tag == Ev_Ir) {
	       helperName = CLG_(cachesim).log_3I0D_name;
	       helperAddr = CLG_(cachesim).log_3I0D;
	       argv = mkIRExprVec_3( i_node_expr,
				     mkIRExpr_HWord( (HWord)ev2->inode ),
				     mkIRExpr_HWord( (HWord)ev3->inode ) );
	       regparms = 3;
	       inew = i+3;
	    }
	    /* Merge an Ir with one following Ir. */
	    else
	    if (ev2 && ev2->tag == Ev_Ir) {
	       helperName = CLG_(cachesim).log_2I0D_name;
	       helperAddr = CLG_(cachesim).log_2I0D;
	       argv = mkIRExprVec_2( i_node_expr,
				     mkIRExpr_HWord( (HWord)ev2->inode ) );
	       regparms = 2;
	       inew = i+2;
	    }
	    /* No merging possible; emit as-is. */
	    else {
	       helperName = CLG_(cachesim).log_1I0D_name;
	       helperAddr = CLG_(cachesim).log_1I0D;
	       argv = mkIRExprVec_1( i_node_expr );
	       regparms = 1;
	       inew = i+1;
	    }
	    break;
	 case Ev_Dr:
	    /* Data read or modify */
	    helperName = CLG_(cachesim).log_0I1Dr_name;
	    helperAddr = CLG_(cachesim).log_0I1Dr;
	    argv = mkIRExprVec_3( i_node_expr,
				  get_Event_dea(ev),
				  mkIRExpr_HWord( get_Event_dszB(ev) ) );
	    regparms = 3;
	    inew = i+1;
	    break;
	 case Ev_Dw:
	 case Ev_Dm:
	    /* Data write */
	    helperName = CLG_(cachesim).log_0I1Dw_name;
	    helperAddr = CLG_(cachesim).log_0I1Dw;
	    argv = mkIRExprVec_3( i_node_expr,
				  get_Event_dea(ev),
				  mkIRExpr_HWord( get_Event_dszB(ev) ) );
	    regparms = 3;
	    inew = i+1;
	    break;
	 default:
	    tl_assert(0);
      }

      CLG_DEBUGIF(5) {
	  if (inew > i+1) {
	      VG_(printf)("   merge ");
	      showEvent( ev2 );
	  }
	  if (inew > i+2) {
	      VG_(printf)("   merge ");
	      showEvent( ev3 );
	  }
	  if (helperAddr)
	      VG_(printf)("   call  %s (%p)\n",
			  helperName, helperAddr);
      }

      /* helper could be unset depending on the simulator used */
      if (helperAddr == 0) continue;

      /* Add the helper. */
      tl_assert(helperName);
      tl_assert(helperAddr);
      tl_assert(argv);
      di = unsafeIRDirty_0_N( regparms,
			      helperName, VG_(fnptr_to_fnentry)( helperAddr ),
			      argv );
      addStmtToIRSB( clgs->sbOut, IRStmt_Dirty(di) );
   }

   clgs->events_used = 0;
}

static void addEvent_Ir ( ClgState* clgs, InstrInfo* inode )
{
   Event* evt;
   tl_assert(clgs->seen_before || (inode->eventset == 0));
   if (!CLG_(clo).simulate_cache) return;

   if (clgs->events_used == N_EVENTS)
      flushEvents(clgs);
   tl_assert(clgs->events_used >= 0 && clgs->events_used < N_EVENTS);
   evt = &clgs->events[clgs->events_used];
   init_Event(evt);
   evt->tag      = Ev_Ir;
   evt->inode    = inode;
   clgs->events_used++;
}

static
void addEvent_Dr ( ClgState* clgs, InstrInfo* inode, Int datasize, IRAtom* ea )
{
   Event* evt;
   tl_assert(isIRAtom(ea));
   tl_assert(datasize >= 1 && datasize <= MIN_LINE_SIZE);
   if (!CLG_(clo).simulate_cache) return;

   if (clgs->events_used == N_EVENTS)
      flushEvents(clgs);
   tl_assert(clgs->events_used >= 0 && clgs->events_used < N_EVENTS);
   evt = &clgs->events[clgs->events_used];
   init_Event(evt);
   evt->tag       = Ev_Dr;
   evt->inode     = inode;
   evt->Ev.Dr.szB = datasize;
   evt->Ev.Dr.ea  = ea;
   clgs->events_used++;
}

static
void addEvent_Dw ( ClgState* clgs, InstrInfo* inode, Int datasize, IRAtom* ea )
{
   Event* lastEvt;
   Event* evt;
   tl_assert(isIRAtom(ea));
   tl_assert(datasize >= 1 && datasize <= MIN_LINE_SIZE);
   if (!CLG_(clo).simulate_cache) return;

   /* Is it possible to merge this write with the preceding read? */
   lastEvt = &clgs->events[clgs->events_used-1];
   if (clgs->events_used > 0
       && lastEvt->tag       == Ev_Dr
       && lastEvt->Ev.Dr.szB == datasize
       && lastEvt->inode     == inode
       && eqIRAtom(lastEvt->Ev.Dr.ea, ea))
   {
      lastEvt->tag   = Ev_Dm;
      return;
   }

   /* No.  Add as normal. */
   if (clgs->events_used == N_EVENTS)
      flushEvents(clgs);
   tl_assert(clgs->events_used >= 0 && clgs->events_used < N_EVENTS);
   evt = &clgs->events[clgs->events_used];
   init_Event(evt);
   evt->tag       = Ev_Dw;
   evt->inode     = inode;
   evt->Ev.Dw.szB = datasize;
   evt->Ev.Dw.ea  = ea;
   clgs->events_used++;
}

/* Initialise or check (if already seen before) an InstrInfo for next insn.
   We only can set instr_offset/instr_size here. The required event set and
   resulting cost offset depend on events (Ir/Dr/Dw/Dm) in guest
   instructions. The event set is extended as required on flush of the event
   queue (when Dm events were determined), cost offsets are determined at
   end of BB instrumentation. */
static
InstrInfo* next_InstrInfo ( ClgState* clgs, UInt instr_size )
{
   InstrInfo* ii;
   tl_assert(clgs->ii_index >= 0);
   tl_assert(clgs->ii_index < clgs->bb->instr_count);
   ii = &clgs->bb->instr[ clgs->ii_index ];

   if (clgs->seen_before) {
       CLG_ASSERT(ii->instr_offset == clgs->instr_offset);
       CLG_ASSERT(ii->instr_size == instr_size);
   }
   else {
       ii->instr_offset = clgs->instr_offset;
       ii->instr_size = instr_size;
       ii->cost_offset = 0;
       ii->eventset = 0;
   }

   clgs->ii_index++;
   clgs->instr_offset += instr_size;
   CLG_(stat).distinct_instrs++;

   return ii;
}

// return total number of cost values needed for this BB
static
UInt update_cost_offsets( ClgState* clgs )
{
    Int i;
    InstrInfo* ii;
    UInt cost_offset = 0;

    CLG_ASSERT(clgs->bb->instr_count == clgs->ii_index);
    for(i=0; i<clgs->ii_index; i++) {
	ii = &clgs->bb->instr[i];
	if (clgs->seen_before) {
	    CLG_ASSERT(ii->cost_offset == cost_offset);
	} else
	    ii->cost_offset = cost_offset;
	cost_offset += ii->eventset ? ii->eventset->size : 0;
    }

    return cost_offset;
}

/*------------------------------------------------------------*/
/*--- Instrumentation                                      ---*/
/*------------------------------------------------------------*/

#if defined(VG_BIGENDIAN)
# define CLGEndness Iend_BE
#elif defined(VG_LITTLEENDIAN)
# define CLGEndness Iend_LE
#else
# error "Unknown endianness"
#endif

static
Addr IRConst2Addr(IRConst* con)
{
    Addr addr;

    if (sizeof(Addr) == 4) {
	CLG_ASSERT( con->tag == Ico_U32 );
	addr = con->Ico.U32;
    }
    else if (sizeof(Addr) == 8) {
	CLG_ASSERT( con->tag == Ico_U64 );
	addr = con->Ico.U64;
    }
    else
	VG_(tool_panic)("Callgrind: invalid Addr type");

    return addr;
}

/* First pass over a BB to instrument, counting instructions and jumps
 * This is needed for the size of the BB struct to allocate
 *
 * Called from CLG_(get_bb)
 */
void CLG_(collectBlockInfo)(IRSB* sbIn,
			    /*INOUT*/ UInt* instrs,
			    /*INOUT*/ UInt* cjmps,
			    /*INOUT*/ Bool* cjmp_inverted)
{
    Int i;
    IRStmt* st;
    Addr instrAddr =0, jumpDst;
    UInt instrLen = 0;
    Bool toNextInstr = False;

    // Ist_Exit has to be ignored in preamble code, before first IMark:
    // preamble code is added by VEX for self modifying code, and has
    // nothing to do with client code
    Bool inPreamble = True;

    if (!sbIn) return;

    for (i = 0; i < sbIn->stmts_used; i++) {
	  st = sbIn->stmts[i];
	  if (Ist_IMark == st->tag) {
	      inPreamble = False;

	      instrAddr = (Addr)ULong_to_Ptr(st->Ist.IMark.addr);
	      instrLen  = st->Ist.IMark.len;

	      (*instrs)++;
	      toNextInstr = False;
	  }
	  if (inPreamble) continue;
	  if (Ist_Exit == st->tag) {
	      jumpDst = IRConst2Addr(st->Ist.Exit.dst);
	      toNextInstr =  (jumpDst == instrAddr + instrLen);

	      (*cjmps)++;
	  }
    }

    /* if the last instructions of BB conditionally jumps to next instruction
     * (= first instruction of next BB in memory), this is a inverted by VEX.
     */
    *cjmp_inverted = toNextInstr;
}

static
void addConstMemStoreStmt( IRSB* bbOut, UWord addr, UInt val, IRType hWordTy)
{
    addStmtToIRSB( bbOut,
		   IRStmt_Store(CLGEndness,
				IRExpr_Const(hWordTy == Ity_I32 ?
					     IRConst_U32( addr ) :
					     IRConst_U64( addr )),
				IRExpr_Const(IRConst_U32(val)) ));
}   


/* add helper call to setup_bbcc, with pointer to BB struct as argument
 *
 * precondition for setup_bbcc:
 * - jmps_passed has number of cond.jumps passed in last executed BB
 * - current_bbcc has a pointer to the BBCC of the last executed BB
 *   Thus, if bbcc_jmpkind is != -1 (JmpNone),
 *     current_bbcc->bb->jmp_addr
 *   gives the address of the jump source.
 *
 * the setup does 2 things:
 * - trace call:
 *   * Unwind own call stack, i.e sync our ESP with real ESP
 *     This is for ESP manipulation (longjmps, C++ exec handling) and RET
 *   * For CALLs or JMPs crossing objects, record call arg +
 *     push are on own call stack
 *
 * - prepare for cache log functions:
 *   set current_bbcc to BBCC that gets the costs for this BB execution
 *   attached
 */
static
void addBBSetupCall(ClgState* clgs)
{
   IRDirty* di;
   IRExpr  *arg1, **argv;

   arg1 = mkIRExpr_HWord( (HWord)clgs->bb );
   argv = mkIRExprVec_1(arg1);
   di = unsafeIRDirty_0_N( 1, "setup_bbcc",
			      VG_(fnptr_to_fnentry)( & CLG_(setup_bbcc) ),
			      argv);
   addStmtToIRSB( clgs->sbOut, IRStmt_Dirty(di) );
}


static
IRSB* CLG_(instrument)( VgCallbackClosure* closure,
			IRSB* sbIn,
			VexGuestLayout* layout,
			VexGuestExtents* vge,
			IRType gWordTy, IRType hWordTy )
{
   Int      i, isize;
   IRStmt*  st;
   Addr     origAddr;
   InstrInfo* curr_inode = NULL;
   ClgState clgs;
   UInt     cJumps = 0;


   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   // No instrumentation if it is switched off
   if (! CLG_(instrument_state)) {
       CLG_DEBUG(5, "instrument(BB %#lx) [Instrumentation OFF]\n",
		 (Addr)closure->readdr);
       return sbIn;
   }

   CLG_DEBUG(3, "+ instrument(BB %#lx)\n", (Addr)closure->readdr);

   /* Set up SB for instrumented IR */
   clgs.sbOut = deepCopyIRSBExceptStmts(sbIn);

   // Copy verbatim any IR preamble preceding the first IMark
   i = 0;
   while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
      addStmtToIRSB( clgs.sbOut, sbIn->stmts[i] );
      i++;
   }

   // Get the first statement, and origAddr from it
   CLG_ASSERT(sbIn->stmts_used >0);
   CLG_ASSERT(i < sbIn->stmts_used);
   st = sbIn->stmts[i];
   CLG_ASSERT(Ist_IMark == st->tag);

   origAddr = (Addr)st->Ist.IMark.addr;
   CLG_ASSERT(origAddr == st->Ist.IMark.addr);  // XXX: check no overflow

   /* Get BB struct (creating if necessary).
    * JS: The hash table is keyed with orig_addr_noredir -- important!
    * JW: Why? If it is because of different chasing of the redirection,
    *     this is not needed, as chasing is switched off in callgrind
    */
   clgs.bb = CLG_(get_bb)(origAddr, sbIn, &(clgs.seen_before));

   addBBSetupCall(&clgs);

   // Set up running state
   clgs.events_used = 0;
   clgs.ii_index = 0;
   clgs.instr_offset = 0;

   for (/*use current i*/; i < sbIn->stmts_used; i++) {

      st = sbIn->stmts[i];
      CLG_ASSERT(isFlatIRStmt(st));

      switch (st->tag) {
	 case Ist_NoOp:
	 case Ist_AbiHint:
	 case Ist_Put:
	 case Ist_PutI:
	 case Ist_MBE:
	    break;

	 case Ist_IMark: {
	    CLG_ASSERT(clgs.instr_offset == (Addr)st->Ist.IMark.addr - origAddr);
	    isize = st->Ist.IMark.len;
	    // If Vex fails to decode an instruction, the size will be zero.
	    // Pretend otherwise.
	    if (isize == 0) isize = VG_MIN_INSTR_SZB;

	    // Sanity-check size.
	    tl_assert( (VG_MIN_INSTR_SZB <= isize && isize <= VG_MAX_INSTR_SZB)
		     || VG_CLREQ_SZB == isize );

	    // Init the inode, record it as the current one.
	    // Subsequent Dr/Dw/Dm events from the same instruction will
	    // also use it.
	    curr_inode = next_InstrInfo (&clgs, isize);

	    addEvent_Ir( &clgs, curr_inode );
	    break;
	 }

	 case Ist_WrTmp: {
	    IRExpr* data = st->Ist.WrTmp.data;
	    if (data->tag == Iex_Load) {
	       IRExpr* aexpr = data->Iex.Load.addr;
	       // Note also, endianness info is ignored.  I guess
	       // that's not interesting.
	       addEvent_Dr( &clgs, curr_inode,
			    sizeofIRType(data->Iex.Load.ty), aexpr );
	    }
	    break;
	 }

	 case Ist_Store: {
	    IRExpr* data  = st->Ist.Store.data;
	    IRExpr* aexpr = st->Ist.Store.addr;
	    addEvent_Dw( &clgs, curr_inode,
			 sizeofIRType(typeOfIRExpr(sbIn->tyenv, data)), aexpr );
	    break;
	 }

	 case Ist_Dirty: {
	    Int      dataSize;
	    IRDirty* d = st->Ist.Dirty.details;
	    if (d->mFx != Ifx_None) {
	       /* This dirty helper accesses memory.  Collect the details. */
	       tl_assert(d->mAddr != NULL);
	       tl_assert(d->mSize != 0);
	       dataSize = d->mSize;
	       // Large (eg. 28B, 108B, 512B on x86) data-sized
	       // instructions will be done inaccurately, but they're
	       // very rare and this avoids errors from hitting more
	       // than two cache lines in the simulation.
	       if (dataSize > MIN_LINE_SIZE)
		  dataSize = MIN_LINE_SIZE;
	       if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify)
		  addEvent_Dr( &clgs, curr_inode, dataSize, d->mAddr );
	       if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify)
		  addEvent_Dw( &clgs, curr_inode, dataSize, d->mAddr );
	    } else {
	       tl_assert(d->mAddr == NULL);
	       tl_assert(d->mSize == 0);
	    }
	    break;
	 }

         case Ist_CAS: {
            /* We treat it as a read and a write of the location.  I
               think that is the same behaviour as it was before IRCAS
               was introduced, since prior to that point, the Vex
               front ends would translate a lock-prefixed instruction
               into a (normal) read followed by a (normal) write. */
            Int    dataSize;
            IRCAS* cas = st->Ist.CAS.details;
            CLG_ASSERT(cas->addr && isIRAtom(cas->addr));
            CLG_ASSERT(cas->dataLo);
            dataSize = sizeofIRType(typeOfIRExpr(sbIn->tyenv, cas->dataLo));
            if (cas->dataHi != NULL)
               dataSize *= 2; /* since this is a doubleword-cas */
            addEvent_Dr( &clgs, curr_inode, dataSize, cas->addr );
            addEvent_Dw( &clgs, curr_inode, dataSize, cas->addr );
            break;
         }

         case Ist_LLSC: {
            IRType dataTy;
            if (st->Ist.LLSC.storedata == NULL) {
               /* LL */
               dataTy = typeOfIRTemp(sbIn->tyenv, st->Ist.LLSC.result);
               addEvent_Dr( &clgs, curr_inode,
                            sizeofIRType(dataTy), st->Ist.LLSC.addr );
            } else {
               /* SC */
               dataTy = typeOfIRExpr(sbIn->tyenv, st->Ist.LLSC.storedata);
               addEvent_Dw( &clgs, curr_inode,
                            sizeofIRType(dataTy), st->Ist.LLSC.addr );
            }
            break;
         }

 	 case Ist_Exit: {
	    UInt jmps_passed;

	    /* We may never reach the next statement, so need to flush
	       all outstanding transactions now. */
	    flushEvents( &clgs );

	    CLG_ASSERT(clgs.ii_index>0);
	    if (!clgs.seen_before) {
		clgs.bb->jmp[cJumps].instr = clgs.ii_index-1;
		clgs.bb->jmp[cJumps].skip = False;
	    }

	    /* Update global variable jmps_passed before the jump
	     * A correction is needed if VEX inverted the last jump condition
	    */
	    jmps_passed = cJumps;
	    if ((cJumps+1 == clgs.bb->cjmp_count) && clgs.bb->cjmp_inverted)
		jmps_passed++;
	    addConstMemStoreStmt( clgs.sbOut,
				  (UWord) &CLG_(current_state).jmps_passed,
				  jmps_passed, hWordTy);
	    cJumps++;

	    break;
	 }

	 default:
	    tl_assert(0);
	    break;
      }

      /* Copy the original statement */
      addStmtToIRSB( clgs.sbOut, st );

      CLG_DEBUGIF(5) {
	 VG_(printf)("   pass  ");
	 ppIRStmt(st);
	 VG_(printf)("\n");
      }
   }

   /* At the end of the bb.  Flush outstandings. */
   flushEvents( &clgs );

   /* Always update global variable jmps_passed at end of bb.
    * A correction is needed if VEX inverted the last jump condition
    */
   {
      UInt jmps_passed = cJumps;
      if (clgs.bb->cjmp_inverted) jmps_passed--;
      addConstMemStoreStmt( clgs.sbOut,
			    (UWord) &CLG_(current_state).jmps_passed,
			    jmps_passed, hWordTy);
   }
   CLG_ASSERT(clgs.bb->cjmp_count == cJumps);
   CLG_ASSERT(clgs.bb->instr_count = clgs.ii_index);

   /* This stores the instr of the call/ret at BB end */
   clgs.bb->jmp[cJumps].instr = clgs.ii_index-1;

   if (clgs.seen_before) {
       CLG_ASSERT(clgs.bb->cost_count == update_cost_offsets(&clgs));
       CLG_ASSERT(clgs.bb->instr_len = clgs.instr_offset);
       CLG_ASSERT(clgs.bb->jmpkind == sbIn->jumpkind);
   }
   else {
       clgs.bb->cost_count = update_cost_offsets(&clgs);
       clgs.bb->instr_len = clgs.instr_offset;
       clgs.bb->jmpkind = sbIn->jumpkind;
   }

   CLG_DEBUG(3, "- instrument(BB %#lx): byteLen %u, CJumps %u, CostLen %u\n",
	     origAddr, clgs.bb->instr_len,
	     clgs.bb->cjmp_count, clgs.bb->cost_count);
   if (cJumps>0) {
       CLG_DEBUG(3, "                     [ ");
       for (i=0;i<cJumps;i++)
	   CLG_DEBUG(3, "%d ", clgs.bb->jmp[i].instr);
       CLG_DEBUG(3, "], last inverted: %s \n",
		 clgs.bb->cjmp_inverted ? "yes":"no");
   }

  return clgs.sbOut;
}

/*--------------------------------------------------------------------*/
/*--- Discarding BB info                                           ---*/
/*--------------------------------------------------------------------*/

// Called when a translation is removed from the translation cache for
// any reason at all: to free up space, because the guest code was
// unmapped or modified, or for any arbitrary reason.
static
void clg_discard_superblock_info ( Addr64 orig_addr64, VexGuestExtents vge )
{
    Addr orig_addr = (Addr)orig_addr64;

    tl_assert(vge.n_used > 0);

   if (0)
      VG_(printf)( "discard_superblock_info: %p, %p, %llu\n",
                   (void*)(Addr)orig_addr,
                   (void*)(Addr)vge.base[0], (ULong)vge.len[0]);

   // Get BB info, remove from table, free BB info.  Simple!  Note that we
   // use orig_addr, not the first instruction address in vge.
   CLG_(delete_bb)(orig_addr);
}


/*------------------------------------------------------------*/
/*--- CLG_(fini)() and related function                     ---*/
/*------------------------------------------------------------*/



static void zero_thread_cost(thread_info* t)
{
  Int i;

  for(i = 0; i < CLG_(current_call_stack).sp; i++) {
    if (!CLG_(current_call_stack).entry[i].jcc) continue;

    /* reset call counters to current for active calls */
    CLG_(copy_cost)( CLG_(sets).full, 
		    CLG_(current_call_stack).entry[i].enter_cost,
		    CLG_(current_state).cost );
    CLG_(current_call_stack).entry[i].jcc->call_counter = 0;
  }

  CLG_(forall_bbccs)(CLG_(zero_bbcc));

  /* set counter for last dump */
  CLG_(copy_cost)( CLG_(sets).full, 
		  t->lastdump_cost, CLG_(current_state).cost );
}

void CLG_(zero_all_cost)(Bool only_current_thread)
{
  if (VG_(clo_verbosity) > 1)
    VG_(message)(Vg_DebugMsg, "  Zeroing costs...\n");

  if (only_current_thread)
    zero_thread_cost(CLG_(get_current_thread)());
  else
    CLG_(forall_threads)(zero_thread_cost);

  if (VG_(clo_verbosity) > 1)
    VG_(message)(Vg_DebugMsg, "  ...done\n");
}

static
void unwind_thread(thread_info* t)
{
  /* unwind signal handlers */
  while(CLG_(current_state).sig !=0)
    CLG_(post_signal)(CLG_(current_tid),CLG_(current_state).sig);

  /* unwind regular call stack */
  while(CLG_(current_call_stack).sp>0)
    CLG_(pop_call_stack)();

  /* reset context and function stack for context generation */
  CLG_(init_exec_state)( &CLG_(current_state) );
  CLG_(current_fn_stack).top = CLG_(current_fn_stack).bottom;
}

static
void zero_state_cost(thread_info* t)
{
    CLG_(zero_cost)( CLG_(sets).full, CLG_(current_state).cost );
}

/* Ups, this can go wrong... */
extern void VG_(discard_translations) ( Addr64 start, ULong range );

void CLG_(set_instrument_state)(Char* reason, Bool state)
{
  if (CLG_(instrument_state) == state) {
    CLG_DEBUG(2, "%s: instrumentation already %s\n",
	     reason, state ? "ON" : "OFF");
    return;
  }
  CLG_(instrument_state) = state;
  CLG_DEBUG(2, "%s: Switching instrumentation %s ...\n",
	   reason, state ? "ON" : "OFF");

  VG_(discard_translations)( (Addr64)0x1000, (ULong) ~0xfffl);

  /* reset internal state: call stacks, simulator */
  CLG_(forall_threads)(unwind_thread);
  CLG_(forall_threads)(zero_state_cost);
  (*CLG_(cachesim).clear)();

  if (VG_(clo_verbosity) > 1)
    VG_(message)(Vg_DebugMsg, "%s: instrumentation switched %s\n",
		 reason, state ? "ON" : "OFF");
}
  

static
Bool CLG_(handle_client_request)(ThreadId tid, UWord *args, UWord *ret)
{
   if (!VG_IS_TOOL_USERREQ('C','T',args[0]))
      return False;

   switch(args[0]) {
   case VG_USERREQ__DUMP_STATS:     
      CLG_(dump_profile)("Client Request", True);
      *ret = 0;                 /* meaningless */
      break;

   case VG_USERREQ__DUMP_STATS_AT:
     {
       Char buf[512];
       VG_(sprintf)(buf,"Client Request: %s", (Char*)args[1]);
       CLG_(dump_profile)(buf, True);
       *ret = 0;                 /* meaningless */
     }
     break;

   case VG_USERREQ__ZERO_STATS:
     CLG_(zero_all_cost)(True);
      *ret = 0;                 /* meaningless */
      break;

   case VG_USERREQ__TOGGLE_COLLECT:
     CLG_(current_state).collect = !CLG_(current_state).collect;
     CLG_DEBUG(2, "Client Request: toggled collection state to %s\n",
	      CLG_(current_state).collect ? "ON" : "OFF");
     *ret = 0;                 /* meaningless */
     break;

   case VG_USERREQ__START_INSTRUMENTATION:
     CLG_(set_instrument_state)("Client Request", True);
     *ret = 0;                 /* meaningless */
     break;

   case VG_USERREQ__STOP_INSTRUMENTATION:
     CLG_(set_instrument_state)("Client Request", False);
     *ret = 0;                 /* meaningless */
     break;

   default:
      return False;
   }

   return True;
}


/* Syscall Timing */

/* struct timeval syscalltime[VG_N_THREADS]; */
#if CLG_MICROSYSTIME
#include <sys/time.h>
#include <sys/syscall.h>
extern Int VG_(do_syscall) ( UInt, ... );

ULong syscalltime[VG_N_THREADS];
#else
UInt syscalltime[VG_N_THREADS];
#endif

static
void CLG_(pre_syscalltime)(ThreadId tid, UInt syscallno,
                           UWord* args, UInt nArgs)
{
  if (CLG_(clo).collect_systime) {
#if CLG_MICROSYSTIME
    struct vki_timeval tv_now;
    VG_(do_syscall)(__NR_gettimeofday, (UInt)&tv_now, (UInt)NULL);
    syscalltime[tid] = tv_now.tv_sec * 1000000ULL + tv_now.tv_usec;
#else
    syscalltime[tid] = VG_(read_millisecond_timer)();
#endif
  }
}

static
void CLG_(post_syscalltime)(ThreadId tid, UInt syscallno,
                            UWord* args, UInt nArgs, SysRes res)
{
  if (CLG_(clo).collect_systime &&
      CLG_(current_state).bbcc) {
    Int o = CLG_(sets).off_full_systime;
#if CLG_MICROSYSTIME
    struct vki_timeval tv_now;
    ULong diff;
    
    VG_(do_syscall)(__NR_gettimeofday, (UInt)&tv_now, (UInt)NULL);
    diff = (tv_now.tv_sec * 1000000ULL + tv_now.tv_usec) - syscalltime[tid];
#else
    UInt diff = VG_(read_millisecond_timer)() - syscalltime[tid];
#endif  
    
    CLG_DEBUG(0,"   Time (Off %d) for Syscall %d: %ull\n", o, syscallno, diff);
    
    if (o<0) return;

    CLG_(current_state).cost[o] ++;
    CLG_(current_state).cost[o+1] += diff;
    if (!CLG_(current_state).bbcc->skipped)
      CLG_(init_cost_lz)(CLG_(sets).full,
			&(CLG_(current_state).bbcc->skipped));
    CLG_(current_state).bbcc->skipped[o] ++;
    CLG_(current_state).bbcc->skipped[o+1] += diff;
  }
}

static
void finish(void)
{
  char buf[RESULTS_BUF_LEN];

  CLG_DEBUG(0, "finish()\n");

  (*CLG_(cachesim).finish)();

  /* pop all remaining items from CallStack for correct sum
   */
  CLG_(forall_threads)(unwind_thread);

  CLG_(dump_profile)(0, False);

  CLG_(finish_command)();

  if (VG_(clo_verbosity) == 0) return;
  
  /* Hash table stats */
  if (VG_(clo_stats)) {
    int BB_lookups =
      CLG_(stat).full_debug_BBs +
      CLG_(stat).fn_name_debug_BBs +
      CLG_(stat).file_line_debug_BBs +
      CLG_(stat).no_debug_BBs;
    
    VG_(message)(Vg_DebugMsg, "\n");
    VG_(message)(Vg_DebugMsg, "Distinct objects: %d\n",
		 CLG_(stat).distinct_objs);
    VG_(message)(Vg_DebugMsg, "Distinct files:   %d\n",
		 CLG_(stat).distinct_files);
    VG_(message)(Vg_DebugMsg, "Distinct fns:     %d\n",
		 CLG_(stat).distinct_fns);
    VG_(message)(Vg_DebugMsg, "Distinct contexts:%d\n",
		 CLG_(stat).distinct_contexts);
    VG_(message)(Vg_DebugMsg, "Distinct BBs:     %d\n",
		 CLG_(stat).distinct_bbs);
    VG_(message)(Vg_DebugMsg, "Cost entries:     %d (Chunks %d)\n",
		 CLG_(costarray_entries), CLG_(costarray_chunks));
    VG_(message)(Vg_DebugMsg, "Distinct BBCCs:   %d\n",
		 CLG_(stat).distinct_bbccs);
    VG_(message)(Vg_DebugMsg, "Distinct JCCs:    %d\n",
		 CLG_(stat).distinct_jccs);
    VG_(message)(Vg_DebugMsg, "Distinct skips:   %d\n",
		 CLG_(stat).distinct_skips);
    VG_(message)(Vg_DebugMsg, "BB lookups:       %d\n",
		 BB_lookups);
    if (BB_lookups>0) {
      VG_(message)(Vg_DebugMsg, "With full      debug info:%3d%% (%d)\n", 
		   CLG_(stat).full_debug_BBs    * 100 / BB_lookups,
		   CLG_(stat).full_debug_BBs);
      VG_(message)(Vg_DebugMsg, "With file/line debug info:%3d%% (%d)\n", 
		   CLG_(stat).file_line_debug_BBs * 100 / BB_lookups,
		   CLG_(stat).file_line_debug_BBs);
      VG_(message)(Vg_DebugMsg, "With fn name   debug info:%3d%% (%d)\n", 
		   CLG_(stat).fn_name_debug_BBs * 100 / BB_lookups,
		   CLG_(stat).fn_name_debug_BBs);
      VG_(message)(Vg_DebugMsg, "With no        debug info:%3d%% (%d)\n", 
		   CLG_(stat).no_debug_BBs      * 100 / BB_lookups,
		   CLG_(stat).no_debug_BBs);
    }
    VG_(message)(Vg_DebugMsg, "BBCC Clones:       %d\n",
		 CLG_(stat).bbcc_clones);
    VG_(message)(Vg_DebugMsg, "BBs Retranslated:  %d\n",
		 CLG_(stat).bb_retranslations);
    VG_(message)(Vg_DebugMsg, "Distinct instrs:   %d\n",
		 CLG_(stat).distinct_instrs);
    VG_(message)(Vg_DebugMsg, "");
    
    VG_(message)(Vg_DebugMsg, "LRU Contxt Misses: %d\n",
		 CLG_(stat).cxt_lru_misses);
    VG_(message)(Vg_DebugMsg, "LRU BBCC Misses:   %d\n",
		 CLG_(stat).bbcc_lru_misses);
    VG_(message)(Vg_DebugMsg, "LRU JCC Misses:    %d\n",
		 CLG_(stat).jcc_lru_misses);
    VG_(message)(Vg_DebugMsg, "BBs Executed:      %llu\n",
		 CLG_(stat).bb_executions);
    VG_(message)(Vg_DebugMsg, "Calls:             %llu\n",
		 CLG_(stat).call_counter);
    VG_(message)(Vg_DebugMsg, "CondJMP followed:  %llu\n",
		 CLG_(stat).jcnd_counter);
    VG_(message)(Vg_DebugMsg, "Boring JMPs:       %llu\n",
		 CLG_(stat).jump_counter);
    VG_(message)(Vg_DebugMsg, "Recursive calls:   %llu\n",
		 CLG_(stat).rec_call_counter);
    VG_(message)(Vg_DebugMsg, "Returns:           %llu\n",
		 CLG_(stat).ret_counter);

    VG_(message)(Vg_DebugMsg, "");
  }

  CLG_(sprint_eventmapping)(buf, CLG_(dumpmap));
  VG_(message)(Vg_UserMsg, "Events    : %s\n", buf);
  CLG_(sprint_mappingcost)(buf, CLG_(dumpmap), CLG_(total_cost));
  VG_(message)(Vg_UserMsg, "Collected : %s\n", buf);
  VG_(message)(Vg_UserMsg, "\n");

  //  if (CLG_(clo).simulate_cache)
  (*CLG_(cachesim).printstat)();
}


void CLG_(fini)(Int exitcode)
{
  finish();
}


/*--------------------------------------------------------------------*/
/*--- Setup                                                        ---*/
/*--------------------------------------------------------------------*/

static void clg_start_client_code_callback ( ThreadId tid, ULong blocks_done )
{
   static ULong last_blocks_done = 0;

   if (0)
      VG_(printf)("%d R %llu\n", (Int)tid, blocks_done);

   /* throttle calls to CLG_(run_thread) by number of BBs executed */
   if (blocks_done - last_blocks_done < 5000) return;
   last_blocks_done = blocks_done;

   CLG_(run_thread)( tid );
}

static
void CLG_(post_clo_init)(void)
{
   VG_(clo_vex_control).iropt_unroll_thresh = 0;
   VG_(clo_vex_control).guest_chase_thresh = 0;

   CLG_DEBUG(1, "  dump threads: %s\n", CLG_(clo).separate_threads ? "Yes":"No");
   CLG_DEBUG(1, "  call sep. : %d\n", CLG_(clo).separate_callers);
   CLG_DEBUG(1, "  rec. sep. : %d\n", CLG_(clo).separate_recursions);

   if (!CLG_(clo).dump_line && !CLG_(clo).dump_instr && !CLG_(clo).dump_bb) {
       VG_(message)(Vg_UserMsg, "Using source line as position.\n");
       CLG_(clo).dump_line = True;
   }

   CLG_(init_dumps)();
   CLG_(init_command)();

   (*CLG_(cachesim).post_clo_init)();

   CLG_(init_eventsets)(0);
   CLG_(init_statistics)(& CLG_(stat));
   CLG_(init_cost_lz)( CLG_(sets).full, &CLG_(total_cost) );

   /* initialize hash tables */
   CLG_(init_obj_table)();
   CLG_(init_cxt_table)();
   CLG_(init_bb_hash)();

   CLG_(init_threads)();
   CLG_(run_thread)(1);

   CLG_(instrument_state) = CLG_(clo).instrument_atstart;

   if (VG_(clo_verbosity > 0)) {
      VG_(message)(Vg_UserMsg,
                   "For interactive control, run 'callgrind_control -h'.\n");
   }
}

static
void CLG_(pre_clo_init)(void)
{
    VG_(details_name)            ("Callgrind");
    VG_(details_version)         (NULL);
    VG_(details_description)     ("a call-graph generating cache profiler");
    VG_(details_copyright_author)("Copyright (C) 2002-2010, and GNU GPL'd, "
				  "by Josef Weidendorfer et al.");
    VG_(details_bug_reports_to)  (VG_BUGS_TO);
    VG_(details_avg_translation_sizeB) ( 500 );

    VG_(basic_tool_funcs)        (CLG_(post_clo_init),
                                  CLG_(instrument),
                                  CLG_(fini));

    VG_(needs_superblock_discards)(clg_discard_superblock_info);


    VG_(needs_command_line_options)(CLG_(process_cmd_line_option),
				    CLG_(print_usage),
				    CLG_(print_debug_usage));

    VG_(needs_client_requests)(CLG_(handle_client_request));
    VG_(needs_syscall_wrapper)(CLG_(pre_syscalltime),
			       CLG_(post_syscalltime));

    VG_(track_start_client_code)  ( & clg_start_client_code_callback );
    VG_(track_pre_deliver_signal) ( & CLG_(pre_signal) );
    VG_(track_post_deliver_signal)( & CLG_(post_signal) );

    CLG_(set_clo_defaults)();
}

VG_DETERMINE_INTERFACE_VERSION(CLG_(pre_clo_init))

/*--------------------------------------------------------------------*/
/*--- end                                                   main.c ---*/
/*--------------------------------------------------------------------*/
