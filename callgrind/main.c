
/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                       main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call graph
   profiling programs.

   Copyright (C) 2002-2007, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

   This skin is derived from and contains code from Cachegrind
   Copyright (C) 2002-2007 Nicholas Nethercote (njn25@cam.ac.uk)

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
/*--- Cache simulation instrumentation phase               ---*/
/*------------------------------------------------------------*/


static Bool loadStoreAddrsMatch(IRExpr* loadAddrExpr, IRExpr* storeAddrExpr)
{
  // I'm assuming that for 'modify' instructions, that Vex always makes
  // the loadAddrExpr and storeAddrExpr be of the same type, ie. both Tmp
  // expressions, or both Const expressions.
  CLG_ASSERT(isIRAtom(loadAddrExpr));
  CLG_ASSERT(isIRAtom(storeAddrExpr));
  return eqIRAtom(loadAddrExpr, storeAddrExpr);
}

static
EventSet* insert_simcall(IRSB* bbOut, InstrInfo* ii, UInt dataSize,
			 Bool instrIssued,
			 IRExpr* loadAddrExpr, IRExpr* storeAddrExpr)
{
    HChar*    helperName;
    void*     helperAddr;
    Int       argc;
    EventSet* es;
    IRExpr   *arg1, *arg2 = 0, *arg3 = 0, **argv;
    IRDirty* di;

    /* Check type of original instruction regarding memory access,
     * and collect info to be able to generate fitting helper call
     */
    if (!loadAddrExpr && !storeAddrExpr) {
	// no load/store
	CLG_ASSERT(0 == dataSize);
	if (instrIssued) {
	    helperName = 0;
	    helperAddr = 0;
	}
	else {
	    helperName = CLG_(cachesim).log_1I0D_name;
	    helperAddr = CLG_(cachesim).log_1I0D;
	}
	argc = 1;
	es = CLG_(sets).D0;
	
    } else if (loadAddrExpr && !storeAddrExpr) {
	// load
	CLG_ASSERT( isIRAtom(loadAddrExpr) );
	if (instrIssued) {
	    helperName = CLG_(cachesim).log_0I1Dr_name;
	    helperAddr = CLG_(cachesim).log_0I1Dr;
	}
	else {
	    helperName = CLG_(cachesim).log_1I1Dr_name;
	    helperAddr = CLG_(cachesim).log_1I1Dr;
	}
	argc = 2;
	arg2 = loadAddrExpr;
	es = CLG_(sets).D1r;

    } else if (!loadAddrExpr && storeAddrExpr) {
	// store
	CLG_ASSERT( isIRAtom(storeAddrExpr) );
	if (instrIssued) {
	    helperName = CLG_(cachesim).log_0I1Dw_name;
	    helperAddr = CLG_(cachesim).log_0I1Dw;
	}
	else {
	    helperName = CLG_(cachesim).log_1I1Dw_name;
	    helperAddr = CLG_(cachesim).log_1I1Dw;
	}
	argc = 2;
	arg2 = storeAddrExpr;
	es = CLG_(sets).D1w;
	
    } else {
	CLG_ASSERT( loadAddrExpr && storeAddrExpr );
	CLG_ASSERT( isIRAtom(loadAddrExpr) );
	CLG_ASSERT( isIRAtom(storeAddrExpr) );
	
	if ( loadStoreAddrsMatch(loadAddrExpr, storeAddrExpr) ) {
	    /* modify: suppose write access, as this is
	     * more resource consuming (as in callgrind for VG2)
	     * Cachegrind does a read here (!)
	     * DISCUSS: Best way depends on simulation model?
	     */
	    if (instrIssued) {
		helperName = CLG_(cachesim).log_0I1Dw_name;
		helperAddr = CLG_(cachesim).log_0I1Dw;
	    }
	    else {
		helperName = CLG_(cachesim).log_1I1Dw_name;
		helperAddr = CLG_(cachesim).log_1I1Dw;
	    }
	    argc = 2;
	    arg2 = storeAddrExpr;
	    es = CLG_(sets).D1w;
	    
	} else {
	    // load/store
	    if (instrIssued) {
		helperName = CLG_(cachesim).log_0I2D_name;
		helperAddr = CLG_(cachesim).log_0I2D;
	    }
	    else {
		helperName = CLG_(cachesim).log_1I2D_name;
		helperAddr = CLG_(cachesim).log_1I2D;
	    }
	    argc = 3;
	    arg2 = loadAddrExpr;
	    arg3 = storeAddrExpr;
	    es = CLG_(sets).D2;
	}
    }

    /* helper could be unset depending on the simulator used */
    if (helperAddr == 0) return 0;
    
    /* Setup 1st arg: InstrInfo */
    arg1 = mkIRExpr_HWord( (HWord)ii );
    
    // Add call to the instrumentation function
    if      (argc == 1)
	argv = mkIRExprVec_1(arg1);
    else if (argc == 2)
	argv = mkIRExprVec_2(arg1, arg2);
    else if (argc == 3)
	argv = mkIRExprVec_3(arg1, arg2, arg3);
    else
	VG_(tool_panic)("argc... not 1 or 2 or 3?");
    
    di = unsafeIRDirty_0_N( argc, helperName, 
                                  VG_(fnptr_to_fnentry)( helperAddr ), argv);
    addStmtToIRSB( bbOut, IRStmt_Dirty(di) );

    return es;
}


/* Instrumentation before a conditional jump or at the end
 * of each original instruction.
 * Fills the InstrInfo struct if not seen before
 */
static
void endOfInstr(IRSB* bbOut, InstrInfo* ii, Bool bb_seen_before,
		UInt instr_offset, UInt instrLen, UInt dataSize, 
		UInt* cost_offset, Bool instrIssued,
		IRExpr* loadAddrExpr, IRExpr* storeAddrExpr)
{
   IRType    wordTy;
   EventSet* es;

   // Stay sane ...
   CLG_ASSERT(sizeof(HWord) == sizeof(void*));
   if (sizeof(HWord) == 4) {
      wordTy = Ity_I32;
   } else
   if (sizeof(HWord) == 8) {
      wordTy = Ity_I64;
   } else {
      VG_(tool_panic)("endOfInstr: strange word size");
   }

   if (loadAddrExpr) 
      CLG_ASSERT(wordTy == typeOfIRExpr(bbOut->tyenv, loadAddrExpr));
   if (storeAddrExpr) 
      CLG_ASSERT(wordTy == typeOfIRExpr(bbOut->tyenv, storeAddrExpr));

   // Large (eg. 28B, 108B, 512B on x86) data-sized instructions will be
   // done inaccurately, but they're very rare and this avoids errors from
   // hitting more than two cache lines in the simulation.
   if (dataSize > MIN_LINE_SIZE) dataSize = MIN_LINE_SIZE;

   /* returns 0 if simulator needs no instrumentation */
   es = insert_simcall(bbOut, ii, dataSize, instrIssued,
		       loadAddrExpr, storeAddrExpr);

   CLG_DEBUG(5, "  Instr +%2d (Size %d, DSize %d): ESet %s (Size %d)\n",
	     instr_offset, instrLen, dataSize, 
	     es ? es->name : (Char*)"(no instrumentation)",
	     es ? es->size : 0);

   if (bb_seen_before) {
       CLG_DEBUG(5, "   before: Instr +%2d (Size %d, DSize %d)\n",
		 ii->instr_offset, ii->instr_size, ii->data_size);

       CLG_ASSERT(ii->instr_offset == instr_offset);
       CLG_ASSERT(ii->instr_size == instrLen);
       CLG_ASSERT(ii->cost_offset == *cost_offset);
       CLG_ASSERT(ii->eventset == es);

       /* Only check size if data size >0.
	* This is needed: e.g. for rep or cmov x86 instructions, the same InstrInfo
	* is used both for 2 simulator calls: for the pure instruction fetch and
        * separately for an memory access (which may not happen depending on flags).
	* If checked always, this triggers an assertion failure on retranslation.
	*/
       if (dataSize>0) CLG_ASSERT(ii->data_size == dataSize);

   }
   else {
       ii->instr_offset = instr_offset;
       ii->instr_size = instrLen;
       ii->cost_offset = *cost_offset;
       ii->eventset = es;
       
       /* data size only relevant if >0 */
       if (dataSize > 0) ii->data_size = dataSize;


       CLG_(stat).distinct_instrs++;
   }

   *cost_offset += es ? es->size : 0;

}

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
void CLG_(collectBlockInfo)(IRSB* bbIn,
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

    if (!bbIn) return;

    for (i = 0; i < bbIn->stmts_used; i++) {
	  st = bbIn->stmts[i];
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
void collectStatementInfo(IRTypeEnv* tyenv, IRSB* bbOut, IRStmt* st,
			  Addr* instrAddr, UInt* instrLen,
			  IRExpr** loadAddrExpr, IRExpr** storeAddrExpr,
			  UInt* dataSize, IRType hWordTy)
{
   CLG_ASSERT(isFlatIRStmt(st));

   switch (st->tag) {
   case Ist_NoOp:
      break;

   case Ist_AbiHint:
      /* ABI hints aren't interesting.  Ignore. */
      break;

   case Ist_IMark:
      /* st->Ist.IMark.addr is a 64-bit int.  ULong_to_Ptr casts this
         to the host's native pointer type; if that is 32 bits then it
         discards the upper 32 bits.  If we are cachegrinding on a
         32-bit host then we are also ensured that the guest word size
         is 32 bits, due to the assertion in cg_instrument that the
         host and guest word sizes must be the same.  Hence
         st->Ist.IMark.addr will have been derived from a 32-bit guest
         code address and truncation of it is safe.  I believe this
         assignment should be correct for both 32- and 64-bit
         machines. */
      *instrAddr = (Addr)ULong_to_Ptr(st->Ist.IMark.addr);
      *instrLen =        st->Ist.IMark.len;
      break;

   case Ist_WrTmp: {
      IRExpr* data = st->Ist.WrTmp.data;
      if (data->tag == Iex_Load) {
         IRExpr* aexpr = data->Iex.Load.addr;
         CLG_ASSERT( isIRAtom(aexpr) );
         // Note also, endianness info is ignored.  I guess that's not
         // interesting.
         // XXX: repe cmpsb does two loads... the first one is ignored here!
         //tl_assert( NULL == *loadAddrExpr );          // XXX: ???
         *loadAddrExpr = aexpr;
         *dataSize = sizeofIRType(data->Iex.Load.ty);
      }
      break;
   }
      
   case Ist_Store: {
      IRExpr* data  = st->Ist.Store.data;
      IRExpr* aexpr = st->Ist.Store.addr;
      CLG_ASSERT( isIRAtom(aexpr) );
      if ( NULL == *storeAddrExpr ) {
          /* this is a kludge: ignore all except the first store from
             an instruction. */
          *storeAddrExpr = aexpr;
          *dataSize = sizeofIRType(typeOfIRExpr(tyenv, data));
      }
      break;
   }
   
   case Ist_Dirty: {
      IRDirty* d = st->Ist.Dirty.details;
      if (d->mFx != Ifx_None) {
         /* This dirty helper accesses memory.  Collect the
            details. */
         CLG_ASSERT(d->mAddr != NULL);
         CLG_ASSERT(d->mSize != 0);
         *dataSize = d->mSize;
         if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify)
            *loadAddrExpr = d->mAddr;
         if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify)
            *storeAddrExpr = d->mAddr;
      } else {
         CLG_ASSERT(d->mAddr == NULL);
         CLG_ASSERT(d->mSize == 0);
      }
      break;
   }

   case Ist_Put:
   case Ist_PutI:
   case Ist_MFence:
   case Ist_Exit:
       break;

   default:
      VG_(printf)("\n");
      ppIRStmt(st);
      VG_(printf)("\n");
      VG_(tool_panic)("Callgrind: unhandled IRStmt");
   }
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

static
IRSB* CLG_(instrument)( VgCallbackClosure* closure,
			IRSB* bbIn,
			VexGuestLayout* layout,
			VexGuestExtents* vge,
			IRType gWordTy, IRType hWordTy )
{
   Int      i;
   IRSB*    bbOut;
   IRStmt*  st, *stnext;
   Addr     instrAddr, origAddr;
   UInt     instrLen = 0, dataSize;
   UInt     instrCount, costOffset;
   IRExpr  *loadAddrExpr, *storeAddrExpr;

   BB*         bb;

   IRDirty* di;
   IRExpr  *arg1, **argv;

   Bool        bb_seen_before     = False;
   UInt        cJumps = 0, cJumpsCorrected;
   Bool        beforeIBoundary, instrIssued;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   // No instrumentation if it is switched off
   if (! CLG_(instrument_state)) {
       CLG_DEBUG(5, "instrument(BB %p) [Instrumentation OFF]\n",
		 (Addr)closure->readdr);
       return bbIn;
   }

   CLG_DEBUG(3, "+ instrument(BB %p)\n", (Addr)closure->readdr);

   /* Set up SB for instrumented IR */
   bbOut = deepCopyIRSBExceptStmts(bbIn);

   // Copy verbatim any IR preamble preceding the first IMark
   i = 0;
   while (i < bbIn->stmts_used && bbIn->stmts[i]->tag != Ist_IMark) {
      addStmtToIRSB( bbOut, bbIn->stmts[i] );
      i++;
   }

   // Get the first statement, and origAddr from it
   CLG_ASSERT(bbIn->stmts_used > 0);
   st = bbIn->stmts[i];
   CLG_ASSERT(Ist_IMark == st->tag);
   instrAddr = origAddr = (Addr)st->Ist.IMark.addr;
   CLG_ASSERT(origAddr == st->Ist.IMark.addr);  // XXX: check no overflow

   /* Get BB (creating if necessary).
    * JS: The hash table is keyed with orig_addr_noredir -- important!
    * JW: Why? If it is because of different chasing of the redirection,
    *     this is not needed, as chasing is switched off in callgrind
    */
   bb = CLG_(get_bb)(origAddr, bbIn, &bb_seen_before);
   //bb = CLG_(get_bb)(orig_addr_noredir, bbIn, &bb_seen_before);

   /* 
    * Precondition:
    * - jmps_passed has number of cond.jumps passed in last executed BB
    * - current_bbcc has a pointer to the BBCC of the last executed BB
    *   Thus, if bbcc_jmpkind is != -1 (JmpNone),
    *     current_bbcc->bb->jmp_addr
    *   gives the address of the jump source.
    *   
    * The BBCC setup does 2 things:
    * - trace call:
    *   * Unwind own call stack, i.e sync our ESP with real ESP
    *     This is for ESP manipulation (longjmps, C++ exec handling) and RET
    *   * For CALLs or JMPs crossing objects, record call arg +
    *     push are on own call stack
    *
    * - prepare for cache log functions:
    *   Set current_bbcc to BBCC that gets the costs for this BB execution
    *   attached
    */

   // helper call to setup_bbcc, with pointer to basic block info struct as argument
   arg1 = mkIRExpr_HWord( (HWord)bb );
   argv = mkIRExprVec_1(arg1);
   di = unsafeIRDirty_0_N( 1, "setup_bbcc", 
                              VG_(fnptr_to_fnentry)( & CLG_(setup_bbcc) ), 
                              argv);
   addStmtToIRSB( bbOut, IRStmt_Dirty(di) );

   instrCount = 0;
   costOffset = 0;

   // loop for each host instruction (starting from 'i')
   do {

      // We should be at an IMark statement
      CLG_ASSERT(Ist_IMark == st->tag);

      // Reset stuff for this original instruction
      loadAddrExpr = storeAddrExpr = NULL;
      instrIssued = False;
      dataSize = 0;

      // Process all the statements for this original instruction (ie. until
      // the next IMark statement, or the end of the block)
      do {
	  i++;
	  stnext = ( i < bbIn->stmts_used ? bbIn->stmts[i] : NULL );
	  beforeIBoundary = !stnext || (Ist_IMark == stnext->tag);
	  collectStatementInfo(bbIn->tyenv, bbOut, st, &instrAddr, &instrLen,
			       &loadAddrExpr, &storeAddrExpr, &dataSize, hWordTy);

	  // instrument a simulator call before conditional jumps
	  if (st->tag == Ist_Exit) {
	      // Nb: instrLen will be zero if Vex failed to decode it.
	      // Also Client requests can appear to be very large (eg. 18
	      // bytes on x86) because they are really multiple instructions.
	      CLG_ASSERT( 0 == instrLen ||
			  bbIn->jumpkind == Ijk_ClientReq ||
			  (instrLen >= VG_MIN_INSTR_SZB && 
			   instrLen <= VG_MAX_INSTR_SZB) );

              // Add instrumentation before this statement
	      endOfInstr(bbOut, &(bb->instr[instrCount]), bb_seen_before,
			 instrAddr - origAddr, instrLen, dataSize, &costOffset,
			 instrIssued, loadAddrExpr, storeAddrExpr);

	      // prepare for a possible further simcall in same host instr
	      loadAddrExpr = storeAddrExpr = NULL;
	      instrIssued = True;

	      if (!bb_seen_before) {
		  bb->jmp[cJumps].instr = instrCount;
		  bb->jmp[cJumps].skip = False;
	      }
	      
	      /* Update global variable jmps_passed (this is before the jump!)
	       * A correction is needed if VEX inverted the last jump condition
	       */
	      cJumpsCorrected = cJumps;
	      if ((cJumps+1 == bb->cjmp_count) && bb->cjmp_inverted) cJumpsCorrected++;
	      addConstMemStoreStmt( bbOut, (UWord) &CLG_(current_state).jmps_passed,
				    cJumpsCorrected, hWordTy);

	      cJumps++;
	  }

	  addStmtToIRSB( bbOut, st );
	  st = stnext;
      } 
      while (!beforeIBoundary);

      // Add instrumentation for this original instruction.
      if (!instrIssued || (loadAddrExpr != 0) || (storeAddrExpr !=0))
	  endOfInstr(bbOut, &(bb->instr[instrCount]), bb_seen_before,
		     instrAddr - origAddr, instrLen, dataSize, &costOffset,
		     instrIssued, loadAddrExpr, storeAddrExpr);

      instrCount++;
   }
   while (st);

   /* Always update global variable jmps_passed (at end of BB)
    * A correction is needed if VEX inverted the last jump condition
    */
   cJumpsCorrected = cJumps;
   if (bb->cjmp_inverted) cJumpsCorrected--;
   addConstMemStoreStmt( bbOut, (UWord) &CLG_(current_state).jmps_passed,
			 cJumpsCorrected, hWordTy);

   /* This stores the instr of the call/ret at BB end */
   bb->jmp[cJumps].instr = instrCount-1;

   CLG_ASSERT(bb->cjmp_count == cJumps);
   CLG_ASSERT(bb->instr_count == instrCount);

   instrAddr += instrLen;
   if (bb_seen_before) {
       CLG_ASSERT(bb->instr_len == instrAddr - origAddr);
       CLG_ASSERT(bb->cost_count == costOffset);
       CLG_ASSERT(bb->jmpkind == bbIn->jumpkind);
   }
   else {
       bb->instr_len = instrAddr - origAddr;
       bb->cost_count = costOffset;
       bb->jmpkind = bbIn->jumpkind;
   }
   
   CLG_DEBUG(3, "- instrument(BB %p): byteLen %u, CJumps %u, CostLen %u\n",
	     origAddr, bb->instr_len, bb->cjmp_count, bb->cost_count);
   if (cJumps>0) {
       CLG_DEBUG(3, "                     [ ");
       for (i=0;i<cJumps;i++)
	   CLG_DEBUG(3, "%d ", bb->jmp[i].instr);
       CLG_DEBUG(3, "], last inverted: %s \n", bb->cjmp_inverted ? "yes":"no");
   }

  return bbOut;
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
  }

  CLG_(forall_bbccs)(CLG_(zero_bbcc));

  /* set counter for last dump */
  CLG_(copy_cost)( CLG_(sets).full, 
		  t->lastdump_cost, CLG_(current_state).cost );
}

void CLG_(zero_all_cost)(Bool only_current_thread)
{
  if (VG_(clo_verbosity) > 1)
    VG_(message)(Vg_DebugMsg, "  Zeroing costs...");

  if (only_current_thread)
    zero_thread_cost(CLG_(get_current_thread)());
  else
    CLG_(forall_threads)(zero_thread_cost);

  if (VG_(clo_verbosity) > 1)
    VG_(message)(Vg_DebugMsg, "  ...done");
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
  (*CLG_(cachesim).clear)();
  if (0)
    CLG_(forall_threads)(zero_thread_cost);

  if (VG_(clo_verbosity) > 1)
    VG_(message)(Vg_DebugMsg, "%s: instrumentation switched %s",
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
       VG_(sprintf)(buf,"Client Request: %s", args[1]);
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
void CLG_(pre_syscalltime)(ThreadId tid, UInt syscallno)
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
void CLG_(post_syscalltime)(ThreadId tid, UInt syscallno, SysRes res)
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
  if (VG_(clo_verbosity) > 1) {
    int BB_lookups =
      CLG_(stat).full_debug_BBs +
      CLG_(stat).fn_name_debug_BBs +
      CLG_(stat).file_line_debug_BBs +
      CLG_(stat).no_debug_BBs;
    
    VG_(message)(Vg_DebugMsg, "");
    VG_(message)(Vg_DebugMsg, "Distinct objects: %d",
		 CLG_(stat).distinct_objs);
    VG_(message)(Vg_DebugMsg, "Distinct files:   %d",
		 CLG_(stat).distinct_files);
    VG_(message)(Vg_DebugMsg, "Distinct fns:     %d",
		 CLG_(stat).distinct_fns);
    VG_(message)(Vg_DebugMsg, "Distinct contexts:%d",
		 CLG_(stat).distinct_contexts);
    VG_(message)(Vg_DebugMsg, "Distinct BBs:     %d",
		 CLG_(stat).distinct_bbs);
    VG_(message)(Vg_DebugMsg, "Cost entries:     %d (Chunks %d)",
		 CLG_(costarray_entries), CLG_(costarray_chunks));
    VG_(message)(Vg_DebugMsg, "Distinct BBCCs:   %d",
		 CLG_(stat).distinct_bbccs);
    VG_(message)(Vg_DebugMsg, "Distinct JCCs:    %d",
		 CLG_(stat).distinct_jccs);
    VG_(message)(Vg_DebugMsg, "Distinct skips:   %d",
		 CLG_(stat).distinct_skips);
    VG_(message)(Vg_DebugMsg, "BB lookups:       %d",
		 BB_lookups);
    if (BB_lookups>0) {
      VG_(message)(Vg_DebugMsg, "With full      debug info:%3d%% (%d)", 
		   CLG_(stat).full_debug_BBs    * 100 / BB_lookups,
		   CLG_(stat).full_debug_BBs);
      VG_(message)(Vg_DebugMsg, "With file/line debug info:%3d%% (%d)", 
		   CLG_(stat).file_line_debug_BBs * 100 / BB_lookups,
		   CLG_(stat).file_line_debug_BBs);
      VG_(message)(Vg_DebugMsg, "With fn name   debug info:%3d%% (%d)", 
		   CLG_(stat).fn_name_debug_BBs * 100 / BB_lookups,
		   CLG_(stat).fn_name_debug_BBs);
      VG_(message)(Vg_DebugMsg, "With no        debug info:%3d%% (%d)", 
		   CLG_(stat).no_debug_BBs      * 100 / BB_lookups,
		   CLG_(stat).no_debug_BBs);
    }
    VG_(message)(Vg_DebugMsg, "BBCC Clones:       %d",
		 CLG_(stat).bbcc_clones);
    VG_(message)(Vg_DebugMsg, "BBs Retranslated:  %d",
		 CLG_(stat).bb_retranslations);
    VG_(message)(Vg_DebugMsg, "Distinct instrs:   %d",
		 CLG_(stat).distinct_instrs);
    VG_(message)(Vg_DebugMsg, "");
    
    VG_(message)(Vg_DebugMsg, "LRU Contxt Misses: %d",
		 CLG_(stat).cxt_lru_misses);
    VG_(message)(Vg_DebugMsg, "LRU BBCC Misses:   %d",
		 CLG_(stat).bbcc_lru_misses);
    VG_(message)(Vg_DebugMsg, "LRU JCC Misses:    %d",
		 CLG_(stat).jcc_lru_misses);
    VG_(message)(Vg_DebugMsg, "BBs Executed:      %llu",
		 CLG_(stat).bb_executions);
    VG_(message)(Vg_DebugMsg, "Calls:             %llu",
		 CLG_(stat).call_counter);
    VG_(message)(Vg_DebugMsg, "CondJMP followed:  %llu",
		 CLG_(stat).jcnd_counter);
    VG_(message)(Vg_DebugMsg, "Boring JMPs:       %llu",
		 CLG_(stat).jump_counter);
    VG_(message)(Vg_DebugMsg, "Recursive calls:   %llu",
		 CLG_(stat).rec_call_counter);
    VG_(message)(Vg_DebugMsg, "Returns:           %llu",
		 CLG_(stat).ret_counter);

    VG_(message)(Vg_DebugMsg, "");
  }

  CLG_(sprint_eventmapping)(buf, CLG_(dumpmap));
  VG_(message)(Vg_UserMsg, "Events    : %s", buf);
  CLG_(sprint_mappingcost)(buf, CLG_(dumpmap), CLG_(total_cost));
  VG_(message)(Vg_UserMsg, "Collected : %s", buf);
  VG_(message)(Vg_UserMsg, "");

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
       VG_(message)(Vg_UserMsg, "Using source line as position.");
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
                   "For interactive control, run 'callgrind_control -h'.");
   }
}

static
void CLG_(pre_clo_init)(void)
{
    VG_(details_name)            ("Callgrind");
    VG_(details_version)         (NULL);
    VG_(details_description)     ("a call-graph generating cache profiler");
    VG_(details_copyright_author)("Copyright (C) 2002-2007, and GNU GPL'd, "
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
