
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (reg_alloc.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>
#include <malloc.h>

#include "basictypes.h"
#include "host_regs.h"


/* How many 64-bit sized spill slots do we have? */
#define N_SPILL64S  16



/* Records information on virtual register live ranges.  Computed once
   and remains unchanged after that. */
typedef
   struct {
      /* Becomes live for the first time after this insn ... */
      Int live_after;
      /* Becomes dead for the last time before this insn ... */
      Int dead_before;
      /* The "home" spill slot, if needed.  Never changes. */
      Int spill_offset;
      Int spill_size;
      /* Preferencing info, if any. */
      Bool has_preference;
      HReg preferred_rreg; /* if True, where I would like to be */
   }
   VRegInfo;


/* Records information on real-register live ranges.  Computed once
   and remains unchanged after that. */
typedef
   struct {
      HReg rreg;
      /* Becomes live after this insn ... */
      Int live_after;
      /* Becomes dead before this insn ... */
      Int dead_before;
   }
   RRegInfo;


/* An array of the following structs comprises the running state of
   the allocator.  It indicates what the current disposition of each
   allocatable real register is.  The array gets updated as the
   allocator processes instructions. */
typedef
   struct {
      /* Which rreg is this for? */
      HReg rreg;
      /* What's it's current disposition? */
      enum { Free,     /* available for use */
             Unavail,  /* in a real-reg live range */
             Bound     /* in use (holding value of some vreg) */
           }
           disp;
      /* If RRegBound, what vreg is it bound to? */
      HReg vreg;
      /* Used when .disp == Bound and we are looking for vregs to
         spill. */
      Bool is_spill_cand;
   }
   RRegState;



/* Does this instruction mention a particular reg? */
static Bool instrMentionsReg ( 
   void (*getRegUsage) (HInstr*, HRegUsage*),
   HInstr* instr, 
   HReg r 
)
{
   Int       i;
   HRegUsage reg_usage;
   (*getRegUsage)(instr, &reg_usage);
   for (i = 0; i < reg_usage.n_used; i++)
      if (reg_usage.hreg[i] == r)
         return True;
   return False;
}


/* A target-independent register allocator for Valgrind.  Requires
   various functions which it uses to deal abstractly with
   instructions and registers, since it cannot have any
   target-specific knowledge.

   Returns a new list of instructions, which, as a result of the
   behaviour of mapRegs, will be in-place modifications of the
   original instructions.

   Requires that the incoming code has been generated using
   vreg numbers 0, 1 .. n_vregs-1.  Appearance of a vreg outside
   that range is a checked run-time error.

   Takes a NULL-terminated array of pointers to unallocated insns.
   Returns a NULL-terminated array of pointers to allocated insns.
*/
HInstr** doRegisterAllocation (

   /* Incoming virtual-registerised code, NULL terminated */ 
   HInstr** instrs,
   Int      n_vregs,

   /* An array listing all the real registers the allocator may use,
      in no particular order. */
   HReg* available_real_regs,
   Int   n_available_real_regs,

   /* Return True iff the given insn is a reg-reg move, in which
      case also return the src and dst regs. */
   Bool (*isMove) (HInstr*, HReg*, HReg*),

   /* Get info about register usage in this insn. */
   void (*getRegUsage) (HInstr*, HRegUsage*),

   /* Apply a reg-reg mapping to an insn. */
   void (*mapRegs) (HRegRemap*, HInstr*),

   /* Return an insn to spill/restore a real reg to a spill slot
      offset. */
   HInstr* (*genSpill) ( HReg, Int ),
   HInstr* (*genReload) ( HReg, Int )
)
{
   /* Iterators and temporaries. */
   Int       ii, j, k, m, furthest, furthest_k;
   HReg      rreg, vreg;
   HRegUsage reg_usage;

   /* Info on vregs and rregs.  Computed once and remains
      unchanged. */
   VRegInfo* vreg_info;
   RRegInfo* rreg_info;
   Int       rreg_info_size;
   Int       rreg_info_used;

   /* Used when constructing vreg_info (for allocating stack
      slots). */
   Int ss_busy_until_before[N_SPILL64S];

   /* Used when constructing rreg_info. */
   Int* rreg_live_after;
   Int* rreg_dead_before;

   /* Running state of the core allocation algorithm. */
   RRegState* state;
   Int        n_state;

   /* The vreg -> rreg map constructed and then applied to each
      instr. */
   HRegRemap remap;

   /* The output array of instructions. */
   HInstr** instrs_out;
   Int      instrs_out_size;
   Int      instrs_out_used;

   /* Number of incoming instructions. */
   Int n_instrs;


#  define INVALID_INSTRNO (-2)

   /* --------- Stage 0: set up output array. --------- */
   n_instrs = 0;
   while (instrs[n_instrs] != NULL)
      n_instrs ++;

   instrs_out_size = 2 * n_instrs;
   instrs_out_used = 0;
   instrs_out      = malloc(instrs_out_size * sizeof(HInstr*));

#  define EMIT_INSTR(_instr)                                         \
      do {                                                           \
         if (instrs_out_used >= instrs_out_used)                     \
            panic("doRegisterAllocation: instrs_out is too small");  \
         instrs_out[instrs_out_used] = (_instr);                     \
         instrs_out_used++;                                          \
      } while (0)


   /* --------- Stage 1: compute vreg live ranges. --------- */

   /* This is relatively simple, because (1) we only seek the complete
      end-to-end live range of each vreg, and are not interested in
      any holes in it, and (2) the vregs are conveniently numbered 0
      .. n_vregs-1, so we can just dump the results in a pre-allocated
      array. */

   vreg_info = NULL;
   if (n_vregs > 0)
      vreg_info = malloc(sizeof(VRegInfo) * n_vregs);

   for (j = 0; j < n_vregs; j++) {
      vreg_info[j].live_after     = INVALID_INSTRNO;
      vreg_info[j].dead_before    = INVALID_INSTRNO;
      vreg_info[j].spill_offset   = 0;
      vreg_info[j].spill_size     = 0;
      vreg_info[j].has_preference = False;
      vreg_info[j].preferred_rreg = INVALID_HREG;
   }

   /* for each insn ... */
   for (ii = 0; ii < n_instrs; ii++) {

      (*getRegUsage)( instrs[ii], &reg_usage );

      /* for each reg mentioned in the insn ... */
      for (j = 0; j < reg_usage.n_used; j++) {

         /* only interested in virtual registers right now. */
         if (!hregIsVirtual(reg_usage.hreg[j]))
            continue;
         k = hregNumber(reg_usage.hreg[j]);
         assert(k >= 0 && k < n_vregs);
         switch (reg_usage.mode[j]) {
            case HRmRead: 
               if (vreg_info[k].live_after == INVALID_INSTRNO)
                  panic("doRegisterAllocation: "
                        "first event for vreg is Read");
               vreg_info[k].dead_before = ii;
               break;
            case HRmWrite:
               if (vreg_info[k].live_after == INVALID_INSTRNO)
                  vreg_info[k].live_after = ii;
               vreg_info[k].dead_before = ii + 1;
               break;
            case HRmModify:
               if (vreg_info[k].live_after == INVALID_INSTRNO)
                  panic("doRegisterAllocation: "
                        "first event for vreg is Modify");
               vreg_info[k].dead_before = ii + 1;
               break;
            default:
               panic("doRegisterAllocation(1)");
         } /* switch */

      } /* iterate over registers */

   } /* iterate over insns */


   /* --------- Stage 2: compute rreg live ranges. --------- */

   /* This is more complex than Stage 1, because we need to compute
      exactly all the live ranges of all the allocatable real regs,
      and we don't know in advance how many there will be. */

   rreg_info_used = 0;
   rreg_info_size = 4;
   rreg_info = malloc(rreg_info_size * sizeof(RRegInfo));

   /* We'll need to track live range start/end points seperately for
      each rreg.  Sigh. */
   assert(n_available_real_regs > 0);
   rreg_live_after  = malloc(n_available_real_regs * sizeof(Int));
   rreg_dead_before = malloc(n_available_real_regs * sizeof(Int));

   for (j = 0; j < n_available_real_regs; j++)
      rreg_live_after[j] = 
      rreg_dead_before[j] = INVALID_INSTRNO;

   /* for each insn ... */
   for (ii = 0; ii < n_instrs; ii++) {

      (*getRegUsage)( instrs[ii], &reg_usage );

      /* for each reg mentioned in the insn ... */
      for (j = 0; j < reg_usage.n_used; j++) {

        /* Dummy initialisations of flush_la and flush_db to avoid
	   possible bogus uninit-var warnings from gcc. */
         Int  flush_la = INVALID_INSTRNO, flush_db = INVALID_INSTRNO;
         Bool flush;

         rreg = reg_usage.hreg[j];

         /* only interested in real registers right now. */
         if (hregIsVirtual(rreg))
            continue;

         /* Furthermore, we're not interested in this rreg unless it's
            one of the allocatable ones.  For example, it could be a
            stack pointer register, or some other register beyond our
            control, in which case we should just ignore it. */
         for (k = 0; k < n_available_real_regs; k++)
            if (available_real_regs[k] == rreg)
               break;
         if (k == n_available_real_regs) 
            continue; /* not found -- ignore. */

         flush = False;
         switch (reg_usage.mode[j]) {
            case HRmWrite:
               flush    = True;
               flush_la = rreg_live_after[k];
               flush_db = rreg_dead_before[k];
               rreg_live_after[k]  = ii;
               rreg_dead_before[k] = ii+1;
               break;
            case HRmRead:
               if (rreg_live_after[k] == INVALID_INSTRNO)
                  panic("doRegisterAllocation: "
                        "first event for rreg is Read");
               rreg_dead_before[k] = ii;
               break;
            case HRmModify:
               if (rreg_live_after[k] == INVALID_INSTRNO)
                  panic("doRegisterAllocation: "
                        "first event for rreg is Modify");
               rreg_dead_before[k] = ii+1;
               break;
            default:
               panic("doRegisterAllocation(2)");
         }

         if (flush) {
            if (rreg_info_used == rreg_info_size) {
               panic("make rreg info array bigger(1)");
            }
            /* An assert to catch pointwise RLRs.  Probably not
               something we can really get away with. */
            assert(flush_db-1 > flush_la);
            rreg_info[rreg_info_used].rreg        = rreg;
            rreg_info[rreg_info_used].live_after  = flush_la;
            rreg_info[rreg_info_used].dead_before = flush_db;
         }

      } /* iterate over regs in the instr */

   } /* iterate over instrs */

   /* Now finish up any live ranges left over. */
   for (j = 0; j < n_available_real_regs; j++) {
      if (rreg_live_after[j] == INVALID_INSTRNO)
         continue;
      if (rreg_info_used == rreg_info_size) {
         panic("make rreg info array bigger(2)");
      }
      rreg_info[rreg_info_used].rreg        = available_real_regs[j];
      rreg_info[rreg_info_used].live_after  = rreg_live_after[j];
      rreg_info[rreg_info_used].dead_before = rreg_dead_before[j];
   }

   free(rreg_live_after);
   free(rreg_dead_before);


   /* --------- Stage 3: allocate spill slots. --------- */

   /* Each spill slot is 8 bytes long.  For 128-bit vregs
      we'll have to allocate two spill slots.  For now, tho,
      ignore the 128-bit problem.

      Do a rank-based allocation of vregs to spill slot numbers.  We
      put as few values as possible in spill slows, but nevertheless
      need to have a spill slot available for all vregs, just in case.
   */
   /* max_ss_no = -1; */

   for (j = 0; j < N_SPILL64S; j++)
      ss_busy_until_before[j] = 0;

   for (j = 0; j < n_vregs; j++) {

      /* True iff this vreg is unused. */
      if (vreg_info[j].live_after == INVALID_INSTRNO)
         continue;

      /* Find the lowest-numbered spill slot which is available at the
         start point of this interval, and assign the interval to
         it. */
      for (k = 0; k < N_SPILL64S; k++)
         if (ss_busy_until_before[k] <= vreg_info[j].live_after)
            break;
      if (k == N_SPILL64S) {
         panic("N_SPILL64S is too low");
      }
      ss_busy_until_before[k] = vreg_info[j].dead_before;
      vreg_info[j].spill_offset = k * 8;
      /* if (j > max_ss_no) */
      /*    max_ss_no = j; */
   }


   /* --------- Stage 4: establish rreg preferences --------- */

   /* It may be advantageous to allocating certain vregs to specific
      rregs, as a way of avoiding reg-reg moves later.  Here we
      establish which, if any, rreg each vreg would prefer to be in.
      Note that this constrains the allocator -- ideally we end up
      with as few as possible vregs expressing a preference. */

   /* For now, ignore this.  It's only an optimisation, not needed for
      correctness. */


   /* --------- Stage 5: process instructions --------- */

   /* This is the main loop of the allocator.  First, we need to
      correctly set up our running state, which tracks the status of
      each real register. */

   /* n_state is no more than a short name for n_available_real_regs. */
   n_state = n_available_real_regs;
   state = malloc(n_available_real_regs * sizeof(RRegState));

   for (j = 0; j < n_state; j++) {
      state[j].rreg          = available_real_regs[j];
      state[j].disp          = Free;
      state[j].vreg          = INVALID_HREG;
      state[j].is_spill_cand = False;
   }

   /* Process each insn in turn. */
   for (ii = 0; ii < n_instrs; ii++) {

      /* ------ Sanity checks ------ */

      /* Sanity check 1: all rregs with a hard live range crossing
         this insn must be marked as unavailable in the running
         state. */
      for (j = 0; j < rreg_info_used; j++) {
         if (rreg_info[j].live_after < ii 
             && ii < rreg_info[j].dead_before) {
            /* ii is the middle of a hard live range for some real reg.
               Check it's marked as such in the running state. */
            assert(state[rreg_info[j].rreg].disp == Unavail);
         }
      }

      /* Sanity check 2: conversely, all rregs marked as unavailable in
         the running state must have a corresponding hard live range
         entry in the rreg_info array. */
      for (j = 0; j < n_available_real_regs; j++) {
         assert(state[j].disp == Free 
                || state[j].disp == Unavail
                || state[j].disp == Bound);
         if (state[j].disp != Unavail)
            continue;
         for (k = 0; k < rreg_info_used; k++) 
            if (rreg_info[k].rreg == state[j].rreg
                && rreg_info[k].live_after < ii 
                && ii < rreg_info[k].dead_before) 
               break;
         /* If this assertion fails, we couldn't find a correspond
            HLR. */
         assert(k < rreg_info_used);
      }

      /* Sanity check 3: No vreg is bound to more than one rreg. */
      for (j = 0; j < n_state; j++) {
         if (state[j].disp != Bound)
            continue;
         for (k = j+1; k < n_state; k++)
            if (state[k].disp == Bound)
               assert(state[k].vreg != state[j].vreg);
      }

      /* Sanity check 4: all vreg-rreg bindings must bind registers of
         the same class. */
      for (j = 0; j < n_state; j++) {
         if (state[j].disp != Bound)
            continue;
         assert(hregClass(state[j].rreg) == hregClass(state[j].vreg));
         assert( hregIsVirtual(state[j].vreg));
         assert(!hregIsVirtual(state[j].rreg));
      }

      /* ------ end of Sanity checks ------ */

      /* Do various optimisations pertaining to register coalescing
         and preferencing:
            MOV  v -> v   coalescing
            MOV  v -> r   coalescing
         Not yet.
      */

      /* Update the local state.  Expire any v -> r bindings for 
         vregs which have become dead. */
      for (j = 0; j < n_state; j++) {
         if (state[j].disp != Bound)
            continue;
         k = hregNumber(state[j].vreg);
         assert(k >= 0 && k < n_vregs);
         if (vreg_info[k].dead_before == ii) {
           /* It's just gone dead.  Free up the associated rreg. */
           state[j].disp = Free;
           state[j].vreg = INVALID_HREG;
         }
      }

      /* Now we have to deal with rregs which are about to be made
         live by this instruction -- in other words, are entering into
         one of their live ranges.  If any such rreg holds a vreg, we
         will have to spill it in order to free up the rreg.

         Note we could do better:
         * Could move it into some other free rreg, if one is available 
         * Don't bother to spill if the spill-slot value is known to
           be consistent.

         Simplest way to do this is to iterate over the collection
         of rreg live ranges.
      */
      for (j = 0; j < rreg_info_used; j++) {
         if (rreg_info[j].live_after == ii-1) {
            /* rreg_info[j].rreg needs to be freed up.  Find out if
               there is a vreg associated with it. */
            for (k = 0; k < n_state; k++) {
               if (state[k].rreg == rreg_info[j].rreg
                   && state[k].disp == Bound)
                  break;
            }
            if (k < n_state) {
               /* Yes, there is an associated vreg.  Spill it. */
               m = hregNumber(state[k].vreg);
               assert(m >= 0 && m < n_vregs);
               EMIT_INSTR( (*genSpill)( vreg_info[m].spill_offset, 
                                        state[k].rreg ) );
               state[k].disp = Free;
               state[k].vreg = INVALID_HREG;
            }
         }
      }

      /* Finally we can begin the processing of this instruction
         itself.  The aim is to free up enough rregs for this insn.
         This may generate spill stores since we may have to evict
         some vregs currently in rregs.  Also generates spill loads.
         We also build up the final vreg->rreg mapping to be applied
         to the insn. */
      
      (*getRegUsage)( instrs[ii], &reg_usage );

      initHRegRemap(&remap);

      /* for each reg mentioned in the insn ... */
      for (j = 0; j < reg_usage.n_used; j++) {

         vreg = reg_usage.hreg[j];

         /* only interested in virtual registers right now. */
         if (!hregIsVirtual(vreg)) 
            continue;

         /* Now we're trying to find a rreg for "vreg".  First of all,
            if it already has an rreg assigned, we don't need to do
            anything more.  Search the current state to find out. */
         for (k = 0; k < n_state; k++)
            if (state[k].vreg == vreg && state[k].disp == Bound)
               break;
         if (k < n_state) {
            addToHRegRemap(&remap, vreg, state[k].rreg);
            continue;
         }

         /* No luck.  The next thing to do is see if there is a
            currently free rreg available, of the correct class.  If
            so, bag it.  NOTE, we could improve this to select an rreg
            for which the next live-range event is as far ahead as
            possible. */
         for (k = 0; k < n_state; k++)
            if (state[k].disp == Free
                && (hregClass(state[k].rreg) == hregClass(vreg)))
               break;
         if (k < n_state) {
            state[k].disp = Bound;
            state[k].vreg = vreg;
            addToHRegRemap(&remap, vreg, state[k].rreg);
            continue;
         }

         /* There are no free registers, so we're going to have to
            spill a vreg.  We need to make a good choice of vreg to
            spill, and of course we need to be careful not to spill a
            vreg which is needed by this insn. */

         /* First, mark in the state, those rregs which are not spill
            candidates, due to holding a vreg mentioned by this
            instruction.  Or being of the wrong class. */
         for (k = 0; k < n_state; k++) {
            state[k].is_spill_cand = False;
            if (state[k].disp != Bound)
               continue;
            if (hregClass(state[k].rreg) != hregClass(vreg))
               continue;
            state[k].is_spill_cand = True;
            for (m = 0; m < reg_usage.n_used; m++) {
               if (state[k].vreg == reg_usage.hreg[m]) {
                  state[k].is_spill_cand = False;
                  break;
               }
            }
         }

         /* We can choose any rreg satisfying state[r].is_spill_cand
            (so to speak).  Choose rreg so that the next use of its
            associated vreg is as far ahead as possible, in the hope
            that this will minimise the number of consequent reloads
            required.  This is a bit expensive, but we don't have to
            do it very often. */
         furthest_k = -1;
         furthest   = -1;
         for (k = 0; k < n_state; k++) {
            if (!state[k].is_spill_cand)
               continue;
            assert(state[k].disp == Bound);
            for (m = ii+1; m < n_instrs; m++)
               if (instrMentionsReg(getRegUsage, 
                                    instrs[m], state[k].vreg))
                  break;
            if (m > furthest) {
               furthest = m;
               furthest_k = k;
            }
         }

         if (furthest_k == -1) {
            /* Hmmmmm.  There don't appear to be any spill candidates.
               We're hosed. */
            fprintf(stderr, "reg_alloc: can't find a register in class: ");
            ppHRegClass(stderr, hregClass(vreg));
            fprintf(stderr, "\n");
            panic("reg_alloc: can't create a free register.");
         }

         /* Right.  So we're going to spill state[furthest_k]. */
         assert(state[furthest_k].disp == Bound);
         /* check it's the right class */
         assert(hregClass(state[furthest_k].rreg) == hregClass(vreg));
         /* check we're not ejecting the vreg for which we are trying
            to free up a register. */
         assert(state[furthest_k].vreg != vreg);

         m = hregNumber(state[furthest_k].vreg);
         assert(m >= 0 && m < n_vregs);
         /* check that the vreg we're ejecting is still live. */
         assert(vreg_info[m].dead_before > ii);

         /* So here's the spill store. */
         EMIT_INSTR( (*genSpill)( vreg_info[m].spill_offset, 
                                  state[furthest_k].rreg ) );

         /* Update the state to reflect the new assignment for this
            rreg. */
         state[furthest_k].vreg = vreg;

         /* Now, if this vreg is being read or modified (as opposed to
            written), we have to generate a reload for it. */
         if (reg_usage.mode[j] != HRmWrite) {
            m = hregNumber(vreg);
            assert(m >= 0 && m < n_vregs);
            EMIT_INSTR( (*genReload)( vreg_info[m].spill_offset,
                                      state[furthest_k].rreg ) );
         }

         /* So after much twisting and turning, we have vreg mapped to
            state[furthest_k].rreg.  Note that in the map. */
         addToHRegRemap(&remap, vreg, state[furthest_k].rreg);

      } /* iterate over registers in this instruction. */

      /* We've finished clowning around with registers in this instruction.
         Three results:
         - the running state[] has been updated
         - a suitable vreg->rreg mapping for this instruction has been 
           constructed
         - spill and reload instructions may have been emitted.

        The final step is to apply the mapping to the instruction, 
        and emit that.
      */

      /* NOTE, DESTRUCTIVELY MODIFIES instrs[ii]. */
      (*mapRegs)( &remap, instrs[ii] );
      EMIT_INSTR( instrs[ii] );

   } /* iterate over insns */

   EMIT_INSTR(NULL);

   free(state);
   free(rreg_info);
   if (vreg_info) free(vreg_info);

   return instrs_out;

#  undef INVALID_INSTRNO
#  undef EMIT_INSTR
}



/*---------------------------------------------------------------*/
/*---                                             reg_alloc.c ---*/
/*---------------------------------------------------------------*/
