
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (reg_alloc.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "vex_util.h"
#include "host_regs.h"


/* How many 64-bit sized spill slots do we have? */
#define N_SPILL64S  16


/* TODO (critical)
   - Need a way to statically establish the vreg classes,
     else we can't allocate spill slots properly.
   - Better consistency checking from what isMove tells us.
   - We can possibly do V-V coalescing even when the src is spilled,
     providing we can arrange for the dst to have the same spill
     slot.
*/


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
      /* What kind of register this is. */
      HRegClass reg_class;
      /* Preferencing info, if any.  Currently unused. */
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
   void (*getRegUsage) (HRegUsage*, HInstr*),
   HInstr* instr, 
   HReg r 
)
{
   Int       i;
   HRegUsage reg_usage;
   (*getRegUsage)(&reg_usage, instr);
   for (i = 0; i < reg_usage.n_used; i++)
      if (reg_usage.hreg[i] == r)
         return True;
   return False;
}


/* Search forward from some given point in the incoming instruction
   sequence.  Point is to select a virtual register to spill, by
   finding the vreg which is mentioned as far ahead as possible, in
   the hope that this will minimise the number of consequent reloads.

   Only do the search for vregs which are Bound in the running state,
   and for which the .mark field is set.  This allows the caller to
   arbitrarily restrict the set of spill candidates to be considered.

   Returns an index into the state array indicating the (v,r) pair to
   spill, or -1 if none was found.  */
static
Int findMostDistantlyMentionedVReg ( 
   void (*getRegUsage) (HRegUsage*, HInstr*),
   HInstrArray* instrs_in,
   Int          search_from_instr,
   RRegState*   state,
   Int          n_state
)
{
   Int k, m;
   Int furthest_k = -1;
   Int furthest   = -1;
   vassert(search_from_instr >= 0);
   for (k = 0; k < n_state; k++) {
      if (!state[k].is_spill_cand)
         continue;
      vassert(state[k].disp == Bound);
      for (m = search_from_instr; m < instrs_in->arr_used; m++) {
         if (instrMentionsReg(getRegUsage, 
                              instrs_in->arr[m], state[k].vreg))
            break;
      }
      if (m > furthest) {
         furthest   = m;
         furthest_k = k;
      }
   }
   return furthest_k;
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

   Takes an expandable array of pointers to unallocated insns.
   Returns an expandable array of pointers to allocated insns.
*/
HInstrArray* doRegisterAllocation (

   /* Incoming virtual-registerised code. */ 
   HInstrArray* instrs_in,

   /* An array listing all the real registers the allocator may use,
      in no particular order. */
   HReg* available_real_regs,
   Int   n_available_real_regs,

   /* Return True iff the given insn is a reg-reg move, in which
      case also return the src and dst regs. */
   Bool (*isMove) (HInstr*, HReg*, HReg*),

   /* Get info about register usage in this insn. */
   void (*getRegUsage) (HRegUsage*, HInstr*),

   /* Apply a reg-reg mapping to an insn. */
   void (*mapRegs) (HRegRemap*, HInstr*),

   /* Return an insn to spill/restore a real reg to a spill slot
      offset. */
   HInstr* (*genSpill) ( HReg, Int ),
   HInstr* (*genReload) ( HReg, Int ),

   /* For debug printing only. */
   void (*ppInstr) ( HInstr* ),
   void (*ppReg) ( HReg )
)
{
   /* Iterators and temporaries. */
   Int       ii, j, k, m, spillee;
   HReg      rreg, vreg, vregS, vregD;
   HRegUsage reg_usage;

   /* Info on vregs and rregs.  Computed once and remains
      unchanged. */
   VRegInfo* vreg_info;
   RRegInfo* rreg_info;
   Int       rreg_info_size;
   Int       rreg_info_used;
   Int       n_vregs;

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
   HInstrArray* instrs_out;


#  define INVALID_INSTRNO (-2)

#  define EMIT_INSTR(_instr)                  \
      do {                                    \
        HInstr* _tmp = (_instr);              \
        if (0) {                              \
           vex_printf("**  ");                \
           (*ppInstr)(_tmp);                  \
           vex_printf("\n");                  \
        }                                     \
        addHInstr ( instrs_out, _tmp );       \
      } while (0)


   /* --------- Stage 0: set up output array. --------- */
   instrs_out = newHInstrArray();


   /* --------- Stage 1: compute vreg live ranges. --------- */

   /* This is relatively simple, because (1) we only seek the complete
      end-to-end live range of each vreg, and are not interested in
      any holes in it, and (2) the vregs are conveniently numbered 0
      .. n_vregs-1, so we can just dump the results in a pre-allocated
      array. */

   n_vregs = instrs_in->n_vregs;
   vreg_info = NULL;
   if (n_vregs > 0)
      vreg_info = LibVEX_Alloc(sizeof(VRegInfo) * n_vregs);

   for (j = 0; j < n_vregs; j++) {
      vreg_info[j].live_after     = INVALID_INSTRNO;
      vreg_info[j].dead_before    = INVALID_INSTRNO;
      vreg_info[j].spill_offset   = 0;
      vreg_info[j].spill_size     = 0;
      vreg_info[j].reg_class      = HRcINVALID;
      vreg_info[j].has_preference = False;
      vreg_info[j].preferred_rreg = INVALID_HREG;
   }

   /* for each insn ... */
   for (ii = 0; ii < instrs_in->arr_used; ii++) {

      (*getRegUsage)( &reg_usage, instrs_in->arr[ii] );

#     if 0
      vex_printf("\n%d  stage1: ", ii);
      (*ppInstr)(instrs_in->arr[ii]);
      vex_printf("\n");
      ppHRegUsage(&reg_usage);
#     endif

      /* for each reg mentioned in the insn ... */
      for (j = 0; j < reg_usage.n_used; j++) {

         vreg = reg_usage.hreg[j];
         /* only interested in virtual registers right now. */
         if (!hregIsVirtual(vreg))
            continue;
         k = hregNumber(vreg);
         if (k < 0 || k >= n_vregs)
            vpanic("doRegisterAllocation: out-of-range vreg");

         /* Take the opportunity to note its regclass.  We'll need
            that when allocating spill slots. */
         if (vreg_info[k].reg_class == HRcINVALID) {
            /* First mention of this vreg. */
            vreg_info[k].reg_class = hregClass(vreg);
         } else {
            /* Seen it before, so check for consistency. */
            vassert(vreg_info[k].reg_class == hregClass(vreg));
         }

         /* Now consider live ranges. */
         switch (reg_usage.mode[j]) {
            case HRmRead: 
               if (vreg_info[k].live_after == INVALID_INSTRNO)
                  vpanic("doRegisterAllocation: "
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
                  vpanic("doRegisterAllocation: "
                         "first event for vreg is Modify");
               vreg_info[k].dead_before = ii + 1;
               break;
            default:
               vpanic("doRegisterAllocation(1)");
         } /* switch */

      } /* iterate over registers */

   } /* iterate over insns */

#  if 0
   for (j = 0; j < n_vregs; j++) {
      vex_printf("vreg %d:  la = %d,  db = %d\n", 
                 j, vreg_info[j].live_after, vreg_info[j].dead_before );
   }
#  endif

   /* --------- Stage 2: compute rreg live ranges. --------- */

   /* This is more complex than Stage 1, because we need to compute
      exactly all the live ranges of all the allocatable real regs,
      and we don't know in advance how many there will be. */

   rreg_info_used = 0;
   rreg_info_size = 4;
   rreg_info = LibVEX_Alloc(rreg_info_size * sizeof(RRegInfo));

   /* We'll need to track live range start/end points seperately for
      each rreg.  Sigh. */
   vassert(n_available_real_regs > 0);
   rreg_live_after  = LibVEX_Alloc(n_available_real_regs * sizeof(Int));
   rreg_dead_before = LibVEX_Alloc(n_available_real_regs * sizeof(Int));

   for (j = 0; j < n_available_real_regs; j++)
      rreg_live_after[j] = 
      rreg_dead_before[j] = INVALID_INSTRNO;

   /* for each insn ... */
   for (ii = 0; ii < instrs_in->arr_used; ii++) {

      (*getRegUsage)( &reg_usage, instrs_in->arr[ii] );

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
               flush_la = rreg_live_after[k];
               flush_db = rreg_dead_before[k];
               if (flush_la != INVALID_INSTRNO 
                   && flush_db != INVALID_INSTRNO)
                  flush = True;
               rreg_live_after[k]  = ii;
               rreg_dead_before[k] = ii+1;
               break;
            case HRmRead:
               if (rreg_live_after[k] == INVALID_INSTRNO)
                  vpanic("doRegisterAllocation: "
                         "first event for rreg is Read");
               rreg_dead_before[k] = ii;
               break;
            case HRmModify:
               if (rreg_live_after[k] == INVALID_INSTRNO)
                  vpanic("doRegisterAllocation: "
                         "first event for rreg is Modify");
               rreg_dead_before[k] = ii+1;
               break;
            default:
               vpanic("doRegisterAllocation(2)");
         }

         if (flush) {
            vassert(flush_la != INVALID_INSTRNO);
            vassert(flush_db != INVALID_INSTRNO);
            vex_printf("FLUSH 1 (%d,%d)\n", flush_la, flush_db);
            if (rreg_info_used == rreg_info_size) {
               vpanic("make rreg info array bigger(1)");
            }
            rreg_info[rreg_info_used].rreg        = rreg;
            rreg_info[rreg_info_used].live_after  = flush_la;
            rreg_info[rreg_info_used].dead_before = flush_db;
            rreg_info_used++;
         }

      } /* iterate over regs in the instr */

   } /* iterate over instrs */

   /* Now finish up any live ranges left over. */
   for (j = 0; j < n_available_real_regs; j++) {

#     if 0
      vex_printf("residual %d:  %d %d\n", j, rreg_live_after[j],
                                             rreg_dead_before[j]);
#     endif 
      vassert( (rreg_live_after[j] == INVALID_INSTRNO 
               && rreg_dead_before[j] == INVALID_INSTRNO)
              ||
              (rreg_live_after[j] != INVALID_INSTRNO 
               && rreg_dead_before[j] != INVALID_INSTRNO)
            );

      if (rreg_live_after[j] == INVALID_INSTRNO)
         continue;
      if (rreg_info_used == rreg_info_size) {
         vpanic("make rreg info array bigger(2)");
      }
      rreg_info[rreg_info_used].rreg        = available_real_regs[j];
      rreg_info[rreg_info_used].live_after  = rreg_live_after[j];
      rreg_info[rreg_info_used].dead_before = rreg_dead_before[j];
      rreg_info_used++;
   }

   /* free(rreg_live_after); */
   /* free(rreg_dead_before); */

#  if 0
   for (j = 0; j < rreg_info_used; j++) {
      (*ppReg)(rreg_info[j].rreg);
      vex_printf("      la = %d,  db = %d\n",
                 rreg_info[j].live_after, rreg_info[j].dead_before );
   }
#  endif

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

      /* True iff this vreg is unused.  In which case we also expect
         that the reg_class field for it has not been set.  */
      if (vreg_info[j].live_after == INVALID_INSTRNO) {
         vassert(vreg_info[j].reg_class == HRcINVALID);
         continue;
      }

      /* Need to allocate two 64-bit spill slots for this. */
      if (vreg_info[j].reg_class == HRcVector128)
         vpanic("can't deal with spilling 128-bit values (yet)");

      /* Find the lowest-numbered spill slot which is available at the
         start point of this interval, and assign the interval to
         it. */
      for (k = 0; k < N_SPILL64S; k++)
         if (ss_busy_until_before[k] <= vreg_info[j].live_after)
            break;
      if (k == N_SPILL64S) {
         vpanic("N_SPILL64S is too low");
      }
      ss_busy_until_before[k] = vreg_info[j].dead_before;
      vreg_info[j].spill_offset = k * 8;
      /* if (j > max_ss_no) */
      /*    max_ss_no = j; */
   }

#  if 0
   vex_printf("\n\n");
   for (j = 0; j < n_vregs; j++)
      vex_printf("vreg %d    --> spill offset %d\n",
                 j, vreg_info[j].spill_offset);
#  endif

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
   state = LibVEX_Alloc(n_available_real_regs * sizeof(RRegState));

   for (j = 0; j < n_state; j++) {
      state[j].rreg          = available_real_regs[j];
      state[j].disp          = Free;
      state[j].vreg          = INVALID_HREG;
      state[j].is_spill_cand = False;
   }

   /* ------ BEGIN: Process each insn in turn. ------ */

   for (ii = 0; ii < instrs_in->arr_used; ii++) {

#     if 0
      vex_printf("\n-----------\n%d   ", ii);
      (*ppInstr)(instrs_in->arr[ii]);
      vex_printf("\n");
      for (j = 0; j < n_state; j++) {
         (*ppReg)(state[j].rreg);
         vex_printf("\t  ");
         switch (state[j].disp) {
            case Free:    vex_printf("Free\n"); break;
            case Unavail: vex_printf("Unavail\n"); break;
            case Bound:   vex_printf("BoundTo "); 
                          (*ppReg)(state[j].vreg);
                          vex_printf("\n"); break;
         }
      }
      vex_printf("\n");
#     endif

      /* ------ Sanity checks ------ */

      /* Sanity check 1: all rregs with a hard live range crossing
         this insn must be marked as unavailable in the running
         state. */
      for (j = 0; j < rreg_info_used; j++) {
         if (rreg_info[j].live_after < ii 
             && ii < rreg_info[j].dead_before) {
            /* ii is the middle of a hard live range for some real reg.
               Check it's marked as such in the running state. */
            vassert(state[rreg_info[j].rreg].disp == Unavail);
         }
      }

      /* Sanity check 2: conversely, all rregs marked as unavailable in
         the running state must have a corresponding hard live range
         entry in the rreg_info array. */
      for (j = 0; j < n_available_real_regs; j++) {
         vassert(state[j].disp == Free 
                || state[j].disp == Unavail
                || state[j].disp == Bound);
         if (state[j].disp != Unavail)
            continue;
         for (k = 0; k < rreg_info_used; k++) 
            if (rreg_info[k].rreg == state[j].rreg
                && rreg_info[k].live_after < ii 
                && ii < rreg_info[k].dead_before) 
               break;
         /* If this vassertion fails, we couldn't find a correspond
            HLR. */
         vassert(k < rreg_info_used);
      }

      /* Sanity check 3: No vreg is bound to more than one rreg. */
      for (j = 0; j < n_state; j++) {
         if (state[j].disp != Bound)
            continue;
         for (k = j+1; k < n_state; k++)
            if (state[k].disp == Bound)
               vassert(state[k].vreg != state[j].vreg);
      }

      /* Sanity check 4: all vreg-rreg bindings must bind registers of
         the same class. */
      for (j = 0; j < n_state; j++) {
         if (state[j].disp != Bound)
            continue;
         vassert(hregClass(state[j].rreg) == hregClass(state[j].vreg));
         vassert( hregIsVirtual(state[j].vreg));
         vassert(!hregIsVirtual(state[j].rreg));
      }

      /* ------ end of Sanity checks ------ */

      /* Do various optimisations pertaining to register coalescing
         and preferencing:
            MOV  v <-> v   coalescing (done here).
            MOV  v <-> r   coalescing (not yet, if ever)
      */
      /* If doing a reg-reg move between two vregs, and the src's live
         range ends here and the dst's live range starts here, bind
         the dst to the src's rreg, and that's all. */
      if ( (*isMove)( instrs_in->arr[ii], &vregS, &vregD ) ) {
         if (!hregIsVirtual(vregS)) goto cannot_coalesce;
         if (!hregIsVirtual(vregD)) goto cannot_coalesce;
         /* Check that *isMove is not telling us a bunch of lies ... */
         vassert(hregClass(vregS) == hregClass(vregD));
         k = hregNumber(vregS);
         m = hregNumber(vregD);
         vassert(k >= 0 && k < n_vregs);
         vassert(m >= 0 && m < n_vregs);
         if (vreg_info[k].dead_before != ii) goto cannot_coalesce;
         if (vreg_info[m].live_after != ii) goto cannot_coalesce;
#        if 0
         vex_printf("COALESCE ");
         (*ppReg)(vregS);
         vex_printf(" -> ");
         (*ppReg)(vregD);
         vex_printf("\n");
#        endif
         /* Find the state entry for vregS. */
         for (m = 0; m < n_state; m++)
            if (state[m].disp == Bound && state[m].vreg == vregS)
               break;
         if (m == n_state)
            /* We failed to find a binding for vregS, which means it's
               currently not in a register.  So we can't do the
               coalescing.  Give up. */
            goto cannot_coalesce;

         /* Finally, we can do the coalescing.  It's trivial -- merely
            claim vregS's register for vregD. */
         state[m].vreg = vregD;
         /* Don't bother to copy this insn, just move on to the next
            insn. */
         continue;
      }
     cannot_coalesce:

      /* ------ Pre-instruction actions for fixed rreg uses ------ */

      /* Now we have to deal with rregs which are about to be made
         live by this instruction -- in other words, are entering into
         one of their live ranges.  If any such rreg holds a vreg, we
         will have to free up the rreg.  The simplest solution which
         is correct is to spill the rreg.

         Note we could do better:
         * Could move it into some other free rreg, if one is available 
         * Don't bother to spill if the spill-slot value is known to
           be consistent.
         * If the associated vreg live range ends at this insn,
           no point in spilling or moving, since (in principle) the
           rreg will be free anyway after that.

         Simplest way to do this is to iterate over the collection
         of rreg live ranges.
      */
      for (j = 0; j < rreg_info_used; j++) {
         if (rreg_info[j].live_after == ii) {
            /* rreg_info[j].rreg needs to be freed up.  Find 
               the associated state entry. */
            /* Note, re rreg_info[j].live_after == ii.  Real register
               live ranges are guaranteed to be well-formed in that
               they start with a write to the register -- Stage 2
               rejects any code not satisfying this.  So the correct
               question to ask is whether rreg_info[j].live_after ==
               ii, that is, whether the reg becomes live after this
               insn -- rather than before it. */
#           if 0
            vex_printf("need to free up rreg: ");
            (*ppReg)(rreg_info[j].rreg);
            vex_printf("\n");
#           endif
            for (k = 0; k < n_state; k++)
               if (state[k].rreg == rreg_info[j].rreg)
                  break;
            /* If this fails, we don't have an entry for this rreg.
               Which we should. */
            vassert(k < n_state);
            if (state[k].disp == Bound) {
               /* Yes, there is an associated vreg.  Spill it if it's
                  still live. */
               m = hregNumber(state[k].vreg);
               vassert(m >= 0 && m < n_vregs);
               if (vreg_info[m].dead_before > ii) {
                  vassert(vreg_info[m].reg_class != HRcINVALID);
                  EMIT_INSTR( (*genSpill)( state[k].rreg,
                                           vreg_info[m].spill_offset ) );
               }
            }
            state[k].disp = Unavail;
            state[k].vreg = INVALID_HREG;
         }
      }

      /* ------ Deal with the current instruction. ------ */

      /* Finally we can begin the processing of this instruction
         itself.  The aim is to free up enough rregs for this insn.
         This may generate spill stores since we may have to evict
         some vregs currently in rregs.  Also generates spill loads.
         We also build up the final vreg->rreg mapping to be applied
         to the insn. */
      
      (*getRegUsage)( &reg_usage, instrs_in->arr[ii] );

      initHRegRemap(&remap);

      /* for each reg mentioned in the insn ... */
      for (j = 0; j < reg_usage.n_used; j++) {

         vreg = reg_usage.hreg[j];

         /* only interested in virtual registers right now. */
         if (!hregIsVirtual(vreg)) 
            continue;

#        if 0
         vex_printf("considering "); (*ppReg)(vreg); vex_printf("\n");
#        endif

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
            so, bag it.  NOTE, we could improve this by selecting an
            rreg for which the next live-range event is as far ahead
            as possible. */
         for (k = 0; k < n_state; k++) {
            if (state[k].disp == Free
                && hregClass(state[k].rreg) == hregClass(vreg))
               break;
         }
         if (k < n_state) {
            state[k].disp = Bound;
            state[k].vreg = vreg;
            addToHRegRemap(&remap, vreg, state[k].rreg);
            /* Generate a reload if needed. */
            if (reg_usage.mode[j] != HRmWrite) {
               m = hregNumber(vreg);
               vassert(m >= 0 && m < n_vregs);
               vassert(vreg_info[m].reg_class != HRcINVALID);
               EMIT_INSTR( (*genReload)( state[k].rreg,
                                         vreg_info[m].spill_offset ) );
            }
            continue;
         }

         /* There are no free rregs, but perhaps we can find one which
            is bound to a vreg which is now dead.  If so, use that.
            NOTE, we could improve this by selecting an rreg for which
            the next live-range event is as far ahead as possible. */
         for (k = 0; k < n_state; k++) {
            if (state[k].disp == Bound
                && hregClass(state[k].rreg) == hregClass(vreg)) {
               m = hregNumber(state[k].vreg);
               vassert(m >= 0 && m < n_vregs);
               if (vreg_info[m].dead_before <= ii) {
                  /* Ok, it's gone dead before this insn.  We can use
                     it. */
                  break;
               }
            }
         }
         if (k < n_state) {
            vassert(state[k].disp == Bound);
            state[k].vreg = vreg;
            addToHRegRemap(&remap, vreg, state[k].rreg);
            /* Generate a reload if needed. */
            if (reg_usage.mode[j] != HRmWrite) {
               m = hregNumber(vreg);
               vassert(m >= 0 && m < n_vregs);
               vassert(vreg_info[m].reg_class != HRcINVALID);
               EMIT_INSTR( (*genReload)( state[k].rreg,
                                         vreg_info[m].spill_offset ) );
            }
            continue;
         }

         /* Well, now we have no option but to spill a vreg.  It's
            important to make a good choice of vreg to spill, and of
            course we need to be careful not to spill a vreg which is
            needed by this insn. */

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

         /* We can choose to spill any rreg satisfying
            state[r].is_spill_cand (so to speak).  Choose r so that
            the next use of its associated vreg is as far ahead as
            possible, in the hope that this will minimise the number
            of consequent reloads required. */
         spillee
            = findMostDistantlyMentionedVReg ( 
                 getRegUsage, instrs_in, ii+1, state, n_state );

         if (spillee == -1) {
            /* Hmmmmm.  There don't appear to be any spill candidates.
               We're hosed. */
            vex_printf("reg_alloc: can't find a register in class: ");
            ppHRegClass(hregClass(vreg));
            vex_printf("\n");
            vpanic("reg_alloc: can't create a free register.");
         }

         /* Right.  So we're going to spill state[spillee]. */
         vassert(spillee >= 0 && spillee < n_state);
         vassert(state[spillee].disp == Bound);
         /* check it's the right class */
         vassert(hregClass(state[spillee].rreg) == hregClass(vreg));
         /* check we're not ejecting the vreg for which we are trying
            to free up a register. */
         vassert(state[spillee].vreg != vreg);

         m = hregNumber(state[spillee].vreg);
         vassert(m >= 0 && m < n_vregs);

         /* So here's the spill store.  Assert that we're spilling a
            live vreg. */
         vassert(vreg_info[m].dead_before > ii);
         vassert(vreg_info[m].reg_class != HRcINVALID);
         EMIT_INSTR( (*genSpill)( state[spillee].rreg,
                                  vreg_info[m].spill_offset ) );

         /* Update the state to reflect the new assignment for this
            rreg. */
         state[spillee].vreg = vreg;

         /* Now, if this vreg is being read or modified (as opposed to
            written), we have to generate a reload for it. */
         if (reg_usage.mode[j] != HRmWrite) {
            m = hregNumber(vreg);
            vassert(m >= 0 && m < n_vregs);
            vassert(vreg_info[m].reg_class != HRcINVALID);
            EMIT_INSTR( (*genReload)( state[spillee].rreg,
                                      vreg_info[m].spill_offset ) );
                                      
         }

         /* So after much twisting and turning, we have vreg mapped to
            state[furthest_k].rreg.  Note that in the map. */
         addToHRegRemap(&remap, vreg, state[spillee].rreg);

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

      /* NOTE, DESTRUCTIVELY MODIFIES instrs_in->arr[ii]. */
      (*mapRegs)( &remap, instrs_in->arr[ii] );
      EMIT_INSTR( instrs_in->arr[ii] );

      /* ------ Post-instruction actions for fixed rreg uses ------ */

      /* Now we need to check for rregs exiting fixed live ranges
         after this instruction, and if so mark them as free. */

      for (j = 0; j < rreg_info_used; j++) {
         if (rreg_info[j].dead_before == ii+1) {
            /* rreg_info[j].rreg is exiting a hard live range.  Mark
               it as such in the main state array. */
            for (k = 0; k < n_state; k++)
               if (state[k].rreg == rreg_info[j].rreg)
                  break;
            /* If this vassertion fails, we don't have an entry for
               this rreg.  Which we should. */
            vassert(k < n_state);
            vassert(state[k].disp == Unavail);
            state[k].disp = Free;
            state[k].vreg = INVALID_HREG;
         }
      }

   } /* iterate over insns */

   /* ------ END: Process each insn in turn. ------ */

   /* free(state); */
   /* free(rreg_info); */
   /* if (vreg_info) free(vreg_info); */

   return instrs_out;

#  undef INVALID_INSTRNO
#  undef EMIT_INSTR
}



/*---------------------------------------------------------------*/
/*---                                             reg_alloc.c ---*/
/*---------------------------------------------------------------*/
