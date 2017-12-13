/*----------------------------------------------------------------------------*/
/*--- begin                                      host_generic_reg_alloc3.c ---*/
/*----------------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation framework.

   Copyright (C) 2017-2017 Ivo Raisr
      ivosh@ivosh.net

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "main_util.h"
#include "host_generic_regs.h"

/* Set to 1 for lots of debugging output. */
#define DEBUG_REGALLOC 0

/* Set to 1 for sanity checking at every instruction.
   Set to 0 for sanity checking only every 17th one and the last one. */
#define SANITY_CHECKS_EVERY_INSTR 0


#define INVALID_INSTRNO (-2)
#define INVALID_INDEX (-2)

/* Register allocator state is kept in an array of VRegState's.
   There is an element for every virtual register (vreg).
   Elements are indexed [0 .. n_vregs-1].
   Records information about vreg live range and its state. */
typedef
   struct {
      /* Live range, register class and spill offset are computed during the
         first register allocator pass and remain unchanged after that. */

      /* This vreg becomes live with this instruction (inclusive). Contains
         either an instruction number or INVALID_INSTRNO. */
      Short live_after;
      /* This vreg becomes dead before this instruction (exclusive). Contains
         either an instruction number or INVALID_INSTRNO. */
      Short dead_before;
      /* What kind of register this is. */
      HRegClass reg_class;

      /* What is its current disposition? */
      enum { Unallocated, /* Neither spilled nor assigned to a real reg. */
             Assigned,    /* Assigned to a real register, viz rreg. */
             Spilled      /* Spilled to the spill slot. */
           } disp;

      /* If .disp == Assigned, what rreg is it bound to? */
      HReg rreg;

      /* The "home" spill slot. The offset is relative to the beginning of
         the guest state. */
      UShort spill_offset;

      /* This vreg (vregS) is coalesced to another vreg
         if |coalescedTo| != INVALID_HREG.
         Coalescing means that there is a MOV instruction which occurs in the
         instruction stream right at vregS' dead_before
         and vregD's live_after. */
      HReg coalescedTo;    /* Which vreg it is coalesced to. */
      HReg coalescedFirst; /* First vreg in the coalescing chain. */

      /* If this vregS is coalesced to another vregD, what is the combined
         dead_before for vregS+vregD. Used to effectively allocate registers. */
      Short effective_dead_before;
   }
   VRegState;

/* The allocator also maintains a redundant array of indexes (rreg_state) from
   rreg numbers back to entries in vreg_state. It is redundant because iff
   rreg_state[r] == v then hregNumber(vreg_state[v].rreg) == r -- that is, the
   two entries point at each other. The purpose of this is to speed up
   activities which involve looking for a particular rreg: there is no need to
   scan the vreg_state looking for it, just index directly into rreg_state.
   The FAQ "does this rreg already have an associated vreg" is the main
   beneficiary.
   The identity of the real register is not recorded here, because the index
   of this structure in |rreg_state| is the index number of the register, and
   the register itself can be extracted from the RRegUniverse (univ). */
typedef
   struct {
      /* What is its current disposition? */
      enum { Free,     /* Not bound to any vreg. */
             Bound,    /* Bound to a vreg, viz vreg. */
             Reserved  /* Reserved for an instruction. */
           } disp;

      /* If .disp == Bound, what vreg is it bound to? */
      HReg vreg;

      /* If .disp == Bound, has the associated vreg been reloaded from its spill
         slot recently and is this rreg still equal to that spill slot?
         Avoids unnecessary spilling that vreg later, when this rreg needs
         to be reserved. */
      Bool eq_spill_slot;
   }
   RRegState;

/* Records information on a real-register live range, associated with
   a particular real register. Computed once; does not change. */
typedef
   struct {
      /* This rreg becomes live with this instruction (inclusive). Contains
         either an instruction number or INVALID_INSTRNO. */
      Short live_after;
      /* This rreg becomes dead before this instruction (exclusive). Contains
         either an instruction number or INVALID_INSTRNO. */
      Short dead_before;
   }
   RRegLR;

/* Live ranges for a single rreg and the current one.
   Live ranges are computed during the first register allocator pass and remain
   unchanged after that.
   The identity of the real register is not recorded here, because the index
   of this structure in |rreg_lr_state| is the index number of the register, and
   the register itself can be extracted from the RRegUniverse (univ). */
typedef
   struct {
      RRegLR* lrs;
      UInt    lrs_size;
      UInt    lrs_used;

      /* Live range corresponding to the currently processed instruction.
         Points into |lrs| array. */
      RRegLR  *lr_current;
      UInt     lr_current_idx;
   }
   RRegLRState;

#define IS_VALID_VREGNO(v) ((v) >= 0 && (v) < n_vregs)
#define IS_VALID_RREGNO(r) ((r) >= 0 && (r) < n_rregs)

#define FREE_VREG(v)             \
   do {                          \
      (v)->disp = Unallocated;   \
      (v)->rreg = INVALID_HREG;  \
   } while (0)

#define FREE_RREG(r)                      \
   do {                                   \
      (r)->disp          = Free;          \
      (r)->vreg          = INVALID_HREG;  \
      (r)->eq_spill_slot = False;         \
   } while (0)


/* Compute the index of the highest and lowest 1 in a ULong, respectively.
   Results are undefined if the argument is zero. Don't pass it zero :) */
static inline UInt ULong__maxIndex ( ULong w64 ) {
   return 63 - __builtin_clzll(w64);
}

static inline UInt ULong__minIndex ( ULong w64 ) {
   return __builtin_ctzll(w64);
}

static inline void enlarge_rreg_lrs(RRegLRState* rreg_lrs)
{
   vassert(rreg_lrs->lrs_used == rreg_lrs->lrs_size);

   RRegLR* lr2 = LibVEX_Alloc_inline(2 * rreg_lrs->lrs_used * sizeof(RRegLR));
   for (UInt l = 0; l < rreg_lrs->lrs_used; l++) {
      lr2[l] = rreg_lrs->lrs[l];
   }

   rreg_lrs->lrs = lr2;
   rreg_lrs->lrs_size = 2 * rreg_lrs->lrs_used;
}

#define PRINT_STATE                                              \
   do {                                                          \
      print_state(con, vreg_state, n_vregs, rreg_state, n_rregs, \
                  rreg_lr_state, ii);                            \
   } while (0)

static inline void print_state(
   const RegAllocControl* con,
   const VRegState* vreg_state, UInt n_vregs,
   const RRegState* rreg_state, UInt n_rregs,
   const RRegLRState* rreg_lr_state,
   UShort current_ii)
{
#  define RIGHT_JUSTIFY(_total, _written)                   \
      do {                                                  \
         for (Int w = (_total) - (_written); w > 0; w--) {  \
            vex_printf(" ");                                \
         }                                                  \
      } while (0)

   for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
      const VRegState* vreg = &vreg_state[v_idx];

      if (vreg->live_after == INVALID_INSTRNO) {
         continue; /* This is a dead vreg. Never comes into live. */
      }
      vex_printf("vreg_state[%3u]    ", v_idx);

      UInt written;
      switch (vreg->disp) {
      case Unallocated:
         written = vex_printf("unallocated");
         break;
      case Assigned:
         written = vex_printf("assigned to ");
         written += con->ppReg(vreg->rreg);
         break;
      case Spilled:
         written = vex_printf("spilled at offset %u", vreg->spill_offset);
         break;
      default:
         vassert(0);
      }
      RIGHT_JUSTIFY(25, written);

      written = vex_printf("lr: [%d, %d) ",
                           vreg->live_after, vreg->dead_before);
      RIGHT_JUSTIFY(15, written);

      written = vex_printf("effective lr: [%d, %d)",
                           vreg->live_after, vreg->effective_dead_before);
      RIGHT_JUSTIFY(25, written);

      if (vreg->live_after > (Short) current_ii) {
         vex_printf("[not live yet]\n");
      } else if ((Short) current_ii >= vreg->dead_before) {
         if (hregIsInvalid(vreg->coalescedTo)) {
            vex_printf("[now dead]\n");
         } else {
            vex_printf("[now dead, coalesced to ");
            con->ppReg(vreg->coalescedTo);
            vex_printf("]\n");
         }
      } else {
         vex_printf("[live]\n");
      }
   }

   for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
      const RRegState*   rreg     = &rreg_state[r_idx];
      const RRegLRState* rreg_lrs = &rreg_lr_state[r_idx];
      vex_printf("rreg_state[%2u] = ", r_idx);
      UInt written = con->ppReg(con->univ->regs[r_idx]);
      RIGHT_JUSTIFY(10, written);

      switch (rreg->disp) {
      case Free:
         vex_printf("free\n");
         break;
      case Bound:
         vex_printf("bound for ");
         con->ppReg(rreg->vreg);
         if (rreg->eq_spill_slot) {
            vex_printf("    (equals to its spill slot)");
         }
         vex_printf("\n");
         break;
      case Reserved:
         vex_printf("reserved - live range [%d, %d)\n",
                    rreg_lrs->lr_current->live_after,
                    rreg_lrs->lr_current->dead_before);
         break;
      }
   }

#  undef RIGHT_JUSTIFY
}

static inline void emit_instr(HInstr* instr, HInstrArray* instrs_out,
                              const RegAllocControl* con, const HChar* why)
{
   if (DEBUG_REGALLOC) {
      vex_printf("**  ");
      con->ppInstr(instr, con->mode64);
      if (why != NULL) {
         vex_printf("          (%s)", why);
      }
      vex_printf("\n\n");
   }

   addHInstr(instrs_out, instr);
}

/* Updates register allocator state after vreg has been spilled. */
static inline void mark_vreg_spilled(
   UInt v_idx, VRegState* vreg_state, UInt n_vregs,
   RRegState* rreg_state, UInt n_rregs)
{
   HReg rreg = vreg_state[v_idx].rreg;
   UInt r_idx = hregIndex(rreg);

   vreg_state[v_idx].disp = Spilled;
   vreg_state[v_idx].rreg = INVALID_HREG;
   FREE_RREG(&rreg_state[r_idx]);
}

/* Spills a vreg assigned to some rreg.
   The vreg is spilled and the rreg is freed.
   Returns rreg's index. */
static inline UInt spill_vreg(
   HReg vreg, UInt v_idx, UInt current_ii, VRegState* vreg_state, UInt n_vregs,
   RRegState* rreg_state, UInt n_rregs, HInstrArray* instrs_out,
   const RegAllocControl* con)
{
   /* Check some invariants first. */
   vassert(IS_VALID_VREGNO((v_idx)));
   vassert(vreg_state[v_idx].disp == Assigned);
   HReg rreg = vreg_state[v_idx].rreg;
   UInt r_idx = hregIndex(rreg);
   vassert(IS_VALID_RREGNO(r_idx));
   vassert(hregClass(con->univ->regs[r_idx]) == hregClass(vreg));
   vassert(vreg_state[v_idx].dead_before > (Short) current_ii);
   vassert(vreg_state[v_idx].reg_class != HRcINVALID);

   /* Generate spill. */
   HInstr* spill1 = NULL;
   HInstr* spill2 = NULL;
   con->genSpill(&spill1, &spill2, rreg, vreg_state[v_idx].spill_offset,
                 con->mode64);
   vassert(spill1 != NULL || spill2 != NULL); /* cannot be both NULL */
   if (spill1 != NULL) {
      emit_instr(spill1, instrs_out, con, "spill1");
   }
   if (spill2 != NULL) {
      emit_instr(spill2, instrs_out, con, "spill2");
   }

   mark_vreg_spilled(v_idx, vreg_state, n_vregs, rreg_state, n_rregs);
   return r_idx;
}

/* Chooses a vreg to be spilled based on various criteria.
   The vreg must not be from the instruction being processed, that is, it must
   not be listed in reg_usage->vRegs. */
static inline HReg find_vreg_to_spill(
   VRegState* vreg_state, UInt n_vregs,
   RRegState* rreg_state, UInt n_rregs,
   const HRegUsage* instr_regusage, HRegClass target_hregclass,
   const HRegUsage* reg_usage, UInt scan_forward_from, UInt scan_forward_max,
   const RegAllocControl* con)
{
   /* Scan forwards a few instructions to find the most distant mentioned
      use of a vreg. We can scan in the range of (inclusive):
      - reg_usage[scan_forward_from]
      - reg_usage[scan_forward_end], where scan_forward_end
           = MIN(scan_forward_max, scan_forward_from + FEW_INSTRUCTIONS). */
#  define FEW_INSTRUCTIONS 20
   UInt scan_forward_end
      = (scan_forward_max <= scan_forward_from + FEW_INSTRUCTIONS) ?
        scan_forward_max : scan_forward_from + FEW_INSTRUCTIONS;
#  undef FEW_INSTRUCTIONS

   HReg vreg_found = INVALID_HREG;
   UInt distance_so_far = 0;

   for (UInt r_idx = con->univ->allocable_start[target_hregclass];
        r_idx <= con->univ->allocable_end[target_hregclass]; r_idx++) {
      if (rreg_state[r_idx].disp == Bound) {
         HReg vreg = rreg_state[r_idx].vreg;
         if (! HRegUsage__contains(instr_regusage, vreg)) {
            UInt ii = scan_forward_from;
            for ( ; ii <= scan_forward_end; ii++) {
               if (HRegUsage__contains(&reg_usage[ii], vreg)) {
                  break;
               }
            }

            if (ii >= distance_so_far) {
               distance_so_far = ii;
               vreg_found = vreg;
               if (distance_so_far == scan_forward_end) {
                  break; /* We are at the end. Nothing could be better. */
               }
            }
         }
      }
   }

   if (hregIsInvalid(vreg_found)) {
      vex_printf("doRegisterAllocation_v3: cannot find a register in class: ");
      ppHRegClass(target_hregclass);
      vex_printf("\n");
      vpanic("doRegisterAllocation_v3: cannot find a register.");
   }

   return vreg_found;
}

/* Find a free rreg of the correct class.
   Tries to find an rreg whose hard live range (if any) starts after the vreg's
   live range ends. If that is not possible, then at least whose live range
   is as far ahead in the incoming instruction stream as possible.
   An ideal rreg candidate is a caller-save register for short-lived vregs
   and a callee-save register for long-lived vregs because it won't need to
   be spilled around helper calls. */
static Int find_free_rreg(
   const VRegState* vreg_state, UInt n_vregs,
   const RRegState* rreg_state, UInt n_rregs,
   const RRegLRState* rreg_lr_state,
   UInt v_idx, UInt current_ii, HRegClass target_hregclass,
   Bool reserve_phase, const RegAllocControl* con)
{
   Int r_idx_found = INVALID_INDEX;
   UInt distance_so_far = 0; /* running max for |live_after - current_ii| */
   const VRegState* vreg = &vreg_state[v_idx];

   /* Assume majority of vregs are short-lived. Start scannig from caller-save
      registers first. */
   for (Int r_idx = (Int) con->univ->allocable_end[target_hregclass];
        r_idx >= (Int) con->univ->allocable_start[target_hregclass]; r_idx--) {
      const RRegState*   rreg     = &rreg_state[r_idx];
      const RRegLRState* rreg_lrs = &rreg_lr_state[r_idx];
      if (rreg->disp == Free) {
         if (rreg_lrs->lrs_used == 0) {
            r_idx_found = r_idx;
            break; /* There could be nothing better, so break now. */
         } else {
            const RRegLR* lr = rreg_lrs->lr_current;
            if (lr->live_after > (Short) current_ii) {
               /* RReg's hard live range is not live, yet. */
               if (vreg->effective_dead_before <= lr->live_after) {
                  r_idx_found = r_idx;
                  break; /* VReg is short-lived; it fits in. */
               }
               if ((lr->live_after - (Short) current_ii) > distance_so_far) {
                  distance_so_far = lr->live_after - (Short) current_ii;
                  r_idx_found = r_idx;
               }
            } else if ((Short) current_ii >= lr->dead_before) {
               /* Now dead. Effectively as if there is no LR now. */
               r_idx_found = r_idx;
               break; /* There could be nothing better, so break now. */
            } else {
               /* Going live for this instruction. This could happen only when
                  rregs are being reserved en mass, for example before
                  a helper call. */
               vassert(reserve_phase);
            }
         }
      }
   }

   return r_idx_found;
}

/* A target-independent register allocator (v3). Requires various functions
   which it uses to deal abstractly with instructions and registers, since it
   cannot have any target-specific knowledge.

   Returns a new list of instructions, which, as a result of the behaviour of
   mapRegs, will be in-place modifications of the original instructions.

   Requires that the incoming code has been generated using vreg numbers
   0, 1 .. n_vregs-1. Appearance of a vreg outside that range is a checked
   run-time error.

   Takes unallocated instructions and returns allocated instructions.
*/
HInstrArray* doRegisterAllocation_v3(
   /* Incoming virtual-registerised code. */
   HInstrArray* instrs_in,

   /* Register allocator controls to use. */
   const RegAllocControl* con
)
{
   vassert((con->guest_sizeB % LibVEX_GUEST_STATE_ALIGN) == 0);

   /* The main register allocator state. */
   UInt       n_vregs = instrs_in->n_vregs;
   VRegState* vreg_state = NULL;
   if (n_vregs > 0) {
      vreg_state = LibVEX_Alloc_inline(n_vregs * sizeof(VRegState));
   }

   /* If this is not so, the universe we have is nonsensical. */
   UInt n_rregs = con->univ->allocable;
   vassert(n_rregs > 0);
   STATIC_ASSERT(N_RREGUNIVERSE_REGS == 64);

   /* Redundant rreg -> vreg state. */
   RRegState* rreg_state = LibVEX_Alloc_inline(n_rregs * sizeof(RRegState));

   /* Info on rreg live ranges. */
   RRegLRState* rreg_lr_state
      = LibVEX_Alloc_inline(n_rregs * sizeof(RRegLRState));

   /* Info on register usage in the incoming instruction array. Computed once
      and remains unchanged, more or less; updated sometimes by the
      direct-reload optimisation. */
   HRegUsage* reg_usage
      = LibVEX_Alloc_inline(sizeof(HRegUsage) * instrs_in->arr_used);

   /* Mark vreg indexes where coalesce chains start at. */
   UInt* coalesce_heads = LibVEX_Alloc_inline(n_vregs * sizeof(UInt));
   UInt nr_coalesce_heads = 0;

   /* The live range numbers are signed shorts, and so limiting the
      number of instructions to 15000 comfortably guards against them
      overflowing 32k. */
   vassert(instrs_in->arr_used <= 15000);

   /* The output array of instructions. */
   HInstrArray* instrs_out = newHInstrArray();


#  define OFFENDING_VREG(_v_idx, _instr, _mode)                        \
   do {                                                                \
      vex_printf("\n\nOffending vreg = %u\n", (_v_idx));               \
      vex_printf("\nOffending instruction = ");                        \
      con->ppInstr((_instr), con->mode64);                             \
      vex_printf("\n");                                                \
      vpanic("doRegisterAllocation_v3: first event for vreg is "#_mode \
             " (should be Write)");                                    \
   } while (0)

#  define OFFENDING_RREG(_r_idx, _instr, _mode)                        \
   do {                                                                \
      vex_printf("\n\nOffending rreg = ");                             \
      con->ppReg(con->univ->regs[(_r_idx)]);                           \
      vex_printf("\nOffending instruction = ");                        \
      con->ppInstr((_instr), con->mode64);                             \
      vex_printf("\n");                                                \
      vpanic("doRegisterAllocation_v3: first event for rreg is "#_mode \
             " (should be Write)");                                    \
   } while (0)


/* Finds an rreg of the correct class.
   If a free rreg is not found, then spills a vreg not used by the current
   instruction and makes free the corresponding rreg. */
#  define FIND_OR_MAKE_FREE_RREG(_ii, _v_idx, _reg_class, _reserve_phase)      \
   ({                                                                          \
      Int _r_free_idx = find_free_rreg(                                        \
                      vreg_state, n_vregs, rreg_state, n_rregs, rreg_lr_state, \
                      (_v_idx), (_ii), (_reg_class), (_reserve_phase), con);   \
      if (_r_free_idx == INVALID_INDEX) {                                      \
         HReg vreg_to_spill = find_vreg_to_spill(                              \
                                     vreg_state, n_vregs, rreg_state, n_rregs, \
                                     &reg_usage[(_ii)], (_reg_class),          \
                                     reg_usage, (_ii) + 1,                     \
                                     instrs_in->arr_used - 1, con);            \
         _r_free_idx = spill_vreg(vreg_to_spill, hregIndex(vreg_to_spill),     \
                                  (_ii), vreg_state, n_vregs,                  \
                                  rreg_state, n_rregs,                         \
                                  instrs_out, con);                            \
      }                                                                        \
                                                                               \
      vassert(IS_VALID_RREGNO(_r_free_idx));                                   \
                                                                               \
      _r_free_idx;                                                             \
   })


   /* --- Stage 0. Initialize the state. --- */
   for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
      vreg_state[v_idx].live_after            = INVALID_INSTRNO;
      vreg_state[v_idx].dead_before           = INVALID_INSTRNO;
      vreg_state[v_idx].reg_class             = HRcINVALID;
      vreg_state[v_idx].disp                  = Unallocated;
      vreg_state[v_idx].rreg                  = INVALID_HREG;
      vreg_state[v_idx].spill_offset          = 0;
      vreg_state[v_idx].coalescedTo           = INVALID_HREG;
      vreg_state[v_idx].coalescedFirst        = INVALID_HREG;
      vreg_state[v_idx].effective_dead_before = INVALID_INSTRNO;
   }

   for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
      rreg_state[r_idx].disp          = Free;
      rreg_state[r_idx].vreg          = INVALID_HREG;
      rreg_state[r_idx].eq_spill_slot = False;
   }

   for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
      RRegLRState* rreg_lrs    = &rreg_lr_state[r_idx];
      rreg_lrs->lrs_size       = 4;
      rreg_lrs->lrs            = LibVEX_Alloc_inline(rreg_lrs->lrs_size
                                                     * sizeof(RRegLR));
      rreg_lrs->lrs_used       = 0;
      rreg_lrs->lr_current     = &rreg_lrs->lrs[0];
      rreg_lrs->lr_current_idx = 0;
   }

   /* --- Stage 1. Scan the incoming instructions. --- */
   for (UShort ii = 0; ii < instrs_in->arr_used; ii++) {
      const HInstr* instr = instrs_in->arr[ii];

      con->getRegUsage(&reg_usage[ii], instr, con->mode64);
      reg_usage[ii].isVregVregMove
         = reg_usage[ii].isRegRegMove
           && hregIsVirtual(reg_usage[ii].regMoveSrc)
           && hregIsVirtual(reg_usage[ii].regMoveDst);

      if (0) {
         vex_printf("\n%u  stage 1: ", ii);
         con->ppInstr(instr, con->mode64);
         vex_printf("\n");
         ppHRegUsage(con->univ, &reg_usage[ii]);
      }

      /* Process virtual registers mentioned in the instruction. */
      for (UInt j = 0; j < reg_usage[ii].n_vRegs; j++) {
         HReg vreg = reg_usage[ii].vRegs[j];
         vassert(hregIsVirtual(vreg));

         UInt v_idx = hregIndex(vreg);
         if (!IS_VALID_VREGNO(v_idx)) {
            vex_printf("\n");
            con->ppInstr(instr, con->mode64);
            vex_printf("\n");
            vex_printf("vreg %u (n_vregs %u)\n", v_idx, n_vregs);
            vpanic("doRegisterAllocation_v3: out-of-range vreg");
         }

         /* Note the register class. */
         if (vreg_state[v_idx].reg_class == HRcINVALID) {
            /* First mention of this vreg. */
            vreg_state[v_idx].reg_class = hregClass(vreg);
         } else {
            /* Seen it before, so check for consistency. */
            vassert(vreg_state[v_idx].reg_class == hregClass(vreg));
         }

         /* Consider live ranges. */
         switch (reg_usage[ii].vMode[j]) {
         case HRmRead:
            if (vreg_state[v_idx].live_after == INVALID_INSTRNO) {
               OFFENDING_VREG(v_idx, instr, "Read");
            }
            break;
         case HRmWrite:
            if (vreg_state[v_idx].live_after == INVALID_INSTRNO) {
               vreg_state[v_idx].live_after = toShort(ii);
            }
            break;
         case HRmModify:
            if (vreg_state[v_idx].live_after == INVALID_INSTRNO) {
               OFFENDING_VREG(v_idx, instr, "Modify");
            }
            break;
         default:
            vassert(0);
         }

         vreg_state[v_idx].dead_before = toShort(ii + 1);
         vreg_state[v_idx].effective_dead_before
            = vreg_state[v_idx].dead_before;
      }

      /* Process real registers mentioned in the instruction. */
      const ULong rRead      = reg_usage[ii].rRead;
      const ULong rWritten   = reg_usage[ii].rWritten;
      const ULong rMentioned = rRead | rWritten;

      if (rMentioned != 0) {
         UInt rReg_minIndex = ULong__minIndex(rMentioned);
         UInt rReg_maxIndex = ULong__maxIndex(rMentioned);
         /* Don't bother to look at registers which are not available
            to the allocator such as the stack or guest state pointers. These
            are unavailable to the register allocator and so we never visit
            them. We asserted above that n_rregs > 0, so (n_rregs - 1) is
            safe. */
         if (rReg_maxIndex >= n_rregs) {
            rReg_maxIndex = n_rregs - 1;
         }

         for (UInt r_idx = rReg_minIndex; r_idx <= rReg_maxIndex; r_idx++) {
            const ULong jMask = 1ULL << r_idx;

            if (LIKELY((rMentioned & jMask) == 0)) {
               continue;
            }

            RRegLRState* rreg_lrs = &rreg_lr_state[r_idx];
            const Bool isR = (rRead    & jMask) != 0;
            const Bool isW = (rWritten & jMask) != 0;

            if (isW && !isR) {
               if (rreg_lrs->lrs_used == rreg_lrs->lrs_size) {
                  enlarge_rreg_lrs(rreg_lrs);
               }

               rreg_lrs->lrs[rreg_lrs->lrs_used].live_after = toShort(ii);
               rreg_lrs->lrs[rreg_lrs->lrs_used].dead_before = toShort(ii + 1);
               rreg_lrs->lrs_used += 1;
             } else if (!isW && isR) {
               if ((rreg_lrs->lrs_used == 0)
                   || (rreg_lrs->lrs[rreg_lrs->lrs_used - 1].live_after
                                                          == INVALID_INSTRNO)) {
                  OFFENDING_RREG(r_idx, instr, "Read");
               }
               rreg_lrs->lrs[rreg_lrs->lrs_used - 1].dead_before
                  = toShort(ii + 1);
            } else {
               vassert(isR && isW);
               if ((rreg_lrs->lrs_used == 0)
                   || (rreg_lrs->lrs[rreg_lrs->lrs_used - 1].live_after
                                                          == INVALID_INSTRNO)) {
                  OFFENDING_RREG(r_idx, instr, "Modify");
               }
               rreg_lrs->lrs[rreg_lrs->lrs_used - 1].dead_before
                  = toShort(ii + 1);
            }
         }
      }
   }

   if (DEBUG_REGALLOC) {
      for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
         vex_printf("vreg %3u:  [%3d, %3d)\n",
                    v_idx, vreg_state[v_idx].live_after,
                    vreg_state[v_idx].dead_before);
      }

      for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
         vex_printf("rreg %2u (", r_idx);
         UInt written = con->ppReg(con->univ->regs[r_idx]);
         vex_printf("):");
         for (Int t = 15 - written; t > 0; t--) {
            vex_printf(" ");
         }

         const RRegLRState* rreg_lrs = &rreg_lr_state[r_idx];
         for (UInt l = 0; l < rreg_lrs->lrs_used; l++) {
            vex_printf("[%3d, %3d) ",
                     rreg_lrs->lrs[l].live_after, rreg_lrs->lrs[l].dead_before);
         }
         vex_printf("\n");
      }
   }


   /* --- Stage 2. MOV coalescing (preparation). --- */
   /* Optimise register coalescing:
         MOV  v <-> v   coalescing (done here).
         MOV  v <-> r   coalescing (TODO: not yet, not here). */
   /* If doing a reg-reg move between two vregs, and the src's live range ends
     here and the dst's live range starts here, coalesce the src vreg
     to the dst vreg. */
   Bool coalesce_happened = False;
   for (UShort ii = 0; ii < instrs_in->arr_used; ii++) {
      if (reg_usage[ii].isVregVregMove) {
         HReg vregS = reg_usage[ii].regMoveSrc;
         HReg vregD = reg_usage[ii].regMoveDst;

         /* Check that |isVregVregMove| is not telling us a bunch of lies ... */
         vassert(hregClass(vregS) == hregClass(vregD));
         UInt vs_idx = hregIndex(vregS);
         UInt vd_idx = hregIndex(vregD);
         vassert(IS_VALID_VREGNO(vs_idx));
         vassert(IS_VALID_VREGNO(vd_idx));
         vassert(! sameHReg(vregS, vregD));
         VRegState* vs_st = &vreg_state[vs_idx];
         VRegState* vd_st = &vreg_state[vd_idx];

         if ((vs_st->dead_before == ii + 1) && (vd_st->live_after == ii)) {
            /* Live ranges are adjacent. */

            vs_st->coalescedTo = vregD;
            if (hregIsInvalid(vs_st->coalescedFirst)) {
               vd_st->coalescedFirst = vregS;
               coalesce_heads[nr_coalesce_heads] = vs_idx;
               nr_coalesce_heads += 1;
            } else {
               vd_st->coalescedFirst = vs_st->coalescedFirst;
            }

            vreg_state[hregIndex(vd_st->coalescedFirst)].effective_dead_before
               = vd_st->dead_before;

            if (DEBUG_REGALLOC) {
               vex_printf("vreg coalescing: ");
               con->ppReg(vregS);
               vex_printf(" -> ");
               con->ppReg(vregD);
               vex_printf("\n");
            }

            coalesce_happened = True;
         }
      }
   }

   /* --- Stage 3. Allocate spill slots. --- */

   /* Each spill slot is 8 bytes long. For vregs which take more than 64 bits
      to spill (for example classes Flt64 and Vec128), we have to allocate two
      consecutive spill slots. For 256 bit registers (class Vec256), we have to
      allocate four consecutive spill slots.

      For Vec128-class on PowerPC, the spill slot's actual address must be
      16-byte aligned. Since the spill slot's address is computed as an offset
      from the guest state pointer, and since the user of the generated code
      must set that pointer to a 32-byte aligned value, we have the residual
      obligation here of choosing a 16-byte aligned spill slot offset for
      Vec128-class values. Since each spill slot is 8 bytes long, that means for
      Vec128-class values we must allocate a spill slot number which is
      zero mod 2.

      Similarly, for Vec256 class on amd64, find a spill slot number which is
      zero mod 4. This guarantees it will be 32-byte aligned, which isn't
      actually necessary on amd64 (we use movUpd etc to spill), but seems like
      a good practice.

      Do a rank-based allocation of vregs to spill slot numbers. We put as few
      values as possible in spill slots, but nevertheless need to have a spill
      slot available for all vregs, just in case. */

#  define N_SPILL64S (LibVEX_N_SPILL_BYTES / 8)
   STATIC_ASSERT((N_SPILL64S % 2) == 0);
   STATIC_ASSERT((LibVEX_N_SPILL_BYTES % LibVEX_GUEST_STATE_ALIGN) == 0);

   Short ss_busy_until_before[N_SPILL64S];
   vex_bzero(&ss_busy_until_before, sizeof(ss_busy_until_before));

   for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
      /* True iff this vreg is unused. In which case we also expect that the
         reg_class field for it has not been set.  */
      if (vreg_state[v_idx].live_after == INVALID_INSTRNO) {
         vassert(vreg_state[v_idx].reg_class == HRcINVALID);
         continue;
      }
      if (! hregIsInvalid(vreg_state[v_idx].coalescedFirst)) {
         /* Coalesced vregs should share the same spill slot with the first vreg
            in the coalescing chain. But we don't have that information, yet. */
         continue;
      }

      /* The spill slots are 64 bits in size.  As per the comment on definition
         of HRegClass in host_generic_regs.h, that means, to spill a vreg of
         class Flt64 or Vec128, we'll need to find two adjacent spill slots to
         use. For Vec256, we'll need to find four adjacent slots to use. Note,
         this logic needs to be kept in sync with the size info on the
         definition of HRegClass. */
      UInt ss_no;
      switch (vreg_state[v_idx].reg_class) {
         case HRcFlt64:
         case HRcVec128:
            /* Find two adjacent free slots which provide up to 128 bits to
               spill the vreg. Since we are trying to find an even:odd pair,
               move along in steps of 2 (slots). */
            for (ss_no = 0; ss_no < N_SPILL64S - 1; ss_no += 2)
               if (ss_busy_until_before[ss_no + 0] <= vreg_state[v_idx].live_after
                 && ss_busy_until_before[ss_no + 1] <= vreg_state[v_idx].live_after)
                  break;
            if (ss_no >= N_SPILL64S - 1) {
               vpanic("N_SPILL64S is too low in VEX. Increase and recompile.");
            }
            ss_busy_until_before[ss_no + 0]
               = vreg_state[v_idx].effective_dead_before;
            ss_busy_until_before[ss_no + 1]
               = vreg_state[v_idx].effective_dead_before;
            break;
         default:
            /* The ordinary case -- just find a single lowest-numbered spill
               slot which is available at the start point of this interval,
               and assign the interval to it. */
            for (ss_no = 0; ss_no < N_SPILL64S; ss_no++) {
               if (ss_busy_until_before[ss_no] <= vreg_state[v_idx].live_after)
                  break;
            }
            if (ss_no == N_SPILL64S) {
               vpanic("N_SPILL64S is too low in VEX. Increase and recompile.");
            }
            ss_busy_until_before[ss_no]
               = vreg_state[v_idx].effective_dead_before;
            break;
      }

      /* This reflects VEX's hard-wired knowledge of the guest state layout:
         the guest state itself, then two equal sized areas following it for two
         sets of shadow state, and then the spill area. */
      vreg_state[v_idx].spill_offset
         = toShort(con->guest_sizeB * 3 + ss_no * 8);

      /* Independent check that we've made a sane choice of the slot. */
      switch (vreg_state[v_idx].reg_class) {
      case HRcVec128: case HRcFlt64:
         vassert((vreg_state[v_idx].spill_offset % 16) == 0);
         break;
      default:
         vassert((vreg_state[v_idx].spill_offset % 8) == 0);
         break;
      }
   }

   /* Fill in the spill offsets and effective_dead_before for coalesced vregs.*/
   for (UInt i = 0; i < nr_coalesce_heads; i++) {
      UInt vs_idx = coalesce_heads[i];
      Short effective_dead_before = vreg_state[vs_idx].effective_dead_before;
      UShort spill_offset         = vreg_state[vs_idx].spill_offset;
      HReg vregD = vreg_state[vs_idx].coalescedTo;
      while (! hregIsInvalid(vregD)) {
         UInt vd_idx = hregIndex(vregD);
         vreg_state[vd_idx].effective_dead_before = effective_dead_before;
         vreg_state[vd_idx].spill_offset          = spill_offset;
         vregD = vreg_state[vd_idx].coalescedTo;
      }
   }

   if (DEBUG_REGALLOC && coalesce_happened) {
      UInt ii = 0;
      vex_printf("After vreg<->vreg MOV coalescing:\n");
      PRINT_STATE;
   }

   if (0) {
      vex_printf("\n\n");
      for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
         if (vreg_state[v_idx].live_after != INVALID_INSTRNO) {
            vex_printf("vreg %3u    --> spill offset %u\n",
                       v_idx, vreg_state[v_idx].spill_offset);
         }
      }
   }


   /* --- State 4. Process instructions. --- */
   for (UShort ii = 0; ii < instrs_in->arr_used; ii++) {
      HInstr* instr = instrs_in->arr[ii];

      if (DEBUG_REGALLOC) {
         vex_printf("\n====----====---- Instr %d ----====----====\n", ii);
         vex_printf("---- ");
         con->ppInstr(instrs_in->arr[ii], con->mode64);
         vex_printf("\n\nInitial state:\n");
         PRINT_STATE;
         vex_printf("\n");
      }

      /* ------------ Sanity checks ------------ */

      /* Sanity checks are relatively expensive. So they are done only once
         every 17 instructions, and just before the last instruction. */
      Bool do_sanity_check
         = toBool(
              SANITY_CHECKS_EVERY_INSTR
              || ii == instrs_in->arr_used - 1
              || (ii > 0 && (ii % 17) == 0)
           );

      if (do_sanity_check) {
         /* Sanity check: the vreg_state and rreg_state mutually-redundant
            mappings are consistent. If vreg_state[v].rreg points at some
            rreg_state entry then that rreg_state entry should point back at
            vreg_state[v]. */
         for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
            if (vreg_state[v_idx].disp == Assigned) {
               vassert(!hregIsVirtual(vreg_state[v_idx].rreg));

               UInt r_idx = hregIndex(vreg_state[v_idx].rreg);
               vassert(IS_VALID_RREGNO(r_idx));
               vassert(rreg_state[r_idx].disp == Bound);
               vassert(hregIndex(rreg_state[r_idx].vreg) == v_idx);

               vassert(hregClass(vreg_state[v_idx].rreg)
                       == hregClass(con->univ->regs[r_idx]));
            }
         }

         for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
            if (rreg_state[r_idx].disp == Bound) {
               vassert(hregIsVirtual(rreg_state[r_idx].vreg));

               UInt v_idx = hregIndex(rreg_state[r_idx].vreg);
               vassert(IS_VALID_VREGNO(v_idx));
               vassert(vreg_state[v_idx].disp == Assigned);
               vassert(hregIndex(vreg_state[v_idx].rreg) == r_idx);
            } else {
               vassert(rreg_state[r_idx].eq_spill_slot == False);
            }
         }

         /* Sanity check: if rreg has been marked as Reserved, there must be
            a corresponding hard live range for it. */
         for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
            if (rreg_state[r_idx].disp == Reserved) {
               const RRegLRState* rreg_lrs = &rreg_lr_state[r_idx];
               vassert(rreg_lrs->lrs_used > 0);
               vassert(rreg_lrs->lr_current_idx < rreg_lrs->lrs_used);
               vassert(rreg_lrs->lr_current->live_after <= (Short) ii);
               vassert((Short) ii < rreg_lrs->lr_current->dead_before);
            }
         }

         /* Sanity check: if vregS has been marked as coalesced to vregD,
            then the effective live range of vregS must also cover live range
            of vregD. */
         /* The following sanity check is quite expensive. Some basic blocks
            contain very lengthy coalescing chains... */
         if (SANITY_CHECKS_EVERY_INSTR) {
            for (UInt vs_idx = 0; vs_idx < n_vregs; vs_idx++) {
               const VRegState* vS_st = &vreg_state[vs_idx];
               HReg vregD = vS_st->coalescedTo;
               while (! hregIsInvalid(vregD)) {
                  const VRegState* vD_st = &vreg_state[hregIndex(vregD)];
                  vassert(vS_st->live_after <= vD_st->live_after);
                  vassert(vS_st->effective_dead_before >= vD_st->dead_before);
                  vregD = vD_st->coalescedTo;
               }
            }
         }
      }


      /* --- MOV coalescing (finishing) --- */
      /* Optimise register coalescing:
            MOV  v <-> v   coalescing (finished here).
            MOV  v <-> r   coalescing (TODO: not yet). */
      if (reg_usage[ii].isVregVregMove) {
         HReg vregS = reg_usage[ii].regMoveSrc;
         HReg vregD = reg_usage[ii].regMoveDst;
         UInt vs_idx = hregIndex(vregS);
         UInt vd_idx = hregIndex(vregD);

         if (sameHReg(vreg_state[vs_idx].coalescedTo, vregD)) {
            /* Finally do the coalescing. */

            HReg rreg = vreg_state[vs_idx].rreg;
            switch (vreg_state[vs_idx].disp) {
            case Assigned:
               vreg_state[vd_idx].rreg = rreg;
               UInt r_idx = hregIndex(rreg);
               vassert(rreg_state[r_idx].disp == Bound);
               rreg_state[r_idx].vreg = vregD;
               break;
            case Spilled:
               vassert(hregIsInvalid(vreg_state[vs_idx].rreg));
               break;
            default:
               vassert(0);
            }

            vreg_state[vd_idx].disp = vreg_state[vs_idx].disp;
            FREE_VREG(&vreg_state[vs_idx]);

            if (DEBUG_REGALLOC) {
               vex_printf("coalesced: ");
               con->ppReg(vregS);
               vex_printf(" -> ");
               con->ppReg(vregD);
               vex_printf("\n\n");
            }

            /* In rare cases it can happen that vregD's live range ends here.
               Check and eventually free the vreg and rreg.
               This effectively means that either the translated program
               contained dead code (but VEX iropt passes are pretty good
               at eliminating it) or the VEX backend generated dead code. */
            if (vreg_state[vd_idx].dead_before <= (Short) ii + 1) {
               if (vreg_state[vd_idx].disp == Assigned) {
                  UInt r_idx = hregIndex(rreg);
                  FREE_RREG(&rreg_state[r_idx]);
               }
               FREE_VREG(&vreg_state[vd_idx]);
            }

            /* Move on to the next instruction. We skip the post-instruction
               stuff because all required house-keeping was done here. */
            continue;
         }
      }


      /* --- Reserve and free rregs if needed. --- */
      /* If the rreg enters its hard live range and is not free:
         1. If the corresponding vreg is not used by the instruction, spill it.
         2. If the corresponding vreg is used by the instruction, then:
         2a. If there are no free rregs, spill a vreg not used by this
             instruction.
         2b. Move the corresponding vreg to a free rreg. This is better than
             spilling it and immediatelly reloading it.
       */
      const ULong rRead      = reg_usage[ii].rRead;
      const ULong rWritten   = reg_usage[ii].rWritten;
      const ULong rMentioned = rRead | rWritten;

      if (rMentioned != 0) {
         UInt rReg_minIndex = ULong__minIndex(rMentioned);
         UInt rReg_maxIndex = ULong__maxIndex(rMentioned);
         if (rReg_maxIndex >= n_rregs) {
            rReg_maxIndex = n_rregs - 1;
         }

         for (UInt r_idx = rReg_minIndex; r_idx <= rReg_maxIndex; r_idx++) {
            const ULong jMask = 1ULL << r_idx;

            if (LIKELY((rMentioned & jMask) == 0)) {
               continue;
            }

            RRegState* rreg = &rreg_state[r_idx];
            const RRegLRState* rreg_lrs = &rreg_lr_state[r_idx];
            if (LIKELY(rreg_lrs->lrs_used == 0)) {
               continue;
            }
            if (rreg->disp == Reserved) {
               continue;
            }

            if ((rreg_lrs->lr_current->live_after <= (Short) ii)
                && ((Short) ii < rreg_lrs->lr_current->dead_before)) {

               switch (rreg->disp) {
               case Bound: {
                  /* Yes, there is an associated vreg. We need to deal with
                     it now somehow. */
                  HReg vreg = rreg->vreg;
                  UInt v_idx = hregIndex(vreg);

                  if (! HRegUsage__contains(&reg_usage[ii], vreg)) {
                     if (rreg->eq_spill_slot) {
                        mark_vreg_spilled(v_idx, vreg_state, n_vregs,
                                          rreg_state, n_rregs);
                     } else {
                        /* Spill the vreg. It is not used by this instruction.*/
                        spill_vreg(vreg, v_idx, ii, vreg_state, n_vregs,
                                   rreg_state, n_rregs, instrs_out, con);
                     }
                  } else {
                     /* Find or make a free rreg where to move this vreg to. */
                     UInt r_free_idx = FIND_OR_MAKE_FREE_RREG(
                                  ii, v_idx, vreg_state[v_idx].reg_class, True);

                     /* Generate "move" between real registers. */
                     HInstr* move = con->genMove(con->univ->regs[r_idx],
                                      con->univ->regs[r_free_idx], con->mode64);
                     vassert(move != NULL);
                     emit_instr(move, instrs_out, con, "move");

                     /* Update the register allocator state. */
                     vassert(vreg_state[v_idx].disp == Assigned);
                     vreg_state[v_idx].rreg = con->univ->regs[r_free_idx];
                     rreg_state[r_free_idx].disp          = Bound;
                     rreg_state[r_free_idx].vreg          = vreg;
                     rreg_state[r_free_idx].eq_spill_slot = rreg->eq_spill_slot;
                     FREE_RREG(rreg);
                  }
                  break;
               }
               case Free:
                  break;
               default:
                  vassert(0);
               }

               /* Finally claim the rreg as reserved. */
               rreg->disp = Reserved;

               if (DEBUG_REGALLOC) {
                  vex_printf("rreg has been reserved: ");
                  con->ppReg(con->univ->regs[r_idx]);
                  vex_printf("\n\n");
               }
            }
         }
      }


      /* --- Direct reload optimisation. --- */
      /* If the instruction reads exactly one vreg which is currently spilled,
         and this is the last use of that vreg, see if we can convert
         the instruction into one that reads directly from the spill slot.
         This is clearly only possible for x86 and amd64 targets, since ppc and
         arm are load-store architectures. If successful, replace
         instrs_in->arr[ii] with this new instruction, and recompute
         its reg_usage, so that the change is invisible to the standard-case
         handling that follows. */
      if ((con->directReload != NULL) && (reg_usage[ii].n_vRegs <= 2)) {
         Bool debug_direct_reload = False;
         Bool nreads = 0;
         HReg vreg_found = INVALID_HREG;
         Short spill_offset = 0;

         for (UInt j = 0; j < reg_usage[ii].n_vRegs; j++) {
            HReg vreg = reg_usage[ii].vRegs[j];
            vassert(hregIsVirtual(vreg));

            if (reg_usage[ii].vMode[j] == HRmRead) {
               nreads++;
               UInt v_idx = hregIndex(vreg);
               vassert(IS_VALID_VREGNO(v_idx));
               if (vreg_state[v_idx].disp == Spilled) {
                  /* Is this its last use? */
                  vassert(vreg_state[v_idx].dead_before >= (Short) (ii + 1));
                  if ((vreg_state[v_idx].dead_before == (Short) (ii + 1))
                      && hregIsInvalid(vreg_found)) {
                     vreg_found = vreg;
                     spill_offset = vreg_state[v_idx].spill_offset;
                  }
               }
            }
         }

         if (!hregIsInvalid(vreg_found) && (nreads == 1)) {
            if (reg_usage[ii].n_vRegs == 2) {
               vassert(! sameHReg(reg_usage[ii].vRegs[0],
                                  reg_usage[ii].vRegs[1]));
            }

            HInstr* reloaded = con->directReload(instrs_in->arr[ii],
                                                 vreg_found, spill_offset);
            if (debug_direct_reload && (reloaded != NULL)) {
               vex_printf("[%3d] ", spill_offset);
               ppHReg(vreg_found);
               vex_printf(": ");
               con->ppInstr(instr, con->mode64);
            }
            if (reloaded != NULL) {
               /* Update info about the instruction, so it looks as if it had
                  been in this form all along. */
               instr = reloaded;
               instrs_in->arr[ii] = reloaded;
               con->getRegUsage(&reg_usage[ii], instr, con->mode64);
               if (debug_direct_reload) {
                  vex_printf("  -->  ");
                  con->ppInstr(reloaded, con->mode64);
               }
            }

            if (debug_direct_reload && (reloaded != NULL)) {
               vex_printf("\n");
            }
         }
      }


      /* The vreg -> rreg map constructed and then applied to each
         instruction. */
         HRegRemap remap;
         initHRegRemap(&remap);

      /* --- Allocate vregs used by the instruction. --- */
      /* Vregs used by the instruction can be in the following states:
         - Unallocated: vreg is entering its live range. Find a free rreg.
         - Assigned: we do nothing; rreg has been allocated previously.
         - Spilled: Find a free rreg and reload vreg into it.
         Naturally, finding a free rreg may involve spilling a vreg not used by
         the instruction. */
      for (UInt j = 0; j < reg_usage[ii].n_vRegs; j++) {
         HReg vreg = reg_usage[ii].vRegs[j];
         vassert(hregIsVirtual(vreg));

         if (0) {
            vex_printf("considering "); con->ppReg(vreg); vex_printf("\n");
         }

         UInt v_idx = hregIndex(vreg);
         vassert(IS_VALID_VREGNO(v_idx));
         HReg rreg = vreg_state[v_idx].rreg;
         UInt r_idx;
         if (vreg_state[v_idx].disp == Assigned) {
            r_idx = hregIndex(rreg);
            vassert(rreg_state[r_idx].disp == Bound);
            addToHRegRemap(&remap, vreg, rreg);
         } else {
            vassert(hregIsInvalid(rreg));

            /* Find or make a free rreg of the correct class. */
            r_idx = FIND_OR_MAKE_FREE_RREG(
                                 ii, v_idx, vreg_state[v_idx].reg_class, False);
            rreg = con->univ->regs[r_idx];

            /* Generate reload only if the vreg is spilled and is about to being
               read or modified. If it is merely written than reloading it first
               would be pointless. */
            if ((vreg_state[v_idx].disp == Spilled)
                && (reg_usage[ii].vMode[j] != HRmWrite)) {

               HInstr* reload1 = NULL;
               HInstr* reload2 = NULL;
               con->genReload(&reload1, &reload2, rreg,
                         vreg_state[v_idx].spill_offset, con->mode64);
               vassert(reload1 != NULL || reload2 != NULL);
               if (reload1 != NULL) {
                  emit_instr(reload1, instrs_out, con, "reload1");
               }
               if (reload2 != NULL) {
                  emit_instr(reload2, instrs_out, con, "reload2");
               }


            }

            rreg_state[r_idx].disp          = Bound;
            rreg_state[r_idx].vreg          = vreg;
            rreg_state[r_idx].eq_spill_slot = True;
            vreg_state[v_idx].disp = Assigned;
            vreg_state[v_idx].rreg = rreg;
            addToHRegRemap(&remap, vreg, rreg);
         }

         /* If this vreg is written or modified, mark it so. */
         if (reg_usage[ii].vMode[j] != HRmRead) {
            rreg_state[r_idx].eq_spill_slot = False;
         }
      }

      con->mapRegs(&remap, instr, con->mode64);
      emit_instr(instr, instrs_out, con, NULL);

      if (DEBUG_REGALLOC) {
         vex_printf("After dealing with current instruction:\n");
         PRINT_STATE;
         vex_printf("\n");
      }

      /* ------ Post-instruction actions. ------ */
      /* Free rregs which:
         - Have been reserved and whose hard live range ended.
         - Have been bound to vregs whose live range ended. */
      for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
         RRegState*   rreg     = &rreg_state[r_idx];
         RRegLRState* rreg_lrs = &rreg_lr_state[r_idx];
         switch (rreg->disp) {
         case Free:
            break;
         case Reserved:
            if (rreg_lrs->lrs_used > 0) {
               /* Consider "dead before" the next instruction. */
               if (rreg_lrs->lr_current->dead_before <= (Short) ii + 1) {
                  FREE_RREG(&rreg_state[r_idx]);
                  if (rreg_lrs->lr_current_idx < rreg_lrs->lrs_used - 1) {
                     rreg_lrs->lr_current_idx += 1;
                     rreg_lrs->lr_current
                        = &rreg_lrs->lrs[rreg_lrs->lr_current_idx];
                  }
               }
            }
            break;
         case Bound: {
            UInt v_idx = hregIndex(rreg->vreg);
            /* Consider "dead before" the next instruction. */
            if (vreg_state[v_idx].dead_before <= (Short) ii + 1) {
               FREE_VREG(&vreg_state[v_idx]);
               FREE_RREG(&rreg_state[r_idx]);
            }
            break;
         }
         default:
            vassert(0);
         }
      }
   }

   return instrs_out;
}

/*----------------------------------------------------------------------------*/
/*---                                            host_generic_reg_alloc3.c ---*/
/*----------------------------------------------------------------------------*/
