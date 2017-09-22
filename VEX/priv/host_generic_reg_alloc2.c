
/*---------------------------------------------------------------*/
/*--- begin                                 host_reg_alloc2.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
      info@open-works.net

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "main_util.h"
#include "host_generic_regs.h"

/* Set to 1 for lots of debugging output. */
#define DEBUG_REGALLOC 0


/* TODO 27 Oct 04:

   We can possibly do V-V coalescing even when the src is spilled,
   providing we can arrange for the dst to have the same spill slot.

   Note that state[].hreg is the same as the available real regs.

   Generally rationalise data structures.  */


/* Records information on virtual register live ranges.  Computed once
   and remains unchanged after that. */
typedef
   struct {
      /* Becomes live for the first time after this insn ... */
      Short live_after;
      /* Becomes dead for the last time before this insn ... */
      Short dead_before;
      /* The "home" spill slot, if needed.  Never changes. */
      Short spill_offset;
      Short spill_size;
      /* What kind of register this is. */
      HRegClass reg_class;
   }
   VRegLR;


/* Records information on real-register live ranges.  Computed once
   and remains unchanged after that. */
typedef
   struct {
      HReg rreg;
      /* Becomes live after this insn ... */
      Short live_after;
      /* Becomes dead before this insn ... */
      Short dead_before;
   }
   RRegLR;


/* An array of the following structs (rreg_state) comprises the
   running state of the allocator.  It indicates what the current
   disposition of each allocatable real register is.  The array gets
   updated as the allocator processes instructions.  The identity of
   the register is not recorded here, because the index of this
   structure in doRegisterAllocation()'s |rreg_state| is the index
   number of the register, and the register itself can be extracted
   from the RRegUniverse supplied to doRegisterAllocation(). */
typedef
   struct {
      /* ------ FIELDS WHICH DO NOT CHANGE ------ */
      /* Is this involved in any HLRs?  (only an optimisation hint) */
      Bool has_hlrs;
      /* ------ FIELDS WHICH DO CHANGE ------ */
      /* 6 May 07: rearranged fields below so the whole struct fits
         into 16 bytes on both x86 and amd64. */
      /* Used when .disp == Bound and we are looking for vregs to
         spill. */
      Bool is_spill_cand;
      /* Optimisation: used when .disp == Bound.  Indicates when the
         rreg has the same value as the spill slot for the associated
         vreg.  Is safely left at False, and becomes True after a
         spill store or reload for this rreg. */
      Bool eq_spill_slot;
      /* What's it's current disposition? */
      enum { Free,     /* available for use */
             Unavail,  /* in a real-reg live range */
             Bound     /* in use (holding value of some vreg) */
           }
           disp;
      /* If .disp == Bound, what vreg is it bound to? */
      HReg vreg;
   }
   RRegState;


/* The allocator also maintains a redundant array of indexes
   (vreg_state) from vreg numbers back to entries in rreg_state.  It
   is redundant because iff vreg_state[i] == j then
   hregNumber(rreg_state[j].vreg) == i -- that is, the two entries
   point at each other.  The purpose of this is to speed up activities
   which involve looking for a particular vreg: there is no need to
   scan the rreg_state looking for it, just index directly into
   vreg_state.  The FAQ "does this vreg already have an associated
   rreg" is the main beneficiary.  

   To indicate, in vreg_state[i], that a given vreg is not currently
   associated with any rreg, that entry can be set to INVALID_RREG_NO.

   Because the vreg_state entries are signed Shorts, the max number
   of vregs that can be handed by regalloc is 32767.
*/

#define INVALID_RREG_NO ((Short)(-1))

#define IS_VALID_VREGNO(_zz) ((_zz) >= 0 && (_zz) < n_vregs)
#define IS_VALID_RREGNO(_zz) ((_zz) >= 0 && (_zz) < n_rregs)


/* Search forward from some given point in the incoming instruction
   sequence.  Point is to select a virtual register to spill, by
   finding the vreg which is mentioned as far ahead as possible, in
   the hope that this will minimise the number of consequent reloads.

   Only do the search for vregs which are Bound in the running state,
   and for which the .is_spill_cand field is set.  This allows the
   caller to arbitrarily restrict the set of spill candidates to be
   considered.

   To do this we don't actually need to see the incoming instruction
   stream.  Rather, what we need us the HRegUsage records for the
   incoming instruction stream.  Hence that is passed in.

   Returns an index into the state array indicating the (v,r) pair to
   spill, or -1 if none was found.  */
static
Int findMostDistantlyMentionedVReg ( 
   HRegUsage*             reg_usages_in,
   Int                    search_from_instr,
   Int                    num_instrs,
   RRegState*             rreg_state,
   HRegClass              hreg_class,
   const RegAllocControl* con
)
{
   Int m;
   Int furthest_k = -1;
   Int furthest   = -1;
   vassert(search_from_instr >= 0);
   for (UInt k = con->univ->allocable_start[hreg_class];
        k <= con->univ->allocable_end[hreg_class]; k++) {
      if (!rreg_state[k].is_spill_cand)
         continue;
      vassert(rreg_state[k].disp == Bound);
      for (m = search_from_instr; m < num_instrs; m++) {
         if (HRegUsage__contains(&reg_usages_in[m], rreg_state[k].vreg))
            break;
      }
      if (m > furthest) {
         furthest   = m;
         furthest_k = k;
      }
   }
   return furthest_k;
}


/* Check that this vreg has been assigned a sane spill offset. */
inline
static void sanity_check_spill_offset ( VRegLR* vreg )
{
   switch (vreg->reg_class) {
      case HRcVec128: case HRcFlt64:
         vassert(0 == ((UShort)vreg->spill_offset % 16)); break;
      default:
         vassert(0 == ((UShort)vreg->spill_offset % 8)); break;
   }
}


/* Double the size of the real-reg live-range array, if needed. */
__attribute__((noinline)) 
static void ensureRRLRspace_SLOW ( RRegLR** info, Int* size, Int used )
{
   Int     k;
   RRegLR* arr2;
   if (0)
      vex_printf("ensureRRLRspace: %d -> %d\n", *size, 2 * *size);
   vassert(used == *size);
   arr2 = LibVEX_Alloc_inline(2 * *size * sizeof(RRegLR));
   for (k = 0; k < *size; k++)
      arr2[k] = (*info)[k];
   *size *= 2;
   *info = arr2;
}
inline
static void ensureRRLRspace ( RRegLR** info, Int* size, Int used )
{
   if (LIKELY(used < *size)) return;
   ensureRRLRspace_SLOW(info, size, used);
}


/* Sort an array of RRegLR entries by either the .live_after or
   .dead_before fields.  This is performance-critical. */
static void sortRRLRarray ( RRegLR* arr, 
                            Int size, Bool by_live_after )
{
   Int    incs[14] = { 1, 4, 13, 40, 121, 364, 1093, 3280,
                       9841, 29524, 88573, 265720,
                       797161, 2391484 };
   Int    lo = 0;
   Int    hi = size-1;
   Int    i, j, h, bigN, hp;
   RRegLR v;

   vassert(size >= 0);
   if (size == 0)
      return;

   bigN = hi - lo + 1; if (bigN < 2) return;
   hp = 0; while (hp < 14 && incs[hp] < bigN) hp++; hp--;

   if (by_live_after) {

      for ( ; hp >= 0; hp--) {
         h = incs[hp];
         for (i = lo + h; i <= hi; i++) {
            v = arr[i];
            j = i;
            while (arr[j-h].live_after > v.live_after) {
               arr[j] = arr[j-h];
               j = j - h;
               if (j <= (lo + h - 1)) break;
            }
            arr[j] = v;
         }
      }

   } else {

      for ( ; hp >= 0; hp--) {
         h = incs[hp];
         for (i = lo + h; i <= hi; i++) {
            v = arr[i];
            j = i;
            while (arr[j-h].dead_before > v.dead_before) {
               arr[j] = arr[j-h];
               j = j - h;
               if (j <= (lo + h - 1)) break;
            }
            arr[j] = v;
         }
      }

   }
}


/* Compute the index of the highest and lowest 1 in a ULong,
   respectively.  Results are undefined if the argument is zero.
   Don't pass it zero :) */
static inline UInt ULong__maxIndex ( ULong w64 ) {
   return 63 - __builtin_clzll(w64);
}

static inline UInt ULong__minIndex ( ULong w64 ) {
   return __builtin_ctzll(w64);
}


/* A target-independent register allocator.  Requires various
   functions which it uses to deal abstractly with instructions and
   registers, since it cannot have any target-specific knowledge.

   Returns a new list of instructions, which, as a result of the
   behaviour of mapRegs, will be in-place modifications of the
   original instructions.

   Requires that the incoming code has been generated using
   vreg numbers 0, 1 .. n_vregs-1.  Appearance of a vreg outside
   that range is a checked run-time error.

   Takes an expandable array of pointers to unallocated insns.
   Returns an expandable array of pointers to allocated insns.
*/
HInstrArray* doRegisterAllocation_v2 (

   /* Incoming virtual-registerised code. */ 
   HInstrArray* instrs_in,

   /* Register allocator controls to use. */
   const RegAllocControl* con
)
{
#  define N_SPILL64S  (LibVEX_N_SPILL_BYTES / 8)

   const Bool eq_spill_opt = True;

   /* Info on vregs and rregs.  Computed once and remains
      unchanged. */
   Int     n_vregs;
   VRegLR* vreg_lrs; /* [0 .. n_vregs-1] */

   /* We keep two copies of the real-reg live range info, one sorted
      by .live_after and the other by .dead_before.  First the
      unsorted info is created in the _la variant is copied into the
      _db variant.  Once that's done both of them are sorted. 
      We also need two integer cursors which record the next
      location in the two arrays to consider. */
   RRegLR* rreg_lrs_la;
   RRegLR* rreg_lrs_db;
   Int     rreg_lrs_size;
   Int     rreg_lrs_used;
   Int     rreg_lrs_la_next;
   Int     rreg_lrs_db_next;

   /* Info on register usage in the incoming instruction array.
      Computed once and remains unchanged, more or less; updated
      sometimes by the direct-reload optimisation. */
   HRegUsage* reg_usage_arr; /* [0 .. instrs_in->arr_used-1] */

   /* Used when constructing vreg_lrs (for allocating stack
      slots). */
   Short ss_busy_until_before[N_SPILL64S];

   /* Used when constructing rreg_lrs. */
   Int* rreg_live_after;
   Int* rreg_dead_before;

   /* Running state of the core allocation algorithm. */
   RRegState* rreg_state;  /* [0 .. n_rregs-1] */
   Int        n_rregs;

   /* .. and the redundant backward map */
   /* Each value is 0 .. n_rregs-1 or is INVALID_RREG_NO.
      This implies n_rregs must be <= 32768. */
   Short*     vreg_state;  /* [0 .. n_vregs-1] */

   /* The vreg -> rreg map constructed and then applied to each
      instr. */
   HRegRemap remap;

   /* The output array of instructions. */
   HInstrArray* instrs_out;

   /* Sanity checks are expensive.  They are only done periodically,
      not at each insn processed. */
   Bool do_sanity_check;

   vassert(0 == (con->guest_sizeB % LibVEX_GUEST_STATE_ALIGN));
   vassert(0 == (LibVEX_N_SPILL_BYTES % LibVEX_GUEST_STATE_ALIGN));
   vassert(0 == (N_SPILL64S % 2));

   /* The live range numbers are signed shorts, and so limiting the
      number of insns to 15000 comfortably guards against them
      overflowing 32k. */
   vassert(instrs_in->arr_used <= 15000);

#  define INVALID_INSTRNO (-2)

#  define EMIT_INSTR(_instr)                  \
      do {                                    \
        HInstr* _tmp = (_instr);              \
        if (DEBUG_REGALLOC) {                 \
           vex_printf("**  ");                \
           con->ppInstr(_tmp, con->mode64);   \
           vex_printf("\n\n");                \
        }                                     \
        addHInstr ( instrs_out, _tmp );       \
      } while (0)

#   define PRINT_STATE						   \
      do {							   \
         Int z, q;						   \
         for (z = 0; z < n_rregs; z++) {			   \
            vex_printf("  rreg_state[%2d] = ", z);		   \
            con->ppReg(con->univ->regs[z]);    			   \
            vex_printf("  \t");					   \
            switch (rreg_state[z].disp) {			   \
               case Free:    vex_printf("Free\n"); break;	   \
               case Unavail: vex_printf("Unavail\n"); break;	   \
               case Bound:   vex_printf("BoundTo "); 		   \
                             con->ppReg(rreg_state[z].vreg);	   \
                             vex_printf("\n"); break;		   \
            }							   \
         }							   \
         vex_printf("\n  vreg_state[0 .. %d]:\n    ", n_vregs-1);  \
         q = 0;                                                    \
         for (z = 0; z < n_vregs; z++) {                           \
            if (vreg_state[z] == INVALID_RREG_NO)                  \
               continue;                                           \
            vex_printf("[%d] -> %d   ", z, vreg_state[z]);         \
            q++;                                                   \
            if (q > 0 && (q % 6) == 0)                             \
               vex_printf("\n    ");                               \
         }                                                         \
         vex_printf("\n");                                         \
      } while (0)


   /* --------- Stage 0: set up output array --------- */
   /* --------- and allocate/initialise running state. --------- */

   instrs_out = newHInstrArray();

   /* ... and initialise running state. */
   /* n_rregs is no more than a short name for n_available_real_regs. */
   n_rregs = con->univ->allocable;
   n_vregs = instrs_in->n_vregs;

   /* If this is not so, vreg_state entries will overflow. */
   vassert(n_vregs < 32767);

   /* If this is not so, the universe we have is nonsensical. */
   vassert(n_rregs > 0);

   rreg_state = LibVEX_Alloc_inline(n_rregs * sizeof(RRegState));
   vreg_state = LibVEX_Alloc_inline(n_vregs * sizeof(Short));

   for (Int j = 0; j < n_rregs; j++) {
      rreg_state[j].has_hlrs      = False;
      rreg_state[j].disp          = Free;
      rreg_state[j].vreg          = INVALID_HREG;
      rreg_state[j].is_spill_cand = False;
      rreg_state[j].eq_spill_slot = False;
   }

   for (Int j = 0; j < n_vregs; j++)
      vreg_state[j] = INVALID_RREG_NO;


   /* --------- Stage 1: compute vreg live ranges. --------- */
   /* --------- Stage 2: compute rreg live ranges. --------- */

   /* ------ start of SET UP TO COMPUTE VREG LIVE RANGES ------ */

   /* This is relatively simple, because (1) we only seek the complete
      end-to-end live range of each vreg, and are not interested in
      any holes in it, and (2) the vregs are conveniently numbered 0
      .. n_vregs-1, so we can just dump the results in a
      pre-allocated array. */

   vreg_lrs = NULL;
   if (n_vregs > 0)
      vreg_lrs = LibVEX_Alloc_inline(sizeof(VRegLR) * n_vregs);

   for (Int j = 0; j < n_vregs; j++) {
      vreg_lrs[j].live_after     = INVALID_INSTRNO;
      vreg_lrs[j].dead_before    = INVALID_INSTRNO;
      vreg_lrs[j].spill_offset   = 0;
      vreg_lrs[j].spill_size     = 0;
      vreg_lrs[j].reg_class      = HRcINVALID;
   }

   /* An array to hold the reg-usage info for the incoming
      instructions. */
   reg_usage_arr = LibVEX_Alloc_inline(sizeof(HRegUsage) * instrs_in->arr_used);

   /* ------ end of SET UP TO COMPUTE VREG LIVE RANGES ------ */

   /* ------ start of SET UP TO COMPUTE RREG LIVE RANGES ------ */

   /* This is more complex than Stage 1, because we need to compute
      exactly all the live ranges of all the allocatable real regs,
      and we don't know in advance how many there will be. */

   rreg_lrs_used = 0;
   rreg_lrs_size = 4;
   rreg_lrs_la = LibVEX_Alloc_inline(rreg_lrs_size * sizeof(RRegLR));
   rreg_lrs_db = NULL; /* we'll create this later */

   /* We'll need to track live range start/end points seperately for
      each rreg.  Sigh. */
   vassert(n_rregs > 0);
   rreg_live_after  = LibVEX_Alloc_inline(n_rregs * sizeof(Int));
   rreg_dead_before = LibVEX_Alloc_inline(n_rregs * sizeof(Int));

   for (Int j = 0; j < n_rregs; j++) {
      rreg_live_after[j] = 
      rreg_dead_before[j] = INVALID_INSTRNO;
   }

   /* ------ end of SET UP TO COMPUTE RREG LIVE RANGES ------ */

   /* ------ start of ITERATE OVER INSNS ------ */

   for (Int ii = 0; ii < instrs_in->arr_used; ii++) {

      con->getRegUsage(&reg_usage_arr[ii], instrs_in->arr[ii], con->mode64);
      reg_usage_arr[ii].isVregVregMove
         = reg_usage_arr[ii].isRegRegMove
           && hregIsVirtual(reg_usage_arr[ii].regMoveSrc)
           && hregIsVirtual(reg_usage_arr[ii].regMoveDst);

      if (0) {
         vex_printf("\n%d  stage1: ", ii);
         con->ppInstr(instrs_in->arr[ii], con->mode64);
         vex_printf("\n");
         ppHRegUsage(con->univ, &reg_usage_arr[ii]);
      }

      /* ------ start of DEAL WITH VREG LIVE RANGES ------ */

      /* for each virtual reg mentioned in the insn ... */
      for (Int j = 0; j < reg_usage_arr[ii].n_vRegs; j++) {

         HReg vreg = reg_usage_arr[ii].vRegs[j];
         vassert(hregIsVirtual(vreg));

         Int k = hregIndex(vreg);
         if (k < 0 || k >= n_vregs) {
            vex_printf("\n");
            con->ppInstr(instrs_in->arr[ii], con->mode64);
            vex_printf("\n");
            vex_printf("vreg %d, n_vregs %d\n", k, n_vregs);
            vpanic("doRegisterAllocation: out-of-range vreg");
         }

         /* Take the opportunity to note its regclass.  We'll need
            that when allocating spill slots. */
         if (vreg_lrs[k].reg_class == HRcINVALID) {
            /* First mention of this vreg. */
            vreg_lrs[k].reg_class = hregClass(vreg);
         } else {
            /* Seen it before, so check for consistency. */
            vassert(vreg_lrs[k].reg_class == hregClass(vreg));
         }

         /* Now consider live ranges. */
         switch (reg_usage_arr[ii].vMode[j]) {
            case HRmRead: 
               if (vreg_lrs[k].live_after == INVALID_INSTRNO) {
                  vex_printf("\n\nOFFENDING VREG = %d\n", k);
                  vpanic("doRegisterAllocation: "
                         "first event for vreg is Read");
               }
               vreg_lrs[k].dead_before = toShort(ii + 1);
               break;
            case HRmWrite:
               if (vreg_lrs[k].live_after == INVALID_INSTRNO)
                  vreg_lrs[k].live_after = toShort(ii);
               vreg_lrs[k].dead_before = toShort(ii + 1);
               break;
            case HRmModify:
               if (vreg_lrs[k].live_after == INVALID_INSTRNO) {
                  vex_printf("\n\nOFFENDING VREG = %d\n", k);
                  vpanic("doRegisterAllocation: "
                         "first event for vreg is Modify");
               }
               vreg_lrs[k].dead_before = toShort(ii + 1);
               break;
            default:
               vpanic("doRegisterAllocation(1)");
         } /* switch */

      } /* iterate over virtual registers */

      /* ------ end of DEAL WITH VREG LIVE RANGES ------ */

      /* ------ start of DEAL WITH RREG LIVE RANGES ------ */

      /* If this doesn't hold, the following iteration over real registers
         will fail miserably. */
      vassert(N_RREGUNIVERSE_REGS == 64);

      const ULong rRead      = reg_usage_arr[ii].rRead;
      const ULong rWritten   = reg_usage_arr[ii].rWritten;
      const ULong rMentioned = rRead | rWritten;

      UInt rReg_minIndex;
      UInt rReg_maxIndex;
      if (rMentioned == 0) {
         /* There are no real register uses in this insn.  Set
            rReg_{min,max}Index so that the following loop doesn't iterate
            at all, so as to avoid wasting time. */
         rReg_minIndex = 1;
         rReg_maxIndex = 0;
      } else {
         rReg_minIndex = ULong__minIndex(rMentioned);
         rReg_maxIndex = ULong__maxIndex(rMentioned);
         /* Don't bother to look at registers which are not available
            to the allocator.  We asserted above that n_rregs > 0, so
            n_rregs-1 is safe. */
         if (rReg_maxIndex >= n_rregs)
            rReg_maxIndex = n_rregs-1;
      }

      /* for each allocator-available real reg mentioned in the insn ... */
      /* Note.  We are allocating only over the real regs available to
         the allocator.  Others, eg the stack or baseblock pointers,
         are unavailable to allocation and so we never visit them.
         Hence the iteration is cut off at n_rregs-1, since n_rregs ==
         univ->allocable. */
      for (Int j = rReg_minIndex; j <= rReg_maxIndex; j++) {

         const ULong jMask = 1ULL << j;
         if (LIKELY((rMentioned & jMask) == 0))
            continue;

         const Bool isR = (rRead    & jMask) != 0;
         const Bool isW = (rWritten & jMask) != 0;

         /* Dummy initialisations of flush_la and flush_db to avoid
            possible bogus uninit-var warnings from gcc. */
         Int  flush_la = INVALID_INSTRNO, flush_db = INVALID_INSTRNO;
         Bool flush = False;

         if (isW && !isR) {
            flush_la = rreg_live_after[j];
            flush_db = rreg_dead_before[j];
            if (flush_la != INVALID_INSTRNO && flush_db != INVALID_INSTRNO)
               flush = True;
            rreg_live_after[j]  = ii;
            rreg_dead_before[j] = ii+1;
         } else if (!isW && isR) {
            if (rreg_live_after[j] == INVALID_INSTRNO) {
               vex_printf("\nOFFENDING RREG = ");
               con->ppReg(con->univ->regs[j]);
               vex_printf("\n");
               vex_printf("\nOFFENDING instr = ");
               con->ppInstr(instrs_in->arr[ii], con->mode64);
               vex_printf("\n");
               vpanic("doRegisterAllocation: "
                      "first event for rreg is Read");
            }
            rreg_dead_before[j] = ii+1;
         } else {
            vassert(isR && isW);
            if (rreg_live_after[j] == INVALID_INSTRNO) {
               vex_printf("\nOFFENDING RREG = ");
               con->ppReg(con->univ->regs[j]);
               vex_printf("\n");
               vex_printf("\nOFFENDING instr = ");
               con->ppInstr(instrs_in->arr[ii], con->mode64);
               vex_printf("\n");
               vpanic("doRegisterAllocation: "
                      "first event for rreg is Modify");
            }
            rreg_dead_before[j] = ii+1;
         }

         if (flush) {
            vassert(flush_la != INVALID_INSTRNO);
            vassert(flush_db != INVALID_INSTRNO);
            ensureRRLRspace(&rreg_lrs_la, &rreg_lrs_size, rreg_lrs_used);
            if (0) 
               vex_printf("FLUSH 1 (%d,%d)\n", flush_la, flush_db);
            rreg_lrs_la[rreg_lrs_used].rreg        = con->univ->regs[j];
            rreg_lrs_la[rreg_lrs_used].live_after  = toShort(flush_la);
            rreg_lrs_la[rreg_lrs_used].dead_before = toShort(flush_db);
            rreg_lrs_used++;
         }

      } /* iterate over rregs in the instr */

      /* ------ end of DEAL WITH RREG LIVE RANGES ------ */

   } /* iterate over insns */

   /* ------ end of ITERATE OVER INSNS ------ */

   /* ------ start of FINALISE RREG LIVE RANGES ------ */

   /* Now finish up any live ranges left over. */
   for (Int j = 0; j < n_rregs; j++) {

      if (0) {
         vex_printf("residual %d:  %d %d\n", j, rreg_live_after[j],
                                                rreg_dead_before[j]);
      }
      vassert( (rreg_live_after[j] == INVALID_INSTRNO 
                && rreg_dead_before[j] == INVALID_INSTRNO)
              ||
               (rreg_live_after[j] != INVALID_INSTRNO 
                && rreg_dead_before[j] != INVALID_INSTRNO)
            );

      if (rreg_live_after[j] == INVALID_INSTRNO)
         continue;

      ensureRRLRspace(&rreg_lrs_la, &rreg_lrs_size, rreg_lrs_used);
      if (0)
         vex_printf("FLUSH 2 (%d,%d)\n", 
                    rreg_live_after[j], rreg_dead_before[j]);
      rreg_lrs_la[rreg_lrs_used].rreg        = con->univ->regs[j];
      rreg_lrs_la[rreg_lrs_used].live_after  = toShort(rreg_live_after[j]);
      rreg_lrs_la[rreg_lrs_used].dead_before = toShort(rreg_dead_before[j]);
      rreg_lrs_used++;
   }

   /* Compute summary hints for choosing real regs.  If a real reg is
      involved in a hard live range, record that fact in the fixed
      part of the running rreg_state.  Later, when offered a choice between
      rregs, it's better to choose one which is not marked as having
      any HLRs, since ones with HLRs may need to be spilled around
      their HLRs.  Correctness of final assignment is unaffected by
      this mechanism -- it is only an optimisation. */

   for (Int j = 0; j < rreg_lrs_used; j++) {
      HReg rreg = rreg_lrs_la[j].rreg;
      vassert(!hregIsVirtual(rreg));
      /* rreg is involved in a HLR.  Record this info in the array, if
         there is space. */
      UInt ix = hregIndex(rreg);
      vassert(ix < n_rregs);
      rreg_state[ix].has_hlrs = True;
   }
   if (0) {
      for (Int j = 0; j < n_rregs; j++) {
         if (!rreg_state[j].has_hlrs)
            continue;
         con->ppReg(con->univ->regs[j]);
         vex_printf(" hinted\n");
      }
   }

   /* Finally, copy the _la variant into the _db variant and
      sort both by their respective fields. */
   rreg_lrs_db = LibVEX_Alloc_inline(rreg_lrs_used * sizeof(RRegLR));
   for (Int j = 0; j < rreg_lrs_used; j++)
      rreg_lrs_db[j] = rreg_lrs_la[j];

   sortRRLRarray( rreg_lrs_la, rreg_lrs_used, True /* by .live_after*/  );
   sortRRLRarray( rreg_lrs_db, rreg_lrs_used, False/* by .dead_before*/ );

   /* And set up the cursors. */
   rreg_lrs_la_next = 0;
   rreg_lrs_db_next = 0;

   for (Int j = 1; j < rreg_lrs_used; j++) {
      vassert(rreg_lrs_la[j-1].live_after  <= rreg_lrs_la[j].live_after);
      vassert(rreg_lrs_db[j-1].dead_before <= rreg_lrs_db[j].dead_before);
   }

   /* ------ end of FINALISE RREG LIVE RANGES ------ */

   if (DEBUG_REGALLOC) {
      for (Int j = 0; j < n_vregs; j++) {
         vex_printf("vreg %d:  la = %d,  db = %d\n", 
                    j, vreg_lrs[j].live_after, vreg_lrs[j].dead_before );
      }
   }

   if (DEBUG_REGALLOC) {
      vex_printf("RRegLRs by LA:\n");
      for (Int j = 0; j < rreg_lrs_used; j++) {
         vex_printf("  ");
         con->ppReg(rreg_lrs_la[j].rreg);
         vex_printf("      la = %d,  db = %d\n",
                    rreg_lrs_la[j].live_after, rreg_lrs_la[j].dead_before );
      }
      vex_printf("RRegLRs by DB:\n");
      for (Int j = 0; j < rreg_lrs_used; j++) {
         vex_printf("  ");
         con->ppReg(rreg_lrs_db[j].rreg);
         vex_printf("      la = %d,  db = %d\n",
                    rreg_lrs_db[j].live_after, rreg_lrs_db[j].dead_before );
      }
   }

   /* --------- Stage 3: allocate spill slots. --------- */

   /* Each spill slot is 8 bytes long.  For vregs which take more than
      64 bits to spill (classes Flt64 and Vec128), we have to allocate
      two consecutive spill slots.  For 256 bit registers (class
      Vec256), we have to allocate four consecutive spill slots.

      For Vec128-class on PowerPC, the spill slot's actual address
      must be 16-byte aligned.  Since the spill slot's address is
      computed as an offset from the guest state pointer, and since
      the user of the generated code must set that pointer to a
      32-aligned value, we have the residual obligation here of
      choosing a 16-aligned spill slot offset for Vec128-class values.
      Since each spill slot is 8 bytes long, that means for
      Vec128-class values we must allocated a spill slot number which
      is zero mod 2.

      Similarly, for Vec256 class on amd64, find a spill slot number
      which is zero mod 4.  This guarantees it will be 32 byte
      aligned, which isn't actually necessary on amd64 (we use movUpd
      etc to spill), but seems like good practice.

      Do a rank-based allocation of vregs to spill slot numbers.  We
      put as few values as possible in spill slots, but nevertheless
      need to have a spill slot available for all vregs, just in case.
   */
   /* Int max_ss_no = -1; */

   vex_bzero(ss_busy_until_before, sizeof(ss_busy_until_before));

   for (Int j = 0; j < n_vregs; j++) {

      /* True iff this vreg is unused.  In which case we also expect
         that the reg_class field for it has not been set.  */
      if (vreg_lrs[j].live_after == INVALID_INSTRNO) {
         vassert(vreg_lrs[j].reg_class == HRcINVALID);
         continue;
      }

      /* The spill slots are 64 bits in size.  As per the comment on
         definition of HRegClass in host_generic_regs.h, that means,
         to spill a vreg of class Flt64 or Vec128, we'll need to find
         two adjacent spill slots to use.  For Vec256, we'll need to
         find four adjacent slots to use.  Note, this logic needs to
         kept in sync with the size info on the definition of
         HRegClass. */
      Int ss_no = -1;
      switch (vreg_lrs[j].reg_class) {

         case HRcVec128: case HRcFlt64:
            /* Find two adjacent free slots in which between them
               provide up to 128 bits in which to spill the vreg.
               Since we are trying to find an even:odd pair, move
               along in steps of 2 (slots). */
            for (ss_no = 0; ss_no < N_SPILL64S-1; ss_no += 2)
               if (ss_busy_until_before[ss_no+0] <= vreg_lrs[j].live_after
                   && ss_busy_until_before[ss_no+1] <= vreg_lrs[j].live_after)
                  break;
            if (ss_no >= N_SPILL64S-1) {
               vpanic("LibVEX_N_SPILL_BYTES is too low.  " 
                      "Increase and recompile.");
            }
            ss_busy_until_before[ss_no+0] = vreg_lrs[j].dead_before;
            ss_busy_until_before[ss_no+1] = vreg_lrs[j].dead_before;
            break;

         default:
            /* The ordinary case -- just find a single spill slot. */
            /* Find the lowest-numbered spill slot which is available
               at the start point of this interval, and assign the
               interval to it. */
            for (ss_no = 0; ss_no < N_SPILL64S; ss_no++)
               if (ss_busy_until_before[ss_no] <= vreg_lrs[j].live_after)
                  break;
            if (ss_no == N_SPILL64S) {
               vpanic("LibVEX_N_SPILL_BYTES is too low.  " 
                      "Increase and recompile.");
            }
            ss_busy_until_before[ss_no] = vreg_lrs[j].dead_before;
            break;

      } /* switch (vreg_lrs[j].reg_class) */

      /* This reflects LibVEX's hard-wired knowledge of the baseBlock
         layout: the guest state, then two equal sized areas following
         it for two sets of shadow state, and then the spill area. */
      vreg_lrs[j].spill_offset = toShort(con->guest_sizeB * 3 + ss_no * 8);

      /* Independent check that we've made a sane choice of slot */
      sanity_check_spill_offset( &vreg_lrs[j] );
      /* if (j > max_ss_no) */
      /*    max_ss_no = j; */
   }

   if (0) {
      vex_printf("\n\n");
      for (Int j = 0; j < n_vregs; j++)
         vex_printf("vreg %d    --> spill offset %d\n",
                    j, vreg_lrs[j].spill_offset);
   }

   /* --------- Stage 4: establish rreg preferences --------- */

   /* It may be advantageous to allocating certain vregs to specific
      rregs, as a way of avoiding reg-reg moves later.  Here we
      establish which, if any, rreg each vreg would prefer to be in.
      Note that this constrains the allocator -- ideally we end up
      with as few as possible vregs expressing a preference.  

      This is an optimisation: if the .preferred_rreg field is never
      set to anything different from INVALID_HREG, the allocator still
      works. */

   /* 30 Dec 04: removed this mechanism as it does not seem to
      help. */

   /* --------- Stage 5: process instructions --------- */

   /* This is the main loop of the allocator.  First, we need to
      correctly set up our running state, which tracks the status of
      each real register. */

   /* ------ BEGIN: Process each insn in turn. ------ */

   for (Int ii = 0; ii < instrs_in->arr_used; ii++) {

      if (DEBUG_REGALLOC) {
         vex_printf("\n====----====---- Insn %d ----====----====\n", ii);
         vex_printf("---- ");
         con->ppInstr(instrs_in->arr[ii], con->mode64);
         vex_printf("\n\nInitial state:\n");
         PRINT_STATE;
         vex_printf("\n");
      }

      /* ------------ Sanity checks ------------ */

      /* Sanity checks are expensive.  So they are done only once
         every 17 instructions, and just before the last
         instruction. */
      do_sanity_check
         = toBool(
              False /* Set to True for sanity checking of all insns. */
              || ii == instrs_in->arr_used-1
              || (ii > 0 && (ii % 17) == 0)
           );

      if (do_sanity_check) {

         /* Sanity check 1: all rregs with a hard live range crossing
            this insn must be marked as unavailable in the running
            state. */
         for (Int j = 0; j < rreg_lrs_used; j++) {
            if (rreg_lrs_la[j].live_after < ii
                && ii < rreg_lrs_la[j].dead_before) {
               /* ii is the middle of a hard live range for some real
                  reg.  Check it's marked as such in the running
                  state. */
               HReg reg = rreg_lrs_la[j].rreg;

               if (0) {
                  vex_printf("considering la %d .. db %d   reg = ", 
                             rreg_lrs_la[j].live_after, 
                             rreg_lrs_la[j].dead_before);
                  con->ppReg(reg);
                  vex_printf("\n");
               }

               /* assert that this rreg is marked as unavailable */
               vassert(!hregIsVirtual(reg));
               vassert(rreg_state[hregIndex(reg)].disp == Unavail);
            }
         }

         /* Sanity check 2: conversely, all rregs marked as
            unavailable in the running rreg_state must have a
            corresponding hard live range entry in the rreg_lrs
            array. */
         for (Int j = 0; j < n_rregs; j++) {
            vassert(rreg_state[j].disp == Bound
                    || rreg_state[j].disp == Free
                    || rreg_state[j].disp == Unavail);
            if (rreg_state[j].disp != Unavail)
               continue;
            Int k;
            for (k = 0; k < rreg_lrs_used; k++) {
               HReg reg = rreg_lrs_la[k].rreg;
               vassert(!hregIsVirtual(reg));
               if (hregIndex(reg) == j
                   && rreg_lrs_la[k].live_after < ii 
                   && ii < rreg_lrs_la[k].dead_before) 
                  break;
            }
            /* If this vassertion fails, we couldn't find a
               corresponding HLR. */
            vassert(k < rreg_lrs_used);
         }

         /* Sanity check 3: all vreg-rreg bindings must bind registers
            of the same class. */
         for (Int j = 0; j < n_rregs; j++) {
            if (rreg_state[j].disp != Bound) {
               vassert(rreg_state[j].eq_spill_slot == False);
               continue;
            }
            vassert(hregClass(con->univ->regs[j])
                    == hregClass(rreg_state[j].vreg));
            vassert( hregIsVirtual(rreg_state[j].vreg));
         }

         /* Sanity check 4: the vreg_state and rreg_state
            mutually-redundant mappings are consistent.  If
            rreg_state[j].vreg points at some vreg_state entry then
            that vreg_state entry should point back at
            rreg_state[j]. */
         for (Int j = 0; j < n_rregs; j++) {
            if (rreg_state[j].disp != Bound)
               continue;
            Int k = hregIndex(rreg_state[j].vreg);
            vassert(IS_VALID_VREGNO(k));
            vassert(vreg_state[k] == j);
         }
         for (Int j = 0; j < n_vregs; j++) {
            Int k = vreg_state[j];
            if (k == INVALID_RREG_NO)
               continue;
            vassert(IS_VALID_RREGNO(k));
            vassert(rreg_state[k].disp == Bound);
            vassert(hregIndex(rreg_state[k].vreg) == j);
         }

      } /* if (do_sanity_check) */

      /* ------------ end of Sanity checks ------------ */

      /* Do various optimisations pertaining to register coalescing
         and preferencing:
            MOV  v <-> v   coalescing (done here).
            MOV  v <-> r   coalescing (not yet, if ever)
      */
      /* If doing a reg-reg move between two vregs, and the src's live
         range ends here and the dst's live range starts here, bind
         the dst to the src's rreg, and that's all. */
      if (reg_usage_arr[ii].isVregVregMove) {
         HReg vregS = reg_usage_arr[ii].regMoveSrc;
         HReg vregD = reg_usage_arr[ii].regMoveDst;
         /* Check that |isVregVregMove| is not telling us a bunch of lies ... */
         vassert(hregClass(vregS) == hregClass(vregD));
         Int k = hregIndex(vregS);
         Int m = hregIndex(vregD);
         vassert(IS_VALID_VREGNO(k));
         vassert(IS_VALID_VREGNO(m));
         if (vreg_lrs[k].dead_before != ii + 1) goto cannot_coalesce;
         if (vreg_lrs[m].live_after != ii) goto cannot_coalesce;
         if (DEBUG_REGALLOC) {
         vex_printf("COALESCE ");
            con->ppReg(vregS);
            vex_printf(" -> ");
            con->ppReg(vregD);
            vex_printf("\n\n");
         }
         /* Find the state entry for vregS. */
         Int n = vreg_state[k]; /* k is the index of vregS */
         if (n == INVALID_RREG_NO) {
            /* vregS is not currently in a real register.  So we can't
               do the coalescing.  Give up. */
            goto cannot_coalesce;
         }
         vassert(IS_VALID_RREGNO(n));

         /* Finally, we can do the coalescing.  It's trivial -- merely
            claim vregS's register for vregD. */
         rreg_state[n].vreg = vregD;
         vassert(IS_VALID_VREGNO(hregIndex(vregD)));
         vassert(IS_VALID_VREGNO(hregIndex(vregS)));
         vreg_state[hregIndex(vregD)] = toShort(n);
         vreg_state[hregIndex(vregS)] = INVALID_RREG_NO;

         /* This rreg has become associated with a different vreg and
            hence with a different spill slot.  Play safe. */
         rreg_state[n].eq_spill_slot = False;

         /* Move on to the next insn.  We skip the post-insn stuff for
            fixed registers, since this move should not interact with
            them in any way. */
         continue;
      }
     cannot_coalesce:

      /* ------ Free up rregs bound to dead vregs ------ */

      /* Look for vregs whose live range has just ended, and 
	 mark the associated rreg as free. */

      for (Int j = 0; j < n_rregs; j++) {
         if (rreg_state[j].disp != Bound)
            continue;
         UInt vregno = hregIndex(rreg_state[j].vreg);
         vassert(IS_VALID_VREGNO(vregno));
         if (vreg_lrs[vregno].dead_before <= ii) {
            rreg_state[j].disp = Free;
            rreg_state[j].eq_spill_slot = False;
            Int m = hregIndex(rreg_state[j].vreg);
            vassert(IS_VALID_VREGNO(m));
            vreg_state[m] = INVALID_RREG_NO;
            if (DEBUG_REGALLOC) {
               vex_printf("free up "); 
               con->ppReg(con->univ->regs[j]);
               vex_printf("\n");
            }
         }
      }

      /* ------ Pre-instruction actions for fixed rreg uses ------ */

      /* Now we have to deal with rregs which are about to be made
         live by this instruction -- in other words, are entering into
         one of their live ranges.  If any such rreg holds a vreg, we
         will have to free up the rreg.  The simplest solution which
         is correct is to spill the rreg.

         Note we could do better:
         * Could move it into some other free rreg, if one is available 

         Do this efficiently, by incrementally stepping along an array
         of rreg HLRs that are known to be sorted by start point
         (their .live_after field).
      */
      while (True) {
         vassert(rreg_lrs_la_next >= 0);
         vassert(rreg_lrs_la_next <= rreg_lrs_used);
         if (rreg_lrs_la_next == rreg_lrs_used)
            break; /* no more real reg live ranges to consider */
         if (ii < rreg_lrs_la[rreg_lrs_la_next].live_after)
            break; /* next live range does not yet start */
         vassert(ii == rreg_lrs_la[rreg_lrs_la_next].live_after);
         /* rreg_lrs_la[rreg_lrs_la_next].rreg needs to be freed up.
            Find the associated rreg_state entry. */
         /* Note, re ii == rreg_lrs_la[rreg_lrs_la_next].live_after.
            Real register live ranges are guaranteed to be well-formed
            in that they start with a write to the register -- Stage 2
            rejects any code not satisfying this.  So the correct
            question to ask is whether
            rreg_lrs_la[rreg_lrs_la_next].live_after == ii, that is,
            whether the reg becomes live after this insn -- rather
            than before it. */
         if (DEBUG_REGALLOC) {
            vex_printf("need to free up rreg: ");
            con->ppReg(rreg_lrs_la[rreg_lrs_la_next].rreg);
            vex_printf("\n\n");
         }
         Int k = hregIndex(rreg_lrs_la[rreg_lrs_la_next].rreg);

         /* If this fails, we don't have an entry for this rreg.
            Which we should. */
         vassert(IS_VALID_RREGNO(k));
         Int m = hregIndex(rreg_state[k].vreg);
         if (rreg_state[k].disp == Bound) {
            /* Yes, there is an associated vreg.  Spill it if it's
               still live. */
            vassert(IS_VALID_VREGNO(m));
            vreg_state[m] = INVALID_RREG_NO;
            if (vreg_lrs[m].dead_before > ii) {
               vassert(vreg_lrs[m].reg_class != HRcINVALID);
               if ((!eq_spill_opt) || !rreg_state[k].eq_spill_slot) {
                  HInstr* spill1 = NULL;
                  HInstr* spill2 = NULL;
                  con->genSpill(&spill1, &spill2, con->univ->regs[k],
                                vreg_lrs[m].spill_offset, con->mode64);
                  vassert(spill1 || spill2); /* can't both be NULL */
                  if (spill1)
                     EMIT_INSTR(spill1);
                  if (spill2)
                     EMIT_INSTR(spill2);
               }
               rreg_state[k].eq_spill_slot = True;
            }
         }
         rreg_state[k].disp = Unavail;
         rreg_state[k].vreg = INVALID_HREG;
         rreg_state[k].eq_spill_slot = False;

         /* check for further rregs entering HLRs at this point */
         rreg_lrs_la_next++;
      }

      if (DEBUG_REGALLOC) {
         vex_printf("After pre-insn actions for fixed regs:\n");
         PRINT_STATE;
         vex_printf("\n");
      }

      /* ------ Deal with the current instruction. ------ */

      /* Finally we can begin the processing of this instruction
         itself.  The aim is to free up enough rregs for this insn.
         This may generate spill stores since we may have to evict
         some vregs currently in rregs.  Also generates spill loads.
         We also build up the final vreg->rreg mapping to be applied
         to the insn. */

      initHRegRemap(&remap);

      /* ------------ BEGIN directReload optimisation ----------- */

      /* If the instruction reads exactly one vreg which is currently
         in a spill slot, and this is last use of that vreg, see if we
         can convert the instruction into one that reads directly from
         the spill slot.  This is clearly only possible for x86 and
         amd64 targets, since ppc and arm are load-store
         architectures.  If successful, replace instrs_in->arr[ii]
         with this new instruction, and recompute its reg usage, so
         that the change is invisible to the standard-case handling
         that follows. */
      
      if (con->directReload != NULL && reg_usage_arr[ii].n_vRegs <= 2) {
         Bool  debug_direct_reload = False;
         HReg  cand     = INVALID_HREG;
         Bool  nreads   = 0;
         Short spilloff = 0;

         for (Int j = 0; j < reg_usage_arr[ii].n_vRegs; j++) {

            HReg vreg = reg_usage_arr[ii].vRegs[j];
            vassert(hregIsVirtual(vreg));

            if (reg_usage_arr[ii].vMode[j] == HRmRead) {
               nreads++;
               Int m = hregIndex(vreg);
               vassert(IS_VALID_VREGNO(m));
               Int k = vreg_state[m];
               if (!IS_VALID_RREGNO(k)) {
                  /* ok, it is spilled.  Now, is this its last use? */
                  vassert(vreg_lrs[m].dead_before >= ii+1);
                  if (vreg_lrs[m].dead_before == ii+1
                      && hregIsInvalid(cand)) {
                     spilloff = vreg_lrs[m].spill_offset;
                     cand = vreg;
                  }
               }
            }
         }

         if (nreads == 1 && ! hregIsInvalid(cand)) {
            HInstr* reloaded;
            if (reg_usage_arr[ii].n_vRegs == 2)
               vassert(! sameHReg(reg_usage_arr[ii].vRegs[0],
                                  reg_usage_arr[ii].vRegs[1]));

            reloaded = con->directReload(instrs_in->arr[ii], cand, spilloff);
            if (debug_direct_reload && !reloaded) {
               vex_printf("[%3d] ", spilloff); ppHReg(cand); vex_printf(" "); 
               con->ppInstr(instrs_in->arr[ii], con->mode64);
            }
            if (reloaded) {
               /* Update info about the insn, so it looks as if it had
                  been in this form all along. */
               instrs_in->arr[ii] = reloaded;
               con->getRegUsage(&reg_usage_arr[ii], instrs_in->arr[ii],
                                con->mode64);
               if (debug_direct_reload && !reloaded) {
                  vex_printf("  -->  ");
                  con->ppInstr(reloaded, con->mode64);
               }
            }

            if (debug_direct_reload && !reloaded)
               vex_printf("\n");
         }

      }

      /* ------------ END directReload optimisation ------------ */

      /* for each virtual reg mentioned in the insn ... */
      for (Int j = 0; j < reg_usage_arr[ii].n_vRegs; j++) {

         HReg vreg = reg_usage_arr[ii].vRegs[j];
         vassert(hregIsVirtual(vreg));

         if (0) {
            vex_printf("considering "); con->ppReg(vreg); vex_printf("\n");
         }

         /* Now we're trying to find a rreg for "vreg".  First of all,
            if it already has an rreg assigned, we don't need to do
            anything more.  Inspect the current state to find out. */
         Int m = hregIndex(vreg);
         vassert(IS_VALID_VREGNO(m));
         Int n = vreg_state[m];
         if (IS_VALID_RREGNO(n)) {
            vassert(rreg_state[n].disp == Bound);
            addToHRegRemap(&remap, vreg, con->univ->regs[n]);
            /* If this rreg is written or modified, mark it as different
               from any spill slot value. */
            if (reg_usage_arr[ii].vMode[j] != HRmRead)
               rreg_state[n].eq_spill_slot = False;
            continue;
         } else {
            vassert(n == INVALID_RREG_NO);
         }

         /* No luck.  The next thing to do is see if there is a
            currently free rreg available, of the correct class.  If
            so, bag it.  NOTE, we could improve this by selecting an
            rreg for which the next live-range event is as far ahead
            as possible. */
         Int k_suboptimal = -1;
         Int k;
         for (k = con->univ->allocable_start[hregClass(vreg)];
              k <= con->univ->allocable_end[hregClass(vreg)]; k++) {
            if (rreg_state[k].disp != Free)
               continue;
            if (rreg_state[k].has_hlrs) {
               /* Well, at least we can use k_suboptimal if we really
                  have to.  Keep on looking for a better candidate. */
               k_suboptimal = k;
            } else {
               /* Found a preferable reg.  Use it. */
               k_suboptimal = -1;
               break;
            }
         }
         if (k_suboptimal >= 0)
            k = k_suboptimal;

         if (k <= con->univ->allocable_end[hregClass(vreg)]) {
            rreg_state[k].disp = Bound;
            rreg_state[k].vreg = vreg;
            Int p = hregIndex(vreg);
            vassert(IS_VALID_VREGNO(p));
            vreg_state[p] = toShort(k);
            addToHRegRemap(&remap, vreg, con->univ->regs[k]);
            /* Generate a reload if needed.  This only creates needed
               reloads because the live range builder for vregs will
               guarantee that the first event for a vreg is a write.
               Hence, if this reference is not a write, it cannot be
               the first reference for this vreg, and so a reload is
               indeed needed. */
            if (reg_usage_arr[ii].vMode[j] != HRmWrite) {
               vassert(vreg_lrs[p].reg_class != HRcINVALID);
               HInstr* reload1 = NULL;
               HInstr* reload2 = NULL;
               con->genReload(&reload1, &reload2, con->univ->regs[k],
                              vreg_lrs[p].spill_offset, con->mode64);
               vassert(reload1 || reload2); /* can't both be NULL */
               if (reload1)
                  EMIT_INSTR(reload1);
               if (reload2)
                  EMIT_INSTR(reload2);
               /* This rreg is read or modified by the instruction.
                  If it's merely read we can claim it now equals the
                  spill slot, but not so if it is modified. */
               if (reg_usage_arr[ii].vMode[j] == HRmRead) {
                  rreg_state[k].eq_spill_slot = True;
               } else {
                  vassert(reg_usage_arr[ii].vMode[j] == HRmModify);
                  rreg_state[k].eq_spill_slot = False;
               }
            } else {
               rreg_state[k].eq_spill_slot = False;
            }

            continue;
         }

         /* Well, now we have no option but to spill a vreg.  It's
            important to make a good choice of vreg to spill, and of
            course we need to be careful not to spill a vreg which is
            needed by this insn. */

         /* First, mark in the rreg_state, those rregs which are not spill
            candidates, due to holding a vreg mentioned by this
            instruction.  Or being of the wrong class. */
         for (k = con->univ->allocable_start[hregClass(vreg)];
              k <= con->univ->allocable_end[hregClass(vreg)]; k++) {
            rreg_state[k].is_spill_cand = False;
            if (rreg_state[k].disp != Bound)
               continue;
            rreg_state[k].is_spill_cand = True;
            /* Note, the following loop visits only the virtual regs
               mentioned by the instruction. */
            for (m = 0; m < reg_usage_arr[ii].n_vRegs; m++) {
               if (sameHReg(rreg_state[k].vreg, reg_usage_arr[ii].vRegs[m])) {
                  rreg_state[k].is_spill_cand = False;
                  break;
               }
            }
         }

         /* We can choose to spill any rreg satisfying
            rreg_state[r].is_spill_cand (so to speak).  Choose r so that
            the next use of its associated vreg is as far ahead as
            possible, in the hope that this will minimise the number
            of consequent reloads required. */
         Int spillee
            = findMostDistantlyMentionedVReg ( 
                 reg_usage_arr, ii+1, instrs_in->arr_used, rreg_state,
                 hregClass(vreg), con);

         if (spillee == -1) {
            /* Hmmmmm.  There don't appear to be any spill candidates.
               We're hosed. */
            vex_printf("reg_alloc: can't find a register in class: ");
            ppHRegClass(hregClass(vreg));
            vex_printf("\n");
            vpanic("reg_alloc: can't create a free register.");
         }

         /* Right.  So we're going to spill rreg_state[spillee]. */
         vassert(IS_VALID_RREGNO(spillee));
         vassert(rreg_state[spillee].disp == Bound);
         /* check it's the right class */
         vassert(hregClass(con->univ->regs[spillee]) == hregClass(vreg));
         /* check we're not ejecting the vreg for which we are trying
            to free up a register. */
         vassert(! sameHReg(rreg_state[spillee].vreg, vreg));

         m = hregIndex(rreg_state[spillee].vreg);
         vassert(IS_VALID_VREGNO(m));

         /* So here's the spill store.  Assert that we're spilling a
            live vreg. */
         vassert(vreg_lrs[m].dead_before > ii);
         vassert(vreg_lrs[m].reg_class != HRcINVALID);
         if ((!eq_spill_opt) || !rreg_state[spillee].eq_spill_slot) {
            HInstr* spill1 = NULL;
            HInstr* spill2 = NULL;
            con->genSpill(&spill1, &spill2, con->univ->regs[spillee],
                          vreg_lrs[m].spill_offset, con->mode64);
            vassert(spill1 || spill2); /* can't both be NULL */
            if (spill1)
               EMIT_INSTR(spill1);
            if (spill2)
               EMIT_INSTR(spill2);
         }

         /* Update the rreg_state to reflect the new assignment for this
            rreg. */
         rreg_state[spillee].vreg = vreg;
         vreg_state[m] = INVALID_RREG_NO;

         rreg_state[spillee].eq_spill_slot = False; /* be safe */

         m = hregIndex(vreg);
         vassert(IS_VALID_VREGNO(m));
         vreg_state[m] = toShort(spillee);

         /* Now, if this vreg is being read or modified (as opposed to
            written), we have to generate a reload for it. */
         if (reg_usage_arr[ii].vMode[j] != HRmWrite) {
            vassert(vreg_lrs[m].reg_class != HRcINVALID);
            HInstr* reload1 = NULL;
            HInstr* reload2 = NULL;
            con->genReload(&reload1, &reload2, con->univ->regs[spillee],
                           vreg_lrs[m].spill_offset, con->mode64);
            vassert(reload1 || reload2); /* can't both be NULL */
            if (reload1)
               EMIT_INSTR(reload1);
            if (reload2)
               EMIT_INSTR(reload2);
            /* This rreg is read or modified by the instruction.
               If it's merely read we can claim it now equals the
               spill slot, but not so if it is modified. */
            if (reg_usage_arr[ii].vMode[j] == HRmRead) {
               rreg_state[spillee].eq_spill_slot = True;
            } else {
               vassert(reg_usage_arr[ii].vMode[j] == HRmModify);
               rreg_state[spillee].eq_spill_slot = False;
            }
         }

         /* So after much twisting and turning, we have vreg mapped to
            rreg_state[spillee].rreg.  Note that in the map. */
         addToHRegRemap(&remap, vreg, con->univ->regs[spillee]);

      } /* iterate over virtual registers in this instruction. */

      /* We've finished clowning around with registers in this instruction.
         Three results:
         - the running rreg_state[] has been updated
         - a suitable vreg->rreg mapping for this instruction has been 
           constructed
         - spill and reload instructions may have been emitted.

        The final step is to apply the mapping to the instruction, 
        and emit that.
      */

      /* NOTE, DESTRUCTIVELY MODIFIES instrs_in->arr[ii]. */
      con->mapRegs(&remap, instrs_in->arr[ii], con->mode64);
      EMIT_INSTR( instrs_in->arr[ii] );

      if (DEBUG_REGALLOC) {
         vex_printf("After dealing with current insn:\n");
         PRINT_STATE;
         vex_printf("\n");
      }

      /* ------ Post-instruction actions for fixed rreg uses ------ */

      /* Now we need to check for rregs exiting fixed live ranges
         after this instruction, and if so mark them as free. */
      while (True) {
         vassert(rreg_lrs_db_next >= 0);
         vassert(rreg_lrs_db_next <= rreg_lrs_used);
         if (rreg_lrs_db_next == rreg_lrs_used)
            break; /* no more real reg live ranges to consider */
         if (ii+1 < rreg_lrs_db[rreg_lrs_db_next].dead_before)
            break; /* next live range does not yet start */
         vassert(ii+1 == rreg_lrs_db[rreg_lrs_db_next].dead_before);
         /* rreg_lrs_db[[rreg_lrs_db_next].rreg is exiting a hard live
            range.  Mark it as such in the main rreg_state array. */
         HReg reg = rreg_lrs_db[rreg_lrs_db_next].rreg;
         vassert(!hregIsVirtual(reg));
         Int k = hregIndex(reg);
         vassert(IS_VALID_RREGNO(k));
         vassert(rreg_state[k].disp == Unavail);
         rreg_state[k].disp = Free;
         rreg_state[k].vreg = INVALID_HREG;
         rreg_state[k].eq_spill_slot = False;

         /* check for further rregs leaving HLRs at this point */
         rreg_lrs_db_next++;
      }

      if (DEBUG_REGALLOC) {
         vex_printf("After post-insn actions for fixed regs:\n");
         PRINT_STATE;
         vex_printf("\n");
      }

   } /* iterate over insns */

   /* ------ END: Process each insn in turn. ------ */

   /* free(rreg_state); */
   /* free(rreg_lrs); */
   /* if (vreg_lrs) free(vreg_lrs); */

   /* Paranoia */
   vassert(rreg_lrs_la_next == rreg_lrs_used);
   vassert(rreg_lrs_db_next == rreg_lrs_used);

   return instrs_out;

#  undef INVALID_INSTRNO
#  undef EMIT_INSTR
#  undef PRINT_STATE
}



/*---------------------------------------------------------------*/
/*---                                       host_reg_alloc2.c ---*/
/*---------------------------------------------------------------*/
