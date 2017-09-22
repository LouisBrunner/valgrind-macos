
/*---------------------------------------------------------------*/
/*--- begin                               host_generic_regs.c ---*/
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


/*---------------------------------------------------------*/
/*--- Representing HOST REGISTERS                       ---*/
/*---------------------------------------------------------*/

void ppHRegClass ( HRegClass hrc )
{
   switch (hrc) {
      case HRcInt32:   vex_printf("HRcInt32"); break;
      case HRcInt64:   vex_printf("HRcInt64"); break;
      case HRcFlt32:   vex_printf("HRcFlt32"); break;
      case HRcFlt64:   vex_printf("HRcFlt64"); break;
      case HRcVec64:   vex_printf("HRcVec64"); break;
      case HRcVec128:  vex_printf("HRcVec128"); break;
      default: vpanic("ppHRegClass");
   }
}

/* Generic printing for registers. */
UInt ppHReg ( HReg r )
{
   if (hregIsInvalid(r)) {
      return vex_printf("HReg_INVALID");
   }
   const Bool   isV     = hregIsVirtual(r);
   const HChar* maybe_v = isV ? "v" : "";
   const UInt   regNN   = isV ? hregIndex(r) : hregEncoding(r);
   /* For real registers, we show the encoding.  But the encoding is
      always zero for virtual registers, so that's pointless -- hence
      show the index number instead. */
   switch (hregClass(r)) {
      case HRcInt32:   return vex_printf("%%%sr%u", maybe_v, regNN);
      case HRcInt64:   return vex_printf("%%%sR%u", maybe_v, regNN);
      case HRcFlt32:   return vex_printf("%%%sF%u", maybe_v, regNN);
      case HRcFlt64:   return vex_printf("%%%sD%u", maybe_v, regNN);
      case HRcVec64:   return vex_printf("%%%sv%u", maybe_v, regNN);
      case HRcVec128:  return vex_printf("%%%sV%u", maybe_v, regNN);
      default: vpanic("ppHReg");
   }
}


/*---------------------------------------------------------*/
/*--- Real register Universes.                          ---*/
/*---------------------------------------------------------*/

void RRegUniverse__init ( /*OUT*/RRegUniverse* univ )
{
   *univ = (RRegUniverse){};
   univ->size      = 0;
   univ->allocable = 0;
   for (UInt i = 0; i < N_RREGUNIVERSE_REGS; i++) {
      univ->regs[i] = INVALID_HREG;
   }

   for (UInt i = 0; i <= HrcLAST; i++) {
      univ->allocable_start[i] = N_RREGUNIVERSE_REGS;
      univ->allocable_end[i]   = N_RREGUNIVERSE_REGS;
   }
}

void RRegUniverse__check_is_sane ( const RRegUniverse* univ )
{
   /* Check Real-Register-Universe invariants.  All of these are
      important. */
   vassert(univ->size > 0);
   vassert(univ->size <= N_RREGUNIVERSE_REGS);
   vassert(univ->allocable <= univ->size);
   for (UInt i = 0; i < univ->size; i++) {
      HReg reg = univ->regs[i];
      vassert(!hregIsInvalid(reg));
      vassert(!hregIsVirtual(reg));
      vassert(hregIndex(reg) == i);
   }
   for (UInt i = univ->size; i < N_RREGUNIVERSE_REGS; i++) {
      HReg reg = univ->regs[i];
      vassert(hregIsInvalid(reg));
   }

   /* Determine register classes used and if they form contiguous range. */
   Bool regclass_used[HrcLAST + 1];
   for (UInt i = 0; i <= HrcLAST; i++) {
      regclass_used[i] = False;
   }

   for (UInt i = 0; i < univ->allocable; i++) {
      HReg reg = univ->regs[i];
      HRegClass regclass = hregClass(reg);
      if (!regclass_used[regclass]) {
         regclass_used[regclass] = True;
      }
   }

   UInt regs_visited = 0;
   for (UInt i = 0; i <= HrcLAST; i++) {
      if (regclass_used[i]) {
         for (UInt j = univ->allocable_start[i];
              j <= univ->allocable_end[i]; j++) {
            vassert(hregClass(univ->regs[j]) == i);
            regs_visited += 1;
         }
      }
   }

   vassert(regs_visited == univ->allocable);
}


/*---------------------------------------------------------*/
/*--- Helpers for recording reg usage (for reg-alloc)   ---*/
/*---------------------------------------------------------*/

void ppHRegUsage ( const RRegUniverse* univ, HRegUsage* tab )
{
   /* This is going to fail miserably if N_RREGUNIVERSE_REGS exceeds
      64.  So let's cause it to fail in an obvious way. */
   vassert(N_RREGUNIVERSE_REGS == 64);

   vex_printf("HRegUsage {\n");
   /* First print the real regs */
   for (UInt i = 0; i < N_RREGUNIVERSE_REGS; i++) {
      Bool rRd = (tab->rRead    & (1ULL << i)) != 0;
      Bool rWr = (tab->rWritten & (1ULL << i)) != 0;
      const HChar* str = "Modify ";
      /**/ if (!rRd && !rWr) { continue; }
      else if ( rRd && !rWr) { str = "Read   "; }
      else if (!rRd &&  rWr) { str = "Write  "; }
      /* else "Modify" is correct */
      vex_printf("   %s ", str);
      ppHReg(univ->regs[i]);
      vex_printf("\n");
   }
   /* and now the virtual registers */
   for (UInt i = 0; i < tab->n_vRegs; i++) {
      const HChar* str = NULL;
      switch (tab->vMode[i]) {
         case HRmRead:   str = "Read   "; break;
         case HRmWrite:  str = "Write  "; break;
         case HRmModify: str = "Modify "; break;
         default: vpanic("ppHRegUsage");
      }
      vex_printf("   %s ", str);
      ppHReg(tab->vRegs[i]);
      vex_printf("\n");
   }
   if (tab->isRegRegMove) {
      vex_printf("   (is a reg-reg move)\n");
   }
   vex_printf("}\n");
}


/* Add a register to a usage table.  Combines incoming read uses with
   existing write uses into a modify use, and vice versa.  Does not
   create duplicate entries -- each reg is only mentioned once.  
*/
void addHRegUse ( HRegUsage* tab, HRegMode mode, HReg reg )
{
   /* Because real and virtual registers are represented differently,
      they have completely different paths here. */
   if (LIKELY(hregIsVirtual(reg))) {
      /* Virtual register */
      UInt i;
      /* Find it ... */
      for (i = 0; i < tab->n_vRegs; i++)
         if (sameHReg(tab->vRegs[i], reg))
            break;
      if (i == tab->n_vRegs) {
         /* Not found, add new entry. */
         vassert(tab->n_vRegs < N_HREGUSAGE_VREGS);
         tab->vRegs[tab->n_vRegs] = reg;
         tab->vMode[tab->n_vRegs] = mode;
         tab->n_vRegs++;
      } else {
         /* Found: combine or ignore. */
         /* This is a greatest-lower-bound operation in the poset:

               R   W
                \ /
                 M

            Need to do: tab->mode[i] = GLB(tab->mode, mode).  In this
            case very simple -- if tab->mode[i] != mode then result must
            be M.
         */
         if (tab->vMode[i] == mode) {
            /* duplicate, ignore */
         } else {
            tab->vMode[i] = HRmModify;
         }
      }
   } else {
      /* Real register */
      UInt ix = hregIndex(reg);
      vassert(ix < N_RREGUNIVERSE_REGS);
      ULong mask = 1ULL << ix;
      switch (mode) {
         case HRmRead:   tab->rRead |= mask; break;
         case HRmWrite:  tab->rWritten |= mask; break;
         case HRmModify: tab->rRead |= mask; tab->rWritten |= mask; break;
         default: vassert(0);
      }
   }
}

Bool HRegUsage__contains ( const HRegUsage* tab, HReg reg )
{
   vassert(!hregIsInvalid(reg));
   if (hregIsVirtual(reg)) {
      for (UInt i = 0; i < tab->n_vRegs; i++) {
         if (sameHReg(reg, tab->vRegs[i]))
            return True;
      }
      return False;
   } else {
      UInt ix = hregIndex(reg);
      vassert(ix < N_RREGUNIVERSE_REGS);
      ULong mentioned = tab->rRead | tab->rWritten;
      return (mentioned & (1ULL << ix)) != 0;
   }
   /*NOTREACHED*/
}


/*---------------------------------------------------------*/
/*--- Indicating register remappings (for reg-alloc)    ---*/
/*---------------------------------------------------------*/

void ppHRegRemap ( HRegRemap* map )
{
   Int   i;
   vex_printf("HRegRemap {\n");
   for (i = 0; i < map->n_used; i++) {
      vex_printf("   ");
      ppHReg(map->orig[i]);
      vex_printf("  -->  ");
      ppHReg(map->replacement[i]);
      vex_printf("\n");
   }
   vex_printf("}\n");
}


void addToHRegRemap ( HRegRemap* map, HReg orig, HReg replacement )
{
   Int i;
   for (i = 0; i < map->n_used; i++)
      if (sameHReg(map->orig[i], orig))
         vpanic("addToHRegMap: duplicate entry");
   if (!hregIsVirtual(orig))
      vpanic("addToHRegMap: orig is not a vreg");
   if (hregIsVirtual(replacement))
      vpanic("addToHRegMap: replacement is a vreg");

   vassert(map->n_used+1 < N_HREG_REMAP);
   map->orig[map->n_used]        = orig;
   map->replacement[map->n_used] = replacement;
   map->n_used++;
}


HReg lookupHRegRemap ( HRegRemap* map, HReg orig )
{
   Int i;
   if (!hregIsVirtual(orig))
      return orig;
   for (i = 0; i < map->n_used; i++)
      if (sameHReg(map->orig[i], orig))
         return map->replacement[i];
   vpanic("lookupHRegRemap: not found");
}


/*---------------------------------------------------------*/
/*--- Abstract instructions                             ---*/
/*---------------------------------------------------------*/

HInstrArray* newHInstrArray ( void )
{
   HInstrArray* ha = LibVEX_Alloc_inline(sizeof(HInstrArray));
   ha->arr_size = 4;
   ha->arr_used = 0;
   ha->arr      = LibVEX_Alloc_inline(ha->arr_size * sizeof(HInstr*));
   ha->n_vregs  = 0;
   return ha;
}

__attribute__((noinline))
void addHInstr_SLOW ( HInstrArray* ha, HInstr* instr )
{
   vassert(ha->arr_used == ha->arr_size);
   Int      i;
   HInstr** arr2 = LibVEX_Alloc_inline(ha->arr_size * 2 * sizeof(HInstr*));
   for (i = 0; i < ha->arr_size; i++) {
      arr2[i] = ha->arr[i];
   }
   ha->arr_size *= 2;
   ha->arr = arr2;
   addHInstr(ha, instr);
}


/*---------------------------------------------------------*/
/*--- C-Call return-location actions                    ---*/
/*---------------------------------------------------------*/

void ppRetLoc ( RetLoc ska )
{
   switch (ska.pri) {
      case RLPri_INVALID:
         vex_printf("RLPri_INVALID"); return;
      case RLPri_None:
         vex_printf("RLPri_None");    return;
      case RLPri_Int:
         vex_printf("RLPri_Int");     return;
      case RLPri_2Int:
         vex_printf("RLPri_2Int");    return;
      case RLPri_V128SpRel:
         vex_printf("RLPri_V128SpRel(%d)", ska.spOff); return;
      case RLPri_V256SpRel:
         vex_printf("RLPri_V256SpRel(%d)", ska.spOff); return;
      default:
         vpanic("ppRetLoc");
   }
}


/*---------------------------------------------------------------*/
/*--- end                                 host_generic_regs.c ---*/
/*---------------------------------------------------------------*/
