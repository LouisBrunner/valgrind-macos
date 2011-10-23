
/*---------------------------------------------------------------*/
/*--- begin                               host_generic_regs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2011 OpenWorks LLP
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
void ppHReg ( HReg r ) 
{
   HChar* maybe_v = hregIsVirtual(r) ? "v" : "";
   Int    regNo   = hregNumber(r);
   switch (hregClass(r)) {
      case HRcInt32:   vex_printf("%%%sr%d", maybe_v, regNo); return;
      case HRcInt64:   vex_printf("%%%sR%d", maybe_v, regNo); return;
      case HRcFlt32:   vex_printf("%%%sF%d", maybe_v, regNo); return;
      case HRcFlt64:   vex_printf("%%%sD%d", maybe_v, regNo); return;
      case HRcVec64:   vex_printf("%%%sv%d", maybe_v, regNo); return;
      case HRcVec128:  vex_printf("%%%sV%d", maybe_v, regNo); return;
      default: vpanic("ppHReg");
   }
}


/*---------------------------------------------------------*/
/*--- Helpers for recording reg usage (for reg-alloc)   ---*/
/*---------------------------------------------------------*/

void ppHRegUsage ( HRegUsage* tab )
{
   Int    i;
   HChar* str;
   vex_printf("HRegUsage {\n");
   for (i = 0; i < tab->n_used; i++) {
      switch (tab->mode[i]) {
         case HRmRead:   str = "Read   "; break;
         case HRmWrite:  str = "Write  "; break;
         case HRmModify: str = "Modify "; break;
         default: vpanic("ppHRegUsage");
      }
      vex_printf("   %s ", str);
      ppHReg(tab->hreg[i]);
      vex_printf("\n");
   }
   vex_printf("}\n");
}


/* Add a register to a usage table.  Combine incoming read uses with
   existing write uses into a modify use, and vice versa.  Do not
   create duplicate entries -- each reg should only be mentioned once.  
*/
void addHRegUse ( HRegUsage* tab, HRegMode mode, HReg reg )
{
   Int i;
   /* Find it ... */
   for (i = 0; i < tab->n_used; i++)
      if (tab->hreg[i] == reg)
         break;
   if (i == tab->n_used) {
      /* Not found, add new entry. */
      vassert(tab->n_used < N_HREG_USAGE);
      tab->hreg[tab->n_used] = reg;
      tab->mode[tab->n_used] = mode;
      tab->n_used++;
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
      if (tab->mode[i] == mode) {
         /* duplicate, ignore */
      } else {
         tab->mode[i] = HRmModify;
      }
   }
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


void initHRegRemap ( HRegRemap* map )
{
   map->n_used = 0;
}


void addToHRegRemap ( HRegRemap* map, HReg orig, HReg replacement )
{
   Int i;
   for (i = 0; i < map->n_used; i++)
      if (map->orig[i] == orig)
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
      if (map->orig[i] == orig)
         return map->replacement[i];
   vpanic("lookupHRegRemap: not found");
}

/*---------------------------------------------------------*/
/*--- Abstract instructions                             ---*/
/*---------------------------------------------------------*/

HInstrArray* newHInstrArray ( void )
{
   HInstrArray* ha = LibVEX_Alloc(sizeof(HInstrArray));
   ha->arr_size = 4;
   ha->arr_used = 0;
   ha->arr      = LibVEX_Alloc(ha->arr_size * sizeof(HInstr*));
   ha->n_vregs  = 0;
   return ha;
}

void addHInstr ( HInstrArray* ha, HInstr* instr )
{
   vassert(ha->arr_used <= ha->arr_size);
   if (ha->arr_used < ha->arr_size) {
      ha->arr[ha->arr_used] = instr;
      ha->arr_used++;
   } else {
      Int      i;
      HInstr** arr2 = LibVEX_Alloc(ha->arr_size * 2 * sizeof(HInstr*));
      for (i = 0; i < ha->arr_size; i++)
         arr2[i] = ha->arr[i];
      ha->arr_size *= 2;
      ha->arr = arr2;
      addHInstr(ha, instr);
   }
}


/*---------------------------------------------------------------*/
/*--- end                                 host_generic_regs.c ---*/
/*---------------------------------------------------------------*/
