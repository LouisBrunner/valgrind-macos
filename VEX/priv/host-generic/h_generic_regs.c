
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-generic/h_generic_regs.c) is            ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004 OpenWorks, LLP.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; Version 2 dated June 1991 of the
   license.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, or liability
   for damages.  See the GNU General Public License for more details.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.
*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "main/vex_util.h"
#include "host-generic/h_generic_regs.h"


void ppHRegClass ( HRegClass hrc )
{
   switch (hrc) {
      case HRcInt32:   vex_printf("HRcInt32"); break;
      case HRcInt64:   vex_printf("HRcInt64"); break;
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
      case HRcFlt64:   vex_printf("%%%sF%d", maybe_v, regNo); return;
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
      if (tab->mode[i] == mode)
         return; /* duplicate, ignore */
      if (mode == HRmModify) {
         tab->mode[i] = HRmModify;
         return; /* modify mode makes previous mode irrelevant */
      }
      vassert( (mode == HRmRead && tab->mode[i] == HRmWrite)
              || (mode == HRmWrite && tab->mode[i] == HRmRead) );
      tab->mode[i] = HRmModify;
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
      vpanic("addToHRegMap: replacement is not a vreg");

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
/*--- end                       host-generic/h_generic_regs.c ---*/
/*---------------------------------------------------------------*/
