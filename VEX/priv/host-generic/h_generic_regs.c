
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-generic/h_generic_regs.c) is            ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "main/vex_util.h"
#include "host-generic/h_generic_regs.h"


void ppHRegClass ( HRegClass hrc )
{
   switch (hrc) {
      case HRcInt:       vex_printf("HRcInt32"); break;
      case HRcInt64:     vex_printf("HRcInt64"); break;
      case HRcFloat:     vex_printf("HRcFloat"); break;
      case HRcVector:    vex_printf("HRcVector64"); break;
      case HRcVector128: vex_printf("HRcVector128"); break;
      default: vpanic("ppHRegClass");
   }
}

/* Generic printing for registers. */
void ppHReg ( HReg r ) 
{
   Char* maybe_v = hregIsVirtual(r) ? "v" : "";
   Int   regNo   = hregNumber(r);
   switch (hregClass(r)) {
      case HRcInt:       vex_printf("%%%sr%d", maybe_v, regNo); return;
      case HRcInt64:     vex_printf("%%%sR%d", maybe_v, regNo); return;
      case HRcFloat:     vex_printf("%%%sf%d", maybe_v, regNo); return;
      case HRcVector:    vex_printf("%%%sv%d", maybe_v, regNo); return;
      case HRcVector128: vex_printf("%%%sV%d", maybe_v, regNo); return;
      default: vpanic("ppHReg");
   }
}


/*---------------------------------------------------------*/
/*--- Helpers for recording reg usage (for reg-alloc)   ---*/
/*---------------------------------------------------------*/

void ppHRegUsage ( HRegUsage* tab )
{
   Int   i;
   Char* str;
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
