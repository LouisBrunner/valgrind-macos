
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host_regs.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>

#include "basictypes.h"
#include "host_regs.h"


HReg mkHReg ( UInt regno, HRegClass rc, Bool virtual )
{
   assert((regno & 0xFF000000) == 0);
   return (regno << 8) | (((UInt)rc) << 4) | (virtual ? 1 : 0);
}

HRegClass hregClass ( HReg r )
{
   UInt rc = (r & 0x000000F0) >> 4;
   assert(rc <= 2);
   return (HRegClass)rc;
}

Bool hregIsVirtual ( HReg r )
{
   return (r & 1) ? True : False;
}

UInt hregNumber ( HReg r )
{
   return (((UInt)r) >> 8);
}


/* Generic printing for registers. */
void ppHReg ( FILE* f, HReg r ) 
{
   Char* maybe_v = hregIsVirtual(r) ? "v" : "";
   Int   regNo   = hregNumber(r);
   switch (hregClass(r)) {
      case HRcInt:    fprintf(f, "%%%sr%d", maybe_v, regNo); return;
      case HRcFloat:  fprintf(f, "%%%sf%d", maybe_v, regNo); return;
      case HRcVector: fprintf(f, "%%%sv%d", maybe_v, regNo); return;
      default: panic("ppHReg");
   }
}


/*---------------------------------------------------------*/
/*--- Helpers for recording reg usage (for reg-alloc)   ---*/
/*---------------------------------------------------------*/

void ppHRegUsage ( FILE* f, HRegUsage* tab )
{
   Int   i;
   Char* str;
   fprintf(f, "HRegUsage {\n");
   for (i = 0; i < tab->n_used; i++) {
      switch (tab->mode[i]) {
         case HRmRead:   str = "Read   "; break;
         case HRmWrite:  str = "Write  "; break;
         case HRmModify: str = "Modify "; break;
         default: panic("ppHRegUsage");
      }
      fprintf(f, "   %s ", str);
      ppHReg(f, tab->hreg[i]);
      fprintf(f, "\n");
   }
   fprintf(f, "}\n");
}


void initHRegUsage ( HRegUsage* tab )
{
   tab->n_used = 0;
}


/* Add a register to a usage table.  Combine incoming read uses with
   existing write uses into a modify use, and vice versa.  Do not
   create duplicate entries -- each reg should only be mentioned once.  
*/
void addHRegUsage ( HRegUsage* tab, HReg reg, HRegMode mode )
{
   Int i;
   /* Find it ... */
   for (i = 0; i < tab->n_used; i++)
      if (tab->hreg[i] == reg)
         break;
   if (i == tab->n_used) {
      /* Not found, add new entry. */
      assert(tab->n_used+1 < N_HREG_USAGE);
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
      assert( (mode == HRmRead && tab->mode[i] == HRmWrite)
              || (mode == HRmWrite && tab->mode[i] == HRmRead) );
      tab->mode[i] = HRmModify;
   }
}


/*---------------------------------------------------------*/
/*--- Indicating register remappings (for reg-alloc)    ---*/
/*---------------------------------------------------------*/

void ppHRegRemap ( FILE* f, HRegRemap* map )
{
   Int   i;
   fprintf(f, "HRegRemap {\n");
   for (i = 0; i < map->n_used; i++) {
      fprintf(f, "   ");
      ppHReg(f, tab->orig[i]);
      fprintf(f, "  -->  ");
      ppHReg(f, tab->replacement[i]);
      fprintf(f, "\n");
   }
   fprintf(f, "}\n");
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
         panic("addToHRegMap: duplicate entry");
   assert(map->n_used+1 < N_HREG_REMAP);
   map->orig[map->n_used]        = orig;
   map->replacement[map->n_used] = replacement;
   map->n_used++;
}


HReg lookupHRegRemap ( HRegRemap* map, HReg orig )
{
   Int i;
   for (i = 0; i < map->n_used; i++)
      if (map->orig[i] == orig)
         return map->replacement[i];
   panic("lookupHRegRemap: not found");
}

/*---------------------------------------------------------------*/
/*---                                             host_regs.c ---*/
/*---------------------------------------------------------------*/
