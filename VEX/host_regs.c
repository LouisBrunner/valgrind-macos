
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host_regs.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>
#include <malloc.h>

#include "basictypes.h"
#include "host_regs.h"


HReg mkHReg ( UInt regno, HRegClass rc, Bool virtual )
{
   UInt r24 = regno & 0x00FFFFFF;
   /* This is critical.  The register number field may only
      occupy 24 bits. */
   if (r24 != regno)
     panic("mkHReg: regno exceeds 2^24");
   return regno | (((UInt)rc) << 28) | (virtual ? (1<<24) : 0);
}

HRegClass hregClass ( HReg r )
{
   UInt rc = r;
   rc = (rc >> 28) & 0x0F;
   assert(rc == HRcInt || rc == HRcFloat || rc == HRcVector);
   return (HRegClass)rc;
}

Bool hregIsVirtual ( HReg r )
{
   return (((UInt)r) & (1<<24)) ? True : False;
}

UInt hregNumber ( HReg r )
{
   return ((UInt)r) & 0x00FFFFFF;
}

void ppHRegClass ( FILE* f, HRegClass hrc )
{
   switch (hrc) {
      case HRcInt:       fprintf(f, "HRcInt32"); break;
      case HRcInt64:     fprintf(f, "HRcInt64"); break;
      case HRcFloat:     fprintf(f, "HRcFloat"); break;
      case HRcVector:    fprintf(f, "HRcVector64"); break;
      case HRcVector128: fprintf(f, "HRcVector128"); break;
      default: panic("ppHRegClass");
   }
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
void addHRegUse ( HRegUsage* tab, HRegMode mode, HReg reg )
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
      ppHReg(f, map->orig[i]);
      fprintf(f, "  -->  ");
      ppHReg(f, map->replacement[i]);
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
   if (!hregIsVirtual(orig))
      panic("addToHRegMap: orig is not a vreg");
   if (hregIsVirtual(replacement))
      panic("addToHRegMap: replacement is not a vreg");

   assert(map->n_used+1 < N_HREG_REMAP);
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
   panic("lookupHRegRemap: not found");
}

/*---------------------------------------------------------*/
/*--- Abstract instructions                             ---*/
/*---------------------------------------------------------*/

HInstrArray* newHInstrArray ( void )
{
   HInstrArray* ha = malloc(sizeof(HInstrArray));
   ha->arr_size = 4;
   ha->arr_used = 0;
   ha->arr = malloc(ha->arr_size * sizeof(HInstr*));
   return ha;
}

void deleteHInstrArray ( HInstrArray* ha )
{
   free(ha->arr);
   free(ha);
}

void addHInstr ( HInstrArray* ha, HInstr* instr )
{
   assert(ha->arr_used <= ha->arr_size);
   if (ha->arr_used < ha->arr_size) {
      ha->arr[ha->arr_used] = instr;
      ha->arr_used++;
   } else {
      Int      i;
      HInstr** arr2 = malloc(ha->arr_size * 2 * sizeof(HInstr*));
      for (i = 0; i < ha->arr_size; i++)
         arr2[i] = ha->arr[i];
      ha->arr_size *= 2;
      free(ha->arr);
      ha->arr = arr2;
      addHInstr(ha, instr);
   }
}


/*---------------------------------------------------------------*/
/*---                                             host_regs.c ---*/
/*---------------------------------------------------------------*/
