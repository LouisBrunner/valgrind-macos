
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

