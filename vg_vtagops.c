
/*--------------------------------------------------------------------*/
/*--- Supporting routines for v-tag operations.                    ---*/
/*---                                                 vg_vtagops.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"
#include "vg_constants.h"


/* ---------------------------------------------------------------------
   Names of the tag ops.
   ------------------------------------------------------------------ */

Char* VG_(nameOfTagOp) ( VgTagOp h )
{
   switch (h) {
      case VgT_PCast40:        return "PCast40";
      case VgT_PCast20:        return "PCast20";
      case VgT_PCast10:        return "PCast10";
      case VgT_PCast01:        return "PCast01";
      case VgT_PCast02:        return "PCast02";
      case VgT_PCast04:        return "PCast04";
      case VgT_PCast14:        return "PCast14";
      case VgT_PCast12:        return "PCast12";
      case VgT_PCast11:        return "PCast11";
      case VgT_Left4:          return "Left4";
      case VgT_Left2:          return "Left2";
      case VgT_Left1:          return "Left1";
      case VgT_SWiden14:       return "SWiden14";
      case VgT_SWiden24:       return "SWiden24";
      case VgT_SWiden12:       return "SWiden12";
      case VgT_ZWiden14:       return "ZWiden14";
      case VgT_ZWiden24:       return "ZWiden24";
      case VgT_ZWiden12:       return "ZWiden12";
      case VgT_UifU4:          return "UifU4";
      case VgT_UifU2:          return "UifU2";
      case VgT_UifU1:          return "UifU1";
      case VgT_UifU0:          return "UifU0";
      case VgT_DifD4:          return "DifD4";
      case VgT_DifD2:          return "DifD2";
      case VgT_DifD1:          return "DifD1";
      case VgT_ImproveAND4_TQ: return "ImproveAND4_TQ";
      case VgT_ImproveAND2_TQ: return "ImproveAND2_TQ";
      case VgT_ImproveAND1_TQ: return "ImproveAND1_TQ";
      case VgT_ImproveOR4_TQ:  return "ImproveOR4_TQ";
      case VgT_ImproveOR2_TQ:  return "ImproveOR2_TQ";
      case VgT_ImproveOR1_TQ:  return "ImproveOR1_TQ";
      case VgT_DebugFn:        return "DebugFn";
      default: VG_(panic)("vg_nameOfTagOp");
   }
}


/* ---------------------------------------------------------------------
   Debugging stuff.
   ------------------------------------------------------------------ */

/* Implementation for checking tag values. */

UInt VG_(DebugFn) ( UInt a1, UInt a2 )
{
   vg_assert(2+2 == 5);
   return 0;
}


/*--------------------------------------------------------------------*/
/*--- end                                             vg_vtagops.c ---*/
/*--------------------------------------------------------------------*/
