/*--------------------------------------------------------------------*/
/*--- Higher-level UCode sequence builders                         ---*/
/*---                                              vg_instrument.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Nicholas Nethercote
      njn25@cam.ac.uk

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

   The GNU General Public License is contained in the file COPYING.
*/

// SSS: should this file should eventually not be in core, but included in
// skins that use it??  Reduces size of core, but increases size of every
// skin that uses it...

/* We only import vg_skin.h here, because this file only provides functions
   for doing things that could be done directly by the skin -- it's just to
   make skins' lives easier, rather than let them do something they
   couldn't otherwise do. */
#include "vg_skin.h"

#define uInstr0   VG_(new_UInstr0)
#define uInstr1   VG_(new_UInstr1)
#define uInstr2   VG_(new_UInstr2)
#define uLiteral  VG_(set_lit_field)
#define uCCall    VG_(set_ccall_fields)
#define newTemp   VG_(get_new_temp)


void VG_(call_helper_0_0)(UCodeBlock* cb, Addr f)
{
   uInstr0(cb, CCALL, 0);
   uCCall(cb, f, 0, 0, 0);
}

void VG_(call_helper_1_0)(UCodeBlock* cb, Addr f, UInt arg1, UInt regparms_n)
{
   UInt t1 = newTemp(cb);

   vg_assert(regparms_n <= 1);
   uInstr2(cb, MOV,   4, Literal, 0, TempReg, t1);
   uLiteral(cb, arg1);
   uInstr1(cb, CCALL, 0, TempReg, t1);
   uCCall(cb, f, 1, regparms_n, 0);
}

void VG_(call_helper_2_0)(UCodeBlock* cb, Addr f, UInt arg1, UInt arg2,
                         UInt regparms_n)
{
   UInt t1 = newTemp(cb);
   UInt t2 = newTemp(cb);

   vg_assert(regparms_n <= 2);
   uInstr2(cb, MOV,   4, Literal, 0, TempReg, t1);
   uLiteral(cb, arg1);
   uInstr2(cb, MOV,   4, Literal, 0, TempReg, t2);
   uLiteral(cb, arg2);
   uInstr2(cb, CCALL, 0, TempReg, t1, TempReg, t2);
   uCCall(cb, f, 2, regparms_n, 0);
}

void VG_(set_global_var)(UCodeBlock* cb, Addr globvar_ptr, UInt val)
{
   Int t_gv  = newTemp(cb);        
   Int t_val = newTemp(cb);        

   uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_val);
   uLiteral(cb, val);
   uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_gv);
   uLiteral(cb, globvar_ptr);
   uInstr2(cb, STORE, 4, TempReg, t_val, TempReg, t_gv);
}

/*--------------------------------------------------------------------*/
/*--- end                                          vg_instrument.c ---*/
/*--------------------------------------------------------------------*/

