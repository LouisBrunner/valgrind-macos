
/*--------------------------------------------------------------------*/
/*--- Higher-level UCode sequence builders                         ---*/
/*---                                              vg_instrument.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Nicholas Nethercote
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

/* We only import vg_skin.h here, because this file only provides functions
   for doing things that could be done directly by the tool -- it's just to
   make tools' lives easier, rather than let them do something they
   couldn't otherwise do. */
#include "vg_skin.h"


void VG_(lit_to_reg)(UCodeBlock* cb, UInt lit, UInt t)
{
   uInstr2 (cb, MOV, 4, Literal, 0, TempReg, t);
   uLiteral(cb, lit);
}

UInt VG_(lit_to_newreg)(UCodeBlock* cb, UInt lit)
{
   UInt t = newTemp(cb);
   uInstr2 (cb, MOV, 4, Literal, 0, TempReg, t);
   uLiteral(cb, lit);
   return t;
}

// f()
void VG_(ccall_0_0)(UCodeBlock* cb, Addr f)
{
   uInstr0(cb, CCALL, 0);
   uCCall(cb, f, 0, 0, /*retval*/False);
}

// f(reg)
void VG_(ccall_R_0)(UCodeBlock* cb, Addr f, UInt t1, UInt regparms_n)
{
   sk_assert(regparms_n <= 1);
   uInstr1(cb, CCALL, 0, TempReg, t1);
   uCCall(cb, f, 1, regparms_n, /*retval*/False);
}

// f(lit)
void VG_(ccall_L_0)(UCodeBlock* cb, Addr f, UInt lit1, UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit1);
   VG_(ccall_R_0)(cb, f, t1, regparms_n);
}

// reg = f(reg)
void VG_(ccall_R_R)(UCodeBlock* cb, Addr f, UInt t1, UInt t_ret,
                    UInt regparms_n)
{
   sk_assert(regparms_n <= 1);
   sk_assert(t1 < VG_(get_num_temps)(cb)); // help catch lits accidentally passed in
   uInstr3(cb, CCALL, 0, TempReg, t1, NoValue, 0, TempReg, t_ret);
   uCCall(cb, f, 1, regparms_n, /*retval*/True);
}

// reg = f(lit)
void VG_(ccall_L_R)(UCodeBlock* cb, Addr f, UInt lit1, UInt t_ret,
                    UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit1);
   VG_(ccall_R_R)(cb, f, t1, t_ret, regparms_n);
}

// f(reg, reg)
void VG_(ccall_RR_0)(UCodeBlock* cb, Addr f, UInt t1, UInt t2, UInt regparms_n)
{
   sk_assert(regparms_n <= 2);
   sk_assert(t1 < VG_(get_num_temps)(cb));
   sk_assert(t2 < VG_(get_num_temps)(cb));
   uInstr2(cb, CCALL, 0, TempReg, t1, TempReg, t2);
   uCCall(cb, f, 2, regparms_n, /*retval*/False);
}

// f(reg, lit)
void VG_(ccall_RL_0)(UCodeBlock* cb, Addr f, UInt t1, UInt lit2,
                     UInt regparms_n)
{
   UInt t2 = VG_(lit_to_newreg)(cb, lit2);
   VG_(ccall_RR_0)(cb, f, t1, t2, regparms_n);
}

// f(lit, reg)
void VG_(ccall_LR_0)(UCodeBlock* cb, Addr f, UInt lit1, UInt t2,
                     UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit1);
   VG_(ccall_RR_0)(cb, f, t1, t2, regparms_n);
}

// f(lit, lit)
void VG_(ccall_LL_0)(UCodeBlock* cb, Addr f, UInt lit1, UInt lit2,
                     UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit1);
   UInt t2 = VG_(lit_to_newreg)(cb, lit2);
   VG_(ccall_RR_0)(cb, f, t1, t2, regparms_n);
}

// reg = f(reg, reg)
void VG_(ccall_RR_R)(UCodeBlock* cb, Addr f, UInt t1, UInt t2, UInt t_ret,
                     UInt regparms_n)
{
   sk_assert(regparms_n <= 2);
   sk_assert(t1 < VG_(get_num_temps)(cb));
   sk_assert(t2 < VG_(get_num_temps)(cb));
   uInstr3(cb, CCALL, 0, TempReg, t1, TempReg, t2, TempReg, t_ret);
   uCCall(cb, f, 2, regparms_n, /*retval*/True);
}

// reg = f(reg, lit)
void VG_(ccall_RL_R)(UCodeBlock* cb, Addr f, UInt t1, UInt lit2, UInt t_ret,
                     UInt regparms_n)
{
   UInt t2 = VG_(lit_to_newreg)(cb, lit2);
   VG_(ccall_RR_R)(cb, f, t1, t2, t_ret, regparms_n);
}

// reg = f(lit, reg)
void VG_(ccall_LR_R)(UCodeBlock* cb, Addr f, UInt lit1, UInt t2, UInt t_ret,
                     UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit1);
   VG_(ccall_RR_R)(cb, f, t1, t2, t_ret, regparms_n);
}

// reg = f(lit, lit)
void VG_(ccall_LL_R)(UCodeBlock* cb, Addr f, UInt lit1, UInt lit2, UInt t_ret,
                     UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit2);
   UInt t2 = VG_(lit_to_newreg)(cb, lit2);
   VG_(ccall_RR_R)(cb, f, t1, t2, t_ret, regparms_n);
}

// f(reg, reg, reg)
void VG_(ccall_RRR_0)(UCodeBlock* cb, Addr f, UInt t1, UInt t2,
                      UInt t3, UInt regparms_n)
{
   sk_assert(regparms_n <= 3);
   sk_assert(t1 < VG_(get_num_temps)(cb));
   sk_assert(t2 < VG_(get_num_temps)(cb));
   sk_assert(t3 < VG_(get_num_temps)(cb));
   uInstr3(cb, CCALL, 0, TempReg, t1, TempReg, t2, TempReg, t3);
   uCCall(cb, f, 3, regparms_n, /*retval*/False);
}

// f(reg, lit, lit)
void VG_(ccall_RLL_0)(UCodeBlock* cb, Addr f, UInt t1, UInt lit2,
                      UInt lit3, UInt regparms_n)
{
   UInt t2 = VG_(lit_to_newreg)(cb, lit2);
   UInt t3 = VG_(lit_to_newreg)(cb, lit3);
   VG_(ccall_RRR_0)(cb, f, t1, t2, t3, regparms_n);
}

// f(lit, reg, reg)
void VG_(ccall_LRR_0)(UCodeBlock* cb, Addr f, UInt lit1, UInt t2,
                      UInt t3, UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit1);
   VG_(ccall_RRR_0)(cb, f, t1, t2, t3, regparms_n);
}

// f(lit, lit, reg)
void VG_(ccall_LLR_0)(UCodeBlock* cb, Addr f, UInt lit1, UInt lit2,
                      UInt t3, UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit1);
   UInt t2 = VG_(lit_to_newreg)(cb, lit2);
   VG_(ccall_RRR_0)(cb, f, t1, t2, t3, regparms_n);
}

// f(lit, lit, lit)
void VG_(ccall_LLL_0)(UCodeBlock* cb, Addr f, UInt lit1, UInt lit2,
                      UInt lit3, UInt regparms_n)
{
   UInt t1 = VG_(lit_to_newreg)(cb, lit1);
   UInt t2 = VG_(lit_to_newreg)(cb, lit2);
   UInt t3 = VG_(lit_to_newreg)(cb, lit3);
   VG_(ccall_RRR_0)(cb, f, t1, t2, t3, regparms_n);
}

void VG_(reg_to_globvar)(UCodeBlock* cb, UInt t, UInt* globvar_ptr)
{
   Int t_gv  = VG_(lit_to_newreg)(cb, (UInt)globvar_ptr);
   uInstr2(cb, STORE, 4, TempReg, t, TempReg, t_gv);
}

void VG_(lit_to_globvar)(UCodeBlock* cb, UInt lit, UInt* globvar_ptr)
{
   Int t_lit = VG_(lit_to_newreg)(cb, lit);
   VG_(reg_to_globvar)(cb, t_lit, globvar_ptr);
}

/*--------------------------------------------------------------------
  Old versions of these functions, for backwards compatibility          
  --------------------------------------------------------------------*/

void VG_(call_helper_0_0)(UCodeBlock* cb, Addr f)
{
   VG_(ccall_0_0)(cb, f);
}

void VG_(call_helper_1_0)(UCodeBlock* cb, Addr f, UInt arg1, UInt regparms_n)
{
   VG_(ccall_L_0)(cb, f, arg1, regparms_n);
}

void VG_(call_helper_2_0)(UCodeBlock* cb, Addr f, UInt arg1, UInt arg2,
                         UInt regparms_n)
{
   VG_(ccall_LL_0)(cb, f, arg1, arg2, regparms_n);
}

void VG_(set_global_var)(UCodeBlock* cb, Addr globvar_ptr, UInt val)
{
   VG_(lit_to_globvar)(cb, val, (UInt*)globvar_ptr);
}

void VG_(set_global_var_tempreg)(UCodeBlock* cb, Addr globvar_ptr, UInt t_val)
{
   VG_(reg_to_globvar)(cb, t_val, (UInt*)globvar_ptr);
}

/*--------------------------------------------------------------------*/
/*--- end                                          vg_instrument.c ---*/
/*--------------------------------------------------------------------*/


