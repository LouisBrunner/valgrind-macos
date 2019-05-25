
/*--------------------------------------------------------------------*/
/*--- Header included by every core C file.      pub_core_basics.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_BASICS_H
#define __PUB_CORE_BASICS_H

//--------------------------------------------------------------------
// PURPOSE: This header should be imported by every single C file
// in the core.  It contains the basic types and other things needed
// everywhere.
//--------------------------------------------------------------------

#include "pub_tool_basics.h"

/* ---------------------------------------------------------------------
   A struct to hold starting values for stack unwinding.
   ------------------------------------------------------------------ */

/* This really shouldn't be here.  But putting it elsewhere leads to a
   veritable swamp of new module cycles. */

/* To support CFA-based stack unwinding, and stack unwinding in
   general, we need to be able to get hold of the values of specific
   registers, in order to start the unwinding process.  This is
   unavoidably arch and platform dependent.  Here is a struct which
   holds the relevant values.  All platforms must have a program
   counter and a stack pointer register, but the other fields (frame
   pointer? link register? misc other regs?) are ad-hoc.  Note, the
   common fields are 64-bit, so as to make this host-independent. */

typedef
   struct {
      ULong r_pc; /* x86:EIP, amd64:RIP, ppc:CIA, arm:R15, mips:pc */
      ULong r_sp; /* x86:ESP, amd64:RSP, ppc:R1,  arm:R13, mips:sp */
      union {
         struct {
            UInt r_ebp;
         } X86;
         struct {
            ULong r_rbp;
         } AMD64;
         struct {
            UInt r_lr;
         } PPC32;
         struct {
            ULong r_lr;
         } PPC64;
         struct {
            UInt r14;
            UInt r12;
            UInt r11;
            UInt r7;
         } ARM;
         struct {
            ULong x29; /* FP */
            ULong x30; /* LR */
         } ARM64;
         struct {
            ULong r_fp;
            ULong r_lr;
            ULong r_f0;
            ULong r_f1;
            ULong r_f2;
            ULong r_f3;
            ULong r_f4;
            ULong r_f5;
            ULong r_f6;
            ULong r_f7;
         } S390X;
         struct {
            UInt r30;  /* Stack frame pointer or subroutine variable  */
            UInt r31;  /* Return address of the last subroutine call */
            UInt r28;
         } MIPS32;
         struct {
            ULong r30;  /* Stack frame pointer or subroutine variable */
            ULong r31;  /* Return address of the last subroutine call */
            ULong r28;
         } MIPS64;
      } misc;
   }
   UnwindStartRegs;


#endif   // __PUB_CORE_BASICS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
