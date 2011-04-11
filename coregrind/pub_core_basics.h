
/*--------------------------------------------------------------------*/
/*--- Header included by every core C file.      pub_core_basics.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward
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
   Other headers to include
   ------------------------------------------------------------------ */

// Might as well have the following two in here, their contents are used so
// broadly (eg. in pub_core_threadstate.h).

#include "libvex.h"

#if defined(VGA_x86)
#  include "libvex_guest_x86.h"
#elif defined(VGA_amd64)
#  include "libvex_guest_amd64.h"
#elif defined(VGA_ppc32)
#  include "libvex_guest_ppc32.h"
#elif defined(VGA_ppc64)
#  include "libvex_guest_ppc64.h"
#elif defined(VGA_arm)
#  include "libvex_guest_arm.h"
#elif defined(VGA_s390x)
#  include "libvex_guest_s390x.h"
#else
#  error Unknown arch
#endif


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
      ULong r_pc; /* x86:EIP, amd64:RIP, ppc:CIA, arm:R15 */
      ULong r_sp; /* x86:ESP, amd64:RSP, ppc:R1,  arm:R13 */
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
            ULong r_fp;
            ULong r_lr;
         } S390X;
      } misc;
   }
   UnwindStartRegs;


#endif   // __PUB_CORE_BASICS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
