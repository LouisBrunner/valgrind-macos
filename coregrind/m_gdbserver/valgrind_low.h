/* Definitions of interface to the "low" (arch specific) functions
   needed for interfacing the Valgrind gdbserver with the Valgrind
   guest.

   Copyright (C) 2011, 2012
   Free Software Foundation, Inc.

   This file has been inspired from a file that is part of GDB.
   It has been modified to integrate it in valgrind

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#ifndef VALGRIND_LOW_H
#define VALGRIND_LOW_H

#include "pub_core_basics.h"    // ThreadId
#include "server.h"             // CORE_ADDR

/* defines the characteristics of the "low" valgrind target architecture.
   In other words, struct valgrind_target_ops defines the functions and 
   data which are specific to the architecture (x86 or amd64 or 
   ppc32 or ...). */
struct valgrind_target_ops
{
   int num_regs;
   struct reg *reg_defs;

   int stack_pointer_regno;
   /* register number of the stack pointer register */
   
   /* transfer the register regno from/to valgrind (guest state)
      to/from buf
      according to transfer_direction.
      *mod set to True if destination content is modified by the transfer
      otherwise it is set to False. */
   void (*transfer_register) (ThreadId tid, int regno, void * buf, 
                              transfer_direction dir, int size, Bool *mod);
   

   CORE_ADDR (*get_pc) (void);
   void (*set_pc) (CORE_ADDR newpc);

   /* What string to report to GDB when it asks for the architecture,
      or NULL not to answer.  */
   const char *arch_string;
   
   /* Returns the target xml description of the set of registers.
      For some architectures (e.g. arm), it is mandatory
      to give a description of the registers, otherwise
      gdb does not understand the reply to the 'g' packet
      (which is used to get the registers).
      If shadow_mode, returns a target xml description
      including the two shadow registers sets.
      This is mandatory to use the option --vgdb-shadow-registers=yes. 
      Returns NULL if there is no target xml file*/
   const char* (*target_xml) (Bool shadow_mode);

};

extern void x86_init_architecture (struct valgrind_target_ops *target);
extern void amd64_init_architecture (struct valgrind_target_ops *target);
extern void arm_init_architecture (struct valgrind_target_ops *target);
extern void arm64_init_architecture (struct valgrind_target_ops *target);
extern void ppc32_init_architecture (struct valgrind_target_ops *target);
extern void ppc64_init_architecture (struct valgrind_target_ops *target);
extern void s390x_init_architecture (struct valgrind_target_ops *target);
extern void mips32_init_architecture (struct valgrind_target_ops *target);
extern void mips64_init_architecture (struct valgrind_target_ops *target);

#endif
