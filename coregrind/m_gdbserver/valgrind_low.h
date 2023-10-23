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

   int stack_pointer_regno;
   /* register number of the stack pointer register */

   struct reg *reg_defs;

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

   /* Returns the address in the thread control block where dtv is found.
      Return NULL if an error occurs or no support for tls/dtv is available.
      Note that the addressability of the returned result has not been
      verified. In other words, target_get_dtv just adds some magic
      offset to the arch specific thread register or thread pointer or ... 
      
      The implementation of this is of course depending on the arch
      but also depends on the way pthread lib arranges its data.
      For background info about tls handling, read
      'ELF Handling For Thread-Local Storage'
      http://www.akkadia.org/drepper/tls.pdf
      (slightly obsolete e.g. the size of a dtv entry is 2 words now).
      The reference is the glibc source, in particular the arch specific
      file tls.h.

      For platforms where the dtv is located in the tcb, the magic offset
      to add to the thread pointer/register/... can be found by doing:
        cd none/tests
        gdb ./tls
        set debug-file-directory /usr/lib/debug # or equivalent
        start
        p &((struct pthread*)0x0)->header.dtv
      Currently the dtv offset is hardcoded, based on the assumption
      that this is relatively stable. If that would be false, then
      getoff-<platform> should be modified to output this offset e.g.
      depending on the glibc version. */
   CORE_ADDR** (*target_get_dtv)(ThreadState *tst);

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
extern void nanomips_init_architecture (struct valgrind_target_ops *target);

#endif
