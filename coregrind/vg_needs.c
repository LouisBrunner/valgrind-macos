
/*--------------------------------------------------------------------*/
/*--- Stuff relating to tool data structures.                      ---*/
/*---                                                   vg_needs.c ---*/
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

#include "vg_include.h"


/* ---------------------------------------------------------------------
   Tool data structure initialisation
   ------------------------------------------------------------------ */

/* Init with default values. */
VgDetails VG_(details) = {
   .name                  = NULL,
   .version               = NULL,
   .description           = NULL,
   .copyright_author      = NULL,
   .bug_reports_to        = NULL,
   .avg_translation_sizeB = VG_DEFAULT_TRANS_SIZEB,
};

VgNeeds VG_(needs) = {
   .core_errors          = False,
   .skin_errors          = False,
   .libc_freeres         = False,
   .basic_block_discards = False,
   .shadow_regs          = False,
   .command_line_options = False,
   .client_requests      = False,
   .extended_UCode       = False,
   .syscall_wrapper      = False,
   .sanity_checks        = False,
   .data_syms	         = False,
   .shadow_memory        = False,
};

/* static */
void VG_(sanity_check_needs) ( void)
{
#define CHECK_NOT(var, value)                               \
   if ((var)==(value)) {                                    \
      VG_(printf)("\nTool error: `%s' not initialised\n",   \
                  VG__STRING(var));                         \
      VG_(skin_panic)("Uninitialised details field\n");     \
   }
   
   /* Ones that must be set */
   CHECK_NOT(VG_(details).name,             NULL);
   /* Nb: .version can be NULL */
   CHECK_NOT(VG_(details).description,      NULL);
   CHECK_NOT(VG_(details).copyright_author, NULL);
   CHECK_NOT(VG_(details).bug_reports_to,   NULL);

   if ( (VG_(defined_new_mem_stack_4)()  ||
         VG_(defined_new_mem_stack_8)()  ||
         VG_(defined_new_mem_stack_12)() ||
         VG_(defined_new_mem_stack_16)() ||
         VG_(defined_new_mem_stack_32)()) &&
       ! VG_(defined_new_mem_stack)()) 
   {
      VG_(printf)("\nTool error: one of the specialised `new_mem_stack_n'\n"
                  "events tracked, but not the generic `new_mem_stack' one.\n");
      VG_(skin_panic)("`new_mem_stack' should be defined\n");
   }

   if ( (VG_(defined_die_mem_stack_4)()  ||
         VG_(defined_die_mem_stack_8)()  ||
         VG_(defined_die_mem_stack_12)() ||
         VG_(defined_die_mem_stack_16)() ||
         VG_(defined_die_mem_stack_32)()) &&
       ! VG_(defined_die_mem_stack)()) 
   {
      VG_(printf)("\nTool error: one of the specialised `die_mem_stack_n'\n"
                  "events tracked, but not the generic `die_mem_stack' one.\n");
      VG_(skin_panic)("`die_mem_stack' should be defined\n");
   }

   if ( (VG_(defined_post_reg_write_syscall_return)()    ||
         VG_(defined_post_reg_write_deliver_signal)()    ||
         VG_(defined_post_reg_write_pthread_return)()    ||
         VG_(defined_post_reg_write_clientreq_return)()  ||
         VG_(defined_post_reg_write_clientcall_return)()) &&
       ! VG_(needs).shadow_regs) 
   {
      VG_(printf)("\nTool error: one of the `post_reg_write'\n"
                  "events tracked, but `shadow_regs' need not set.\n");
      VG_(skin_panic)("`shadow_regs' should be set\n");
   }

   if (VG_(needs).shadow_memory != (VG_(get_shadow_size)() != 0)) {
      if (VG_(get_shadow_size)() != 0)
	 VG_(printf)("\nTool error: tool allocated shadow memory, but apparently doesn't "
		     "need it.\n");
      else
	 VG_(printf)("\nTool error: tool didn't allocated shadow memory, but apparently "
		     "needs it.\n");
      VG_(skin_panic)("VG_(needs).shadow_memory need should be set to match SK_(shadow_ratio)\n");
   }

#undef CHECK_NOT
}

/*--------------------------------------------------------------------*/
/* Setting details */

/* Use macro because they're so repetitive */
#define DETAILS(type, detail)                       \
   extern void VG_(details_##detail)(type detail)   \
   {                                                \
      VG_(details).detail = detail;                 \
   }

DETAILS(Char*, name)
DETAILS(Char*, version)
DETAILS(Char*, description)
DETAILS(Char*, copyright_author)
DETAILS(Char*, bug_reports_to)
DETAILS(UInt,  avg_translation_sizeB)

/*--------------------------------------------------------------------*/
/* Setting needs */

/* Use macro because they're so repetitive */
#define NEEDS(need)  \
   extern void VG_(needs_##need)(void) \
   {                                   \
      VG_(needs).need = True;          \
   }

NEEDS(libc_freeres)
NEEDS(core_errors)
NEEDS(skin_errors)
NEEDS(basic_block_discards)
NEEDS(shadow_regs)
NEEDS(command_line_options)
NEEDS(client_requests)
NEEDS(extended_UCode)
NEEDS(syscall_wrapper)
NEEDS(sanity_checks)
NEEDS(data_syms)
NEEDS(shadow_memory)

/*--------------------------------------------------------------------*/
/*--- end                                               vg_needs.c ---*/
/*--------------------------------------------------------------------*/


