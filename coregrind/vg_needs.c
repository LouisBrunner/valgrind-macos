
/*--------------------------------------------------------------------*/
/*--- Stuff relating to skin data structures.                      ---*/
/*---                                                   vg_needs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Nicholas Nethercote
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
   Skin data structure initialisation
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
};

VgTrackEvents VG_(track_events) = {
   /* Memory events */
   .new_mem_startup              = NULL,
   .new_mem_stack_signal         = NULL,
   .new_mem_brk                  = NULL,
   .new_mem_mmap                 = NULL,

   .copy_mem_remap               = NULL,
   .change_mem_mprotect          = NULL,

   .die_mem_stack_signal         = NULL,
   .die_mem_brk                  = NULL,
   .die_mem_munmap               = NULL,

   .new_mem_stack_4              = NULL,
   .new_mem_stack_8              = NULL,
   .new_mem_stack_12             = NULL,
   .new_mem_stack_16             = NULL,
   .new_mem_stack_32             = NULL,
   .new_mem_stack                = NULL,

   .die_mem_stack_4              = NULL,
   .die_mem_stack_8              = NULL,
   .die_mem_stack_12             = NULL,
   .die_mem_stack_16             = NULL,
   .die_mem_stack_32             = NULL,
   .die_mem_stack                = NULL,

   .ban_mem_stack                = NULL,

   .pre_mem_read                 = NULL,
   .pre_mem_read_asciiz          = NULL,
   .pre_mem_write                = NULL,
   .post_mem_write               = NULL,

   /* Register events */
   .post_regs_write_init             = NULL,
   .post_reg_write_syscall_return    = NULL,
   .post_reg_write_deliver_signal    = NULL,
   .post_reg_write_pthread_return    = NULL,
   .post_reg_write_clientreq_return  = NULL,
   .post_reg_write_clientcall_return = NULL,

   /* Scheduler events */
   .thread_run                   = NULL,

   /* Mutex events */
   .post_mutex_lock              = NULL,
   .post_mutex_unlock            = NULL,

   /* Signal events */
   .pre_deliver_signal           = NULL,
   .post_deliver_signal          = NULL,
};

/* static */
void VG_(sanity_check_needs) ( void)
{
#define CHECK_NOT(var, value)                               \
   if ((var)==(value)) {                                    \
      VG_(printf)("\nSkin error: `%s' not initialised\n",   \
                  VG__STRING(var));                         \
      VG_(skin_panic)("Uninitialised details field\n");     \
   }
   
   /* Ones that must be set */
   CHECK_NOT(VG_(details).name,             NULL);
   /* Nb: .version can be NULL */
   CHECK_NOT(VG_(details).description,      NULL);
   CHECK_NOT(VG_(details).copyright_author, NULL);
   CHECK_NOT(VG_(details).bug_reports_to,   NULL);

   if ( (VG_(track_events).new_mem_stack_4  ||
         VG_(track_events).new_mem_stack_8  ||
         VG_(track_events).new_mem_stack_12 ||
         VG_(track_events).new_mem_stack_16 ||
         VG_(track_events).new_mem_stack_32) &&
       ! VG_(track_events).new_mem_stack) 
   {
      VG_(printf)("\nSkin error: one of the specialised `new_mem_stack_n'\n"
                  "events tracked, but not the generic `new_mem_stack' one.\n");
      VG_(skin_panic)("`new_mem_stack' should be defined\n");
   }

   if ( (VG_(track_events).die_mem_stack_4  ||
         VG_(track_events).die_mem_stack_8  ||
         VG_(track_events).die_mem_stack_12 ||
         VG_(track_events).die_mem_stack_16 ||
         VG_(track_events).die_mem_stack_32) &&
       ! VG_(track_events).die_mem_stack) 
   {
      VG_(printf)("\nSkin error: one of the specialised `die_mem_stack_n'\n"
                  "events tracked, but not the generic `die_mem_stack' one.\n");
      VG_(skin_panic)("`die_mem_stack' should be defined\n");
   }

   if ( (VG_(track_events).post_reg_write_syscall_return    ||
         VG_(track_events).post_reg_write_deliver_signal    ||
         VG_(track_events).post_reg_write_pthread_return    ||
         VG_(track_events).post_reg_write_clientreq_return  ||
         VG_(track_events).post_reg_write_clientcall_return) &&
       ! VG_(needs).shadow_regs) 
   {
      VG_(printf)("\nSkin error: one of the `post_reg_write'\n"
                  "events tracked, but `shadow_regs' need not set.\n");
      VG_(skin_panic)("`shadow_regs' should be set\n");
   }

#undef CHECK_NOT
#undef INVALID_Bool
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

/*--------------------------------------------------------------------*/
#define TRACK(event, args...)  \
   void VG_(track_##event)(void (*f)(args)) \
   {                                      \
      VG_(track_events).event = f;        \
   }

/* Memory events */
TRACK(new_mem_startup,       Addr a, UInt len, Bool rr, Bool ww, Bool xx)
TRACK(new_mem_stack_signal,  Addr a, UInt len)
TRACK(new_mem_brk,           Addr a, UInt len)
TRACK(new_mem_mmap,          Addr a, UInt len, Bool rr, Bool ww, Bool xx)

TRACK(copy_mem_remap,      Addr from, Addr to, UInt len)
TRACK(change_mem_mprotect, Addr a, UInt len, Bool rr, Bool ww, Bool xx)

TRACK(die_mem_stack_signal,  Addr a, UInt len)
TRACK(die_mem_brk,           Addr a, UInt len)
TRACK(die_mem_munmap,        Addr a, UInt len)

TRACK(new_mem_stack_4,       Addr new_ESP)
TRACK(new_mem_stack_8,       Addr new_ESP)
TRACK(new_mem_stack_12,      Addr new_ESP)
TRACK(new_mem_stack_16,      Addr new_ESP)
TRACK(new_mem_stack_32,      Addr new_ESP)
TRACK(new_mem_stack,         Addr a, UInt len)

TRACK(die_mem_stack_4,       Addr new_ESP)
TRACK(die_mem_stack_8,       Addr new_ESP)
TRACK(die_mem_stack_12,      Addr new_ESP)
TRACK(die_mem_stack_16,      Addr new_ESP)
TRACK(die_mem_stack_32,      Addr new_ESP)
TRACK(die_mem_stack,         Addr a, UInt len)

TRACK(ban_mem_stack, Addr a, UInt len)

TRACK(pre_mem_read,        CorePart part, ThreadState* tst, Char* s, Addr a,
                           UInt size)
TRACK(pre_mem_read_asciiz, CorePart part, ThreadState* tst, Char* s, Addr a)
TRACK(pre_mem_write,       CorePart part, ThreadState* tst, Char* s, Addr a,
                           UInt size)
TRACK(post_mem_write,      Addr a, UInt size)

TRACK(post_regs_write_init,             void );
TRACK(post_reg_write_syscall_return,    ThreadId tid, UInt reg );
TRACK(post_reg_write_deliver_signal,    ThreadId tid, UInt reg );
TRACK(post_reg_write_pthread_return,    ThreadId tid, UInt reg );
TRACK(post_reg_write_clientreq_return,  ThreadId tid, UInt reg );
TRACK(post_reg_write_clientcall_return, ThreadId tid, UInt reg, Addr f );

TRACK(thread_run, ThreadId tid)

TRACK(post_thread_create, ThreadId tid, ThreadId child)
TRACK(post_thread_join,   ThreadId joiner, ThreadId joinee)

TRACK( pre_mutex_lock,   ThreadId tid, void* /*pthread_mutex_t* */ mutex)
TRACK(post_mutex_lock,   ThreadId tid, void* /*pthread_mutex_t* */ mutex)
TRACK(post_mutex_unlock, ThreadId tid, void* /*pthread_mutex_t* */ mutex)

TRACK( pre_deliver_signal, ThreadId tid, Int sigNum, Bool alt_stack)
TRACK(post_deliver_signal, ThreadId tid, Int sigNum)

/*--------------------------------------------------------------------*/
/* UCodeBlocks */

Int VG_(get_num_instrs) ( UCodeBlock* cb )
{
   return cb->used;
}

Int VG_(get_num_temps) ( UCodeBlock* cb )
{
   return cb->nextTemp;
}

UInstr* VG_(get_instr) ( UCodeBlock* cb, Int i )
{
   return & cb->instrs[i];
}

UInstr* VG_(get_last_instr) ( UCodeBlock* cb )
{
   return & cb->instrs[cb->used-1];
}
   
/*--------------------------------------------------------------------*/
/* Suppressions */

SuppKind VG_(get_supp_kind) ( Supp* su )
{
   return su->skind;
}

Char* VG_(get_supp_string) ( Supp* su )
{
   return su->string;
}

void* VG_(get_supp_extra)  ( Supp* su )
{
   return su->extra;
}


void VG_(set_supp_kind)   ( Supp* su, SuppKind skind )
{
   su->skind = skind;
}

void VG_(set_supp_string) ( Supp* su, Char* string )
{
   su->string = string;
}

void VG_(set_supp_extra)  ( Supp* su, void* extra )
{
   su->extra = extra;
}

/*--------------------------------------------------------------------*/
/* Errors */

ExeContext* VG_(get_error_where) ( Error* err )
{
   return err->where;
}

ErrorKind VG_(get_error_kind) ( Error* err )
{
   return err->ekind;
}

Addr VG_(get_error_address) ( Error* err )
{
   return err->addr;
}

Char* VG_(get_error_string) ( Error* err )
{
   return err->string;
}

void* VG_(get_error_extra)  ( Error* err )
{
   return err->extra;
}

/*--------------------------------------------------------------------*/
/*--- end                                               vg_needs.c ---*/
/*--------------------------------------------------------------------*/


