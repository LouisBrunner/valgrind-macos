
/*--------------------------------------------------------------------*/
/*--- Stuff relating to skin data structures.                      ---*/
/*---                                                   vg_needs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2002 Nicholas Nethercote
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
   .sizeof_shadow_block  = 0,
   .basic_block_discards = False,
   .shadow_regs          = False,
   .command_line_options = False,
   .client_requests      = False,
   .extended_UCode       = False,
   .syscall_wrapper      = False,
   .alternative_free     = False,
   .sanity_checks        = False,
   .data_syms	         = False,
};

VgTrackEvents VG_(track_events) = {
   /* Memory events */
   .new_mem_startup              = NULL,
   .new_mem_heap                 = NULL,
   .new_mem_stack                = NULL,
   .new_mem_stack_aligned        = NULL,
   .new_mem_stack_signal         = NULL,
   .new_mem_brk                  = NULL,
   .new_mem_mmap                 = NULL,

   .copy_mem_heap                = NULL,
   .copy_mem_remap               = NULL,
   .change_mem_mprotect          = NULL,

   .ban_mem_heap                 = NULL,
   .ban_mem_stack                = NULL,

   .die_mem_heap                 = NULL,
   .die_mem_stack                = NULL,
   .die_mem_stack_aligned        = NULL,
   .die_mem_stack_signal         = NULL,
   .die_mem_brk                  = NULL,
   .die_mem_munmap               = NULL,

   .bad_free                     = NULL,
   .mismatched_free              = NULL,

   .pre_mem_read                 = NULL,
   .pre_mem_read_asciiz          = NULL,
   .pre_mem_write                = NULL,
   .post_mem_write               = NULL,

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

extern void VG_(needs_sizeof_shadow_block)(Int size)
{
   VG_(needs).sizeof_shadow_block = size;
}

NEEDS(alternative_free)
NEEDS(sanity_checks)
NEEDS(data_syms)

/*--------------------------------------------------------------------*/
#define TRACK(event, args...)  \
   void VG_(track_##event)(void (*f)(args)) \
   {                                      \
      VG_(track_events).event = f;        \
   }

TRACK(new_mem_startup,       Addr a, UInt len, Bool rr, Bool ww, Bool xx)
TRACK(new_mem_heap,          Addr a, UInt len, Bool is_inited)
TRACK(new_mem_stack,         Addr a, UInt len)
TRACK(new_mem_stack_aligned, Addr a, UInt len)
TRACK(new_mem_stack_signal,  Addr a, UInt len)
TRACK(new_mem_brk,           Addr a, UInt len)
TRACK(new_mem_mmap,          Addr a, UInt len, Bool rr, Bool ww, Bool xx)

TRACK(copy_mem_heap,       Addr from, Addr to, UInt len)
TRACK(copy_mem_remap,      Addr from, Addr to, UInt len)
TRACK(change_mem_mprotect, Addr a, UInt len, Bool rr, Bool ww, Bool xx)

TRACK(ban_mem_heap,  Addr a, UInt len)
TRACK(ban_mem_stack, Addr a, UInt len)

TRACK(die_mem_heap,          Addr a, UInt len)
TRACK(die_mem_stack,         Addr a, UInt len)
TRACK(die_mem_stack_aligned, Addr a, UInt len)
TRACK(die_mem_stack_signal,  Addr a, UInt len)
TRACK(die_mem_brk,           Addr a, UInt len)
TRACK(die_mem_munmap,        Addr a, UInt len)

TRACK(bad_free,        ThreadState* tst, Addr a)
TRACK(mismatched_free, ThreadState* tst, Addr a)

TRACK(pre_mem_read,        CorePart part, ThreadState* tst, Char* s, Addr a,
                           UInt size)
TRACK(pre_mem_read_asciiz, CorePart part, ThreadState* tst, Char* s, Addr a)
TRACK(pre_mem_write,       CorePart part, ThreadState* tst, Char* s, Addr a,
                           UInt size)
TRACK(post_mem_write,      Addr a, UInt size)

TRACK(thread_run, ThreadId tid)

TRACK(post_thread_create, ThreadId tid, ThreadId child)
TRACK(post_thread_join,   ThreadId joiner, ThreadId joinee)

TRACK( pre_mutex_lock,   ThreadId tid, void* /*pthread_mutex_t* */ mutex)
TRACK(post_mutex_lock,   ThreadId tid, void* /*pthread_mutex_t* */ mutex)
TRACK(post_mutex_unlock, ThreadId tid, void* /*pthread_mutex_t* */ mutex)

TRACK(pre_deliver_signal,  ThreadId tid, Int sigNum, Bool alt_stack)
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
/* ShadowChunks */

UInt VG_(get_sc_size)  ( ShadowChunk* sc )
{
   return sc->size;
}

Addr VG_(get_sc_data)  ( ShadowChunk* sc )
{
   return sc->data;
}

UInt VG_(get_sc_extra) ( ShadowChunk* sc, UInt i )
{
   vg_assert(i < VG_(needs).sizeof_shadow_block);
   return sc->extra[i];
}

ShadowChunk* VG_(get_sc_next)  ( ShadowChunk* sc )
{
   return sc->next;
}

void VG_(set_sc_extra) ( ShadowChunk* sc, UInt i, UInt word )
{
   vg_assert(i < VG_(needs).sizeof_shadow_block);
   sc->extra[i] = word;
}

void VG_(set_sc_next)  ( ShadowChunk* sc, ShadowChunk* next )
{
   sc->next = next;
}


/*--------------------------------------------------------------------*/
/*--- end                                               vg_needs.c ---*/
/*--------------------------------------------------------------------*/


