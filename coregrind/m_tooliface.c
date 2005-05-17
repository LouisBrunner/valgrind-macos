
/*--------------------------------------------------------------------*/
/*--- Stuff relating to tool data structures.                      ---*/
/*---                                                m_tooliface.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn@valgrind.org

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

#include "core.h"
#include "pub_core_tooliface.h"

// The core/tool dictionary of functions (initially zeroed, as we want it)
VgToolInterface VG_(tdict);

/*--------------------------------------------------------------------*/
/* Setting basic functions */

void VG_(basic_tool_funcs)(
   void(*post_clo_init)(void),
   IRBB*(*instrument)(IRBB*, VexGuestLayout*, IRType, IRType ),
   void(*fini)(Int)
)
{
   VG_(tdict).tool_post_clo_init = post_clo_init;
   VG_(tdict).tool_instrument    = instrument;
   VG_(tdict).tool_fini          = fini;
}


/*--------------------------------------------------------------------*/
/* Setting details */

/* Init with default values. */
VgDetails VG_(details) = {
   .name                  = NULL,
   .version               = NULL,
   .description           = NULL,
   .copyright_author      = NULL,
   .bug_reports_to        = NULL,
   .avg_translation_sizeB = VG_DEFAULT_TRANS_SIZEB,
};

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

VgNeeds VG_(needs) = {
   .core_errors          = False,
   .tool_errors          = False,
   .libc_freeres         = False,
   .basic_block_discards = False,
   .no_longer_used_1     = False,
   .command_line_options = False,
   .client_requests      = False,
   .no_longer_used_0     = False,
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
      VG_(printf)("\nTool error: '%s' not initialised\n",   \
                  VG_STRINGIFY(var));                       \
      VG_(tool_panic)("Uninitialised details field\n");     \
   }
   
   /* Ones that must be set */
   CHECK_NOT(VG_(details).name,             NULL);
   /* Nb: .version can be NULL */
   CHECK_NOT(VG_(details).description,      NULL);
   CHECK_NOT(VG_(details).copyright_author, NULL);
   CHECK_NOT(VG_(details).bug_reports_to,   NULL);

   if ( (VG_(tdict).track_new_mem_stack_4  ||
         VG_(tdict).track_new_mem_stack_8  ||
         VG_(tdict).track_new_mem_stack_12 ||
         VG_(tdict).track_new_mem_stack_16 ||
         VG_(tdict).track_new_mem_stack_32 ) &&
       ! VG_(tdict).track_new_mem_stack) 
   {
      VG_(printf)("\nTool error: one of the specialised 'new_mem_stack_n'\n"
                  "events tracked, but not the generic 'new_mem_stack' one.\n");
      VG_(tool_panic)("'new_mem_stack' should be defined\n");
   }

   if ( (VG_(tdict).track_die_mem_stack_4  ||
         VG_(tdict).track_die_mem_stack_8  ||
         VG_(tdict).track_die_mem_stack_12 ||
         VG_(tdict).track_die_mem_stack_16 ||
         VG_(tdict).track_die_mem_stack_32 ) &&
       ! VG_(tdict).track_die_mem_stack) 
   {
      VG_(printf)("\nTool error: one of the specialised 'die_mem_stack_n'\n"
                  "events tracked, but not the generic 'die_mem_stack' one.\n");
      VG_(tool_panic)("'die_mem_stack' should be defined\n");
   }

   if (VG_(needs).shadow_memory != (VG_(get_shadow_size)() != 0)) {
      if (VG_(get_shadow_size)() != 0)
	 VG_(printf)("\nTool error: tool allocated shadow memory, but apparently doesn't "
		     "need it.\n");
      else
	 VG_(printf)("\nTool error: tool didn't allocate shadow memory, but apparently "
		     "needs it.\n");
      VG_(tool_panic)("VG_(needs).shadow_memory need should be set to match 'shadow_ratio'\n");
   }

#undef CHECK_NOT
}

/* Use macro because they're so repetitive */
#define NEEDS(need)  \
   extern void VG_(needs_##need)(void) \
   {                                   \
      VG_(needs).need = True;          \
   }

// These ones don't require any tool-supplied functions
NEEDS(libc_freeres)
NEEDS(core_errors)
NEEDS(data_syms)
NEEDS(shadow_memory)

void VG_(needs_basic_block_discards)(
   void (*discard)(Addr, SizeT)
)
{
   VG_(needs).basic_block_discards = True;
   VG_(tdict).tool_discard_basic_block_info = discard;
}

void VG_(needs_tool_errors)(
   Bool (*eq)         (VgRes, Error*, Error*),
   void (*pp)         (Error*),
   UInt (*update)     (Error*),
   Bool (*recog)      (Char*, Supp*),
   Bool (*read_extra) (Int, Char*, Int, Supp*),
   Bool (*matches)    (Error*, Supp*),
   Char* (*name)      (Error*),
   void (*print_extra)(Error*)
)
{
   VG_(needs).tool_errors = True;
   VG_(tdict).tool_eq_Error                     = eq;
   VG_(tdict).tool_pp_Error                     = pp;
   VG_(tdict).tool_update_extra                 = update;
   VG_(tdict).tool_recognised_suppression       = recog;
   VG_(tdict).tool_read_extra_suppression_info  = read_extra;
   VG_(tdict).tool_error_matches_suppression    = matches;
   VG_(tdict).tool_get_error_name               = name;
   VG_(tdict).tool_print_extra_suppression_info = print_extra;
}

void VG_(needs_command_line_options)(
   Bool (*process)(Char*),
   void (*usage)(void),
   void (*debug_usage)(void)
)
{
   VG_(needs).command_line_options = True;
   VG_(tdict).tool_process_cmd_line_option = process;
   VG_(tdict).tool_print_usage             = usage;
   VG_(tdict).tool_print_debug_usage       = debug_usage;
}

void VG_(needs_client_requests)(
   Bool (*handle)(ThreadId, UWord*, UWord*)
)
{
   VG_(needs).client_requests = True;
   VG_(tdict).tool_handle_client_request = handle;
}

void VG_(needs_syscall_wrapper)(
   void(*pre) (ThreadId, UInt),
   void(*post)(ThreadId, UInt, Int res)
)
{
   VG_(needs).syscall_wrapper = True;
   VG_(tdict).tool_pre_syscall  = pre;
   VG_(tdict).tool_post_syscall = post;
}

void VG_(needs_sanity_checks)(
   Bool(*cheap)(void),
   Bool(*expen)(void)
)
{
   VG_(needs).sanity_checks = True;
   VG_(tdict).tool_cheap_sanity_check     = cheap;
   VG_(tdict).tool_expensive_sanity_check = expen;
}


/*--------------------------------------------------------------------*/
/* Replacing malloc() */

extern void VG_(malloc_funcs)(
   void* (*malloc)               ( ThreadId, SizeT ),
   void* (*__builtin_new)        ( ThreadId, SizeT ),
   void* (*__builtin_vec_new)    ( ThreadId, SizeT ),
   void* (*memalign)             ( ThreadId, SizeT, SizeT ),
   void* (*calloc)               ( ThreadId, SizeT, SizeT ),
   void  (*free)                 ( ThreadId, void* ),
   void  (*__builtin_delete)     ( ThreadId, void* ),
   void  (*__builtin_vec_delete) ( ThreadId, void* ),
   void* (*realloc)              ( ThreadId, void*, SizeT ),
   SizeT client_malloc_redzone_szB
)
{
   VG_(tdict).malloc_malloc               = malloc;
   VG_(tdict).malloc___builtin_new        = __builtin_new;
   VG_(tdict).malloc___builtin_vec_new    = __builtin_vec_new;
   VG_(tdict).malloc_memalign             = memalign;
   VG_(tdict).malloc_calloc               = calloc;
   VG_(tdict).malloc_free                 = free;
   VG_(tdict).malloc___builtin_delete     = __builtin_delete;
   VG_(tdict).malloc___builtin_vec_delete = __builtin_vec_delete;
   VG_(tdict).malloc_realloc              = realloc;

   VG_(set_client_malloc_redzone_szB)( client_malloc_redzone_szB );
}


/*--------------------------------------------------------------------*/
/* Tracked events */

#define DEF(fn, args...) \
void VG_(fn)(void(*f)(args)) \
{ \
   VG_(tdict).fn = f; \
}

#define DEF2(fn, args...) \
void VG_(fn)(VGA_REGPARM(1) void(*f)(args)) \
{ \
   VG_(tdict).fn = f; \
}

DEF(track_new_mem_startup,       Addr, SizeT, Bool, Bool, Bool)
DEF(track_new_mem_stack_signal,  Addr, SizeT)
DEF(track_new_mem_brk,           Addr, SizeT)
DEF(track_new_mem_mmap,          Addr, SizeT, Bool, Bool, Bool)

DEF(track_copy_mem_remap,        Addr, Addr, SizeT)
DEF(track_change_mem_mprotect,   Addr, SizeT, Bool, Bool, Bool)
DEF(track_die_mem_stack_signal,  Addr, SizeT)
DEF(track_die_mem_brk,           Addr, SizeT)
DEF(track_die_mem_munmap,        Addr, SizeT)

DEF2(track_new_mem_stack_4,      Addr)
DEF2(track_new_mem_stack_8,      Addr)
DEF2(track_new_mem_stack_12,     Addr)
DEF2(track_new_mem_stack_16,     Addr)
DEF2(track_new_mem_stack_32,     Addr)
DEF (track_new_mem_stack,        Addr, SizeT)

DEF2(track_die_mem_stack_4,      Addr)
DEF2(track_die_mem_stack_8,      Addr)
DEF2(track_die_mem_stack_12,     Addr)
DEF2(track_die_mem_stack_16,     Addr)
DEF2(track_die_mem_stack_32,     Addr)
DEF (track_die_mem_stack,        Addr, SizeT)

DEF(track_ban_mem_stack,         Addr, SizeT)

DEF(track_pre_mem_read,          CorePart, ThreadId, Char*, Addr, SizeT)
DEF(track_pre_mem_read_asciiz,   CorePart, ThreadId, Char*, Addr)
DEF(track_pre_mem_write,         CorePart, ThreadId, Char*, Addr, SizeT)
DEF(track_post_mem_write,        CorePart, ThreadId, Addr, SizeT)

DEF(track_pre_reg_read,          CorePart, ThreadId, Char*, OffT, SizeT)
DEF(track_post_reg_write,        CorePart, ThreadId,        OffT, SizeT)

DEF(track_post_reg_write_clientcall_return, ThreadId, OffT, SizeT, Addr)

DEF(track_thread_run,            ThreadId)

DEF(track_post_thread_create,    ThreadId, ThreadId)
DEF(track_post_thread_join,      ThreadId, ThreadId)

DEF(track_pre_mutex_lock,        ThreadId, void*)
DEF(track_post_mutex_lock,       ThreadId, void*)
DEF(track_post_mutex_unlock,     ThreadId, void*)

DEF(track_pre_deliver_signal,    ThreadId, Int sigNo, Bool)
DEF(track_post_deliver_signal,   ThreadId, Int sigNo)

DEF(track_init_shadow_page,      Addr)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/


