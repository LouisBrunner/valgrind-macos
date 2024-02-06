
/*--------------------------------------------------------------------*/
/*--- Stuff relating to tool data structures.                      ---*/
/*---                                                m_tooliface.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Nicholas Nethercote
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_tooliface.h"
#include "pub_core_transtab.h"     /* VG_(ok_to_discard_translations) */

// The core/tool dictionary of functions (initially zeroed, as we want it)
VgToolInterface VG_(tdict);

/*--------------------------------------------------------------------*/
/* Setting basic functions */

void VG_(basic_tool_funcs)(
   void(*post_clo_init)(void),
   IRSB*(*instrument)(VgCallbackClosure*, IRSB*, 
                      const VexGuestLayout*, const VexGuestExtents*,
                      const VexArchInfo*, IRType, IRType),
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

DETAILS(const HChar*, name)
DETAILS(const HChar*, version)
DETAILS(const HChar*, description)
DETAILS(const HChar*, copyright_author)
DETAILS(const HChar*, bug_reports_to)
DETAILS(UInt,  avg_translation_sizeB)


/*--------------------------------------------------------------------*/
/* Setting needs */

VgNeeds VG_(needs) = {
   .core_errors          = False,
   .tool_errors          = False,
   .libc_freeres         = False,
   .cxx_freeres          = False,
   .superblock_discards  = False,
   .command_line_options = False,
   .client_requests      = False,
   .syscall_wrapper      = False,
   .sanity_checks        = False,
   .print_stats          = False,
   .info_location        = False,
   .var_info	         = False,
   .malloc_replacement   = False,
   .xml_output           = False,
   .final_IR_tidy_pass   = False
};

/* static */
Bool VG_(finish_needs_init)(const HChar** failmsg)
{
   Bool any_new_mem_stack_N, any_new_mem_stack_N_w_ECU;
   Bool any_new_mem_stack_w_conflicting_otags;
   Bool any_die_mem_stack_N;

#define CHECK_NOT(var, value)                                  \
   if ((var)==(value)) {                                       \
      *failmsg = "Tool error: '" #var "' not initialised\n";   \
      return False;                                            \
   }
   
   /* Ones that must be set */
   CHECK_NOT(VG_(details).name,             NULL);
   /* Nb: .version can be NULL */
   CHECK_NOT(VG_(details).description,      NULL);
   CHECK_NOT(VG_(details).copyright_author, NULL);
   CHECK_NOT(VG_(details).bug_reports_to,   NULL);

   /* Check that new_mem_stack is defined if any new_mem_stack_N
      are. */
   any_new_mem_stack_N
      = VG_(tdict).track_new_mem_stack_4   ||
        VG_(tdict).track_new_mem_stack_8   ||
        VG_(tdict).track_new_mem_stack_12  ||
        VG_(tdict).track_new_mem_stack_16  ||
        VG_(tdict).track_new_mem_stack_32  ||
        VG_(tdict).track_new_mem_stack_112 ||
        VG_(tdict).track_new_mem_stack_128 ||
        VG_(tdict).track_new_mem_stack_144 ||
        VG_(tdict).track_new_mem_stack_160;

   if (any_new_mem_stack_N && ! VG_(tdict).track_new_mem_stack) {
      *failmsg = "Tool error: one of the specialised 'new_mem_stack_N'\n"
                 "   events tracked, but not the generic 'new_mem_stack' one.\n"
                 "   'new_mem_stack' should be defined\n";
      return False;
   }

   /* Check that new_mem_stack_w_ECU is defined if any
      new_mem_stack_N_w_ECU are. */
   any_new_mem_stack_N_w_ECU
      = VG_(tdict).track_new_mem_stack_4_w_ECU   ||
        VG_(tdict).track_new_mem_stack_8_w_ECU   ||
        VG_(tdict).track_new_mem_stack_12_w_ECU  ||
        VG_(tdict).track_new_mem_stack_16_w_ECU  ||
        VG_(tdict).track_new_mem_stack_32_w_ECU  ||
        VG_(tdict).track_new_mem_stack_112_w_ECU ||
        VG_(tdict).track_new_mem_stack_128_w_ECU ||
        VG_(tdict).track_new_mem_stack_144_w_ECU ||
        VG_(tdict).track_new_mem_stack_160_w_ECU;

   if (any_new_mem_stack_N_w_ECU && ! VG_(tdict).track_new_mem_stack_w_ECU) {
      *failmsg = "Tool error: one of the specialised 'new_mem_stack_N_w_ECU'\n"
                 "   events tracked, but not the generic 'new_mem_stack_w_ECU' one.\n"
                 "   'new_mem_stack_w_ECU' should be defined\n";
      return False;
   }

   /* Check that in no cases are both with- and without-otag versions of the
      same new_mem_stack_ function defined. */
   any_new_mem_stack_w_conflicting_otags
      = (VG_(tdict).track_new_mem_stack_4   && VG_(tdict).track_new_mem_stack_4_w_ECU)   ||
        (VG_(tdict).track_new_mem_stack_8   && VG_(tdict).track_new_mem_stack_8_w_ECU)   ||
        (VG_(tdict).track_new_mem_stack_12  && VG_(tdict).track_new_mem_stack_12_w_ECU)  ||
        (VG_(tdict).track_new_mem_stack_16  && VG_(tdict).track_new_mem_stack_16_w_ECU)  ||
        (VG_(tdict).track_new_mem_stack_32  && VG_(tdict).track_new_mem_stack_32_w_ECU)  ||
        (VG_(tdict).track_new_mem_stack_112 && VG_(tdict).track_new_mem_stack_112_w_ECU) ||
        (VG_(tdict).track_new_mem_stack_128 && VG_(tdict).track_new_mem_stack_128_w_ECU) ||
        (VG_(tdict).track_new_mem_stack_144 && VG_(tdict).track_new_mem_stack_144_w_ECU) ||
        (VG_(tdict).track_new_mem_stack_160 && VG_(tdict).track_new_mem_stack_160_w_ECU) ||
        (VG_(tdict).track_new_mem_stack     && VG_(tdict).track_new_mem_stack_w_ECU);

   if (any_new_mem_stack_w_conflicting_otags) {
      *failmsg = "Tool error: tool supplies both a 'new_mem_stack_N' and a\n"
                 "   'new_mem_stack_N_w_ECU' function for some N (or none),\n"
                 "   but you can only have one or the other (not both)\n";
      return False;
   }
   VG_(tdict).any_new_mem_stack
      = VG_(tdict).track_new_mem_stack || VG_(tdict).track_new_mem_stack_w_ECU
      || any_new_mem_stack_N || any_new_mem_stack_N_w_ECU;

   /* Check that die_mem_stack is defined if any die_mem_stack_N
      are. */
   any_die_mem_stack_N
      = VG_(tdict).track_die_mem_stack_4   ||
        VG_(tdict).track_die_mem_stack_8   ||
        VG_(tdict).track_die_mem_stack_12  ||
        VG_(tdict).track_die_mem_stack_16  ||
        VG_(tdict).track_die_mem_stack_32  ||
        VG_(tdict).track_die_mem_stack_112 ||
        VG_(tdict).track_die_mem_stack_128 ||
        VG_(tdict).track_die_mem_stack_144 ||
        VG_(tdict).track_die_mem_stack_160;

    if (any_die_mem_stack_N && ! VG_(tdict).track_die_mem_stack) {
      *failmsg = "Tool error: one of the specialised 'die_mem_stack_N'\n"
                 "   events tracked, but not the generic 'die_mem_stack' one.\n"
                 "   'die_mem_stack' should be defined\n";
      return False;
   }
   VG_(tdict).any_die_mem_stack
      = VG_(tdict).track_die_mem_stack || any_die_mem_stack_N;

   return True;

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
NEEDS(cxx_freeres)
NEEDS(core_errors)
NEEDS(var_info)

void VG_(needs_superblock_discards)(
   void (*discard)(Addr, VexGuestExtents)
)
{
   VG_(needs).superblock_discards = True;
   VG_(tdict).tool_discard_superblock_info = discard;
}

void VG_(needs_tool_errors)(
   Bool (*eq)         (VgRes, const Error*, const Error*),
   void (*before_pp)  (const Error*),
   void (*pp)         (const Error*),
   Bool show_TIDs,
   UInt (*update)     (const Error*),
   Bool (*recog)      (const HChar*, Supp*),
   Bool (*read_extra) (Int, HChar**, SizeT*, Int*, Supp*),
   Bool (*matches)    (const Error*, const Supp*),
   const HChar* (*name) (const Error*),
   SizeT (*get_xtra_si)(const Error*,/*OUT*/HChar*,Int),
   SizeT (*print_xtra_su)(const Supp*,/*OUT*/HChar*,Int),
   void (*update_xtra_su)(const Error*, const Supp*)
)
{
   VG_(needs).tool_errors = True;
   VG_(tdict).tool_eq_Error                     = eq;
   VG_(tdict).tool_before_pp_Error              = before_pp;
   VG_(tdict).tool_pp_Error                     = pp;
   VG_(tdict).tool_show_ThreadIDs_for_errors    = show_TIDs;
   VG_(tdict).tool_update_extra                 = update;
   VG_(tdict).tool_recognised_suppression       = recog;
   VG_(tdict).tool_read_extra_suppression_info  = read_extra;
   VG_(tdict).tool_error_matches_suppression    = matches;
   VG_(tdict).tool_get_error_name               = name;
   VG_(tdict).tool_get_extra_suppression_info   = get_xtra_si;
   VG_(tdict).tool_print_extra_suppression_use  = print_xtra_su;
   VG_(tdict).tool_update_extra_suppression_use = update_xtra_su;
}

void VG_(needs_command_line_options)(
   Bool (*process)(const HChar*),
   void (*usage)(void),
   void (*debug_usage)(void)
)
{
   VG_(needs).command_line_options = True;
   VG_(tdict).tool_process_cmd_line_option = process;
   VG_(tdict).tool_print_usage             = usage;
   VG_(tdict).tool_print_debug_usage       = debug_usage;
}

/* The tool's function for handling client requests. */
static Bool (*tool_handle_client_request_func)(ThreadId, UWord *, UWord *);

static Bool wrap_tool_handle_client_request(ThreadId tid, UWord *arg1,
                                            UWord *arg2)
{
   Bool ret;
   VG_(ok_to_discard_translations) = True;
   ret = tool_handle_client_request_func(tid, arg1, arg2);
   VG_(ok_to_discard_translations) = False;
   return ret;
}

void VG_(needs_client_requests)(
   Bool (*handle)(ThreadId, UWord*, UWord*)
)
{
   VG_(needs).client_requests = True;
   tool_handle_client_request_func = handle;   /* Stash away */
   /* Register the wrapper function */
   VG_(tdict).tool_handle_client_request = wrap_tool_handle_client_request;
}

void VG_(needs_syscall_wrapper)(
   void(*pre) (ThreadId, UInt, UWord*, UInt),
   void(*post)(ThreadId, UInt, UWord*, UInt, SysRes res)
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

void VG_(needs_print_stats) (
   void (*print_stats)(void)
)
{
   VG_(needs).print_stats = True;
   VG_(tdict).tool_print_stats = print_stats;
}

void VG_(needs_info_location) (
   void (*info_location)(DiEpoch, Addr)
)
{
   VG_(needs).info_location = True;
   VG_(tdict).tool_info_location = info_location;
}

void VG_(needs_malloc_replacement)(
   void* (*malloc)               ( ThreadId, SizeT ),
   void* (*__builtin_new)        ( ThreadId, SizeT ),
   void* (*__builtin_new_aligned)( ThreadId, SizeT, SizeT, SizeT ),
   void* (*__builtin_vec_new)    ( ThreadId, SizeT ),
   void* (*__builtin_vec_new_aligned)( ThreadId, SizeT, SizeT, SizeT ),
   void* (*memalign)             ( ThreadId, SizeT, SizeT, SizeT ),
   void* (*calloc)               ( ThreadId, SizeT, SizeT ),
   void  (*free)                 ( ThreadId, void* ),
   void  (*__builtin_delete)     ( ThreadId, void* ),
   void  (*__builtin_delete_aligned)     ( ThreadId, void*, SizeT ),
   void  (*__builtin_vec_delete) ( ThreadId, void* ),
   void  (*__builtin_vec_delete_aligned) ( ThreadId, void*, SizeT ),
   void* (*realloc)              ( ThreadId, void*, SizeT ),
   SizeT (*malloc_usable_size)   ( ThreadId, void* ), 
   SizeT client_malloc_redzone_szB
)
{
   VG_(needs).malloc_replacement        = True;
   VG_(tdict).tool_malloc               = malloc;
   VG_(tdict).tool___builtin_new        = __builtin_new;
   VG_(tdict).tool___builtin_new_aligned = __builtin_new_aligned;
   VG_(tdict).tool___builtin_vec_new    = __builtin_vec_new;
   VG_(tdict).tool___builtin_vec_new_aligned = __builtin_vec_new_aligned;
   VG_(tdict).tool_memalign             = memalign;
   VG_(tdict).tool_calloc               = calloc;
   VG_(tdict).tool_free                 = free;
   VG_(tdict).tool___builtin_delete     = __builtin_delete;
   VG_(tdict).tool___builtin_delete_aligned = __builtin_delete_aligned;
   VG_(tdict).tool___builtin_vec_delete = __builtin_vec_delete;
   VG_(tdict).tool___builtin_vec_delete_aligned = __builtin_vec_delete_aligned;
   VG_(tdict).tool_realloc              = realloc;
   VG_(tdict).tool_malloc_usable_size   = malloc_usable_size;
   VG_(tdict).tool_client_redzone_szB   = client_malloc_redzone_szB;
}

void VG_(needs_xml_output)( void )
{
   VG_(needs).xml_output = True;
}

void VG_(needs_final_IR_tidy_pass)( 
   IRSB*(*final_tidy)(IRSB*)
)
{
   VG_(needs).final_IR_tidy_pass = True;
   VG_(tdict).tool_final_IR_tidy_pass = final_tidy;
}

/*--------------------------------------------------------------------*/
/* Tracked events.  Digit 'n' on DEFn is the REGPARMness. */

#define DEF0(fn, args...) \
void VG_(fn)(void(*f)(args)) { \
   VG_(tdict).fn = f; \
}

#define DEF1(fn, args...) \
void VG_(fn)(VG_REGPARM(1) void(*f)(args)) { \
   VG_(tdict).fn = f; \
}

#define DEF2(fn, args...) \
void VG_(fn)(VG_REGPARM(2) void(*f)(args)) { \
   VG_(tdict).fn = f; \
}

DEF0(track_new_mem_startup,       Addr, SizeT, Bool, Bool, Bool, ULong)
DEF0(track_new_mem_stack_signal,  Addr, SizeT, UInt)
DEF0(track_new_mem_brk,           Addr, SizeT, UInt)
DEF0(track_new_mem_mmap,          Addr, SizeT, Bool, Bool, Bool, ULong)

DEF0(track_copy_mem_remap,        Addr, Addr, SizeT)
DEF0(track_change_mem_mprotect,   Addr, SizeT, Bool, Bool, Bool)
DEF0(track_die_mem_stack_signal,  Addr, SizeT)
DEF0(track_die_mem_brk,           Addr, SizeT)
DEF0(track_die_mem_munmap,        Addr, SizeT)

DEF2(track_new_mem_stack_4_w_ECU,    Addr, UInt)
DEF2(track_new_mem_stack_8_w_ECU,    Addr, UInt)
DEF2(track_new_mem_stack_12_w_ECU,   Addr, UInt)
DEF2(track_new_mem_stack_16_w_ECU,   Addr, UInt)
DEF2(track_new_mem_stack_32_w_ECU,   Addr, UInt)
DEF2(track_new_mem_stack_112_w_ECU,  Addr, UInt)
DEF2(track_new_mem_stack_128_w_ECU,  Addr, UInt)
DEF2(track_new_mem_stack_144_w_ECU,  Addr, UInt)
DEF2(track_new_mem_stack_160_w_ECU,  Addr, UInt)
DEF0(track_new_mem_stack_w_ECU,      Addr, SizeT, UInt)

DEF1(track_new_mem_stack_4,       Addr)
DEF1(track_new_mem_stack_8,       Addr)
DEF1(track_new_mem_stack_12,      Addr)
DEF1(track_new_mem_stack_16,      Addr)
DEF1(track_new_mem_stack_32,      Addr)
DEF1(track_new_mem_stack_112,     Addr)
DEF1(track_new_mem_stack_128,     Addr)
DEF1(track_new_mem_stack_144,     Addr)
DEF1(track_new_mem_stack_160,     Addr)
DEF0(track_new_mem_stack,         Addr, SizeT)

DEF1(track_die_mem_stack_4,       Addr)
DEF1(track_die_mem_stack_8,       Addr)
DEF1(track_die_mem_stack_12,      Addr)
DEF1(track_die_mem_stack_16,      Addr)
DEF1(track_die_mem_stack_32,      Addr)
DEF1(track_die_mem_stack_112,     Addr)
DEF1(track_die_mem_stack_128,     Addr)
DEF1(track_die_mem_stack_144,     Addr)
DEF1(track_die_mem_stack_160,     Addr)
DEF0(track_die_mem_stack,         Addr, SizeT)

DEF0(track_ban_mem_stack,         Addr, SizeT)

DEF0(track_register_stack,        Addr, Addr)

DEF0(track_pre_mem_read,          CorePart, ThreadId, const HChar*, Addr, SizeT)
DEF0(track_pre_mem_read_asciiz,   CorePart, ThreadId, const HChar*, Addr)
DEF0(track_pre_mem_write,         CorePart, ThreadId, const HChar*, Addr, SizeT)
DEF0(track_post_mem_write,        CorePart, ThreadId, Addr, SizeT)

DEF0(track_pre_reg_read,          CorePart, ThreadId, const HChar*, PtrdiffT, SizeT)
DEF0(track_post_reg_write,        CorePart, ThreadId,               PtrdiffT, SizeT)

DEF0(track_copy_mem_to_reg,       CorePart, ThreadId, Addr, PtrdiffT, SizeT)
DEF0(track_copy_reg_to_mem,       CorePart, ThreadId, PtrdiffT, Addr, SizeT)

DEF0(track_post_reg_write_clientcall_return, ThreadId, PtrdiffT, SizeT, Addr)

DEF0(track_start_client_code,     ThreadId, ULong)
DEF0(track_stop_client_code,      ThreadId, ULong)

DEF0(track_pre_thread_ll_create,  ThreadId, ThreadId)
DEF0(track_pre_thread_first_insn, ThreadId)
DEF0(track_pre_thread_ll_exit,    ThreadId)

DEF0(track_pre_deliver_signal,    ThreadId, Int sigNo, Bool)
DEF0(track_post_deliver_signal,   ThreadId, Int sigNo)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
