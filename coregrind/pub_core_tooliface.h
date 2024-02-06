
/*--------------------------------------------------------------------*/
/*--- The core/tool interface.                pub_core_tooliface.h ---*/
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

#ifndef __PUB_CORE_TOOLIFACE_H
#define __PUB_CORE_TOOLIFACE_H

#include "pub_tool_tooliface.h"

//--------------------------------------------------------------------
// PURPOSE: This module encapsulates the key parts of the core/tool
// interface: 'details', 'needs' and 'trackable events'.
//--------------------------------------------------------------------

// Note the use of C's comma operator here -- it means that we execute both
// statements, and the rvalue of the whole thing is the rvalue of the last
// statement.  This lets us say "x = VG_TDICT_CALL(...)" in the required
// places, while still checking the assertion.
#define VG_TDICT_CALL(fn, args...) \
   ( vg_assert2(VG_(tdict).fn, \
                "you forgot to set VgToolInterface function '" #fn "'"), \
     VG_(tdict).fn(args) )

#define VG_TRACK(fn, args...) 			\
   do {						\
      if (VG_(tdict).track_##fn)		\
	 VG_(tdict).track_##fn(args);           \
   } while(0)

/* These structs are not exposed to tools to mitigate possibility of
   binary-incompatibilities when the core/tool interface changes.  Instead,
   set functions are provided (see include/pub_tool_tooliface.h). */

/* ---------------------------------------------------------------------
   'Details'
   ------------------------------------------------------------------ */

typedef
   struct {
      const HChar* name;
      const HChar* version;
      const HChar* description;
      const HChar* copyright_author;
      const HChar* bug_reports_to;
      UInt  avg_translation_sizeB;
   }
   VgDetails;

extern VgDetails VG_(details);

/* ---------------------------------------------------------------------
   'Needs'
   ------------------------------------------------------------------ */

typedef
   struct {
      Bool libc_freeres;
      Bool cxx_freeres;
      Bool core_errors;
      Bool tool_errors;
      Bool superblock_discards;
      Bool command_line_options;
      Bool client_requests;
      Bool syscall_wrapper;
      Bool sanity_checks;
      Bool print_stats;
      Bool info_location;
      Bool var_info;
      Bool malloc_replacement;
      Bool xml_output;
      Bool final_IR_tidy_pass;
   } 
   VgNeeds;

extern VgNeeds VG_(needs);

/* ---------------------------------------------------------------------
   The dictionary of callable tool functions
   ------------------------------------------------------------------ */

typedef struct {
   // -- 'Needs'-related functions ----------------------------------
   // Basic functions
   void  (*tool_pre_clo_init) (void);
   void  (*tool_post_clo_init)(void);
   IRSB* (*tool_instrument)   (VgCallbackClosure*,
                               IRSB*, 
                               const VexGuestLayout*, const VexGuestExtents*, 
                               const VexArchInfo*, IRType, IRType);
   void  (*tool_fini)         (Int);

   // VG_(needs).core_errors
   // (none)
   
   // VG_(needs).tool_errors
   Bool  (*tool_eq_Error)                  (VgRes, const Error*, const Error*);
   void  (*tool_before_pp_Error)           (const Error*);
   void  (*tool_pp_Error)                  (const Error*);
   Bool  tool_show_ThreadIDs_for_errors;
   UInt  (*tool_update_extra)                (const Error*);
   Bool  (*tool_recognised_suppression)      (const HChar*, Supp*);
   Bool  (*tool_read_extra_suppression_info) (Int, HChar**, SizeT*, Int*,
                                              Supp*);
   Bool  (*tool_error_matches_suppression)   (const Error*, const Supp*);
   const HChar* (*tool_get_error_name)       (const Error*);
   SizeT (*tool_get_extra_suppression_info)  (const Error*,/*OUT*/HChar*,Int);
   SizeT (*tool_print_extra_suppression_use) (const Supp*,/*OUT*/HChar*,Int);
   void  (*tool_update_extra_suppression_use) (const Error*, const Supp*);

   // VG_(needs).superblock_discards
   void (*tool_discard_superblock_info)(Addr, VexGuestExtents);

   // VG_(needs).command_line_options
   Bool (*tool_process_cmd_line_option)(const HChar*);
   void (*tool_print_usage)            (void);
   void (*tool_print_debug_usage)      (void);

   // VG_(needs).client_requests
   Bool (*tool_handle_client_request)(ThreadId, UWord*, UWord*);

   // VG_(needs).syscall_wrapper
   void (*tool_pre_syscall) (ThreadId, UInt, UWord*, UInt);
   void (*tool_post_syscall)(ThreadId, UInt, UWord*, UInt, SysRes);

   // VG_(needs).sanity_checks
   Bool (*tool_cheap_sanity_check)(void);
   Bool (*tool_expensive_sanity_check)(void);

   // VG_(needs).print_stats
   void (*tool_print_stats)(void);

   // VG_(needs).info_location
   void (*tool_info_location)(DiEpoch ep, Addr a);

   // VG_(needs).malloc_replacement
   void* (*tool_malloc)              (ThreadId, SizeT);
   void* (*tool___builtin_new)       (ThreadId, SizeT);
   void* (*tool___builtin_new_aligned)       (ThreadId, SizeT, SizeT, SizeT);
   void* (*tool___builtin_vec_new)   (ThreadId, SizeT);
   void* (*tool___builtin_vec_new_aligned)   (ThreadId, SizeT, SizeT, SizeT);
   void* (*tool_memalign)            (ThreadId, SizeT, SizeT, SizeT);
   void* (*tool_calloc)              (ThreadId, SizeT, SizeT);
   void  (*tool_free)                (ThreadId, void*);
   void  (*tool___builtin_delete)    (ThreadId, void*);
   void  (*tool___builtin_delete_aligned)    (ThreadId, void*, SizeT);
   void  (*tool___builtin_vec_delete)(ThreadId, void*);
   void  (*tool___builtin_vec_delete_aligned)(ThreadId, void*, SizeT);
   void* (*tool_realloc)             (ThreadId, void*, SizeT);
   SizeT (*tool_malloc_usable_size)  (ThreadId, void*);
   SizeT tool_client_redzone_szB;

   // VG_(needs).final_IR_tidy_pass
   IRSB* (*tool_final_IR_tidy_pass)  (IRSB*);

   // VG_(needs).xml_output
   // (none)

   // -- Event tracking functions ------------------------------------
   void (*track_new_mem_startup)     (Addr, SizeT, Bool, Bool, Bool, ULong);
   void (*track_new_mem_stack_signal)(Addr, SizeT, ThreadId);
   void (*track_new_mem_brk)         (Addr, SizeT, ThreadId);
   void (*track_new_mem_mmap)        (Addr, SizeT, Bool, Bool, Bool, ULong);

   void (*track_copy_mem_remap)      (Addr src, Addr dst, SizeT);
   void (*track_change_mem_mprotect) (Addr, SizeT, Bool, Bool, Bool);
   void (*track_die_mem_stack_signal)(Addr, SizeT);
   void (*track_die_mem_brk)         (Addr, SizeT);
   void (*track_die_mem_munmap)      (Addr, SizeT);

   void VG_REGPARM(2) (*track_new_mem_stack_4_w_ECU)  (Addr,UInt);
   void VG_REGPARM(2) (*track_new_mem_stack_8_w_ECU)  (Addr,UInt);
   void VG_REGPARM(2) (*track_new_mem_stack_12_w_ECU) (Addr,UInt);
   void VG_REGPARM(2) (*track_new_mem_stack_16_w_ECU) (Addr,UInt);
   void VG_REGPARM(2) (*track_new_mem_stack_32_w_ECU) (Addr,UInt);
   void VG_REGPARM(2) (*track_new_mem_stack_112_w_ECU)(Addr,UInt);
   void VG_REGPARM(2) (*track_new_mem_stack_128_w_ECU)(Addr,UInt);
   void VG_REGPARM(2) (*track_new_mem_stack_144_w_ECU)(Addr,UInt);
   void VG_REGPARM(2) (*track_new_mem_stack_160_w_ECU)(Addr,UInt);
   void (*track_new_mem_stack_w_ECU)(Addr,SizeT,UInt);

   void VG_REGPARM(1) (*track_new_mem_stack_4)  (Addr);
   void VG_REGPARM(1) (*track_new_mem_stack_8)  (Addr);
   void VG_REGPARM(1) (*track_new_mem_stack_12) (Addr);
   void VG_REGPARM(1) (*track_new_mem_stack_16) (Addr);
   void VG_REGPARM(1) (*track_new_mem_stack_32) (Addr);
   void VG_REGPARM(1) (*track_new_mem_stack_112)(Addr);
   void VG_REGPARM(1) (*track_new_mem_stack_128)(Addr);
   void VG_REGPARM(1) (*track_new_mem_stack_144)(Addr);
   void VG_REGPARM(1) (*track_new_mem_stack_160)(Addr);
   void (*track_new_mem_stack)(Addr,SizeT);

   Bool any_new_mem_stack; // True if one or more track_new_mem_stack is set

   void VG_REGPARM(1) (*track_die_mem_stack_4)  (Addr);
   void VG_REGPARM(1) (*track_die_mem_stack_8)  (Addr);
   void VG_REGPARM(1) (*track_die_mem_stack_12) (Addr);
   void VG_REGPARM(1) (*track_die_mem_stack_16) (Addr);
   void VG_REGPARM(1) (*track_die_mem_stack_32) (Addr);
   void VG_REGPARM(1) (*track_die_mem_stack_112)(Addr);
   void VG_REGPARM(1) (*track_die_mem_stack_128)(Addr);
   void VG_REGPARM(1) (*track_die_mem_stack_144)(Addr);
   void VG_REGPARM(1) (*track_die_mem_stack_160)(Addr);
   void (*track_die_mem_stack)(Addr, SizeT);

   Bool any_die_mem_stack; // True if one or more track_die_mem_stack is set

   void (*track_ban_mem_stack)(Addr, SizeT);

   void (*track_register_stack)(Addr, Addr);

   void (*track_pre_mem_read)       (CorePart, ThreadId, const HChar*, Addr, SizeT);
   void (*track_pre_mem_read_asciiz)(CorePart, ThreadId, const HChar*, Addr);
   void (*track_pre_mem_write)      (CorePart, ThreadId, const HChar*, Addr, SizeT);
   void (*track_post_mem_write)     (CorePart, ThreadId, Addr, SizeT);

   void (*track_pre_reg_read)  (CorePart, ThreadId, const HChar*, PtrdiffT, SizeT);
   void (*track_post_reg_write)(CorePart, ThreadId,               PtrdiffT, SizeT);
   void (*track_post_reg_write_clientcall_return)(ThreadId, PtrdiffT, SizeT,
                                                  Addr);

   void (*track_copy_mem_to_reg)(CorePart, ThreadId, Addr, PtrdiffT, SizeT);
   void (*track_copy_reg_to_mem)(CorePart, ThreadId, PtrdiffT, Addr, SizeT);

   void (*track_start_client_code)(ThreadId, ULong);
   void (*track_stop_client_code) (ThreadId, ULong);

   void (*track_pre_thread_ll_create)(ThreadId, ThreadId);
   void (*track_pre_thread_first_insn)(ThreadId);
   void (*track_pre_thread_ll_exit)  (ThreadId);

   void (*track_pre_deliver_signal) (ThreadId, Int sigNo, Bool);
   void (*track_post_deliver_signal)(ThreadId, Int sigNo);

} VgToolInterface;

extern VgToolInterface VG_(tdict);

/* ---------------------------------------------------------------------
   Miscellaneous functions
   ------------------------------------------------------------------ */

/* Sanity checks and finish the initialisation of the tool needs.
   Returns False and sets a failmsg if the needs are inconsistent. */
Bool VG_(finish_needs_init) ( const HChar** failmsg );

#endif   // __PUB_CORE_TOOLIFACE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
