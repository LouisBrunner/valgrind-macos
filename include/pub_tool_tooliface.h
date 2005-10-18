
/*--------------------------------------------------------------------*/
/*--- The core/tool interface.                pub_tool_tooliface.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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

#ifndef __PUB_TOOL_TOOLIFACE_H
#define __PUB_TOOL_TOOLIFACE_H

#include "pub_tool_errormgr.h"   // for Error, Supp
#include "libvex.h"              // for VexGuestLayout

/* ------------------------------------------------------------------ */
/* The interface version */

/* The version number indicates binary-incompatible changes to the
   interface;  if the core and tool versions don't match, Valgrind
   will abort.  */
#define VG_CORE_INTERFACE_VERSION   9

typedef struct _ToolInfo {
   Int	sizeof_ToolInfo;
   Int	interface_version;

   /* Initialise tool.   Must do the following:
      - initialise the `details' struct, via the VG_(details_*)() functions
      - register any helpers called by generated code
      
      May do the following:
      - initialise the `needs' struct to indicate certain requirements, via
      the VG_(needs_*)() functions
      - initialize all the tool's entrypoints via the VG_(init_*)() functions
      - register any tool-specific profiling events
      - any other tool-specific initialisation
   */
   void (*tl_pre_clo_init) ( void );
} ToolInfo;

extern const ToolInfo VG_(tool_info);

/* Every tool must include this macro somewhere, exactly once. */
#define VG_DETERMINE_INTERFACE_VERSION(pre_clo_init)           \
   const ToolInfo VG_(tool_info) = {                           \
      .sizeof_ToolInfo   = sizeof(ToolInfo),                   \
      .interface_version = VG_CORE_INTERFACE_VERSION,          \
      .tl_pre_clo_init   = pre_clo_init,                       \
   };

/* ------------------------------------------------------------------ */
/* Basic tool functions */

extern void VG_(basic_tool_funcs)(
   // Do any initialisation that can only be done after command line
   // processing.
   void  (*post_clo_init)(void),

   // Instrument a basic block.  Must be a true function, ie. the same
   // input always results in the same output, because basic blocks
   // can be retranslated.  Unless you're doing something really
   // strange...  Note that orig_addr_noredir is not necessarily the
   // same as the address of the first instruction in the IR, due to
   // function redirection.
   IRBB* (*instrument)(IRBB* bb_in, VexGuestLayout* layout,
                       Addr64 orig_addr_noredir, VexGuestExtents* vge, 
                       IRType gWordTy, IRType hWordTy ),

   // Finish up, print out any results, etc.  `exitcode' is program's exit
   // code.  The shadow can be found with VG_(get_exit_status_shadow)().
   void  (*fini)(Int)
);

/* ------------------------------------------------------------------ */
/* Details */

/* Default value for avg_translations_sizeB (in bytes), indicating typical
   code expansion of about 6:1. */
#define VG_DEFAULT_TRANS_SIZEB   100

/* Information used in the startup message.  `name' also determines the
   string used for identifying suppressions in a suppression file as
   belonging to this tool.  `version' can be NULL, in which case (not
   surprisingly) no version info is printed; this mechanism is designed for
   tools distributed with Valgrind that share a version number with
   Valgrind.  Other tools not distributed as part of Valgrind should
   probably have their own version number.  */
extern void VG_(details_name)                  ( Char* name );
extern void VG_(details_version)               ( Char* version );
extern void VG_(details_description)           ( Char* description );
extern void VG_(details_copyright_author)      ( Char* copyright_author );

/* Average size of a translation, in bytes, so that the translation
   storage machinery can allocate memory appropriately.  Not critical,
   setting is optional. */
extern void VG_(details_avg_translation_sizeB) ( UInt size );

/* String printed if an `tl_assert' assertion fails or VG_(tool_panic)
   is called.  Should probably be an email address. */
extern void VG_(details_bug_reports_to)   ( Char* bug_reports_to );

/* ------------------------------------------------------------------ */
/* Needs */

/* Booleans that decide core behaviour, but don't require extra
   operations to be defined if `True' */

/* Should __libc_freeres() be run?  Bugs in it can crash the tool. */
extern void VG_(needs_libc_freeres) ( void );

/* Want to have errors detected by Valgrind's core reported?  Includes:
   - pthread API errors (many;  eg. unlocking a non-locked mutex) 
     [currently disabled]
   - invalid file descriptors to syscalls like read() and write()
   - bad signal numbers passed to sigaction()
   - attempt to install signal handler for SIGKILL or SIGSTOP */
extern void VG_(needs_core_errors) ( void );

/* Booleans that indicate extra operations are defined;  if these are True,
   the corresponding template functions (given below) must be defined.  A
   lot like being a member of a type class. */

/* Want to report errors from tool?  This implies use of suppressions, too. */
extern void VG_(needs_tool_errors) (
   // Identify if two errors are equal, or equal enough.  `res' indicates how
   // close is "close enough".  `res' should be passed on as necessary, eg. if
   // the Error's `extra' part contains an ExeContext, `res' should be
   // passed to VG_(eq_ExeContext)() if the ExeContexts are considered.  Other
   // than that, probably don't worry about it unless you have lots of very
   // similar errors occurring.
   Bool (*eq_Error)(VgRes res, Error* e1, Error* e2),

   // Print error context.
   void (*pp_Error)(Error* err),

   // Should fill in any details that could be postponed until after the
   // decision whether to ignore the error (ie. details not affecting the
   // result of VG_(tdict).tool_eq_Error()).  This saves time when errors
   // are ignored.
   // Yuk.
   // Return value: must be the size of the `extra' part in bytes -- used by
   // the core to make a copy.
   UInt (*update_extra)(Error* err),

   // Return value indicates recognition.  If recognised, must set skind using
   // VG_(set_supp_kind)().
   Bool (*recognised_suppression)(Char* name, Supp* su),

   // Read any extra info for this suppression kind.  Most likely for filling
   // in the `extra' and `string' parts (with VG_(set_supp_{extra, string})())
   // of a suppression if necessary.  Should return False if a syntax error
   // occurred, True otherwise.
   Bool (*read_extra_suppression_info)(Int fd, Char* buf, Int nBuf, Supp* su),

   // This should just check the kinds match and maybe some stuff in the
   // `string' and `extra' field if appropriate (using VG_(get_supp_*)() to
   // get the relevant suppression parts).
   Bool (*error_matches_suppression)(Error* err, Supp* su),

   // This should return the suppression name, for --gen-suppressions, or NULL
   // if that error type cannot be suppressed.  This is the inverse of
   // VG_(tdict).tool_recognised_suppression().
   Char* (*get_error_name)(Error* err),

   // This should print any extra info for the error, for --gen-suppressions,
   // including the newline.  This is the inverse of
   // VG_(tdict).tool_read_extra_suppression_info().
   void (*print_extra_suppression_info)(Error* err)
);

/* Is information kept by the tool about specific instructions or
   translations?  (Eg. for cachegrind there are cost-centres for every
   instruction, stored in a per-translation fashion.)  If so, the info
   may have to be discarded when translations are unloaded (eg. due to
   .so unloading, or otherwise at the discretion of m_transtab, eg
   when the table becomes too full) to avoid stale information being
   reused for new translations. */
extern void VG_(needs_basic_block_discards) (
   // Discard any information that pertains to specific translations
   // or instructions within the address range given.  There are two
   // possible approaches.
   // - If info is being stored at a per-translation level, use orig_addr
   //   to identify which translation is being discarded.  Each translation
   //   will be discarded exactly once.
   //   This orig_addr will match the orig_addr which was passed to
   //   to instrument() when this translation was made.  Note that orig_addr
   //   won't necessarily be the same as the first address in "extents".
   // - If info is being stored at a per-instruction level, you can get
   //   the address range(s) being discarded by stepping through "extents".
   //   Note that any single instruction may belong to more than one
   //   translation, and so could be covered by the "extents" of more than
   //   one call to this function.
   // Doing it the first way (as eg. Cachegrind does) is probably easier.
   void (*discard_basic_block_info)(Addr64 orig_addr, VexGuestExtents extents)
);

/* Tool defines its own command line options? */
extern void VG_(needs_command_line_options) (
   // Return True if option was recognised.  Presumably sets some state to
   // record the option as well.
   Bool (*process_cmd_line_option)(Char* argv),

   // Print out command line usage for options for normal tool operation.
   void (*print_usage)(void),

   // Print out command line usage for options for debugging the tool.
   void (*print_debug_usage)(void)
);

/* Tool defines its own client requests? */
extern void VG_(needs_client_requests) (
   // If using client requests, the number of the first request should be equal
   // to VG_USERREQ_TOOL_BASE('X', 'Y'), where 'X' and 'Y' form a suitable two
   // character identification for the string.  The second and subsequent
   // requests should follow.
   //
   // This function should use the VG_IS_TOOL_USERREQ macro (in
   // include/valgrind.h) to first check if it's a request for this tool.  Then
   // should handle it if it's recognised (and return True), or return False if
   // not recognised.  arg_block[0] holds the request number, any further args
   // from the request are in arg_block[1..].  'ret' is for the return value...
   // it should probably be filled, if only with 0.
   Bool (*handle_client_request)(ThreadId tid, UWord* arg_block, UWord* ret)
);

/* Tool does stuff before and/or after system calls? */
// Nb: If either of the pre_ functions malloc() something to return, the
// corresponding post_ function had better free() it!
extern void VG_(needs_syscall_wrapper) (
   void (* pre_syscall)(ThreadId tid, UInt syscallno),
   void (*post_syscall)(ThreadId tid, UInt syscallno, SysRes res)
);

/* Are tool-state sanity checks performed? */
// Can be useful for ensuring a tool's correctness.  cheap_sanity_check()
// is called very frequently;  expensive_sanity_check() is called less
// frequently and can be more involved.
extern void VG_(needs_sanity_checks) (
   Bool(*cheap_sanity_check)(void),
   Bool(*expensive_sanity_check)(void)
);

/* Do we need to see data symbols? */
extern void VG_(needs_data_syms) ( void );

/* Does the tool need shadow memory allocated? */
extern void VG_(needs_shadow_memory)( void );

/* Does the tool replace malloc() and friends with its own versions?
   This has to be combined with the use of a vgpreload_<tool>.so module
   or it won't work.  See massif/Makefile.am for how to build it. */
// The 'p' prefix avoids GCC complaints about overshadowing global names.
extern void VG_(needs_malloc_replacement)(
   void* (*pmalloc)               ( ThreadId tid, SizeT n ),
   void* (*p__builtin_new)        ( ThreadId tid, SizeT n ),
   void* (*p__builtin_vec_new)    ( ThreadId tid, SizeT n ),
   void* (*pmemalign)             ( ThreadId tid, SizeT align, SizeT n ),
   void* (*pcalloc)               ( ThreadId tid, SizeT nmemb, SizeT size1 ),
   void  (*pfree)                 ( ThreadId tid, void* p ),
   void  (*p__builtin_delete)     ( ThreadId tid, void* p ),
   void  (*p__builtin_vec_delete) ( ThreadId tid, void* p ),
   void* (*prealloc)              ( ThreadId tid, void* p, SizeT new_size ),
   SizeT client_malloc_redzone_szB
);

/* ------------------------------------------------------------------ */
/* Core events to track */

/* Part of the core from which this call was made.  Useful for determining
   what kind of error message should be emitted. */
typedef
   enum { Vg_CoreStartup, Vg_CorePThread, Vg_CoreSignal, Vg_CoreSysCall,
          Vg_CoreTranslate, Vg_CoreClientReq }
   CorePart;

/* Events happening in core to track.  To be notified, pass a callback
   function to the appropriate function.  To ignore an event, don't do
   anything (the default is for events to be ignored).

   Note that most events aren't passed a ThreadId.  If the event is one called
   from generated code (eg. new_mem_stack_*), you can use
   VG_(get_running_tid)() to find it.  Otherwise, it has to be passed in,
   as in pre_mem_read, and so the event signature will require changing.

   Memory events (Nb: to track heap allocation/freeing, a tool must replace
   malloc() et al.  See above how to do this.)

   These ones occur at startup, upon some signals, and upon some syscalls
 */
void VG_(track_new_mem_startup)     (void(*f)(Addr a, SizeT len,
                                              Bool rr, Bool ww, Bool xx));
void VG_(track_new_mem_stack_signal)(void(*f)(Addr a, SizeT len));
void VG_(track_new_mem_brk)         (void(*f)(Addr a, SizeT len));
void VG_(track_new_mem_mmap)        (void(*f)(Addr a, SizeT len,
                                              Bool rr, Bool ww, Bool xx));

void VG_(track_copy_mem_remap)      (void(*f)(Addr from, Addr to, SizeT len));
void VG_(track_change_mem_mprotect) (void(*f)(Addr a, SizeT len,
                                              Bool rr, Bool ww, Bool xx));
void VG_(track_die_mem_stack_signal)(void(*f)(Addr a, SizeT len));
void VG_(track_die_mem_brk)         (void(*f)(Addr a, SizeT len));
void VG_(track_die_mem_munmap)      (void(*f)(Addr a, SizeT len));

/* These ones are called when SP changes.  A tool could track these itself
   (except for ban_mem_stack) but it's much easier to use the core's help.

   The specialised ones are called in preference to the general one, if they
   are defined.  These functions are called a lot if they are used, so
   specialising can optimise things significantly.  If any of the
   specialised cases are defined, the general case must be defined too.

   Nb: all the specialised ones must use the VG_REGPARM(n) attribute.
 */
void VG_(track_new_mem_stack_4) (VG_REGPARM(1) void(*f)(Addr new_ESP));
void VG_(track_new_mem_stack_8) (VG_REGPARM(1) void(*f)(Addr new_ESP));
void VG_(track_new_mem_stack_12)(VG_REGPARM(1) void(*f)(Addr new_ESP));
void VG_(track_new_mem_stack_16)(VG_REGPARM(1) void(*f)(Addr new_ESP));
void VG_(track_new_mem_stack_32)(VG_REGPARM(1) void(*f)(Addr new_ESP));
void VG_(track_new_mem_stack)                 (void(*f)(Addr a, SizeT len));

void VG_(track_die_mem_stack_4) (VG_REGPARM(1) void(*f)(Addr die_ESP));
void VG_(track_die_mem_stack_8) (VG_REGPARM(1) void(*f)(Addr die_ESP));
void VG_(track_die_mem_stack_12)(VG_REGPARM(1) void(*f)(Addr die_ESP));
void VG_(track_die_mem_stack_16)(VG_REGPARM(1) void(*f)(Addr die_ESP));
void VG_(track_die_mem_stack_32)(VG_REGPARM(1) void(*f)(Addr die_ESP));
void VG_(track_die_mem_stack)                 (void(*f)(Addr a, SizeT len));

/* Used for redzone at end of thread stacks */
void VG_(track_ban_mem_stack)      (void(*f)(Addr a, SizeT len));

/* These ones occur around syscalls, signal handling, etc */
void VG_(track_pre_mem_read)       (void(*f)(CorePart part, ThreadId tid,
                                             Char* s, Addr a, SizeT size));
void VG_(track_pre_mem_read_asciiz)(void(*f)(CorePart part, ThreadId tid,
                                             Char* s, Addr a));
void VG_(track_pre_mem_write)      (void(*f)(CorePart part, ThreadId tid,
                                             Char* s, Addr a, SizeT size));
void VG_(track_post_mem_write)     (void(*f)(CorePart part, ThreadId tid,
                                             Addr a, SizeT size));

/* Register events.  Use VG_(set_shadow_state_area)() to set the shadow regs
   for these events.  */
void VG_(track_pre_reg_read)  (void(*f)(CorePart part, ThreadId tid,
                                        Char* s, OffT guest_state_offset,
                                        SizeT size));
void VG_(track_post_reg_write)(void(*f)(CorePart part, ThreadId tid,
                                        OffT guest_state_offset,
                                        SizeT size));

/* This one is called for malloc() et al if they are replaced by a tool. */
void VG_(track_post_reg_write_clientcall_return)(
      void(*f)(ThreadId tid, OffT guest_state_offset, SizeT size, Addr f));


/* Scheduler events (not exhaustive) */
void VG_(track_thread_run)(void(*f)(ThreadId tid));


/* Thread events (not exhaustive)

   Called during thread create, before the new thread has run any
   instructions (or touched any memory).
 */
void VG_(track_post_thread_create)(void(*f)(ThreadId tid, ThreadId child));
void VG_(track_post_thread_join)  (void(*f)(ThreadId joiner, ThreadId joinee));

/* Mutex events (not exhaustive)
   "void *mutex" is really a pthread_mutex *

   Called before a thread can block while waiting for a mutex (called
   regardless of whether the thread will block or not).  */
void VG_(track_pre_mutex_lock)(void(*f)(ThreadId tid, void* mutex));

/* Called once the thread actually holds the mutex (always paired with
   pre_mutex_lock).  */
void VG_(track_post_mutex_lock)(void(*f)(ThreadId tid, void* mutex));

/* Called after a thread has released a mutex (no need for a corresponding
   pre_mutex_unlock, because unlocking can't block).  */
void VG_(track_post_mutex_unlock)(void(*f)(ThreadId tid, void* mutex));

/* Signal events (not exhaustive)

   ... pre_send_signal, post_send_signal ...

   Called before a signal is delivered;  `alt_stack' indicates if it is
   delivered on an alternative stack.  */
void VG_(track_pre_deliver_signal) (void(*f)(ThreadId tid, Int sigNo,
                                             Bool alt_stack));
/* Called after a signal is delivered.  Nb: unfortunately, if the signal
   handler longjmps, this won't be called.  */
void VG_(track_post_deliver_signal)(void(*f)(ThreadId tid, Int sigNo));

/* Others... condition variables...
   ...
 */

#endif   // __PUB_TOOL_TOOLIFACE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
