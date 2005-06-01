
/*--------------------------------------------------------------------*/
/*--- A header file for various private parts of Valgrind's core.  ---*/
/*---                                                       core.h ---*/
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

#ifndef __CORE_H
#define __CORE_H

#include "tool.h"          // tool stuff
#include "core_arch.h"     // arch-specific stuff,  eg. x86/core_arch.h

#include "core_os.h"       // OS-specific stuff,    eg. linux/core_os.h

#include <setjmp.h>        // for jmp_buf

#include "pub_core_mallocfree.h"  // for type 'ArenaId'
#include "pub_core_scheduler.h"   // for types 'ThreadState', 'ThreadArchState'

/* ---------------------------------------------------------------------
   Global macros.
   ------------------------------------------------------------------ */

/* Max length of a text fragment used to construct error messages. */
#define VG_ERRTXT_LEN 4096

/* Useful macros */
/* a - alignment - must be a power of 2 */
#define ROUNDDN(p, a)	((Addr)(p) & ~((Addr)(a)-1))
#define ROUNDUP(p, a)	ROUNDDN((p)+(a)-1, (a))
#define PGROUNDDN(p)	ROUNDDN(p, VKI_PAGE_SIZE)
#define PGROUNDUP(p)	ROUNDUP(p, VKI_PAGE_SIZE)


/* ---------------------------------------------------------------------
   Environment variables
   ------------------------------------------------------------------ */

/* The directory we look for all our auxillary files in */
#define VALGRINDLIB	"VALGRINDLIB"

/* Application-visible file descriptor limits */
extern Int VG_(fd_soft_limit);
extern Int VG_(fd_hard_limit);

/* ---------------------------------------------------------------------
   Exports of vg_intercept.c
   ------------------------------------------------------------------ */

/* These are the internal client request codes.  The publically-visible
   request codes are also defined in valgrind.h, and similar headers for
   some tools. */

/* Get the tool's malloc-wrapping functions */
#define VG_USERREQ__GET_MALLOCFUNCS	    0x3030

/* Internal equivalent of VALGRIND_PRINTF . */
#define VG_USERREQ__INTERNAL_PRINTF         0x3103

/* Denote the finish of __libc_freeres_wrapper(). 
   A synonym for exit. */
#define VG_USERREQ__LIBC_FREERES_DONE       0x3029

/* Intercept prefix stuff.  See
   coregrind/m_replace_malloc/vg_replace_malloc.c for details.
   Unfortunately the "_vgi_" literal is also hardcoded in that file, so if
   you change this one you must also change the other one. */
#define VG_INTERCEPT_PREFIX "_vgi_"
#define VG_INTERCEPT_PREFIX_LEN 5

/* Not sure what these are for.  Todo: clarify */
#define VG_WRAPPER_PREFIX "_vgw_"
#define VG_WRAPPER_PREFIX_LEN 5
#define VG_WRAPPER(name) _vgw_##name
#define VG_WRAPPER_ALIAS(name) "_vgw_" #name


/* ---------------------------------------------------------------------
   Exports of vg_mylibc.c
   ------------------------------------------------------------------ */

// Useful for making failing stubs, when certain things haven't yet been
// implemented.
#define I_die_here                                             \
   VG_(assert_fail) (/*isCore*//*BOGUS*/True,                  \
                     "Unimplemented functionality",            \
                     __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                     "valgrind", VG_BUGS_TO, "")

#define vg_assert(expr)                                                 \
  ((void) ((expr) ? 0 :                                                 \
           (VG_(assert_fail) (/*isCore*/True, VG_STRINGIFY(expr),       \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              ""),                                      \
                              0)))

#define vg_assert2(expr, format, args...)                               \
  ((void) ((expr) ? 0 :                                                 \
           (VG_(assert_fail) (/*isCore*/True, VG_STRINGIFY(expr),       \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              format, ##args),                          \
                              0)))

__attribute__ ((__noreturn__))
extern void  VG_(core_panic)      ( Char* str );
__attribute__ ((__noreturn__))
extern void  VG_(core_panic_at)   ( Char* str, Addr ip, Addr sp, Addr fp );

/* Called when some unhandleable client behaviour is detected.
   Prints a msg and aborts. */
extern void VG_(unimplemented) ( Char* msg )
            __attribute__((__noreturn__));

/* Tell the logging mechanism whether we are logging to a file
   descriptor or a socket descriptor. */
extern Bool VG_(logging_to_socket);

/* Tools use VG_(strdup)() which doesn't expose ArenaId */
extern Char* VG_(arena_strdup) ( ArenaId aid, const Char* s);

extern Int VG_(fcntl) ( Int fd, Int cmd, Int arg );
extern Int VG_(poll)( struct vki_pollfd *, UInt nfds, Int timeout);

/* system/mman.h */
extern void* VG_(mmap)       ( void* start, SizeT length, UInt prot, UInt flags,
                               UInt sf_flags, UInt fd, OffT offset );
extern void* VG_(mmap_native)( void* start, SizeT length, UInt prot, UInt flags,
                                              UInt fd, OffT offset );
extern Int VG_(munmap)       ( void* start, SizeT length );
extern Int VG_(mprotect)       ( void *start, SizeT length, UInt prot );
extern Int VG_(mprotect_native)( void *start, SizeT length, UInt prot );


/* Move an fd into the Valgrind-safe range */
Int VG_(safe_fd)(Int oldfd);

extern Int VG_(write_socket)( Int sd, void *msg, Int count );

/* --- Connecting over the network --- */
extern Int VG_(connect_via_socket)( UChar* str );

/* Environment manipulations */
extern Char **VG_(env_setenv)   ( Char ***envp, const Char* varname,
                                  const Char *val );
extern void   VG_(env_unsetenv) ( Char **env, const Char *varname );
extern void   VG_(env_remove_valgrind_env_stuff) ( Char** env ); 

extern void   VG_(nanosleep)(struct vki_timespec *);

/* ---------------------------------------------------------------------
   Exports of vg_syscall.S
   ------------------------------------------------------------------ */

// We use a full prototype rather than "..." here to ensure that all
// arguments get converted to a UWord appropriately.  Not doing so can
// cause problems when passing 32-bit integers on 64-bit platforms, because
// the top 32-bits might not be zeroed appropriately, eg. as would happen
// with the 6th arg on AMD64 which is passed on the stack.
extern Word VG_(do_syscall) ( UInt, UWord, UWord, UWord, UWord, UWord, UWord );

// Macros make life easier.
#define vgPlain_do_syscall0(s)             VG_(do_syscall)((s),0,0,0,0,0,0)
#define vgPlain_do_syscall1(s,a)           VG_(do_syscall)((s),(a),0,0,0,0,0)
#define vgPlain_do_syscall2(s,a,b)         VG_(do_syscall)((s),(a),(b),0,0,0,0)
#define vgPlain_do_syscall3(s,a,b,c)       VG_(do_syscall)((s),(a),(b),(c),0,0,0)
#define vgPlain_do_syscall4(s,a,b,c,d)     VG_(do_syscall)((s),(a),(b),(c),(d),0,0)
#define vgPlain_do_syscall5(s,a,b,c,d,e)   VG_(do_syscall)((s),(a),(b),(c),(d),(e),0)
#define vgPlain_do_syscall6(s,a,b,c,d,e,f) VG_(do_syscall)((s),(a),(b),(c),(d),(e),(f))

extern void VG_(sigreturn)(void);

/* ---------------------------------------------------------------------
   Exports of vg_helpers.S
   ------------------------------------------------------------------ */

/* Information about trampoline code (for signal return and syscalls) */
extern const Char VG_(trampoline_code_start);
extern const Int  VG_(trampoline_code_length);
extern const Int  VG_(tramp_sigreturn_offset);
extern const Int  VG_(tramp_rt_sigreturn_offset);
extern const Int  VG_(tramp_syscall_offset);
extern const Int  VG_(tramp_gettimeofday_offset);
extern const Int  VG_(tramp_time_offset);
 
// ---------------------------------------------------------------------
// Architecture-specific things defined in eg. x86/*.c
// ---------------------------------------------------------------------

// Returns the architecture and subarchitecture, or indicates
// that this subarchitecture is unable to run Valgrind
// Returns False to indicate we cannot proceed further.
extern Bool VGA_(getArchAndSubArch)( /*OUT*/VexArch*, 
                                     /*OUT*/VexSubArch* );
// Accessors for the ThreadArchState
#define INSTR_PTR(regs)    ((regs).vex.VGA_INSTR_PTR)
#define STACK_PTR(regs)    ((regs).vex.VGA_STACK_PTR)
#define FRAME_PTR(regs)    ((regs).vex.VGA_FRAME_PTR)
#define CLREQ_ARGS(regs)   ((regs).vex.VGA_CLREQ_ARGS)
#define CLREQ_RET(regs)    ((regs).vex.VGA_CLREQ_RET)
// Offsets for the Vex state
#define O_STACK_PTR        (offsetof(VexGuestArchState, VGA_STACK_PTR))
#define O_FRAME_PTR        (offsetof(VexGuestArchState, VGA_FRAME_PTR))
#define O_CLREQ_RET        (offsetof(VexGuestArchState, VGA_CLREQ_RET))


// Setting up the initial thread (1) state
extern void 
       VGA_(init_thread1state) ( Addr client_eip, 
                                 Addr esp_at_startup,
                                 /*MOD*/ ThreadArchState* arch );

// OS/Platform-specific thread clear (after thread exit)
extern void VGO_(os_state_clear)(ThreadState *);

// OS/Platform-specific thread init (at scheduler init time)
extern void VGO_(os_state_init)(ThreadState *);

// Run a thread from beginning to end. 
extern VgSchedReturnCode VGO_(thread_wrapper)(Word /*ThreadId*/ tid);

// Call here to exit the entire Valgrind system.
extern void VGO_(terminate_NORETURN)(ThreadId tid, VgSchedReturnCode src);

// Allocates a stack for the first thread, then runs it,
// as if the thread had been set up by clone()
extern void VGP_(main_thread_wrapper_NORETURN)(ThreadId tid);

// Return how many bytes of a thread's Valgrind stack are unused
extern SSizeT VGA_(stack_unused)(ThreadId tid);

// wait until all other threads are dead
extern void VGA_(reap_threads)(ThreadId self);

// handle an arch-specific client request
extern Bool VGA_(client_request)(ThreadId tid, UWord *args);

// For attaching the debugger
extern Int  VGA_(ptrace_setregs_from_tst) ( Int pid, ThreadArchState* arch );

// Used by leakcheck
extern void VGA_(mark_from_registers)(ThreadId tid, void (*marker)(Addr));

// Set up the libc freeres wrapper
extern void VGA_(intercept_libc_freeres_wrapper)(Addr);

// Clean up the client by calling before the final reports
extern void VGA_(final_tidyup)(ThreadId tid);

// Arch-specific client requests
extern Bool VGA_(client_requests)(ThreadId tid, UWord *args);


///* ---------------------------------------------------------------------
//   Thread modelling
//   ------------------------------------------------------------------ */
//extern void VG_(tm_thread_create)  (ThreadId creator, ThreadId tid, Bool detached);
//extern void VG_(tm_thread_exit)    (ThreadId tid);
//extern Bool VG_(tm_thread_exists)  (ThreadId tid);
//extern void VG_(tm_thread_detach)  (ThreadId tid);
//extern void VG_(tm_thread_join)    (ThreadId joiner, ThreadId joinee);
//extern void VG_(tm_thread_switchto)(ThreadId tid);
//
//extern void VG_(tm_mutex_init)   (ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_destroy)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_trylock)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_giveup) (ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_acquire)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_tryunlock)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_unlock) (ThreadId tid, Addr mutexp);
//extern Bool VG_(tm_mutex_exists) (Addr mutexp);
//
//extern UInt VG_(tm_error_update_extra) (Error *err);
//extern Bool VG_(tm_error_equal) (VgRes res, Error *e1, Error *e2);
//extern void VG_(tm_error_print) (Error *err);
//
//extern void VG_(tm_init) ();
//
//extern void VG_(tm_cond_init)    (ThreadId tid, Addr condp);
//extern void VG_(tm_cond_destroy) (ThreadId tid, Addr condp);
//extern void VG_(tm_cond_wait)    (ThreadId tid, Addr condp, Addr mutexp);
//extern void VG_(tm_cond_wakeup)  (ThreadId tid, Addr condp, Addr mutexp);
//extern void VG_(tm_cond_signal)  (ThreadId tid, Addr condp);
//
///* ----- pthreads ----- */
//extern void VG_(pthread_init)      ();
//extern void VG_(pthread_startfunc_wrapper)(Addr wrapper);
//
//struct vg_pthread_newthread_data {
//   void	*(*startfunc)(void *arg);
//   void *arg;
//};

/* ---------------------------------------------------------------------
   Finally - autoconf-generated settings
   ------------------------------------------------------------------ */

#include "config.h"

#endif /* ndef __CORE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
