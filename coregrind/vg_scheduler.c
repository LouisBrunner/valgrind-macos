
/*--------------------------------------------------------------------*/
/*--- A user-space pthreads implementation.         vg_scheduler.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"
#include "vg_constants.h"
#include "valgrind.h" /* for VG_USERREQ__MAKE_NOACCESS and
                         VG_USERREQ__DO_LEAK_CHECK */

/* BORKAGE/ISSUES as of 14 Apr 02

Note!  This pthreads implementation is so poor as to not be
suitable for use by anyone at all!

- Currently, when a signal is run, just the ThreadStatus.status fields 
  are saved in the signal frame, along with the CPU state.  Question: 
  should I also save and restore:
     ThreadStatus.joiner 
     ThreadStatus.waited_on_mid
     ThreadStatus.awaken_at
     ThreadStatus.retval
  Currently unsure, and so am not doing so.

- Signals interrupting read/write and nanosleep: SA_RESTART settings.
  Read/write correctly return with EINTR when SA_RESTART isn't
  specified and they are interrupted by a signal.  nanosleep just
  pretends signals don't exist -- should be fixed.

- Read/write syscall starts: don't crap out when the initial
  nonblocking read/write returns an error.

- Get rid of restrictions re use of sigaltstack; they are no longer
  needed.  

- Fix signals properly, so that each thread has its own blocking mask.
  Currently this isn't done, and (worse?) signals are delivered to
  Thread 1 (the root thread) regardless.  

  So, what's the deal with signals and mutexes?  If a thread is
  blocked on a mutex, or for a condition variable for that matter, can
  signals still be delivered to it?  This has serious consequences --
  deadlocks, etc.

*/


/* ---------------------------------------------------------------------
   Types and globals for the scheduler.
   ------------------------------------------------------------------ */

/* type ThreadId is defined in vg_include.h. */

/* struct ThreadState is defined in vg_include.h. */

/* Private globals.  A statically allocated array of threads.  NOTE:
   [0] is never used, to simplify the simulation of initialisers for
   LinuxThreads. */
static ThreadState vg_threads[VG_N_THREADS];

/* The tid of the thread currently in VG_(baseBlock). */
static Int vg_tid_currently_in_baseBlock = VG_INVALID_THREADID;


/* vg_oursignalhandler() might longjmp().  Here's the jmp_buf. */
jmp_buf VG_(scheduler_jmpbuf);
/* ... and if so, here's the signal which caused it to do so. */
Int     VG_(longjmpd_on_signal);


/* Machinery to keep track of which threads are waiting on which
   fds. */
typedef
   struct {
      /* The thread which made the request. */
      ThreadId tid;

      /* The next two fields describe the request. */
      /* File descriptor waited for.  -1 means this slot is not in use */
      Int      fd;
      /* The syscall number the fd is used in. */
      Int      syscall_no;

      /* False => still waiting for select to tell us the fd is ready
         to go.  True => the fd is ready, but the results have not yet
         been delivered back to the calling thread.  Once the latter
         happens, this entire record is marked as no longer in use, by
         making the fd field be -1.  */
      Bool     ready; 
   }
   VgWaitedOnFd;

static VgWaitedOnFd vg_waiting_fds[VG_N_WAITING_FDS];


/* Keeping track of keys. */
typedef
   struct {
      /* Has this key been allocated ? */
      Bool inuse;
      /* If .inuse==True, records the address of the associated
         destructor, or NULL if none. */
      void (*destructor)(void*);
   }
   ThreadKeyState;

/* And our array of thread keys. */
static ThreadKeyState vg_thread_keys[VG_N_THREAD_KEYS];

typedef UInt ThreadKey;


/* Forwards */
static void do_pthread_cond_timedwait_TIMEOUT ( ThreadId tid );

static void do_nontrivial_clientreq ( ThreadId tid );

static void scheduler_sanity ( void );

static void do_pthread_mutex_unlock ( ThreadId, 
                                      void* /* pthread_mutex_t* */ );
static void do_pthread_mutex_lock ( ThreadId, Bool, 
                                    void* /* pthread_mutex_t* */ );

static void do_pthread_getspecific ( ThreadId,
                                     UInt /* pthread_key_t */ );


/* ---------------------------------------------------------------------
   Helper functions for the scheduler.
   ------------------------------------------------------------------ */

__inline__
Bool VG_(is_valid_tid) ( ThreadId tid )
{
   /* tid is unsigned, hence no < 0 test. */
   if (tid == 0) return False;
   if (tid >= VG_N_THREADS) return False;
   return True;
}


/* For constructing error messages only: try and identify a thread
   whose stack this address currently falls within, or return
   VG_INVALID_THREADID if it doesn't.  A small complication is dealing
   with any currently VG_(baseBlock)-resident thread. 
*/
ThreadId VG_(identify_stack_addr)( Addr a )
{
   ThreadId tid, tid_to_skip;

   tid_to_skip = VG_INVALID_THREADID;

   /* First check to see if there's a currently-loaded thread in
      VG_(baseBlock). */
   if (vg_tid_currently_in_baseBlock != VG_INVALID_THREADID) {
      tid = vg_tid_currently_in_baseBlock;
      if (VG_(baseBlock)[VGOFF_(m_esp)] <= a
          && a <= vg_threads[tid].stack_highest_word) 
         return tid;
      else
         tid_to_skip = tid;
   }

   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (vg_threads[tid].status == VgTs_Empty) continue;
      if (tid == tid_to_skip) continue;
      if (vg_threads[tid].m_esp <= a 
          && a <= vg_threads[tid].stack_highest_word)
         return tid;
   }
   return VG_INVALID_THREADID;
}
 

/* Print the scheduler status. */
void VG_(pp_sched_status) ( void )
{
   Int i; 
   VG_(printf)("\nsched status:\n"); 
   for (i = 1; i < VG_N_THREADS; i++) {
      if (vg_threads[i].status == VgTs_Empty) continue;
      VG_(printf)("\nThread %d: status = ", i);
      switch (vg_threads[i].status) {
         case VgTs_Runnable:   VG_(printf)("Runnable"); break;
         case VgTs_WaitFD:     VG_(printf)("WaitFD"); break;
         case VgTs_WaitJoiner: VG_(printf)("WaitJoiner(%d)", 
                                           vg_threads[i].joiner); break;
         case VgTs_WaitJoinee: VG_(printf)("WaitJoinee"); break;
         case VgTs_Sleeping:   VG_(printf)("Sleeping"); break;
         case VgTs_WaitMX:     VG_(printf)("WaitMX"); break;
         case VgTs_WaitCV:     VG_(printf)("WaitCV"); break;
         case VgTs_WaitSIG:    VG_(printf)("WaitSIG"); break;
         default: VG_(printf)("???"); break;
      }
      VG_(printf)(", associated_mx = %p, associated_cv = %p\n", 
                  vg_threads[i].associated_mx,
                  vg_threads[i].associated_cv );
      VG_(pp_ExeContext)( 
         VG_(get_ExeContext)( False, vg_threads[i].m_eip, 
                                     vg_threads[i].m_ebp ));
   }
   VG_(printf)("\n");
}

static
void add_waiting_fd ( ThreadId tid, Int fd, Int syscall_no )
{
   Int i;

   vg_assert(fd != -1); /* avoid total chaos */

   for (i = 0;  i < VG_N_WAITING_FDS; i++)
      if (vg_waiting_fds[i].fd == -1)
         break;

   if (i == VG_N_WAITING_FDS)
      VG_(panic)("add_waiting_fd: VG_N_WAITING_FDS is too low");
   /*
   VG_(printf)("add_waiting_fd: add (tid %d, fd %d) at slot %d\n", 
               tid, fd, i);
   */
   vg_waiting_fds[i].fd         = fd;
   vg_waiting_fds[i].tid        = tid;
   vg_waiting_fds[i].ready      = False;
   vg_waiting_fds[i].syscall_no = syscall_no;
}



static
void print_sched_event ( ThreadId tid, Char* what )
{
   VG_(message)(Vg_DebugMsg, "  SCHED[%d]: %s", tid, what );
}


static
void print_pthread_event ( ThreadId tid, Char* what )
{
   VG_(message)(Vg_DebugMsg, "PTHREAD[%d]: %s", tid, what );
}


static
Char* name_of_sched_event ( UInt event )
{
   switch (event) {
      case VG_TRC_EBP_JMP_SYSCALL:    return "SYSCALL";
      case VG_TRC_EBP_JMP_CLIENTREQ:  return "CLIENTREQ";
      case VG_TRC_INNER_COUNTERZERO:  return "COUNTERZERO";
      case VG_TRC_INNER_FASTMISS:     return "FASTMISS";
      case VG_TRC_UNRESUMABLE_SIGNAL: return "FATALSIGNAL";
      default:                        return "??UNKNOWN??";
  }
}


/* Create a translation of the client basic block beginning at
   orig_addr, and add it to the translation cache & translation table.
   This probably doesn't really belong here, but, hey ... 
*/
static
void create_translation_for ( ThreadId tid, Addr orig_addr )
{
   Addr    trans_addr;
   TTEntry tte;
   Int orig_size, trans_size;
   /* Ensure there is space to hold a translation. */
   VG_(maybe_do_lru_pass)();
   VG_(translate)( &vg_threads[tid],
                   orig_addr, &orig_size, &trans_addr, &trans_size );
   /* Copy data at trans_addr into the translation cache.
      Returned pointer is to the code, not to the 4-byte
      header. */
   /* Since the .orig_size and .trans_size fields are
      UShort, be paranoid. */
   vg_assert(orig_size > 0 && orig_size < 65536);
   vg_assert(trans_size > 0 && trans_size < 65536);
   tte.orig_size  = orig_size;
   tte.orig_addr  = orig_addr;
   tte.trans_size = trans_size;
   tte.trans_addr = VG_(copy_to_transcache)
                       ( trans_addr, trans_size );
   tte.mru_epoch  = VG_(current_epoch);
   /* Free the intermediary -- was allocated by VG_(emit_code). */
   VG_(jitfree)( (void*)trans_addr );
   /* Add to trans tab and set back pointer. */
   VG_(add_to_trans_tab) ( &tte );
   /* Update stats. */
   VG_(this_epoch_in_count) ++;
   VG_(this_epoch_in_osize) += orig_size;
   VG_(this_epoch_in_tsize) += trans_size;
   VG_(overall_in_count) ++;
   VG_(overall_in_osize) += orig_size;
   VG_(overall_in_tsize) += trans_size;
   /* Record translated area for SMC detection. */
   VG_(smc_mark_original) ( orig_addr, orig_size );
}


/* Allocate a completely empty ThreadState record. */
static
ThreadId vg_alloc_ThreadState ( void )
{
   Int i;
   for (i = 1; i < VG_N_THREADS; i++) {
      if (vg_threads[i].status == VgTs_Empty)
         return i;
   }
   VG_(printf)("vg_alloc_ThreadState: no free slots available\n");
   VG_(printf)("Increase VG_N_THREADS, rebuild and try again.\n");
   VG_(panic)("VG_N_THREADS is too low");
   /*NOTREACHED*/
}


ThreadState* VG_(get_thread_state_UNCHECKED) ( ThreadId tid )
{
   vg_assert(VG_(is_valid_tid)(tid));
   return & vg_threads[tid];
}


ThreadState* VG_(get_thread_state) ( ThreadId tid )
{
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(vg_threads[tid].status != VgTs_Empty);
   return & vg_threads[tid];
}


ThreadState* VG_(get_current_thread_state) ( void )
{
   vg_assert(vg_tid_currently_in_baseBlock != VG_INVALID_THREADID);
   return VG_(get_thread_state) ( vg_tid_currently_in_baseBlock );
}


ThreadId VG_(get_current_tid) ( void )
{
   vg_assert(vg_tid_currently_in_baseBlock != VG_INVALID_THREADID);
   return vg_tid_currently_in_baseBlock;
}


/* Copy the saved state of a thread into VG_(baseBlock), ready for it
   to be run. */
__inline__
void VG_(load_thread_state) ( ThreadId tid )
{
   Int i;
   vg_assert(vg_tid_currently_in_baseBlock == VG_INVALID_THREADID);

   VG_(baseBlock)[VGOFF_(m_eax)] = vg_threads[tid].m_eax;
   VG_(baseBlock)[VGOFF_(m_ebx)] = vg_threads[tid].m_ebx;
   VG_(baseBlock)[VGOFF_(m_ecx)] = vg_threads[tid].m_ecx;
   VG_(baseBlock)[VGOFF_(m_edx)] = vg_threads[tid].m_edx;
   VG_(baseBlock)[VGOFF_(m_esi)] = vg_threads[tid].m_esi;
   VG_(baseBlock)[VGOFF_(m_edi)] = vg_threads[tid].m_edi;
   VG_(baseBlock)[VGOFF_(m_ebp)] = vg_threads[tid].m_ebp;
   VG_(baseBlock)[VGOFF_(m_esp)] = vg_threads[tid].m_esp;
   VG_(baseBlock)[VGOFF_(m_eflags)] = vg_threads[tid].m_eflags;
   VG_(baseBlock)[VGOFF_(m_eip)] = vg_threads[tid].m_eip;

   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      VG_(baseBlock)[VGOFF_(m_fpustate) + i] = vg_threads[tid].m_fpu[i];

   VG_(baseBlock)[VGOFF_(sh_eax)] = vg_threads[tid].sh_eax;
   VG_(baseBlock)[VGOFF_(sh_ebx)] = vg_threads[tid].sh_ebx;
   VG_(baseBlock)[VGOFF_(sh_ecx)] = vg_threads[tid].sh_ecx;
   VG_(baseBlock)[VGOFF_(sh_edx)] = vg_threads[tid].sh_edx;
   VG_(baseBlock)[VGOFF_(sh_esi)] = vg_threads[tid].sh_esi;
   VG_(baseBlock)[VGOFF_(sh_edi)] = vg_threads[tid].sh_edi;
   VG_(baseBlock)[VGOFF_(sh_ebp)] = vg_threads[tid].sh_ebp;
   VG_(baseBlock)[VGOFF_(sh_esp)] = vg_threads[tid].sh_esp;
   VG_(baseBlock)[VGOFF_(sh_eflags)] = vg_threads[tid].sh_eflags;

   vg_tid_currently_in_baseBlock = tid;
}


/* Copy the state of a thread from VG_(baseBlock), presumably after it
   has been descheduled.  For sanity-check purposes, fill the vacated
   VG_(baseBlock) with garbage so as to make the system more likely to
   fail quickly if we erroneously continue to poke around inside
   VG_(baseBlock) without first doing a load_thread_state().  
*/
__inline__
void VG_(save_thread_state) ( ThreadId tid )
{
   Int i;
   const UInt junk = 0xDEADBEEF;

   vg_assert(vg_tid_currently_in_baseBlock != VG_INVALID_THREADID);

   vg_threads[tid].m_eax = VG_(baseBlock)[VGOFF_(m_eax)];
   vg_threads[tid].m_ebx = VG_(baseBlock)[VGOFF_(m_ebx)];
   vg_threads[tid].m_ecx = VG_(baseBlock)[VGOFF_(m_ecx)];
   vg_threads[tid].m_edx = VG_(baseBlock)[VGOFF_(m_edx)];
   vg_threads[tid].m_esi = VG_(baseBlock)[VGOFF_(m_esi)];
   vg_threads[tid].m_edi = VG_(baseBlock)[VGOFF_(m_edi)];
   vg_threads[tid].m_ebp = VG_(baseBlock)[VGOFF_(m_ebp)];
   vg_threads[tid].m_esp = VG_(baseBlock)[VGOFF_(m_esp)];
   vg_threads[tid].m_eflags = VG_(baseBlock)[VGOFF_(m_eflags)];
   vg_threads[tid].m_eip = VG_(baseBlock)[VGOFF_(m_eip)];

   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      vg_threads[tid].m_fpu[i] = VG_(baseBlock)[VGOFF_(m_fpustate) + i];

   vg_threads[tid].sh_eax = VG_(baseBlock)[VGOFF_(sh_eax)];
   vg_threads[tid].sh_ebx = VG_(baseBlock)[VGOFF_(sh_ebx)];
   vg_threads[tid].sh_ecx = VG_(baseBlock)[VGOFF_(sh_ecx)];
   vg_threads[tid].sh_edx = VG_(baseBlock)[VGOFF_(sh_edx)];
   vg_threads[tid].sh_esi = VG_(baseBlock)[VGOFF_(sh_esi)];
   vg_threads[tid].sh_edi = VG_(baseBlock)[VGOFF_(sh_edi)];
   vg_threads[tid].sh_ebp = VG_(baseBlock)[VGOFF_(sh_ebp)];
   vg_threads[tid].sh_esp = VG_(baseBlock)[VGOFF_(sh_esp)];
   vg_threads[tid].sh_eflags = VG_(baseBlock)[VGOFF_(sh_eflags)];

   /* Fill it up with junk. */
   VG_(baseBlock)[VGOFF_(m_eax)] = junk;
   VG_(baseBlock)[VGOFF_(m_ebx)] = junk;
   VG_(baseBlock)[VGOFF_(m_ecx)] = junk;
   VG_(baseBlock)[VGOFF_(m_edx)] = junk;
   VG_(baseBlock)[VGOFF_(m_esi)] = junk;
   VG_(baseBlock)[VGOFF_(m_edi)] = junk;
   VG_(baseBlock)[VGOFF_(m_ebp)] = junk;
   VG_(baseBlock)[VGOFF_(m_esp)] = junk;
   VG_(baseBlock)[VGOFF_(m_eflags)] = junk;
   VG_(baseBlock)[VGOFF_(m_eip)] = junk;

   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      VG_(baseBlock)[VGOFF_(m_fpustate) + i] = junk;

   vg_tid_currently_in_baseBlock = VG_INVALID_THREADID;
}


/* Run the thread tid for a while, and return a VG_TRC_* value to the
   scheduler indicating what happened. */
static
UInt run_thread_for_a_while ( ThreadId tid )
{
   volatile UInt trc = 0;
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(vg_threads[tid].status == VgTs_Runnable);
   vg_assert(VG_(bbs_to_go) > 0);

   VGP_PUSHCC(VgpRun);
   VG_(load_thread_state) ( tid );
   if (__builtin_setjmp(VG_(scheduler_jmpbuf)) == 0) {
      /* try this ... */
      trc = VG_(run_innerloop)();
      /* We get here if the client didn't take a fault. */
   } else {
      /* We get here if the client took a fault, which caused our
         signal handler to longjmp. */
      vg_assert(trc == 0);
      trc = VG_TRC_UNRESUMABLE_SIGNAL;
   }
   VG_(save_thread_state) ( tid );
   VGP_POPCC;
   return trc;
}


/* Increment the LRU epoch counter. */
static
void increment_epoch ( void )
{
   VG_(current_epoch)++;
   if (VG_(clo_verbosity) > 2) {
      UInt tt_used, tc_used;
      VG_(get_tt_tc_used) ( &tt_used, &tc_used );
      VG_(message)(Vg_UserMsg,
         "%lu bbs, in: %d (%d -> %d), out %d (%d -> %d), TT %d, TC %d",
          VG_(bbs_done), 
          VG_(this_epoch_in_count),
          VG_(this_epoch_in_osize),
          VG_(this_epoch_in_tsize),
          VG_(this_epoch_out_count),
          VG_(this_epoch_out_osize),
          VG_(this_epoch_out_tsize),
          tt_used, tc_used
       );
   }
   VG_(this_epoch_in_count) = 0;
   VG_(this_epoch_in_osize) = 0;
   VG_(this_epoch_in_tsize) = 0;
   VG_(this_epoch_out_count) = 0;
   VG_(this_epoch_out_osize) = 0;
   VG_(this_epoch_out_tsize) = 0;
}


/* Initialise the scheduler.  Create a single "main" thread ready to
   run, with special ThreadId of one.  This is called at startup; the
   caller takes care to park the client's state is parked in
   VG_(baseBlock).  
*/
void VG_(scheduler_init) ( void )
{
   Int      i;
   Addr     startup_esp;
   ThreadId tid_main;

   startup_esp = VG_(baseBlock)[VGOFF_(m_esp)];

   if (VG_STACK_MATCHES_BASE(startup_esp, VG_STARTUP_STACK_BASE_1)
       || VG_STACK_MATCHES_BASE(startup_esp, VG_STARTUP_STACK_BASE_2)) {
      /* Jolly good! */
   } else {
      VG_(printf)("%%esp at startup = %p is not near %p or %p; aborting\n", 
                  (void*)startup_esp, 
                  (void*)VG_STARTUP_STACK_BASE_1,
                  (void*)VG_STARTUP_STACK_BASE_2 );
      VG_(panic)("unexpected %esp at startup");
   }

   for (i = 0 /* NB; not 1 */; i < VG_N_THREADS; i++) {
      vg_threads[i].status     = VgTs_Empty;
      vg_threads[i].stack_size = 0;
      vg_threads[i].stack_base = (Addr)NULL;
      vg_threads[i].tid        = i;
      VG_(ksigemptyset)(&vg_threads[i].sig_mask);
      VG_(ksigemptyset)(&vg_threads[i].sigs_waited_for);
   }

   for (i = 0; i < VG_N_WAITING_FDS; i++)
      vg_waiting_fds[i].fd = -1; /* not in use */

   for (i = 0; i < VG_N_THREAD_KEYS; i++) {
      vg_thread_keys[i].inuse      = False;
      vg_thread_keys[i].destructor = NULL;
   }

   /* Assert this is thread zero, which has certain magic
      properties. */
   tid_main = vg_alloc_ThreadState();
   vg_assert(tid_main == 1); 

   vg_threads[tid_main].status        = VgTs_Runnable;
   vg_threads[tid_main].joiner        = VG_INVALID_THREADID;
   vg_threads[tid_main].associated_mx = NULL;
   vg_threads[tid_main].associated_cv = NULL;
   vg_threads[tid_main].retval        = NULL; /* not important */
   for (i = 0; i < VG_N_THREAD_KEYS; i++)
      vg_threads[tid_main].specifics[i] = NULL;

   /* Copy VG_(baseBlock) state to tid_main's slot. */
   vg_tid_currently_in_baseBlock = tid_main;
   VG_(save_thread_state) ( tid_main );

   vg_threads[tid_main].stack_highest_word 
      = vg_threads[tid_main].m_esp /* -4  ??? */;

   /* So now ... */
   vg_assert(vg_tid_currently_in_baseBlock == VG_INVALID_THREADID);
}


/* What if fd isn't a valid fd? */
static
void set_fd_nonblocking ( Int fd )
{
   Int res = VG_(fcntl)( fd, VKI_F_GETFL, 0 );
   vg_assert(!VG_(is_kerror)(res));
   res |= VKI_O_NONBLOCK;
   res = VG_(fcntl)( fd, VKI_F_SETFL, res );
   vg_assert(!VG_(is_kerror)(res));
}

static
void set_fd_blocking ( Int fd )
{
   Int res = VG_(fcntl)( fd, VKI_F_GETFL, 0 );
   vg_assert(!VG_(is_kerror)(res));
   res &= ~VKI_O_NONBLOCK;
   res = VG_(fcntl)( fd, VKI_F_SETFL, res );
   vg_assert(!VG_(is_kerror)(res));
}

static
Bool fd_is_blockful ( Int fd )
{
   Int res = VG_(fcntl)( fd, VKI_F_GETFL, 0 );
   vg_assert(!VG_(is_kerror)(res));
   return (res & VKI_O_NONBLOCK) ? False : True;
}



/* Possibly do a for tid.  Return values are:

   True = request done.  Thread may or may not be still runnable;
   caller must check.  If it is still runnable, the result will be in
   the thread's %EDX as expected.

   False = request not done.  A more capable but slower mechanism will
   deal with it.  
*/
static
Bool maybe_do_trivial_clientreq ( ThreadId tid )
{
#  define SIMPLE_RETURN(vvv)                      \
       { tst->m_edx = (vvv);                      \
         tst->sh_edx = VGM_WORD_VALID;            \
         return True;                             \
       }

   ThreadState* tst    = &vg_threads[tid];
   UInt*        arg    = (UInt*)(tst->m_eax);
   UInt         req_no = arg[0];

   /* VG_(printf)("req no = 0x%x\n", req_no); */
   switch (req_no) {
      case VG_USERREQ__MALLOC:
         SIMPLE_RETURN(
            (UInt)VG_(client_malloc) ( tst, arg[1], Vg_AllocMalloc ) 
         );
      case VG_USERREQ__BUILTIN_NEW:
         SIMPLE_RETURN(
            (UInt)VG_(client_malloc) ( tst, arg[1], Vg_AllocNew )
         );
      case VG_USERREQ__BUILTIN_VEC_NEW:
         SIMPLE_RETURN(
            (UInt)VG_(client_malloc) ( tst, arg[1], Vg_AllocNewVec )
         );
      case VG_USERREQ__FREE:
         VG_(client_free) ( tst, (void*)arg[1], Vg_AllocMalloc );
	 SIMPLE_RETURN(0); /* irrelevant */
      case VG_USERREQ__BUILTIN_DELETE:
         VG_(client_free) ( tst, (void*)arg[1], Vg_AllocNew );
	 SIMPLE_RETURN(0); /* irrelevant */
      case VG_USERREQ__BUILTIN_VEC_DELETE:
         VG_(client_free) ( tst, (void*)arg[1], Vg_AllocNewVec );
	 SIMPLE_RETURN(0); /* irrelevant */
      case VG_USERREQ__CALLOC:
         SIMPLE_RETURN(
            (UInt)VG_(client_calloc) ( tst, arg[1], arg[2] )
         );
      case VG_USERREQ__REALLOC:
         SIMPLE_RETURN(
            (UInt)VG_(client_realloc) ( tst, (void*)arg[1], arg[2] )
         );
      case VG_USERREQ__MEMALIGN:
         SIMPLE_RETURN(
            (UInt)VG_(client_memalign) ( tst, arg[1], arg[2] )
         );

      /* These are heavily used -- or at least we want them to be
         cheap. */
      case VG_USERREQ__PTHREAD_GET_THREADID:
         SIMPLE_RETURN(tid);
      case VG_USERREQ__RUNNING_ON_VALGRIND:
         SIMPLE_RETURN(1);
      case VG_USERREQ__GET_PTHREAD_TRACE_LEVEL:
         SIMPLE_RETURN(VG_(clo_trace_pthread_level));
      case VG_USERREQ__READ_MILLISECOND_TIMER:
         SIMPLE_RETURN(VG_(read_millisecond_timer)());

      case VG_USERREQ__PTHREAD_MUTEX_UNLOCK:
         do_pthread_mutex_unlock( tid, (void *)(arg[1]) );
         return True;

      /* This may make thread tid non-runnable, but the scheduler
         checks for that on return from this function. */
      case VG_USERREQ__PTHREAD_MUTEX_LOCK:
         do_pthread_mutex_lock( tid, False, (void *)(arg[1]) );
         return True;

      case VG_USERREQ__PTHREAD_MUTEX_TRYLOCK:
         do_pthread_mutex_lock( tid, True, (void *)(arg[1]) );
         return True;

      case VG_USERREQ__PTHREAD_GETSPECIFIC:
 	 do_pthread_getspecific ( tid, (UInt)(arg[1]) );
 	 return True;

      default:
         /* Too hard; wimp out. */
         return False;
   }
#  undef SIMPLE_RETURN
}


/* vthread tid is returning from a signal handler; modify its
   stack/regs accordingly. */

/* [Helper fn for handle_signal_return] tid, assumed to be in WaitFD
   for read or write, has been interrupted by a signal.  Find and
   clear the relevant vg_waiting_fd[] entry.  Most of the code in this
   procedure is total paranoia, if you look closely. */
static
void cleanup_waiting_fd_table ( ThreadId tid )
{
   Int  i, waiters;

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(vg_threads[tid].status == VgTs_WaitFD);
   vg_assert(vg_threads[tid].m_eax == __NR_read 
             || vg_threads[tid].m_eax == __NR_write);

   /* Excessively paranoidly ... find the fd this op was waiting
      for, and mark it as not being waited on. */
   waiters = 0;
   for (i = 0; i < VG_N_WAITING_FDS; i++) {
      if (vg_waiting_fds[i].tid == tid) {
         waiters++;
         vg_assert(vg_waiting_fds[i].syscall_no == vg_threads[tid].m_eax);
      }
   }
   vg_assert(waiters == 1);
   for (i = 0; i < VG_N_WAITING_FDS; i++)
      if (vg_waiting_fds[i].tid == tid)
         break;
   vg_assert(i < VG_N_WAITING_FDS);
   vg_assert(vg_waiting_fds[i].fd != -1);
   vg_waiting_fds[i].fd = -1; /* not in use */
}


static
void handle_signal_return ( ThreadId tid )
{
   Char msg_buf[100];
   Bool restart_blocked_syscalls;

   vg_assert(VG_(is_valid_tid)(tid));

   restart_blocked_syscalls = VG_(signal_returns)(tid);

   if (restart_blocked_syscalls)
      /* Easy; we don't have to do anything. */
      return;

   if (vg_threads[tid].status == VgTs_WaitFD
       && (vg_threads[tid].m_eax == __NR_read 
           || vg_threads[tid].m_eax == __NR_write)) {
      /* read() or write() interrupted.  Force a return with EINTR. */
      cleanup_waiting_fd_table(tid);
      vg_threads[tid].m_eax = -VKI_EINTR;
      vg_threads[tid].status = VgTs_Runnable;

      if (VG_(clo_trace_sched)) {
         VG_(sprintf)(msg_buf, 
            "read() / write() interrupted by signal; return EINTR" );
         print_sched_event(tid, msg_buf);
      }
      return;
   }

   if (vg_threads[tid].status == VgTs_WaitFD
       && vg_threads[tid].m_eax == __NR_nanosleep) {
      /* We interrupted a nanosleep().  The right thing to do is to
         write the unused time to nanosleep's second param and return
         EINTR, but I'm too lazy for that. */
      return;
   }

   if (vg_threads[tid].status == VgTs_WaitFD) {
      VG_(panic)("handle_signal_return: unknown interrupted syscall");
   }

   /* All other cases?  Just return. */
}


static
void sched_do_syscall ( ThreadId tid )
{
   UInt saved_eax;
   UInt res, syscall_no;
   UInt fd;
   Bool orig_fd_blockness;
   Char msg_buf[100];

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(vg_threads[tid].status == VgTs_Runnable);

   syscall_no = vg_threads[tid].m_eax; /* syscall number */

   if (syscall_no == __NR_nanosleep) {
      UInt t_now, t_awaken;
      struct vki_timespec* req;
      req = (struct vki_timespec*)vg_threads[tid].m_ebx; /* arg1 */
      t_now = VG_(read_millisecond_timer)();     
      t_awaken 
         = t_now
           + (UInt)1000ULL * (UInt)(req->tv_sec) 
           + (UInt)(req->tv_nsec) / 1000000;
      vg_threads[tid].status    = VgTs_Sleeping;
      vg_threads[tid].awaken_at = t_awaken;
      if (VG_(clo_trace_sched)) {
         VG_(sprintf)(msg_buf, "at %d: nanosleep for %d", 
                               t_now, t_awaken-t_now);
	 print_sched_event(tid, msg_buf);
      }
      /* Force the scheduler to run something else for a while. */
      return;
   }

   if (syscall_no != __NR_read && syscall_no != __NR_write) {
      /* We think it's non-blocking.  Just do it in the normal way. */
      VG_(perform_assumed_nonblocking_syscall)(tid);
      /* The thread is still runnable. */
      return;
   }

   /* Set the fd to nonblocking, and do the syscall, which will return
      immediately, in order to lodge a request with the Linux kernel.
      We later poll for I/O completion using select().  */

   fd = vg_threads[tid].m_ebx /* arg1 */;
   orig_fd_blockness = fd_is_blockful(fd);
   set_fd_nonblocking(fd);
   vg_assert(!fd_is_blockful(fd));
   VG_(check_known_blocking_syscall)(tid, syscall_no, NULL /* PRE */);

   /* This trashes the thread's %eax; we have to preserve it. */
   saved_eax = vg_threads[tid].m_eax;
   KERNEL_DO_SYSCALL(tid,res);

   /* Restore original blockfulness of the fd. */
   if (orig_fd_blockness)
      set_fd_blocking(fd);
   else
      set_fd_nonblocking(fd);

   if (res != -VKI_EWOULDBLOCK || !orig_fd_blockness) {
      /* Finish off in the normal way.  Don't restore %EAX, since that
         now (correctly) holds the result of the call.  We get here if either:
         1.  The call didn't block, or
         2.  The fd was already in nonblocking mode before we started to
             mess with it.  In this case, we're not expecting to handle 
             the I/O completion -- the client is.  So don't file a 
             completion-wait entry. 
      */
      VG_(check_known_blocking_syscall)(tid, syscall_no, &res /* POST */);
      /* We're still runnable. */
      vg_assert(vg_threads[tid].status == VgTs_Runnable);

   } else {

      vg_assert(res == -VKI_EWOULDBLOCK && orig_fd_blockness);

      /* It would have blocked.  First, restore %EAX to what it was
         before our speculative call. */
      vg_threads[tid].m_eax = saved_eax;
      /* Put this fd in a table of fds on which we are waiting for
         completion. The arguments for select() later are constructed
         from this table.  */
      add_waiting_fd(tid, fd, saved_eax /* which holds the syscall # */);
      /* Deschedule thread until an I/O completion happens. */
      vg_threads[tid].status = VgTs_WaitFD;
      if (VG_(clo_trace_sched)) {
         VG_(sprintf)(msg_buf,"block until I/O ready on fd %d", fd);
	 print_sched_event(tid, msg_buf);
      }

   }
}


/* Find out which of the fds in vg_waiting_fds are now ready to go, by
   making enquiries with select(), and mark them as ready.  We have to
   wait for the requesting threads to fall into the the WaitFD state
   before we can actually finally deliver the results, so this
   procedure doesn't do that; complete_blocked_syscalls() does it.

   It might seem odd that a thread which has done a blocking syscall
   is not in WaitFD state; the way this can happen is if it initially
   becomes WaitFD, but then a signal is delivered to it, so it becomes
   Runnable for a while.  In this case we have to wait for the
   sighandler to return, whereupon the WaitFD state is resumed, and
   only at that point can the I/O result be delivered to it.  However,
   this point may be long after the fd is actually ready.  

   So, poll_for_ready_fds() merely detects fds which are ready.
   complete_blocked_syscalls() does the second half of the trick,
   possibly much later: it delivers the results from ready fds to
   threads in WaitFD state. 
*/
static
void poll_for_ready_fds ( void )
{
   vki_ksigset_t      saved_procmask;
   vki_fd_set         readfds;
   vki_fd_set         writefds;
   vki_fd_set         exceptfds;
   struct vki_timeval timeout;
   Int                fd, fd_max, i, n_ready, syscall_no, n_ok;
   ThreadId           tid;
   Bool               rd_ok, wr_ok, ex_ok;
   Char               msg_buf[100];

   struct vki_timespec* rem;
   UInt                 t_now;

   /* Awaken any sleeping threads whose sleep has expired. */
   for (tid = 1; tid < VG_N_THREADS; tid++)
      if (vg_threads[tid].status == VgTs_Sleeping)
         break;

   /* Avoid pointless calls to VG_(read_millisecond_timer). */
   if (tid < VG_N_THREADS) {
      t_now = VG_(read_millisecond_timer)();
      for (tid = 1; tid < VG_N_THREADS; tid++) {
         if (vg_threads[tid].status != VgTs_Sleeping)
            continue;
         if (t_now >= vg_threads[tid].awaken_at) {
            /* Resume this thread.  Set to zero the remaining-time
               (second) arg of nanosleep, since it's used up all its
               time. */
            vg_assert(vg_threads[tid].m_eax == __NR_nanosleep);
            rem = (struct vki_timespec *)vg_threads[tid].m_ecx; /* arg2 */
            if (rem != NULL) {
	       rem->tv_sec = 0;
               rem->tv_nsec = 0;
            }
            /* Make the syscall return 0 (success). */
            vg_threads[tid].m_eax = 0;
	    /* Reschedule this thread. */
            vg_threads[tid].status = VgTs_Runnable;
            if (VG_(clo_trace_sched)) {
               VG_(sprintf)(msg_buf, "at %d: nanosleep done", 
                                     t_now);
               print_sched_event(tid, msg_buf);
            }
         }
      }
   }

   /* And look for threads waiting on file descriptors which are now
      ready for I/O.*/
   timeout.tv_sec = 0;
   timeout.tv_usec = 0;

   VKI_FD_ZERO(&readfds);
   VKI_FD_ZERO(&writefds);
   VKI_FD_ZERO(&exceptfds);
   fd_max = -1;
   for (i = 0; i < VG_N_WAITING_FDS; i++) {
      if (vg_waiting_fds[i].fd == -1 /* not in use */) 
         continue;
      if (vg_waiting_fds[i].ready /* already ready? */) 
         continue;
      fd = vg_waiting_fds[i].fd;
      /* VG_(printf)("adding QUERY for fd %d\n", fd); */
      vg_assert(fd >= 0);
      if (fd > fd_max) 
         fd_max = fd;
      tid = vg_waiting_fds[i].tid;
      vg_assert(VG_(is_valid_tid)(tid));
      syscall_no = vg_waiting_fds[i].syscall_no;
      switch (syscall_no) {
         case __NR_read:
            /* In order to catch timeout events on fds which are
               readable and which have been ioctl(TCSETA)'d with a
               VTIMEout, we appear to need to ask if the fd is
               writable, for some reason.  Ask me not why.  Since this
               is strange and potentially troublesome we only do it if
               the user asks specially. */
            if (VG_(strstr)(VG_(clo_weird_hacks), "ioctl-VTIME") != NULL)
               VKI_FD_SET(fd, &writefds);
            VKI_FD_SET(fd, &readfds); break;
         case __NR_write: 
            VKI_FD_SET(fd, &writefds); break;
         default: 
            VG_(panic)("poll_for_ready_fds: unexpected syscall");
            /*NOTREACHED*/
            break;
      }
   }

   /* Short cut: if no fds are waiting, give up now. */
   if (fd_max == -1)
      return;

   /* BLOCK ALL SIGNALS.  We don't want the complication of select()
      getting interrupted. */
   VG_(block_all_host_signals)( &saved_procmask );

   n_ready = VG_(select)
                ( fd_max+1, &readfds, &writefds, &exceptfds, &timeout);
   if (VG_(is_kerror)(n_ready)) {
      VG_(printf)("poll_for_ready_fds: select returned %d\n", n_ready);
      VG_(panic)("poll_for_ready_fds: select failed?!");
      /*NOTREACHED*/
   }
   
   /* UNBLOCK ALL SIGNALS */
   VG_(restore_host_signals)( &saved_procmask );

   /* VG_(printf)("poll_for_io_completions: %d fs ready\n", n_ready); */

   if (n_ready == 0)
      return;   

   /* Inspect all the fds we know about, and handle any completions that
      have happened. */
   /*
   VG_(printf)("\n\n");
   for (fd = 0; fd < 100; fd++)
     if (VKI_FD_ISSET(fd, &writefds) || VKI_FD_ISSET(fd, &readfds)) {
       VG_(printf)("X"); } else { VG_(printf)("."); };
   VG_(printf)("\n\nfd_max = %d\n", fd_max);
   */

   for (fd = 0; fd <= fd_max; fd++) {
      rd_ok = VKI_FD_ISSET(fd, &readfds);
      wr_ok = VKI_FD_ISSET(fd, &writefds);
      ex_ok = VKI_FD_ISSET(fd, &exceptfds);

      n_ok = (rd_ok ? 1 : 0) + (wr_ok ? 1 : 0) + (ex_ok ? 1 : 0);
      if (n_ok == 0) 
         continue;
      if (n_ok > 1) {
         VG_(printf)("offending fd = %d\n", fd);
         VG_(panic)("poll_for_ready_fds: multiple events on fd");
      }
      
      /* An I/O event completed for fd.  Find the thread which
         requested this. */
      for (i = 0; i < VG_N_WAITING_FDS; i++) {
         if (vg_waiting_fds[i].fd == -1 /* not in use */) 
            continue;
         if (vg_waiting_fds[i].fd == fd) 
            break;
      }

      /* And a bit more paranoia ... */
      vg_assert(i >= 0 && i < VG_N_WAITING_FDS);

      /* Mark the fd as ready. */      
      vg_assert(! vg_waiting_fds[i].ready);
      vg_waiting_fds[i].ready = True;
   }
}


/* See comment attached to poll_for_ready_fds() for explaination. */
static
void complete_blocked_syscalls ( void )
{
   Int      fd, i, res, syscall_no;
   ThreadId tid;
   Char     msg_buf[100];

   /* Inspect all the outstanding fds we know about. */

   for (i = 0; i < VG_N_WAITING_FDS; i++) {
      if (vg_waiting_fds[i].fd == -1 /* not in use */) 
         continue;
      if (! vg_waiting_fds[i].ready)
         continue;

      fd  = vg_waiting_fds[i].fd;
      tid = vg_waiting_fds[i].tid;
      vg_assert(VG_(is_valid_tid)(tid));

      /* The thread actually has to be waiting for the I/O event it
         requested before we can deliver the result! */
      if (vg_threads[tid].status != VgTs_WaitFD)
         continue;

      /* Ok, actually do it!  We can safely use %EAX as the syscall
         number, because the speculative call made by
         sched_do_syscall() doesn't change %EAX in the case where the
         call would have blocked. */

      syscall_no = vg_waiting_fds[i].syscall_no;
      vg_assert(syscall_no == vg_threads[tid].m_eax);
      KERNEL_DO_SYSCALL(tid,res);
      VG_(check_known_blocking_syscall)(tid, syscall_no, &res /* POST */);

      /* Reschedule. */
      vg_threads[tid].status = VgTs_Runnable;
      /* Mark slot as no longer in use. */
      vg_waiting_fds[i].fd = -1;
      /* pp_sched_status(); */
      if (VG_(clo_trace_sched)) {
         VG_(sprintf)(msg_buf,"resume due to I/O completion on fd %d", fd);
	 print_sched_event(tid, msg_buf);
      }
   }
}


static
void check_for_pthread_cond_timedwait ( void )
{
   Int i, now;
   for (i = 1; i < VG_N_THREADS; i++) {
      if (vg_threads[i].status != VgTs_WaitCV)
         continue;
      if (vg_threads[i].awaken_at == 0xFFFFFFFF /* no timeout */)
         continue;
      now = VG_(read_millisecond_timer)();
      if (now >= vg_threads[i].awaken_at) {
         do_pthread_cond_timedwait_TIMEOUT(i);
      }
   }
}


static
void nanosleep_for_a_while ( void )
{
   Int res;
   struct vki_timespec req;
   struct vki_timespec rem;
   req.tv_sec = 0;
   req.tv_nsec = 20 * 1000 * 1000;
   res = VG_(nanosleep)( &req, &rem );   
   vg_assert(res == 0 /* ok */ || res == 1 /* interrupted by signal */);
}


/* ---------------------------------------------------------------------
   The scheduler proper.
   ------------------------------------------------------------------ */

/* Run user-space threads until either
   * Deadlock occurs
   * One thread asks to shutdown Valgrind
   * The specified number of basic blocks has gone by.
*/
VgSchedReturnCode VG_(scheduler) ( void )
{
   ThreadId tid, tid_next;
   UInt     trc;
   UInt     dispatch_ctr_SAVED;
   Int      request_code, done_this_time, n_in_bounded_wait;
   Char     msg_buf[100];
   Addr     trans_addr;
   Bool     sigs_delivered;

   /* For the LRU structures, records when the epoch began. */
   ULong lru_epoch_started_at = 0;

   /* Start with the root thread.  tid in general indicates the
      currently runnable/just-finished-running thread. */
   VG_(last_run_tid) = tid = 1;

   /* This is the top level scheduler loop.  It falls into three
      phases. */
   while (True) {

      /* ======================= Phase 0 of 3 =======================
	 Be paranoid.  Always a good idea. */
     stage1:
      scheduler_sanity();
      VG_(do_sanity_checks)( False );

      /* ======================= Phase 1 of 3 =======================
         Handle I/O completions and signals.  This may change the
         status of various threads.  Then select a new thread to run,
         or declare deadlock, or sleep if there are no runnable
         threads but some are blocked on I/O.  */

      /* Age the LRU structures if an epoch has been completed. */
      if (VG_(bbs_done) - lru_epoch_started_at >= VG_BBS_PER_EPOCH) {
         lru_epoch_started_at = VG_(bbs_done);
         increment_epoch();
      }

      /* Was a debug-stop requested? */
      if (VG_(bbs_to_go) == 0) 
         goto debug_stop;

      /* Do the following loop until a runnable thread is found, or
         deadlock is detected. */
      while (True) {

         /* For stats purposes only. */
         VG_(num_scheduling_events_MAJOR) ++;

         /* See if any I/O operations which we were waiting for have
            completed, and, if so, make runnable the relevant waiting
            threads. */
         poll_for_ready_fds();
         complete_blocked_syscalls();
         check_for_pthread_cond_timedwait();

         /* See if there are any signals which need to be delivered.  If
            so, choose thread(s) to deliver them to, and build signal
            delivery frames on those thread(s) stacks. */

	 /* Be careful about delivering signals to a thread waiting
            for a mutex.  In particular, when the handler is running,
            that thread is temporarily apparently-not-waiting for the
            mutex, so if it is unlocked by another thread whilst the
            handler is running, this thread is not informed.  When the
            handler returns, the thread resumes waiting on the mutex,
            even if, as a result, it has missed the unlocking of it.
            Potential deadlock.  This sounds all very strange, but the
            POSIX standard appears to require this behaviour.  */
         sigs_delivered = VG_(deliver_signals)();
	 if (sigs_delivered)
            VG_(do_sanity_checks)( False );

         /* Try and find a thread (tid) to run. */
         tid_next = tid;
         n_in_bounded_wait = 0;
         while (True) {
            tid_next++;
            if (tid_next >= VG_N_THREADS) tid_next = 1;
            if (vg_threads[tid_next].status == VgTs_WaitFD
                || vg_threads[tid_next].status == VgTs_Sleeping
                || vg_threads[tid_next].status == VgTs_WaitSIG
                || (vg_threads[tid_next].status == VgTs_WaitCV 
                    && vg_threads[tid_next].awaken_at != 0xFFFFFFFF))
               n_in_bounded_wait ++;
            if (vg_threads[tid_next].status == VgTs_Runnable) 
               break; /* We can run this one. */
            if (tid_next == tid) 
               break; /* been all the way round */
         }
         tid = tid_next;
       
         if (vg_threads[tid].status == VgTs_Runnable) {
            /* Found a suitable candidate.  Fall out of this loop, so
               we can advance to stage 2 of the scheduler: actually
               running the thread. */
            break;
	 }

         /* We didn't find a runnable thread.  Now what? */
         if (n_in_bounded_wait == 0) {
            /* No runnable threads and no prospect of any appearing
               even if we wait for an arbitrary length of time.  In
               short, we have a deadlock. */
	    VG_(pp_sched_status)();
            return VgSrc_Deadlock;
         }

         /* At least one thread is in a fd-wait state.  Delay for a
            while, and go round again, in the hope that eventually a
            thread becomes runnable. */
         nanosleep_for_a_while();
	 /* pp_sched_status(); */
	 /* VG_(printf)("."); */
      }


      /* ======================= Phase 2 of 3 =======================
         Wahey!  We've finally decided that thread tid is runnable, so
         we now do that.  Run it for as much of a quanta as possible.
         Trivial requests are handled and the thread continues.  The
         aim is not to do too many of Phase 1 since it is expensive.  */

      if (0)
         VG_(printf)("SCHED: tid %d\n", tid);

      /* Figure out how many bbs to ask vg_run_innerloop to do.  Note
         that it decrements the counter before testing it for zero, so
         that if VG_(dispatch_ctr) is set to N you get at most N-1
         iterations.  Also this means that VG_(dispatch_ctr) must
         exceed zero before entering the innerloop.  Also also, the
         decrement is done before the bb is actually run, so you
         always get at least one decrement even if nothing happens.
      */
      if (VG_(bbs_to_go) >= VG_SCHEDULING_QUANTUM)
         VG_(dispatch_ctr) = VG_SCHEDULING_QUANTUM + 1;
      else
         VG_(dispatch_ctr) = (UInt)VG_(bbs_to_go) + 1;

      /* ... and remember what we asked for. */
      dispatch_ctr_SAVED = VG_(dispatch_ctr);

      /* paranoia ... */
      vg_assert(vg_threads[tid].tid == tid);

      /* Actually run thread tid. */
      while (True) {

         VG_(last_run_tid) = tid;

         /* For stats purposes only. */
         VG_(num_scheduling_events_MINOR) ++;

         if (0)
            VG_(message)(Vg_DebugMsg, "thread %d: running for %d bbs", 
                                      tid, VG_(dispatch_ctr) - 1 );
#        if 0
         if (VG_(bbs_done) > 31700000 + 0) {
            dispatch_ctr_SAVED = VG_(dispatch_ctr) = 2;
            VG_(translate)(&vg_threads[tid], vg_threads[tid].m_eip,
                           NULL,NULL,NULL);
         }
         vg_assert(vg_threads[tid].m_eip != 0);
#        endif

         trc = run_thread_for_a_while ( tid );

#        if 0
         if (0 == vg_threads[tid].m_eip) {
            VG_(printf)("tid = %d,  dc = %llu\n", tid, VG_(bbs_done));
            vg_assert(0 != vg_threads[tid].m_eip);
         }
#        endif

         /* Deal quickly with trivial scheduling events, and resume the
            thread. */

         if (trc == VG_TRC_INNER_FASTMISS) {
            vg_assert(VG_(dispatch_ctr) > 0);

            /* Trivial event.  Miss in the fast-cache.  Do a full
               lookup for it. */
            trans_addr 
               = VG_(search_transtab) ( vg_threads[tid].m_eip );
            if (trans_addr == (Addr)0) {
               /* Not found; we need to request a translation. */
               create_translation_for( tid, vg_threads[tid].m_eip ); 
               trans_addr = VG_(search_transtab) ( vg_threads[tid].m_eip ); 
               if (trans_addr == (Addr)0)
                  VG_(panic)("VG_TRC_INNER_FASTMISS: missing tt_fast entry");
            }
            continue; /* with this thread */
         }

         if (trc == VG_TRC_EBP_JMP_CLIENTREQ) {
            Bool done; 
            /* VG_(printf)("request 0x%x\n", 
                           *(UInt*)(vg_threads[tid].m_eax)); */
            done = maybe_do_trivial_clientreq(tid);
            if (done) {
               /* The request is done.  We try and continue with the
                  same thread if still runnable.  If not, go back to
                  Stage 1 to select a new thread to run. */
               if (vg_threads[tid].status == VgTs_Runnable)
                  continue; /* with this thread */
               else
                  goto stage1;
	    }
	 }

         if (trc == VG_TRC_EBP_JMP_SYSCALL) {
            /* Do a syscall for the vthread tid.  This could cause it
               to become non-runnable.  One special case: spot the
               client doing calls to exit() and take this as the cue
               to exit. */
#           if 0
            { UInt* esp; Int i;
              esp=(UInt*)vg_threads[tid].m_esp;
              VG_(printf)("\nBEFORE\n");
              for (i = 10; i >= -10; i--)
                 VG_(printf)("%2d  %p  =  0x%x\n", i, &esp[i], esp[i]);
            }
#           endif

            if (vg_threads[tid].m_eax == __NR_exit)
               return VgSrc_ExitSyscall;

            sched_do_syscall(tid);

#           if 0
            { UInt* esp; Int i;
              esp=(UInt*)vg_threads[tid].m_esp;
              VG_(printf)("AFTER\n");
              for (i = 10; i >= -10; i--)
                 VG_(printf)("%2d  %p  =  0x%x\n", i, &esp[i], esp[i]);
            }
#           endif

            if (vg_threads[tid].status == VgTs_Runnable)
               continue; /* with this thread */
            else
               goto stage1;          
	 }

	 /* It's an event we can't quickly deal with.  Give up running
            this thread and handle things the expensive way. */
	 break;
      }

      /* ======================= Phase 3 of 3 =======================
         Handle non-trivial thread requests, mostly pthread stuff. */

      /* Ok, we've fallen out of the dispatcher for a
         non-completely-trivial reason. First, update basic-block
         counters. */

      done_this_time = (Int)dispatch_ctr_SAVED - (Int)VG_(dispatch_ctr) - 1;
      vg_assert(done_this_time >= 0);
      VG_(bbs_to_go)   -= (ULong)done_this_time;
      VG_(bbs_done)    += (ULong)done_this_time;

      if (0 && trc != VG_TRC_INNER_FASTMISS)
         VG_(message)(Vg_DebugMsg, "thread %d:   completed %d bbs, trc %d", 
                                   tid, done_this_time, (Int)trc );

      if (0 && trc != VG_TRC_INNER_FASTMISS)
         VG_(message)(Vg_DebugMsg, "thread %d:  %ld bbs, event %s", 
                                   tid, VG_(bbs_done),
                                   name_of_sched_event(trc) );

      /* Examine the thread's return code to figure out why it
         stopped, and handle requests. */

      switch (trc) {

         case VG_TRC_INNER_FASTMISS:
            VG_(panic)("VG_(scheduler):  VG_TRC_INNER_FASTMISS");
            /*NOTREACHED*/
            break;

         case VG_TRC_INNER_COUNTERZERO:
            /* Timeslice is out.  Let a new thread be scheduled,
               simply by doing nothing, causing us to arrive back at
               Phase 1. */
            if (VG_(bbs_to_go) == 0) {
               goto debug_stop;
            }
            vg_assert(VG_(dispatch_ctr) == 0);
            break;

         case VG_TRC_UNRESUMABLE_SIGNAL:
            /* It got a SIGSEGV/SIGBUS, which we need to deliver right
               away.  Again, do nothing, so we wind up back at Phase
               1, whereupon the signal will be "delivered". */
	    break;

         case VG_TRC_EBP_JMP_CLIENTREQ: 
            /* Do a client request for the vthread tid.  Note that
               some requests will have been handled by
               maybe_do_trivial_clientreq(), so we don't expect to see
               those here. 
            */
            /* The thread's %EAX points at an arg block, the first
               word of which is the request code. */
            request_code = ((UInt*)(vg_threads[tid].m_eax))[0];
            if (0) {
               VG_(sprintf)(msg_buf, "request 0x%x", request_code );
               print_sched_event(tid, msg_buf);
	    }
	    /* Do a non-trivial client request for thread tid.  tid's
               %EAX points to a short vector of argument words, the
               first of which is the request code.  The result of the
               request is put in tid's %EDX.  Alternatively, perhaps
               the request causes tid to become non-runnable and/or
               other blocked threads become runnable.  In general we
               can and often do mess with the state of arbitrary
               threads at this point. */
            do_nontrivial_clientreq(tid);
            break;

         default: 
            VG_(printf)("\ntrc = %d\n", trc);
            VG_(panic)("VG_(scheduler), phase 3: "
                       "unexpected thread return code");
            /* NOTREACHED */
            break;

      } /* switch (trc) */

      /* That completes Phase 3 of 3.  Return now to the top of the
	 main scheduler loop, to Phase 1 of 3. */

   } /* top-level scheduler loop */


   /* NOTREACHED */
   VG_(panic)("scheduler: post-main-loop ?!");
   /* NOTREACHED */

  debug_stop:
   /* If we exited because of a debug stop, print the translation 
      of the last block executed -- by translating it again, and 
      throwing away the result. */
   VG_(printf)(
      "======vvvvvvvv====== LAST TRANSLATION ======vvvvvvvv======\n");
   VG_(translate)( &vg_threads[tid], vg_threads[tid].m_eip, NULL, NULL, NULL );
   VG_(printf)("\n");
   VG_(printf)(
      "======^^^^^^^^====== LAST TRANSLATION ======^^^^^^^^======\n");

   return VgSrc_BbsDone;
}


/* ---------------------------------------------------------------------
   The pthread implementation.
   ------------------------------------------------------------------ */

#include <pthread.h>
#include <errno.h>

#define VG_PTHREAD_STACK_MIN \
   (VG_PTHREAD_STACK_SIZE - VG_AR_CLIENT_STACKBASE_REDZONE_SZB)

/*  /usr/include/bits/pthreadtypes.h:
    typedef unsigned long int pthread_t;
*/

/* Write a value to the client's %EDX (request return value register)
   and set the shadow to indicate it is defined. */
#define SET_EDX(zztid, zzval)                          \
   do { vg_threads[zztid].m_edx = (zzval);             \
        vg_threads[zztid].sh_edx = VGM_WORD_VALID;     \
   } while (0)


/* -----------------------------------------------------------
   Thread CREATION, JOINAGE and CANCELLATION.
   -------------------------------------------------------- */

/* Release resources and generally clean up once a thread has finally
   disappeared. */
static
void cleanup_after_thread_exited ( ThreadId tid )
{
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(vg_threads[tid].status == VgTs_Empty);
   /* Mark its stack no-access */
   if (VG_(clo_instrument) && tid != 1)
      VGM_(make_noaccess)( vg_threads[tid].stack_base,
                           vg_threads[tid].stack_size );
   /* Forget about any pending signals directed specifically at this
      thread. */
   VG_(notify_signal_machinery_of_thread_exit)( tid );

   /* Get rid of signal handlers specifically arranged for this
      thread. */
   VG_(update_sigstate_following_WaitSIG_change)();
}


static
void do_pthread_cancel ( ThreadId  tid,
                         pthread_t tid_cancellee )
{
   Char msg_buf[100];

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(vg_threads[tid].status != VgTs_Empty);

   if (!VG_(is_valid_tid)(tid_cancellee)
       || vg_threads[tid_cancellee].status == VgTs_Empty) {
      SET_EDX(tid, ESRCH);
      return;
   }

   /* We want make is appear that this thread has returned to
      do_pthread_create_bogusRA with PTHREAD_CANCELED as the
      return value.  So: simple: put PTHREAD_CANCELED into %EAX
      and &do_pthread_create_bogusRA into %EIP and keep going! */
   if (VG_(clo_trace_sched)) {
      VG_(sprintf)(msg_buf, "cancelled by %d", tid);
      print_sched_event(tid_cancellee, msg_buf);
   }
   vg_threads[tid_cancellee].m_eax  = (UInt)PTHREAD_CANCELED;
   vg_threads[tid_cancellee].m_eip  = (UInt)&VG_(pthreadreturn_bogusRA);
   vg_threads[tid_cancellee].status = VgTs_Runnable;

   /* We return with success (0). */
   SET_EDX(tid, 0);
}


static
void do_pthread_exit ( ThreadId tid, void* retval )
{
   Char msg_buf[100];
   /* We want make is appear that this thread has returned to
      do_pthread_create_bogusRA with retval as the
      return value.  So: simple: put retval into %EAX
      and &do_pthread_create_bogusRA into %EIP and keep going! */
   if (VG_(clo_trace_sched)) {
      VG_(sprintf)(msg_buf, "exiting with %p", retval);
      print_sched_event(tid, msg_buf);
   }
   vg_threads[tid].m_eax  = (UInt)retval;
   vg_threads[tid].m_eip  = (UInt)&VG_(pthreadreturn_bogusRA);
   vg_threads[tid].status = VgTs_Runnable;
}


/* Thread tid is exiting, by returning from the function it was
   created with.  Or possibly due to pthread_exit or cancellation.
   The main complication here is to resume any thread waiting to join
   with this one. */
static 
void handle_pthread_return ( ThreadId tid, void* retval )
{
   ThreadId jnr; /* joiner, the thread calling pthread_join. */
   UInt*    jnr_args;
   void**   jnr_thread_return;
   Char     msg_buf[100];

   /* Mark it as not in use.  Leave the stack in place so the next
      user of this slot doesn't reallocate it. */
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(vg_threads[tid].status != VgTs_Empty);

   vg_threads[tid].retval = retval;

   if (vg_threads[tid].joiner == VG_INVALID_THREADID) {
      /* No one has yet done a join on me */
      vg_threads[tid].status = VgTs_WaitJoiner;
      if (VG_(clo_trace_sched)) {
         VG_(sprintf)(msg_buf, 
            "root fn returns, waiting for a call pthread_join(%d)", 
            tid);
         print_sched_event(tid, msg_buf);
      }
   } else {
      /* Some is waiting; make their join call return with success,
         putting my exit code in the place specified by the caller's
         thread_return param.  This is all very horrible, since we
         need to consult the joiner's arg block -- pointed to by its
         %EAX -- in order to extract the 2nd param of its pthread_join
         call.  TODO: free properly the slot (also below). 
      */
      jnr = vg_threads[tid].joiner;
      vg_assert(VG_(is_valid_tid)(jnr));
      vg_assert(vg_threads[jnr].status == VgTs_WaitJoinee);
      jnr_args = (UInt*)vg_threads[jnr].m_eax;
      jnr_thread_return = (void**)(jnr_args[2]);
      if (jnr_thread_return != NULL)
         *jnr_thread_return = vg_threads[tid].retval;
      SET_EDX(jnr, 0); /* success */
      vg_threads[jnr].status = VgTs_Runnable;
      vg_threads[tid].status = VgTs_Empty; /* bye! */
      cleanup_after_thread_exited ( tid );
      if (VG_(clo_trace_sched)) {
         VG_(sprintf)(msg_buf, 
            "root fn returns, to find a waiting pthread_join(%d)", tid);
         print_sched_event(tid, msg_buf);
         VG_(sprintf)(msg_buf, 
            "my pthread_join(%d) returned; resuming", tid);
         print_sched_event(jnr, msg_buf);
      }
   }

   /* Return value is irrelevant; this thread will not get
      rescheduled. */
}


static
void do_pthread_join ( ThreadId tid, ThreadId jee, void** thread_return )
{
   Char msg_buf[100];

   /* jee, the joinee, is the thread specified as an arg in thread
      tid's call to pthread_join.  So tid is the join-er. */
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(vg_threads[tid].status == VgTs_Runnable);

   if (jee == tid) {
      SET_EDX(tid, EDEADLK); /* libc constant, not a kernel one */
      vg_threads[tid].status = VgTs_Runnable;
      return;
   }

   if (jee < 0 
       || jee >= VG_N_THREADS
       || vg_threads[jee].status == VgTs_Empty) {
      /* Invalid thread to join to. */
      SET_EDX(tid, EINVAL);
      vg_threads[tid].status = VgTs_Runnable;
      return;
   }

   if (vg_threads[jee].joiner != VG_INVALID_THREADID) {
      /* Someone already did join on this thread */
      SET_EDX(tid, EINVAL);
      vg_threads[tid].status = VgTs_Runnable;
      return;
   }

   /* if (vg_threads[jee].detached) ... */

   /* Perhaps the joinee has already finished?  If so return
      immediately with its return code, and free up the slot. TODO:
      free it properly (also above). */
   if (vg_threads[jee].status == VgTs_WaitJoiner) {
      vg_assert(vg_threads[jee].joiner == VG_INVALID_THREADID);
      SET_EDX(tid, 0); /* success */
      if (thread_return != NULL) {
         *thread_return = vg_threads[jee].retval;
	 /* Not really right, since it makes the thread's return value
            appear to be defined even if it isn't. */
         if (VG_(clo_instrument))
            VGM_(make_readable)( (Addr)thread_return, sizeof(void*) );
      }
      vg_threads[tid].status = VgTs_Runnable;
      vg_threads[jee].status = VgTs_Empty; /* bye! */
      cleanup_after_thread_exited ( jee );
      if (VG_(clo_trace_sched)) {
	 VG_(sprintf)(msg_buf,
		      "someone called pthread_join() on me; bye!");
         print_sched_event(jee, msg_buf);
	 VG_(sprintf)(msg_buf,
            "my pthread_join(%d) returned immediately", 
            jee );
         print_sched_event(tid, msg_buf);
      }
      return;
   }

   /* Ok, so we'll have to wait on jee. */
   vg_threads[jee].joiner = tid;
   vg_threads[tid].status = VgTs_WaitJoinee;
   if (VG_(clo_trace_sched)) {
      VG_(sprintf)(msg_buf,
         "blocking on call of pthread_join(%d)", jee );
      print_sched_event(tid, msg_buf);
   }
   /* So tid's join call does not return just now. */
}


static
void do_pthread_create ( ThreadId parent_tid,
                         pthread_t* thread, 
                         pthread_attr_t* attr, 
                         void* (*start_routine)(void *), 
                         void* arg )
{
   Int      i;
   Addr     new_stack;
   UInt     new_stk_szb;
   ThreadId tid;
   Char     msg_buf[100];

   /* Paranoia ... */
   vg_assert(sizeof(pthread_t) == sizeof(UInt));

   vg_assert(vg_threads[parent_tid].status != VgTs_Empty);

   tid = vg_alloc_ThreadState();

   /* If we've created the main thread's tid, we're in deep trouble :) */
   vg_assert(tid != 1);
   vg_assert(VG_(is_valid_tid)(tid));

   /* Copy the parent's CPU state into the child's, in a roundabout
      way (via baseBlock). */
   VG_(load_thread_state)(parent_tid);
   VG_(save_thread_state)(tid);

   /* Consider allocating the child a stack, if the one it already has
      is inadequate. */
   new_stk_szb = VG_PTHREAD_STACK_MIN;

   if (new_stk_szb > vg_threads[tid].stack_size) {
      /* Again, for good measure :) We definitely don't want to be
         allocating a stack for the main thread. */
      vg_assert(tid != 1);
      /* for now, we don't handle the case of anything other than
         assigning it for the first time. */
      vg_assert(vg_threads[tid].stack_size == 0);
      vg_assert(vg_threads[tid].stack_base == (Addr)NULL);
      new_stack = (Addr)VG_(get_memory_from_mmap)( new_stk_szb );
      vg_threads[tid].stack_base = new_stack;
      vg_threads[tid].stack_size = new_stk_szb;
      vg_threads[tid].stack_highest_word
         = new_stack + new_stk_szb 
                     - VG_AR_CLIENT_STACKBASE_REDZONE_SZB; /* -4  ??? */;
   }

   vg_threads[tid].m_esp 
      = vg_threads[tid].stack_base 
        + vg_threads[tid].stack_size
        - VG_AR_CLIENT_STACKBASE_REDZONE_SZB;

   if (VG_(clo_instrument))
      VGM_(make_noaccess)( vg_threads[tid].m_esp, 
                           VG_AR_CLIENT_STACKBASE_REDZONE_SZB );
   
   /* push arg */
   vg_threads[tid].m_esp -= 4;
   * (UInt*)(vg_threads[tid].m_esp) = (UInt)arg;

   /* push (magical) return address */
   vg_threads[tid].m_esp -= 4;
   * (UInt*)(vg_threads[tid].m_esp) = (UInt)VG_(pthreadreturn_bogusRA);

   if (VG_(clo_instrument))
      VGM_(make_readable)( vg_threads[tid].m_esp, 2 * 4 );

   /* this is where we start */
   vg_threads[tid].m_eip = (UInt)start_routine;

   if (VG_(clo_trace_sched)) {
      VG_(sprintf)(msg_buf,
         "new thread, created by %d", parent_tid );
      print_sched_event(tid, msg_buf);
   }

   /* store the thread id in *thread. */
   //   if (VG_(clo_instrument))
   // ***** CHECK *thread is writable
   *thread = (pthread_t)tid;
   if (VG_(clo_instrument))
      VGM_(make_readable)( (Addr)thread, sizeof(pthread_t) );

   vg_threads[tid].associated_mx = NULL;
   vg_threads[tid].associated_cv = NULL;
   vg_threads[tid].joiner        = VG_INVALID_THREADID;
   vg_threads[tid].status        = VgTs_Runnable;

   for (i = 0; i < VG_N_THREAD_KEYS; i++)
      vg_threads[tid].specifics[i] = NULL;

   /* We inherit our parent's signal mask. (?!) */
   vg_threads[tid].sig_mask = vg_threads[parent_tid].sig_mask;
   VG_(ksigemptyset)(&vg_threads[i].sigs_waited_for);

   /* return zero */
   SET_EDX(parent_tid, 0); /* success */
}


/* -----------------------------------------------------------
   MUTEXes
   -------------------------------------------------------- */

/* pthread_mutex_t is a struct with at 5 words:
      typedef struct
      {
        int __m_reserved;         -- Reserved for future use
        int __m_count;            -- Depth of recursive locking
        _pthread_descr __m_owner; -- Owner thread (if recursive or errcheck)
        int __m_kind;      -- Mutex kind: fast, recursive or errcheck
        struct _pthread_fastlock __m_lock;  -- Underlying fast lock
      } pthread_mutex_t;

   #define PTHREAD_MUTEX_INITIALIZER \
     {0, 0, 0, PTHREAD_MUTEX_TIMED_NP, __LOCK_INITIALIZER}
   # define PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP \
     {0, 0, 0, PTHREAD_MUTEX_RECURSIVE_NP, __LOCK_INITIALIZER}
   # define PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP \
     {0, 0, 0, PTHREAD_MUTEX_ERRORCHECK_NP, __LOCK_INITIALIZER}
   # define PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP \
     {0, 0, 0, PTHREAD_MUTEX_ADAPTIVE_NP, __LOCK_INITIALIZER}

   How we use it:

   __m_kind  never changes and indicates whether or not it is recursive.

   __m_count indicates the lock count; if 0, the mutex is not owned by 
             anybody.  

   __m_owner has a ThreadId value stuffed into it.  We carefully arrange 
             that ThreadId == 0 is invalid (VG_INVALID_THREADID), so that
             statically initialised mutexes correctly appear 
             to belong to nobody.

   In summary, a not-in-use mutex is distinguised by having __m_owner
   == 0 (VG_INVALID_THREADID) and __m_count == 0 too.  If one of those
   conditions holds, the other should too.

   There is no linked list of threads waiting for this mutex.  Instead
   a thread in WaitMX state points at the mutex with its waited_on_mx
   field.  This makes _unlock() inefficient, but simple to implement the
   right semantics viz-a-viz signals.

   We don't have to deal with mutex initialisation; the client side
   deals with that for us.  
*/

/* Helper fns ... */
static
void release_one_thread_waiting_on_mutex ( pthread_mutex_t* mutex, 
                                           Char* caller )
{
   Int  i;
   Char msg_buf[100];

   /* Find some arbitrary thread waiting on this mutex, and make it
      runnable.  If none are waiting, mark the mutex as not held. */
   for (i = 1; i < VG_N_THREADS; i++) {
      if (vg_threads[i].status == VgTs_Empty) 
         continue;
      if (vg_threads[i].status == VgTs_WaitMX 
          && vg_threads[i].associated_mx == mutex)
         break;
   }

   vg_assert(i <= VG_N_THREADS);
   if (i == VG_N_THREADS) {
      /* Nobody else is waiting on it. */
      mutex->__m_count = 0;
      mutex->__m_owner = VG_INVALID_THREADID;
   } else {
      /* Notionally transfer the hold to thread i, whose
         pthread_mutex_lock() call now returns with 0 (success). */
      /* The .count is already == 1. */
      vg_assert(vg_threads[i].associated_mx == mutex);
      mutex->__m_owner = (_pthread_descr)i;
      vg_threads[i].status        = VgTs_Runnable;
      vg_threads[i].associated_mx = NULL;
      /* m_edx already holds pth_mx_lock() success (0) */

      if (VG_(clo_trace_pthread_level) >= 1) {
         VG_(sprintf)(msg_buf, "%s       mx %p: RESUME", 
                               caller, mutex );
         print_pthread_event(i, msg_buf);
      }
   }
}


static
void do_pthread_mutex_lock( ThreadId tid, 
                            Bool is_trylock, 
                            void* /* pthread_mutex_t* */ mutexV )
{
   Char  msg_buf[100];
   Char* caller
      = is_trylock ? "pthread_mutex_trylock"
                   : "pthread_mutex_lock   ";

   pthread_mutex_t* mutex = (pthread_mutex_t*)mutexV;

   if (VG_(clo_trace_pthread_level) >= 2) {
      VG_(sprintf)(msg_buf, "%s    mx %p ...", caller, mutex );
      print_pthread_event(tid, msg_buf);
   }

   /* Paranoia ... */
   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   /* POSIX doesn't mandate this, but for sanity ... */
   if (mutex == NULL) {
      /* VG_(printf)("NULL mutex\n"); */
      SET_EDX(tid, EINVAL);
      return;
   }

   /* More paranoia ... */
   switch (mutex->__m_kind) {
#     ifndef GLIBC_2_1
      case PTHREAD_MUTEX_TIMED_NP:
      case PTHREAD_MUTEX_ADAPTIVE_NP:
#     endif
#     ifdef GLIBC_2_1
      case PTHREAD_MUTEX_FAST_NP:
#     endif
      case PTHREAD_MUTEX_RECURSIVE_NP:
      case PTHREAD_MUTEX_ERRORCHECK_NP:
         if (mutex->__m_count >= 0) break;
         /* else fall thru */
      default:
         /* VG_(printf)("unknown __m_kind %d in mutex\n", mutex->__m_kind); */
         SET_EDX(tid, EINVAL);
         return;
   }

   if (mutex->__m_count > 0) {

      vg_assert(VG_(is_valid_tid)((ThreadId)mutex->__m_owner));

      /* Someone has it already. */
      if ((ThreadId)mutex->__m_owner == tid) {
         /* It's locked -- by me! */
         if (mutex->__m_kind == PTHREAD_MUTEX_RECURSIVE_NP) {
            /* return 0 (success). */
            mutex->__m_count++;
            SET_EDX(tid, 0);
            if (0)
               VG_(printf)("!!!!!! tid %d, mx %p -> locked %d\n", 
                           tid, mutex, mutex->__m_count);
            return;
         } else {
            if (is_trylock)
               SET_EDX(tid, EBUSY);
            else
               SET_EDX(tid, EDEADLK);
            return;
         }
      } else {
         /* Someone else has it; we have to wait.  Mark ourselves
            thusly. */
         /* GUARD: __m_count > 0 && __m_owner is valid */
         if (is_trylock) {
            /* caller is polling; so return immediately. */
            SET_EDX(tid, EBUSY);
         } else {
            vg_threads[tid].status        = VgTs_WaitMX;
            vg_threads[tid].associated_mx = mutex;
            SET_EDX(tid, 0); /* pth_mx_lock success value */
            if (VG_(clo_trace_pthread_level) >= 1) {
               VG_(sprintf)(msg_buf, "%s    mx %p: BLOCK", 
                                     caller, mutex );
               print_pthread_event(tid, msg_buf);
            }
	 }
         return;
      }

   } else {
      /* Nobody owns it.  Sanity check ... */
      vg_assert(mutex->__m_owner == VG_INVALID_THREADID);
      /* We get it! [for the first time]. */
      mutex->__m_count = 1;
      mutex->__m_owner = (_pthread_descr)tid;
      vg_assert(vg_threads[tid].associated_mx == NULL);
      /* return 0 (success). */
      SET_EDX(tid, 0);
   }

}


static
void do_pthread_mutex_unlock ( ThreadId tid,
                               void* /* pthread_mutex_t* */ mutexV )
{
   Char msg_buf[100];
   pthread_mutex_t* mutex = (pthread_mutex_t*)mutexV;

   if (VG_(clo_trace_pthread_level) >= 2) {
      VG_(sprintf)(msg_buf, "pthread_mutex_unlock     mx %p ...", mutex );
      print_pthread_event(tid, msg_buf);
   }

   /* Paranoia ... */
   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   if (mutex == NULL) {
      SET_EDX(tid, EINVAL);
      return;
   }

   /* More paranoia ... */
   switch (mutex->__m_kind) {
#     ifndef GLIBC_2_1    
      case PTHREAD_MUTEX_TIMED_NP:
      case PTHREAD_MUTEX_ADAPTIVE_NP:
#     endif
#     ifdef GLIBC_2_1
      case PTHREAD_MUTEX_FAST_NP:
#     endif
      case PTHREAD_MUTEX_RECURSIVE_NP:
      case PTHREAD_MUTEX_ERRORCHECK_NP:
         if (mutex->__m_count >= 0) break;
         /* else fall thru */
      default:
         SET_EDX(tid, EINVAL);
         return;
   }

   /* Barf if we don't currently hold the mutex. */
   if (mutex->__m_count == 0 /* nobody holds it */
       || (ThreadId)mutex->__m_owner != tid /* we don't hold it */) {
      SET_EDX(tid, EPERM);
      return;
   }

   /* If it's a multiply-locked recursive mutex, just decrement the
      lock count and return. */
   if (mutex->__m_count > 1) {
      vg_assert(mutex->__m_kind == PTHREAD_MUTEX_RECURSIVE_NP);
      mutex->__m_count --;
      SET_EDX(tid, 0); /* success */
      return;
   }

   /* Now we're sure it is locked exactly once, and by the thread who
      is now doing an unlock on it.  */
   vg_assert(mutex->__m_count == 1);
   vg_assert((ThreadId)mutex->__m_owner == tid);

   /* Release at max one thread waiting on this mutex. */
   release_one_thread_waiting_on_mutex ( mutex, "pthread_mutex_lock" );

   /* Our (tid's) pth_unlock() returns with 0 (success). */
   SET_EDX(tid, 0); /* Success. */
}


/* -----------------------------------------------------------
   CONDITION VARIABLES
   -------------------------------------------------------- */

/* The relevant native types are as follows:
   (copied from /usr/include/bits/pthreadtypes.h)

   -- Conditions (not abstract because of PTHREAD_COND_INITIALIZER
   typedef struct
   {
     struct _pthread_fastlock __c_lock; -- Protect against concurrent access
     _pthread_descr __c_waiting;        -- Threads waiting on this condition
   } pthread_cond_t;

   -- Attribute for conditionally variables.
   typedef struct
   {
     int __dummy;
   } pthread_condattr_t;

   #define PTHREAD_COND_INITIALIZER {__LOCK_INITIALIZER, 0}

   We don't use any fields of pthread_cond_t for anything at all.
   Only the identity of the CVs is important.

   Linux pthreads supports no attributes on condition variables, so we
   don't need to think too hard there.  */


static 
void do_pthread_cond_timedwait_TIMEOUT ( ThreadId tid )
{
   Char             msg_buf[100];
   pthread_mutex_t* mx;
   pthread_cond_t*  cv;

   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_WaitCV
             && vg_threads[tid].awaken_at != 0xFFFFFFFF);
   mx = vg_threads[tid].associated_mx;
   vg_assert(mx != NULL);
   cv = vg_threads[tid].associated_cv;
   vg_assert(cv != NULL);

   if (mx->__m_owner == VG_INVALID_THREADID) {
      /* Currently unheld; hand it out to thread tid. */
      vg_assert(mx->__m_count == 0);
      vg_threads[tid].status        = VgTs_Runnable;
      SET_EDX(tid, ETIMEDOUT);      /* pthread_cond_wait return value */
      vg_threads[tid].associated_cv = NULL;
      vg_threads[tid].associated_mx = NULL;
      mx->__m_owner = (_pthread_descr)tid;
      mx->__m_count = 1;

      if (VG_(clo_trace_pthread_level) >= 1) {
         VG_(sprintf)(msg_buf, 
            "pthread_cond_timedwai cv %p: TIMEOUT with mx %p", 
            cv, mx );
         print_pthread_event(tid, msg_buf);
      }
   } else {
      /* Currently held.  Make thread tid be blocked on it. */
      vg_assert(mx->__m_count > 0);
      vg_threads[tid].status        = VgTs_WaitMX;
      SET_EDX(tid, ETIMEDOUT);      /* pthread_cond_wait return value */
      vg_threads[tid].associated_cv = NULL;
      vg_threads[tid].associated_mx = mx;
      if (VG_(clo_trace_pthread_level) >= 1) {
         VG_(sprintf)(msg_buf, 
            "pthread_cond_timedwai cv %p: TIMEOUT -> BLOCK for mx %p", 
            cv, mx );
         print_pthread_event(tid, msg_buf);
      }

   }
}


static
void release_N_threads_waiting_on_cond ( pthread_cond_t* cond, 
                                         Int n_to_release, 
                                         Char* caller )
{
   Int              i;
   Char             msg_buf[100];
   pthread_mutex_t* mx;

   while (True) {
      if (n_to_release == 0)
         return;

      /* Find a thread waiting on this CV. */
      for (i = 1; i < VG_N_THREADS; i++) {
         if (vg_threads[i].status == VgTs_Empty) 
            continue;
         if (vg_threads[i].status == VgTs_WaitCV 
             && vg_threads[i].associated_cv == cond)
            break;
      }
      vg_assert(i <= VG_N_THREADS);

      if (i == VG_N_THREADS) {
         /* Nobody else is waiting on it. */
         return;
      }

      mx = vg_threads[i].associated_mx;
      vg_assert(mx != NULL);

      if (mx->__m_owner == VG_INVALID_THREADID) {
         /* Currently unheld; hand it out to thread i. */
         vg_assert(mx->__m_count == 0);
         vg_threads[i].status        = VgTs_Runnable;
         vg_threads[i].associated_cv = NULL;
         vg_threads[i].associated_mx = NULL;
         mx->__m_owner = (_pthread_descr)i;
         mx->__m_count = 1;
         /* .m_edx already holds pth_cond_wait success value (0) */

         if (VG_(clo_trace_pthread_level) >= 1) {
            VG_(sprintf)(msg_buf, "%s   cv %p: RESUME with mx %p", 
                                  caller, cond, mx );
            print_pthread_event(i, msg_buf);
         }

      } else {
         /* Currently held.  Make thread i be blocked on it. */
         vg_assert(mx->__m_count > 0);
         vg_threads[i].status        = VgTs_WaitMX;
         vg_threads[i].associated_cv = NULL;
         vg_threads[i].associated_mx = mx;
         SET_EDX(i, 0); /* pth_cond_wait success value */

         if (VG_(clo_trace_pthread_level) >= 1) {
            VG_(sprintf)(msg_buf, "%s   cv %p: BLOCK for mx %p", 
                                  caller, cond, mx );
            print_pthread_event(i, msg_buf);
         }

      }

      n_to_release--;
   }
}


static
void do_pthread_cond_wait ( ThreadId tid,
                            pthread_cond_t *cond, 
                            pthread_mutex_t *mutex,
			    UInt ms_end )
{
   Char msg_buf[100];

   /* If ms_end == 0xFFFFFFFF, wait forever (no timeout).  Otherwise,
      ms_end is the ending millisecond. */

   /* pre: mutex should be a valid mutex and owned by tid. */
   if (VG_(clo_trace_pthread_level) >= 2) {
      VG_(sprintf)(msg_buf, "pthread_cond_wait        cv %p, mx %p, end %d ...", 
                            cond, mutex, ms_end );
      print_pthread_event(tid, msg_buf);
   }

   /* Paranoia ... */
   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   if (mutex == NULL || cond == NULL) {
      SET_EDX(tid, EINVAL);
      return;
   }

   /* More paranoia ... */
   switch (mutex->__m_kind) {
#     ifndef GLIBC_2_1    
      case PTHREAD_MUTEX_TIMED_NP:
      case PTHREAD_MUTEX_ADAPTIVE_NP:
#     endif
#     ifdef GLIBC_2_1
      case PTHREAD_MUTEX_FAST_NP:
#     endif
      case PTHREAD_MUTEX_RECURSIVE_NP:
      case PTHREAD_MUTEX_ERRORCHECK_NP:
         if (mutex->__m_count >= 0) break;
         /* else fall thru */
      default:
         SET_EDX(tid, EINVAL);
         return;
   }

   /* Barf if we don't currently hold the mutex. */
   if (mutex->__m_count == 0 /* nobody holds it */
       || (ThreadId)mutex->__m_owner != tid /* we don't hold it */) {
      SET_EDX(tid, EINVAL);
      return;
   }

   /* Queue ourselves on the condition. */
   vg_threads[tid].status        = VgTs_WaitCV;
   vg_threads[tid].associated_cv = cond;
   vg_threads[tid].associated_mx = mutex;
   vg_threads[tid].awaken_at     = ms_end;

   if (VG_(clo_trace_pthread_level) >= 1) {
      VG_(sprintf)(msg_buf, 
                   "pthread_cond_wait        cv %p, mx %p: BLOCK", 
                   cond, mutex );
      print_pthread_event(tid, msg_buf);
   }

   /* Release the mutex. */
   release_one_thread_waiting_on_mutex ( mutex, "pthread_cond_wait " );
}


static
void do_pthread_cond_signal_or_broadcast ( ThreadId tid, 
                                           Bool broadcast,
                                           pthread_cond_t *cond )
{
   Char  msg_buf[100];
   Char* caller 
      = broadcast ? "pthread_cond_broadcast" 
                  : "pthread_cond_signal   ";

   if (VG_(clo_trace_pthread_level) >= 2) {
      VG_(sprintf)(msg_buf, "%s   cv %p ...", 
                            caller, cond );
      print_pthread_event(tid, msg_buf);
   }

   /* Paranoia ... */
   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   if (cond == NULL) {
      SET_EDX(tid, EINVAL);
      return;
   }
   
   release_N_threads_waiting_on_cond ( 
      cond,
      broadcast ? VG_N_THREADS : 1, 
      caller
   );

   SET_EDX(tid, 0); /* success */
}


/* -----------------------------------------------------------
   THREAD SPECIFIC DATA
   -------------------------------------------------------- */

static __inline__
Bool is_valid_key ( ThreadKey k )
{
   /* k unsigned; hence no < 0 check */
   if (k >= VG_N_THREAD_KEYS) return False;
   if (!vg_thread_keys[k].inuse) return False;
   return True;
}

static
void do_pthread_key_create ( ThreadId tid,
                             pthread_key_t* key,
                             void (*destructor)(void*) )
{
   Int  i;
   Char msg_buf[100];

   if (VG_(clo_trace_pthread_level) >= 1) {
      VG_(sprintf)(msg_buf, "pthread_key_create      *key %p, destr %p", 
                            key, destructor );
      print_pthread_event(tid, msg_buf);
   }

   vg_assert(sizeof(pthread_key_t) == sizeof(ThreadKey));
   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   for (i = 0; i < VG_N_THREAD_KEYS; i++)
      if (!vg_thread_keys[i].inuse)   
         break;

   if (i == VG_N_THREAD_KEYS) {
      /* SET_EDX(tid, EAGAIN); 
         return; 
      */
      VG_(panic)("pthread_key_create: VG_N_THREAD_KEYS is too low;"
                 " increase and recompile");
   }

   vg_thread_keys[i].inuse = True;

   /* TODO: check key for addressibility */
   *key = i;
   if (VG_(clo_instrument))
      VGM_(make_readable)( (Addr)key, sizeof(pthread_key_t) );

   SET_EDX(tid, 0);
}


static
void do_pthread_key_delete ( ThreadId tid, pthread_key_t key )
{
   Char msg_buf[100];
   if (VG_(clo_trace_pthread_level) >= 1) {
      VG_(sprintf)(msg_buf, "pthread_key_delete       key %d", 
                            key );
      print_pthread_event(tid, msg_buf);
   }

   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);
   
   if (!is_valid_key(key)) {
      SET_EDX(tid, EINVAL);
      return;
   }

   vg_thread_keys[key].inuse = False;

   /* Optional.  We're not required to do this, although it shouldn't
      make any difference to programs which use the key/specifics
      functions correctly.  */
#  if 1
   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (vg_threads[tid].status != VgTs_Empty)
         vg_threads[tid].specifics[key] = NULL;
   }
#  endif
}


static 
void do_pthread_getspecific ( ThreadId tid, pthread_key_t key )
{
   Char msg_buf[100];
   if (VG_(clo_trace_pthread_level) >= 1) {
      VG_(sprintf)(msg_buf, "pthread_getspecific      key %d", 
                            key );
      print_pthread_event(tid, msg_buf);
   }

   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   if (!is_valid_key(key)) {
      SET_EDX(tid, (UInt)NULL);
      return;
   }

   SET_EDX(tid, (UInt)vg_threads[tid].specifics[key]);
}


static
void do_pthread_setspecific ( ThreadId tid, 
                              pthread_key_t key, 
                              void *pointer )
{
   Char msg_buf[100];
   if (VG_(clo_trace_pthread_level) >= 1) {
      VG_(sprintf)(msg_buf, "pthread_setspecific      key %d, ptr %p", 
                            key, pointer );
      print_pthread_event(tid, msg_buf);
   }

   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   if (!is_valid_key(key)) {
      SET_EDX(tid, EINVAL);
      return;
   }

   vg_threads[tid].specifics[key] = pointer;
   SET_EDX(tid, 0);
}


/* ---------------------------------------------------
   SIGNALS
   ------------------------------------------------ */

/* See comment in vg_libthread.c:pthread_sigmask() regarding
   deliberate confusion of types sigset_t and vki_sigset_t.  Also re
   meaning of the mashed_how value.  Return 0 for OK and 1 for some
   kind of addressing error, which the vg_libpthread.c routine turns
   into return values 0 and EFAULT respectively. */
static
void do_pthread_sigmask ( ThreadId tid,
                          Int mashed_how,
                          vki_ksigset_t* newmask, 
                          vki_ksigset_t* oldmask )
{
   Char msg_buf[100];
   if (VG_(clo_trace_pthread_level) >= 1) {
      VG_(sprintf)(msg_buf, 
         "pthread_sigmask          m_how %d, newmask %p, oldmask %p",
         mashed_how, newmask, oldmask );
      print_pthread_event(tid, msg_buf);
   }

   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   if (VG_(clo_instrument)) {
      /* TODO check newmask/oldmask are addressible/defined */
   }

   if (oldmask != NULL) {
      *oldmask = vg_threads[tid].sig_mask;
      if (VG_(clo_instrument)) {
         VGM_(make_readable)( (Addr)oldmask, sizeof(vki_ksigset_t) );
      }
   }

   switch (mashed_how) {
      case 1: /* SIG_SETMASK */
         vg_threads[tid].sig_mask = *newmask;
         break;
      case 2: /* SIG_BLOCK */
         VG_(ksigaddset_from_set)( & vg_threads[tid].sig_mask, newmask);
         break;
      case 3: /* SIG_UNBLOCK */
         VG_(ksigdelset_from_set)( & vg_threads[tid].sig_mask, newmask);
         break;
      default: 
        VG_(panic)("do_pthread_sigmask: invalid mashed_how");
        /*NOTREACHED*/
        break;
   }

   SET_EDX(tid, 0);
}


static
void do_sigwait ( ThreadId tid,
                  vki_ksigset_t* set, 
                  Int* sig )
{
   Char msg_buf[100];
   if (VG_(clo_trace_signals) || VG_(clo_trace_sched)) {
      VG_(sprintf)(msg_buf, 
         "suspend due to sigwait(): set %p, sig %p",
         set, sig );
      print_pthread_event(tid, msg_buf);
   }

   vg_assert(VG_(is_valid_tid)(tid) 
             && vg_threads[tid].status == VgTs_Runnable);

   vg_threads[tid].sigs_waited_for = *set;
   vg_threads[tid].status = VgTs_WaitSIG;
   VG_(update_sigstate_following_WaitSIG_change)();
}


/* ---------------------------------------------------------------------
   Handle non-trivial client requests.
   ------------------------------------------------------------------ */

static
void do_nontrivial_clientreq ( ThreadId tid )
{
   UInt* arg    = (UInt*)(vg_threads[tid].m_eax);
   UInt  req_no = arg[0];
   switch (req_no) {

      case VG_USERREQ__PTHREAD_CREATE:
         do_pthread_create( tid, 
                            (pthread_t*)arg[1], 
                            (pthread_attr_t*)arg[2], 
                            (void*(*)(void*))arg[3], 
                            (void*)arg[4] );
         break;

      case VG_USERREQ__PTHREAD_RETURNS:
         handle_pthread_return( tid, (void*)arg[1] );
         break;

      case VG_USERREQ__PTHREAD_JOIN:
         do_pthread_join( tid, arg[1], (void**)(arg[2]) );
         break;

      case VG_USERREQ__PTHREAD_CANCEL:
         do_pthread_cancel( tid, (pthread_t)(arg[1]) );
         break;

      case VG_USERREQ__PTHREAD_EXIT:
         do_pthread_exit( tid, (void*)(arg[1]) );
         break;

      case VG_USERREQ__PTHREAD_COND_WAIT:
         do_pthread_cond_wait( tid, 
                               (pthread_cond_t *)(arg[1]),
                               (pthread_mutex_t *)(arg[2]),
                               0xFFFFFFFF /* no timeout */ );
         break;

      case VG_USERREQ__PTHREAD_COND_TIMEDWAIT:
         do_pthread_cond_wait( tid, 
                               (pthread_cond_t *)(arg[1]),
                               (pthread_mutex_t *)(arg[2]),
                               arg[3] /* timeout millisecond point */ );
         break;

      case VG_USERREQ__PTHREAD_COND_SIGNAL:
         do_pthread_cond_signal_or_broadcast( 
            tid, 
	    False, /* signal, not broadcast */
            (pthread_cond_t *)(arg[1]) );
         break;

      case VG_USERREQ__PTHREAD_COND_BROADCAST:
         do_pthread_cond_signal_or_broadcast( 
            tid, 
	    True, /* broadcast, not signal */
            (pthread_cond_t *)(arg[1]) );
         break;

      case VG_USERREQ__PTHREAD_KEY_CREATE:
 	 do_pthread_key_create ( tid, 
                                 (pthread_key_t*)(arg[1]),
                                 (void(*)(void*))(arg[2]) );
	 break;

      case VG_USERREQ__PTHREAD_KEY_DELETE:
 	 do_pthread_key_delete ( tid, 
                                 (pthread_key_t)(arg[1]) );
 	 break;

      case VG_USERREQ__PTHREAD_SETSPECIFIC:
 	 do_pthread_setspecific ( tid, 
                                  (pthread_key_t)(arg[1]),
				  (void*)(arg[2]) );
 	 break;

      case VG_USERREQ__PTHREAD_SIGMASK:
         do_pthread_sigmask ( tid,
                              arg[1],
                              (vki_ksigset_t*)(arg[2]),
                              (vki_ksigset_t*)(arg[3]) );
	 break;

      case VG_USERREQ__SIGWAIT:
         do_sigwait ( tid,
                      (vki_ksigset_t*)(arg[1]),
                      (Int*)(arg[2]) );
	 break;

      case VG_USERREQ__MAKE_NOACCESS:
      case VG_USERREQ__MAKE_WRITABLE:
      case VG_USERREQ__MAKE_READABLE:
      case VG_USERREQ__DISCARD:
      case VG_USERREQ__CHECK_WRITABLE:
      case VG_USERREQ__CHECK_READABLE:
      case VG_USERREQ__MAKE_NOACCESS_STACK:
      case VG_USERREQ__RUNNING_ON_VALGRIND:
      case VG_USERREQ__DO_LEAK_CHECK:
         SET_EDX(
            tid, 
            VG_(handle_client_request) ( &vg_threads[tid], arg )
         );
	 break;

      case VG_USERREQ__SIGNAL_RETURNS: 
         handle_signal_return(tid);
	 break;

      default:
         VG_(printf)("panic'd on private request = 0x%x\n", arg[0] );
         VG_(panic)("handle_private_client_pthread_request: "
                    "unknown request");
         /*NOTREACHED*/
         break;
   }
}


/* ---------------------------------------------------------------------
   Sanity checking.
   ------------------------------------------------------------------ */

/* Internal consistency checks on the sched/pthread structures. */
static
void scheduler_sanity ( void )
{
   pthread_mutex_t* mx;
   pthread_cond_t*  cv;
   Int              i;

   /* VG_(printf)("scheduler_sanity\n"); */
   for (i = 1; i < VG_N_THREADS; i++) {
      mx = vg_threads[i].associated_mx;
      cv = vg_threads[i].associated_cv;
      if (vg_threads[i].status == VgTs_WaitMX) {
	 /* If we're waiting on a MX: (1) the mx is not null, (2, 3)
            it's actually held by someone, since otherwise this thread
            is deadlocked, (4) the mutex's owner is not us, since
            otherwise this thread is also deadlocked.  The logic in
            do_pthread_mutex_lock rejects attempts by a thread to lock
            a (non-recursive) mutex which it already owns.

            (2) has been seen to fail sometimes.  I don't know why.
            Possibly to do with signals. */
         vg_assert(cv == NULL);
         /* 1 */ vg_assert(mx != NULL);
	 /* 2 */ vg_assert(mx->__m_count > 0);
         /* 3 */ vg_assert(VG_(is_valid_tid)((ThreadId)mx->__m_owner));
         /* 4 */ vg_assert(i != (ThreadId)mx->__m_owner); 
      } else 
      if (vg_threads[i].status == VgTs_WaitCV) {
         vg_assert(cv != NULL);
         vg_assert(mx != NULL);
      } else {
         /* Unfortunately these don't hold true when a sighandler is
            running.  To be fixed. */
         /* vg_assert(cv == NULL); */
         /* vg_assert(mx == NULL); */
      }

      if (vg_threads[i].status != VgTs_Empty) {
         Int
         stack_used = (Addr)vg_threads[i].stack_highest_word 
                      - (Addr)vg_threads[i].m_esp;
         if (i > 1 /* not the root thread */ 
             && stack_used 
                >= (VG_PTHREAD_STACK_MIN - 1000 /* paranoia */)) {
            VG_(message)(Vg_UserMsg,
               "Warning: STACK OVERFLOW: "
               "thread %d: stack used %d, available %d", 
               i, stack_used, VG_PTHREAD_STACK_MIN );
            VG_(message)(Vg_UserMsg,
               "Terminating Valgrind.  If thread(s) "
               "really need more stack, increase");
            VG_(message)(Vg_UserMsg,
               "VG_PTHREAD_STACK_SIZE in vg_include.h and recompile.");
            VG_(exit)(1);
	 }

         if (vg_threads[i].status == VgTs_WaitSIG) {
            vg_assert( ! VG_(kisemptysigset)(
                            & vg_threads[i].sigs_waited_for) );
	 } else {
            vg_assert( VG_(kisemptysigset)(
                          & vg_threads[i].sigs_waited_for) );
	 }

      }
   }

   for (i = 0; i < VG_N_THREAD_KEYS; i++) {
      if (!vg_thread_keys[i].inuse)
         vg_assert(vg_thread_keys[i].destructor == NULL);
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                           vg_scheduler.c ---*/
/*--------------------------------------------------------------------*/
