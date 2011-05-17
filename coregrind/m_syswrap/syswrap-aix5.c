
/*--------------------------------------------------------------------*/
/*--- AIX5-specific syscalls.                       syswrap-aix5.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2010 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#if defined(VGO_aix5)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuginfo.h"    // VG_(di_notify_*)
#include "pub_core_transtab.h"     // VG_(discard_translations)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_debuglog.h"
#include "pub_tool_gdbserver.h"     // VG_(gdbserver)
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_mallocfree.h"
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_sigframe.h"     // VG_(sigframe_destroy)
#include "pub_core_syswrap.h"
#include "pub_core_stacktrace.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-aix5.h"



/* ---------------------------------------------------------------------
   Misc helpers
   ------------------------------------------------------------------ */

/* Allocate a stack for this thread, if it doesn't already have one.
   They're allocated lazily, and never freed.  Returns the initial stack
   pointer value to use, or 0 if allocation failed. */
Addr ML_(allocstack)(ThreadId tid)
{
   ThreadState* tst = VG_(get_ThreadState)(tid);
   VgStack*     stack;
   Addr         initial_SP;

   /* Either the stack_base and stack_init_SP are both zero (in which
      case a stack hasn't been allocated) or they are both non-zero,
      in which case it has. */

   if (tst->os_state.valgrind_stack_base == 0)
      vg_assert(tst->os_state.valgrind_stack_init_SP == 0);

   if (tst->os_state.valgrind_stack_base != 0)
      vg_assert(tst->os_state.valgrind_stack_init_SP != 0);

   /* If no stack is present, allocate one. */
   if (tst->os_state.valgrind_stack_base == 0) {
      stack = VG_(am_alloc_VgStack)( &initial_SP );
      if (stack) {
         /* Leave some space above SP because AIX's ABI stores
            stuff there. */
         initial_SP -= 256;
         vg_assert(initial_SP > (Addr)stack);
         tst->os_state.valgrind_stack_base    = (Addr)stack;
         tst->os_state.valgrind_stack_init_SP = initial_SP;
      } else {
         return 0; /* allocation of stack failed */
      }
   }

   if (0)
      VG_(printf)( "stack for tid %d at %p; init_SP=%p\n",
                   tid,
                   (void*)tst->os_state.valgrind_stack_base,
                   (void*)tst->os_state.valgrind_stack_init_SP );

   return tst->os_state.valgrind_stack_init_SP;
}


/* If we know or believe a module load/unload event has happened, get
   aspacem to re-read /proc/../map to update its picture of what text
   and data segments are present.  This also notifies all the usual
   parties that need to know about address space changes. */

void ML_(aix5_rescan_procmap_after_load_or_unload) ( void )
{
   AixCodeSegChange* changes;
   Int changes_size, changes_used, i;

   /* Find out how many AixCodeSegChange records we will need, and
      acquire them. */
   changes_size = VG_(am_aix5_reread_procmap_howmany_directives)(); 
   changes = VG_(arena_malloc)(VG_AR_CORE, "syswrap-aix5.arpalou.1",
                               changes_size * sizeof(AixCodeSegChange));
   vg_assert(changes);

   /* Now re-read /proc/<pid>/map and acquire a change set */
   VG_(am_aix5_reread_procmap)( changes, &changes_used );
   vg_assert(changes_used >= 0 && changes_used <= changes_size);

   /* And notify all parties of the changes. */
   for (i = 0; i < changes_used; i++) {
      ULong di_handle = VG_(di_aix5_notify_segchange)(
                           changes[i].code_start,
                           changes[i].code_len,
                           changes[i].data_start,
                           changes[i].data_len,
                           changes[i].file_name,
                           changes[i].mem_name,
                           changes[i].is_mainexe,
                           changes[i].acquire
                        );

      if (changes[i].acquire) {
         VG_TRACK( new_mem_mmap, 
                   changes[i].code_start, changes[i].code_len, 
                   /*r*/True, /*w*/False, /*x*/True, di_handle );
         VG_TRACK( new_mem_mmap, 
                   changes[i].data_start, changes[i].data_len, 
                   /*r*/True, /*w*/True, /*x*/False, 0/*or di_handle?*/ );
      } else {
         VG_TRACK( die_mem_munmap, 
                   changes[i].code_start, changes[i].code_len );
         VG_TRACK( die_mem_munmap, 
                   changes[i].data_start, changes[i].data_len );
         VG_(discard_translations)(
                   changes[i].code_start, changes[i].code_len,
                   "POST(sys___loadx/sys__kload)(code)" );
         VG_(discard_translations)(
                    changes[i].data_start, changes[i].data_len,
                   "POST(sys___loadx/sys__kload)(data)" );
      }
   }

   VG_(arena_free)(VG_AR_CORE, changes);
}


/* Mess with the given thread's pc/toc so that it is entering
   pthread_exit() with argument PTHREAD_CANCELED.  Returns True if ok,
   False if it failed to do so, due to not being able to find
   pthread_exit() by searching symbol tables. */
Bool ML_(aix5_force_thread_into_pthread_exit)( ThreadId tid )
{
   Addr ent = 0, toc = 0;
   Bool found;
   ThreadState* tst = VG_(get_ThreadState)(tid);
   found = VG_(lookup_symbol_SLOW)("libpthread*.a(*.o)", "pthread_exit", 
                                   &ent, &toc);
   if (found) {
      if (0) 
         VG_(printf)("THREAD CANCELED, new cia,toc = %#lx,%#lx\n", ent, toc);
      tst->arch.vex.guest_CIA  = ent;
      tst->arch.vex.guest_GPR2 = toc;
      tst->arch.vex.guest_GPR3 = (Word)(-1); /* == PTHREAD_CANCELED */
      /* If the thread is blocked in a syscall, we better bop it on
         the head with SIGVGKILL in order to get it out of said
         syscall. */
      if (tst->status == VgTs_WaitSys) {
         if (VG_(clo_trace_syscalls))
            VG_(printf)("(sending SIGVGKILL to tid %d)", (Int)tid);
         VG_(get_thread_out_of_syscall)( tid  );
      }
      return True; /* ok */
   } else {
      // urk.  Now we're hosed.  Let the caller figure out what to do.
      return False; /* failed */
   }
}


/* For various reasons, on AIX we may have to just give up if
   continuing is too difficult (eg, risk of future deadlock).  This
   sets up the process state to exit straight away, but does not
   actually itself exit. */
void ML_(aix5_set_threadstate_for_emergency_exit)(ThreadId tid, HChar* why)
{
   ThreadState* tst = VG_(get_ThreadState)(tid);
   /* Set the thread's status to be exiting and taking out the
      entire process, then claim that the syscall succeeded. */
   tst->exitreason = VgSrc_ExitProcess;
   tst->os_state.exitcode = 1;
   if (!VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, 
         "WARNING: AIX: %s\n", why);
      VG_(message)(Vg_UserMsg, 
         "WARNING: (too difficult to continue past this point).\n");
      VG_(get_and_pp_StackTrace)(tid, 10);
   }
}


/* Update aspacem etc on conclusion of a successful sbrk/__libc_sbrk
   call.  2006-08-24: this was not completed because I don't 
   understand what sbrk/__libc_sbrk are doing. */

static void handle_sbrk ( Word delta )
{
   return;
   /*NOTREACHED*/
   if (delta > 0) {
      /* Map in VG_(brk_limit) for delta */
      /* using notify_mmap ? */
      VG_(brk_limit) += delta;
   }
   if (delta < 0) {
     Addr tmp = VG_(brk_limit);
     VG_(brk_limit) += delta;
     /* Can't move below original starting point */
     if (VG_(brk_limit) < VG_(brk_base))
        VG_(brk_limit) = VG_(brk_base);
     if (VG_(brk_limit) < tmp)
        /* Unmap VG_(brk_limit) for tmp - VG_(brk_limit) */
        /* using notify_munmap ? */
        ;
   }
   if (VG_(clo_trace_syscalls))
      VG_(printf)("new brk: 0x%010llx-0x%010llx (size %lld)\n",
                  (ULong)VG_(brk_base),
                  (ULong)VG_(brk_limit),
                  (ULong)VG_(brk_limit) - (ULong)VG_(brk_base));
}


/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <sys/thread.h>
#include <sys/poll.h>
#include <sys/times.h>
#include <sys/shm.h>
#include <semaphore.h>
#include <sys/statfs.h>
#include <sys/utsname.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

HChar* ML_(aix5debugstuff_pc_to_fnname) ( Addr pc )
{
   Bool ok;
   static HChar name[100];
   ok = VG_(get_fnname_w_offset)(pc, name, 100);
   if (!ok) VG_(strcpy)(name, "???");
   return &name[0];
}

static void aix5debugstuff_show_sigset ( vki_sigset_t* set )
{
  Int i;
  UChar* p = (UChar*)set;
  for (i = 0; i < sizeof(vki_sigset_t); i++)
     VG_(printf)("%02x", (Int)p[i]);
}

static HChar* aix5debugstuff_name_of_tstate_flag ( UWord flag )
{
   Int i, nset;
   nset = 0;
   for (i = 0; i < 8*sizeof(UWord); i++)
      if (flag & (1U << i))
         nset++;
   vg_assert(nset == 1);
   switch (flag) {
      case TSTATE_LOCAL:           return "LOCAL";
      case TSTATE_CANCEL_DEFER:    return "CANCEL_DEFER";
      case TSTATE_CANCEL_DISABLE:  return "CANCEL_DISABLE";
      case TSTATE_CANCEL_PENDING:  return "CANCEL_PENDING";
      case TSTATE_CANCEL_CHKPT:    return "CANCEL_CHKPT";
      case TSTATE_INTR:            return "INTR";
      case TSTATE_EXEMPT:          return "EXEMPT";
#ifdef TSTATE_PROFILING_OFF
      case TSTATE_PROFILING_OFF:   return "PROFILING_OFF";
#endif
      case TSTATE_SUSPEND:         return "SUSPEND";
      case TSTATE_CONT:            return "CONT";
#ifdef TSTATE_CREDS
      case TSTATE_CREDS:           return "CREDS";
#endif
#ifdef TSTATE_PROCHANDLERS
      case TSTATE_PROCHANDLERS:    return "PROCHANDLERS";
#endif
      case TSTATE_ADVH:            return "ADVH";
      case TSTATE_SYNCH:           return "SYNCH";
      case TSTATE_USCHED:          return "USCHED";
      case TSTATE_DEFAULT_SCHED:   return "DEFAULT_SCHED";
#ifdef TSTATE_INHERIT_SCHED
      case TSTATE_INHERIT_SCHED:   return "INHERIT_SCHED";
#endif
#ifdef TSTATE_LOCAL_INIT
      case TSTATE_LOCAL_INIT:      return "LOCAL_INIT";
#endif
#ifdef TSTATE_LOCAL_TERM
      case TSTATE_LOCAL_TERM:      return "LOCAL_TERM";
#endif
#ifdef TSTATE_LOCAL_MCHANGE
      case TSTATE_LOCAL_MCHANGE:   return "LOCAL_MCHANGE";
#endif
      case TSTATE_CHANGE_ALL:      return "CHANGE_ALL";
#ifdef TSTATE_CHANGE_PTID
      case TSTATE_CHANGE_PTID:     return "CHANGE_PTID";
#endif
#ifdef TSTATE_CHANGE_PROFILE
      case TSTATE_CHANGE_PROFILE:  return "CHANGE_PROFILE";
#endif
#ifdef TSTATE_CHANGE_SSTACK
      case TSTATE_CHANGE_SSTACK:   return "CHANGE_SSTACK";
#endif
      case TSTATE_CHANGE_ERRNOP:   return "CHANGE_ERRNOP";
      case TSTATE_CHANGE_SIGMASK:  return "CHANGE_SIGMASK";
      case TSTATE_CHANGE_PSIG:     return "CHANGE_PSIG";
      case TSTATE_CHANGE_SCHED:    return "CHANGE_SCHED";
      case TSTATE_CHANGE_FLAGS:    return "CHANGE_FLAGS";
      case TSTATE_CHANGE_USERDATA: return "CHANGE_USERDATA";
      default: return "???";
   }
}

void ML_(aix5debugstuff_show_tstate_flags) ( UWord w )
{
   const Int step = 5;
   Int i, j;
   UWord m;
   j = 0;
   for (i = 0; i < 8*sizeof(UWord); i++) {
      m = 1U << i;
      if ((w & m) == 0)
         continue;
      if ((j % step) == 0)
         VG_(printf)("  ");
      VG_(printf)("%s ", aix5debugstuff_name_of_tstate_flag(w & m));
      if ((j % step) == step-1 && j > 0)
         VG_(printf)("\n");
      j++;
   }
   if (((j-1) % step) != step-1 && j > 0)
      VG_(printf)("\n");
}

void ML_(aix5debugstuff_show_tstate) ( Addr tsA, HChar* who )
{
   Int i;
   const Int step = sizeof(void*)==8  ? 3 : 5;
   struct tstate* ts = (struct tstate*)tsA;
   VG_(printf)("\n{ ========= %s =========\n", who);
   for (i = 0; i < _NGPRS; i++) {
      if ((i % step) == 0) 
         VG_(printf)("  [%2d]  ", i);
      if (sizeof(void*)==8)
         VG_(printf)("%016llx  ", (ULong)ts->mst.gpr[i]);
      else
         VG_(printf)("%08llx  ", (ULong)ts->mst.gpr[i]);
      if ((i == _NGPRS-1) || ((i % step) == step-1 && i > 0)) 
         VG_(printf)("\n");
   }
   VG_(printf)("  [iar] %#llx %s\n", (ULong)ts->mst.iar, 
               ML_(aix5debugstuff_pc_to_fnname)(ts->mst.iar));

   VG_(printf)("  errnop_addr      %p\n", ts->errnop_addr);

   VG_(printf)("  sigmask          ");
   aix5debugstuff_show_sigset( (vki_sigset_t*)&ts->sigmask );
   VG_(printf)("\n");

   VG_(printf)("  psig             ");
   aix5debugstuff_show_sigset( (vki_sigset_t*)&ts->psig );
   VG_(printf)("\n");

   VG_(printf)("  policy           %d\n", ts->policy);
   VG_(printf)("  priority         %d\n", ts->priority);
   VG_(printf)("  flags            0x%x\n", ts->flags);
   ML_(aix5debugstuff_show_tstate_flags)( (UWord)ts->flags );
   VG_(printf)("  flagmask         0x%x\n", ts->flagmask);
   VG_(printf)("  userdata         %p\n", (void*)ts->userdata);
   VG_(printf)("  fpinfo           %d\n", ts->fpinfo);
   VG_(printf)("  fpscrx           %d\n", ts->fpscrx);
   VG_(printf)("  sigaltstack      ??\n");
   VG_(printf)("  thread_control_p 0x%llx\n", (ULong)ts->thread_control_p);
//   AIX 5.1 does not seem to have these members
//   VG_(printf)("  prbase           %p\n", (void*)ts->prbase);
//   VG_(printf)("  credp            %p\n", (void*)ts->credp);
//   VG_(printf)("  ptid             %d\n", (int)ts->ptid);
//   VG_(printf)("  tct_clock        %d\n", (int)ts->tct_clock);
   UInt* p = (UInt*)tsA;
   for (i = 0; i < sizeof(struct tstate)/sizeof(UInt); i++) {
      HChar* s = ML_(aix5debugstuff_pc_to_fnname)( (Addr)p[i] );
      if (0==VG_(strcmp)(s,"???"))
         continue;
      VG_(printf)("  [%d] %x %s\n", i, p[i], s);
   }
   VG_(printf)("}\n");
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for arch-generic, AIX5-specific syscalls.  Note:
   in fact AIX5 doesn't share any wrappers with Linux since it's
   difficult to get syswrap-generic.c to compile on AIX.  Hence in
   fact this file also serves the role of syswrap-generic.c for AIX.
   This could probably be improved at the cost of some extra effort.
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// m_syswrap/syswrap-generic.c for notes about how they work.

#define PRE(name)       DEFN_PRE_TEMPLATE(aix5, name)
#define POST(name)      DEFN_POST_TEMPLATE(aix5, name)


// How to make __libc_sbrk appear to fail, from libc's point of view:
//  SysRes r;
//  r.res = -1; /* significant to libc */
//  r.err = VKI_ENOMEM; /* not significant to libc */
//  SET_STATUS_from_SysRes( r );
//  return;

PRE(sys___libc_sbrk)
{
   PRINT("__libc_sbrk (BOGUS HANDLER)( %#lx )",ARG1);
   PRE_REG_READ1(long, "__libc_sbrk", long, arg1);
   /* After a zero sbrk, disallow aspacem from doing sbrk, since libc
      might rely on the value returned by this syscall. */
   /* 1 Oct 06: not currently used (aspacemgr-aix5.c ignores it) */
   VG_(am_aix5_sbrk_allowed) = toBool(ARG1 != 0);
   /* Disallow libc from moving the brk backwards as that might trash
      SkPreAlloc sections acquired by aspacem from previous uses of
      sbrk. */
   if (ARG1 < 0)
      ARG1 = 0;
   /* Do this as a sync syscall, so the sbrk_allowed flag gets turned
      back on ASAP.  Typically libc does sbrk(0) and then sbrk(x > 0)
      in quick succession.  Although surely it should hold some kind
      of lock at that point, else it cannot safely use the result from
      the first sbrk call to influence the second one? */
   *flags &= ~SfMayBlock;
}
POST(sys___libc_sbrk)
{
   vg_assert(SUCCESS);
   handle_sbrk(ARG1);
}

/* __loadx is handled in the platform-specific files. */

PRE(sys___msleep)
{
   PRINT("__msleep (BOGUS HANDLER) ( %#lx )", ARG1);
   PRE_REG_READ1(long, "msleep", void*, arg1);
}

/* __unload is handled in the platform-specific files. */

PRE(sys__clock_settime)
{
   PRINT("_clock_settime (UNDOCUMENTED) ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(int, "_clock_settime", int, arg1, int, arg2);
}

PRE(sys__exit)
{
   ThreadState* tst;
   /* simple; just make this thread exit */
   PRINT("_exit( %ld )", ARG1);
   PRE_REG_READ1(void, "exit", int, exitcode);

   tst = VG_(get_ThreadState)(tid);
   /* Set the thread's status to be exiting and taking out the entire
      process, then claim that the syscall succeeded. */
   tst->exitreason = VgSrc_ExitProcess;
   tst->os_state.exitcode = ARG1;
   SET_STATUS_Success(0);
}

PRE(sys__fp_fpscrx_sc)
{
   PRINT("_fp_fpscrx_sc (BOGUS HANDLER)");
}

PRE(sys__getpgrp)
{
   PRINT("_getpgrp (BOGUS HANDLER)");
}

PRE(sys__getpid)
{
   PRINT("_getpid ( )");
}

PRE(sys__getppid)
{
   PRINT("_getppid ( )");
}

PRE(sys__getpriority)
{
   PRINT("_getpriority (BOGUS HANDLER)");
}

PRE(sys__nsleep)
{
   *flags |= SfMayBlock;
   PRINT("_nsleep( %#lx, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(void, "_nsleep", struct timestruc_t*, arg1,
                                  struct timestruc_t*, arg2);
   /* In 64-bit mode, struct ends in 4 padding bytes.  Hence: */
   if (ARG1)
      PRE_MEM_READ("_nsleep(arg1)", 
                   ARG1, 
                   sizeof(void*)==4 ? sizeof(struct timestruc_t)
                                    : sizeof(struct timestruc_t)-4 );
   if (ARG2)
      PRE_MEM_WRITE("_nsleep(arg2)", ARG2, sizeof(struct timestruc_t));
}
POST(sys__nsleep)
{
   if (ARG2)
      POST_MEM_WRITE(ARG2, sizeof(struct timestruc_t));
}

PRE(sys__pause)
{
  *flags |= SfMayBlock;
  PRINT("_pause ( )");
  PRE_REG_READ0(long, "pause");
}

PRE(sys__poll)
{
   UInt i;
   struct pollfd* ufds = (struct pollfd *)ARG1;
   *flags |= SfMayBlock;
   PRINT("_poll ( %#lx, %ld, %ld )\n", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "_poll",
                 struct pollfd *, ufds, unsigned int, nfds, long, timeout);

   for (i = 0; i < ARG2; i++) {
      PRE_MEM_READ( "poll(ufds.fd)",
                    (Addr)(&ufds[i].fd), sizeof(ufds[i].fd) );
      PRE_MEM_READ( "poll(ufds.events)",
                    (Addr)(&ufds[i].events), sizeof(ufds[i].events) );
      PRE_MEM_WRITE( "poll(ufds.reventss)",
                      (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
   }
}
POST(sys__poll)
{
   if (RES > 0) {
      UInt i;
      struct pollfd* ufds = (struct pollfd *)ARG1;
      for (i = 0; i < ARG2; i++)
         POST_MEM_WRITE( (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
   }
}

PRE(sys__select)
{
   UInt nfds, nmqids;
   *flags |= SfMayBlock;
   /* XXX: copy of generic; I don't know if this is right or not. */
   PRINT("_select ( %ld, %#lx, %#lx, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "_select",
                 int, n, struct sellist *, readfds, 
                         struct sellist *, writefds,
                         struct sellist *, exceptfds, 
                         struct timeval *, timeout);
   nfds   = ((UInt)ARG1) & 0xFFFF;
   nmqids = (((UInt)ARG1) >> 16) & 0xFFFF;

   // XXX: this possibly understates how much memory is read.
   if (ARG2 != 0)
     PRE_MEM_READ( "select(readfds)",   
		   ARG2, nfds/8 /* __FD_SETSIZE/8 */ );
   if (ARG3 != 0)
     PRE_MEM_READ( "select(writefds)",  
		   ARG3, nfds/8 /* __FD_SETSIZE/8 */ );
   if (ARG4 != 0)
     PRE_MEM_READ( "select(exceptfds)", 
		   ARG4, nfds/8 /* __FD_SETSIZE/8 */ );
   if (ARG5 != 0)
     PRE_MEM_READ( "select(timeout)", ARG5, 
                   /* in 64-bit mode, struct timeval has 4 bytes of
                      padding at the end, which tend to not be
                      initialised. */
                   sizeof(void*)==4  ? sizeof(struct timeval)
                                     : sizeof(struct timeval)-4
     );
}

PRE(sys__sem_wait)
{
   *flags |= SfMayBlock;
   PRINT("_sem_wait (BOGUS HANDLER) ( %#lx, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "_sem_wait", void*, arg1, void*, arg2, long, arg3 );
   /* Not sure what the two pointer args are.  Hence no proper handler.*/
}

PRE(sys__setpgid)
{
   PRINT("setpgid ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(int, "setpgid", int, pid, int, pgid);
}

PRE(sys__setsid)
{
   PRINT("setsid ( )");
}

PRE(sys__sigaction) /* COL, more or less */
{
   PRINT("_sigaction ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "_sigaction",
                 int, signum, const struct sigaction *, act,
                 struct sigaction *, oldact);

   if (ARG2 != 0) {
      struct vki_sigaction *sa = (struct vki_sigaction *)ARG2;
      PRE_MEM_READ( "_sigaction(act->sa_handler)", 
                    (Addr)&sa->ksa_handler, sizeof(sa->ksa_handler));
      PRE_MEM_READ( "_sigaction(act->sa_mask)", 
                    (Addr)&sa->sa_mask, sizeof(sa->sa_mask));
      PRE_MEM_READ( "rt_sigaction(act->sa_flags)", 
                    (Addr)&sa->sa_flags, sizeof(sa->sa_flags));
   }
   if (ARG3 != 0)
      PRE_MEM_WRITE( "rt_sigaction(oldact)", ARG3, sizeof(struct vki_sigaction));

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigaction)(ARG1, (const struct vki_sigaction *)ARG2,
                                  (struct vki_sigaction *)ARG3)
   );
}
POST(sys__sigaction)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_sigaction));
}

PRE(sys__thread_self)
{
   PRINT("_thread_self ( )");
}

PRE(sys__thread_setsched)
{
   PRINT("_thread_setsched ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "_thread_setsched", long, arg1, long, arg2, long, arg3);
}

PRE(sys_access)
{
   PRINT("access ( %#lx(%s), %ld )", ARG1,(Char*)ARG1, ARG2);
   PRE_REG_READ2(int, "access", char*, pathname, int, mode);
   PRE_MEM_RASCIIZ( "access(pathname)", ARG1 );
}

PRE(sys_accessx)
{
   PRINT("accessx ( %#lx(%s), %ld, %ld )", ARG1,(Char*)ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "accessx", char*, pathname, int, mode, int, who);
   PRE_MEM_RASCIIZ( "accessx(pathname)", ARG1 );
}

PRE(sys_appgetrlimit)
{
   /* Note: assumes kernel struct == libc struct */
   PRINT("appgetrlimit ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(int, "appgetrlimit", int, arg1, struct rlimit*, arg2);
   PRE_MEM_WRITE( "appgetrlimit(buf)", ARG2, sizeof(struct rlimit) );
}
POST(sys_appgetrlimit)
{
   POST_MEM_WRITE( ARG2, sizeof(struct rlimit) );
}

PRE(sys_appgetrusage)
{
   /* Note: assumes kernel struct == libc struct */
   PRINT("appgetrusage ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(int, "appgetrusage", int, arg1, struct rusage*, arg2);
   PRE_MEM_WRITE( "appgetrusage(buf)", ARG2, sizeof(struct rusage) );
}
POST(sys_appgetrusage)
{
   POST_MEM_WRITE( ARG2, sizeof(struct rusage) );
}

PRE(sys_apprestimer)
{
   PRINT("apprestimer (BOGUS HANDLER)");
}

PRE(sys_appsetrlimit)
{
   PRINT("appsetrlimit (BOGUS HANDLER)");
}

PRE(sys_appulimit)
{
   PRINT("appulimit ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(long, "appulimit", long, arg1, long, arg2);
}

PRE(sys_bind)
{
   PRINT("bind ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "bind", int, socket, 
                              void*, address, int, addresslen);
   /* Hmm.  This isn't really right - see pre_mem_read_sockaddr. */
   PRE_MEM_READ( "bind(address)", ARG2, ARG3 );
}

PRE(sys_chdir)
{
  PRINT("chdir ( %#lx(%s) )", ARG1,(Char*)ARG1);
  PRE_REG_READ1(long, "chdir", const char *, path);
  PRE_MEM_RASCIIZ( "chdir(path)", ARG1 );
}

PRE(sys_chmod)
{
   PRINT("chmod ( %#lx(%s), 0x%lx )", ARG1,(Char*)ARG1, ARG2 );
   PRE_REG_READ2(int, "chmod", char*, path, int, mode);
   PRE_MEM_RASCIIZ( "chmod(path)", ARG1 );
}

PRE(sys_chown)
{
   PRINT("chown ( %#lx(%s), %ld, %ld )", ARG1,(Char*)ARG1, ARG2, ARG3 );
   PRE_REG_READ3(int, "chown", char*, path, int, owner, int, group);
   PRE_MEM_RASCIIZ( "chown(path)", ARG1 );
}

PRE(sys_close)
{
   PRINT("close ( %ld )", ARG1);
   PRE_REG_READ1(void, "close", UInt, fd);
   /* If doing -d style logging (which is to fd=2), don't allow that
      to be closed. */
   if (ARG1 == 2/*stderr*/ && VG_(debugLog_getLevel)() > 0)
      SET_STATUS_Failure( VKI_EBADF );
}

PRE(sys_connext)
{
   /* apparently undocumented.  I don't know what it does. */
   /* Although /usr/include/net/proto_uipc.h does mention it.
      Args are apparently (int, caddr_t, int).  I suspect the
      first arg is a fd and the third a flags value. */
   PRINT("connext (UNDOCUMENTED)( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "connext", int, arg1, caddr_t*, arg2, int, arg3);
}

//--- PRE(sys_execve) ---//
// Pre_read a char** argument.
static void pre_argv_envp(Addr a, ThreadId tid, Char* s1, Char* s2)
{
   while (True) {
      Addr a_deref;
      Addr* a_p = (Addr*)a;
      PRE_MEM_READ( s1, (Addr)a_p, sizeof(Addr) );
      a_deref = *a_p;
      if (0 == a_deref)
         break;
      PRE_MEM_RASCIIZ( s2, a_deref );
      a += sizeof(char*);
   }
}
static SysRes simple_pre_exec_check ( const HChar* exe_name,
                                      Bool trace_this_child )
{
   Int fd, ret;
   SysRes res;
   Bool setuid_allowed;

   // Check it's readable
   res = VG_(open)(exe_name, VKI_O_RDONLY, 0);
   if (res.isError) {
      return res;
   }
   fd = res.res;
   VG_(close)(fd);

   // Check we have execute permissions.  We allow setuid executables
   // to be run only in the case when we are not simulating them, that
   // is, they to be run natively.
   setuid_allowed = trace_this_child  ? False  : True;
   ret = VG_(check_executable)(NULL/*&is_setuid*/,
                               (HChar*)exe_name, setuid_allowed);
   if (0 != ret) {
      return VG_(mk_SysRes_Error)(ret);
   }
   return VG_(mk_SysRes_Success)(0);
}
PRE(sys_execve)
{
   Char*        path = NULL;       /* path to executable */
   Char**       envp = NULL;
   Char**       argv = NULL;
   Char**       arg2copy;
   Char*        launcher_basename = NULL;
   ThreadState* tst;
   Int          i, j, tot_args;
   SysRes       res;
   Bool         trace_this_child;

   PRINT("sys_execve ( %#lx(%s), %#lx, %#lx )", ARG1, (Char*)ARG1, ARG2, ARG3);
   PRE_REG_READ3(vki_off_t, "execve",
                 char *, filename, char **, argv, char **, envp);
   PRE_MEM_RASCIIZ( "execve(filename)", ARG1 );
   if (ARG2 != 0)
      pre_argv_envp( ARG2, tid, "execve(argv)", "execve(argv[i])" );
   if (ARG3 != 0)
      pre_argv_envp( ARG3, tid, "execve(envp)", "execve(envp[i])" );

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Erk.  If the exec fails, then the following will have made a
      mess of things which makes it hard for us to continue.  The
      right thing to do is piece everything together again in
      POST(execve), but that's close to impossible.  Instead, we make
      an effort to check that the execve will work before actually
      doing it. */

   /* Check that the name at least begins in client-accessible storage. */
   /* XXX: causes execve to fail for non-memcheck tools, presumably
      because ARG1 is thought to not to being in client-accessible
      storage due to inadequate address space tracking.  May or may
      not be due to non-tracking of brk. */
   //if (!VG_(am_is_valid_for_client)( ARG1, 1, VKI_PROT_READ )) {
   //   SET_STATUS_Failure( VKI_EFAULT );
   //   return;
   //}
   if (ARG1 == 0 /* obviously bogus */) {
      SET_STATUS_Failure( VKI_EFAULT );
   }

   // Decide whether or not we want to follow along
   trace_this_child = VG_(should_we_trace_this_child)( (HChar*)ARG1 );

   // Do the important checks:  it is a file, is executable, permissions are
   // ok, etc.
   res = simple_pre_exec_check( (const HChar*)ARG1, trace_this_child );
   if (res.isError) {
      SET_STATUS_Failure( res.err );
      return;
   }

   /* If we're tracing the child, and the launcher name looks bogus
      (possibly because launcher.c couldn't figure it out, see
      comments therein) then we have no option but to fail. */
   if (trace_this_child 
       && (VG_(name_of_launcher) == NULL
           || VG_(name_of_launcher)[0] != '/')) {
      SET_STATUS_Failure( VKI_ECHILD ); /* "No child processes" */
      return;
   }

   /* After this point, we can't recover if the execve fails. */
   VG_(debugLog)(1, "syswrap", "Exec of %s\n", (Char*)ARG1);

   // Terminate gdbserver if it is active.
   if (VG_(clo_vgdb)  != Vg_VgdbNo) {
      // If the child will not be traced, we need to terminate gdbserver
      // to cleanup the gdbserver resources (e.g. the FIFO files).
      // If child will be traced, we also terminate gdbserver: the new 
      // Valgrind will start a fresh gdbserver after exec.
      VG_(gdbserver) (0);
   }

   /* Resistance is futile.  Nuke all other threads.  POSIX mandates
      this. (Really, nuke them all, since the new process will make
      its own new thread.) */
   VG_(nuke_all_threads_except)( tid, VgSrc_ExitThread );
   VG_(reap_threads)(tid);

   // Set up the child's exe path.
   //
   if (trace_this_child) {

      // We want to exec the launcher.  Get its pre-remembered path.
      path = VG_(name_of_launcher);
      // VG_(name_of_launcher) should have been acquired by m_main at
      // startup.
      vg_assert(path);

      launcher_basename = VG_(strrchr)(path, '/');
      if (launcher_basename == NULL || launcher_basename[1] == 0) {
         launcher_basename = path;  // hmm, tres dubious
      } else {
         launcher_basename++;
      }

   } else {
      path = (Char*)ARG1;
   }

   // Set up the child's environment.
   //
   // Remove the valgrind-specific stuff from the environment so the
   // child doesn't get vgpreload_core.so, vgpreload_<tool>.so, etc.  
   // This is done unconditionally, since if we are tracing the child,
   // the child valgrind will set up the appropriate client environment.
   // Nb: we make a copy of the environment before trying to mangle it
   // as it might be in read-only memory (this was bug #101881).
   //
   // Then, if tracing the child, set VALGRIND_LIB for it.
   //
   if (ARG3 == 0) {
      envp = NULL;
   } else {
      envp = VG_(env_clone)( (Char**)ARG3 );
      if (envp == NULL) goto hosed;
      VG_(env_remove_valgrind_env_stuff)( envp );
   }

   if (trace_this_child) {
      // Set VALGRIND_LIB in ARG3 (the environment)
      VG_(env_setenv)( &envp, VALGRIND_LIB, VG_(libdir));
   }

   // Set up the child's args.  If not tracing it, they are
   // simply ARG2.  Otherwise, they are
   //
   // [launcher_basename] ++ VG_(args_for_valgrind) ++ [ARG1] ++ ARG2[1..]
   //
   // except that the first VG_(args_for_valgrind_noexecpass) args
   // are omitted.
   //
   if (!trace_this_child) {
      argv = (Char**)ARG2;
   } else {
      vg_assert( VG_(args_for_valgrind_noexecpass) >= 0 );
      vg_assert( VG_(args_for_valgrind_noexecpass) 
                   <= VG_(sizeXA)( VG_(args_for_valgrind) ) );
      /* how many args in total will there be? */
      // launcher basename
      tot_args = 1;
      // V's args
      tot_args += VG_(sizeXA)( VG_(args_for_valgrind) );
      tot_args -= VG_(args_for_valgrind_noexecpass);
      // name of client exe
      tot_args++;
      // args for client exe, skipping [0]
      arg2copy = (Char**)ARG2;
      if (arg2copy && arg2copy[0]) {
         for (i = 1; arg2copy[i]; i++)
            tot_args++;
      }
      // allocate
      argv = VG_(malloc)( "syswrap-aix5.pre_sys_execve.1",
                          (tot_args+1) * sizeof(HChar*) );
      if (argv == 0) goto hosed;
      // copy
      j = 0;
      argv[j++] = launcher_basename;
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         if (i < VG_(args_for_valgrind_noexecpass))
            continue;
         argv[j++] = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      }
      argv[j++] = (Char*)ARG1;
      if (arg2copy && arg2copy[0])
         for (i = 1; arg2copy[i]; i++)
            argv[j++] = arg2copy[i];
      argv[j++] = NULL;
      // check
      vg_assert(j == tot_args+1);
   }

   /* restore the DATA rlimit for the child */
   VG_(setrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));

   /*
      Set the signal state up for exec.

      We need to set the real signal state to make sure the exec'd
      process gets SIG_IGN properly.

      Also set our real sigmask to match the client's sigmask so that
      the exec'd child will get the right mask.  First we need to
      clear out any pending signals so they they don't get delivered,
      which would confuse things.

      XXX This is a bug - the signals should remain pending, and be
      delivered to the new process after exec.  There's also a
      race-condition, since if someone delivers us a signal between
      the sigprocmask and the execve, we'll still get the signal. Oh
      well.
   */
   {
      vki_sigset_t allsigs;
      vki_siginfo_t info;

      for (i = 1; i < VG_(max_signal); i++) {
         struct vki_sigaction sa;
         VG_(do_sys_sigaction)(i, NULL, &sa);
         if (sa.ksa_handler == VKI_SIG_IGN)
            VG_(sigaction)(i, &sa, NULL);
         else {
            sa.ksa_handler = VKI_SIG_DFL;
            VG_(sigaction)(i, &sa, NULL);
         }
      }

      VG_(sigfillset)(&allsigs);
      while(VG_(sigtimedwait_zero)(&allsigs, &info) > 0)
         ;

      VG_(sigprocmask)(VKI_SIG_SETMASK, &tst->sig_mask, NULL);
   }

   if (0) {
      Char **cpp;
      VG_(printf)("exec: %s\n", path);
      for (cpp = argv; cpp && *cpp; cpp++)
         VG_(printf)("argv: %s\n", *cpp);
      if (0)
         for (cpp = envp; cpp && *cpp; cpp++)
            VG_(printf)("env: %s\n", *cpp);
   }

   SET_STATUS_from_SysRes( 
      VG_(do_syscall3)(__NR_execve, (UWord)path, (UWord)argv, (UWord)envp) 
   );

   /* If we got here, then the execve failed.  We've already made way
      too much of a mess to continue, so we have to abort. */
  hosed:
   vg_assert(FAILURE);
   VG_(message)(Vg_UserMsg, "execve(%#lx(%s), %#lx, %#lx) failed, errno %ld\n",
                ARG1, (Char*)ARG1, ARG2, ARG3, ERR);
   VG_(message)(Vg_UserMsg, "EXEC FAILED: I can't recover from "
                            "execve() failing, so I'm dying.\n");
   VG_(message)(Vg_UserMsg, "Add more stringent tests in PRE(sys_execve), "
                            "or work out how to recover.\n");
   VG_(exit)(101);
}

PRE(sys_finfo)
{
   PRINT("finfo ( %#lx(%s), %ld, %#lx, %ld )",
          ARG1,(Char*)ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "finfo", 
                      char*, Path1, int, cmd, void*, buffer, int, length);
   PRE_MEM_RASCIIZ( "finfo(Path1)", ARG1 );
   PRE_MEM_WRITE( "finfo(buffer)", ARG3, ARG4 );
}
POST(sys_finfo)
{
   POST_MEM_WRITE( ARG3, ARG4 );
}

PRE(sys_fstatfs)
{
   PRINT("sys_fstatfs ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(UWord, "fstatfs", UWord, fd, struct statfs *, buf);
   PRE_MEM_WRITE( "fstatfs(buf)", ARG2, sizeof(struct statfs) );
}
POST(sys_fstatfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct statfs) );
}

PRE(sys_fstatx)
{
   PRINT("fstatx ( %ld, %#lx, %ld, %ld )", ARG1, ARG2, ARG3, ARG4 );
   PRE_REG_READ4(Word, "fstatx", UWord, fd, void*, buf,
                                 UWord, len, UWord, cmd);
   PRE_MEM_WRITE( "fstatx(buf)", ARG2, ARG3 );
}
POST(sys_fstatx)
{
   POST_MEM_WRITE( ARG2, ARG3 );
}

PRE(sys_fsync)
{
   PRINT("fsync ( %ld )", ARG1);
   PRE_REG_READ1(int, "fsync", int, fd);
}

PRE(sys_getdirent)
{
   *flags |= SfMayBlock;
   /* this is pretty much like 'read':
      getdirent(fd, buffer, nbytes) -> # actually read */
   PRINT("getdirent ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(Word, "getdirent", UWord, fd, UChar*, buf, UWord, count);
   PRE_MEM_WRITE( "getdirent(buf)", ARG2, ARG3 );
}
POST(sys_getdirent)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_getdirent64)
{
   /* same as getdirent, from our point of view? */
   *flags |= SfMayBlock;
   /* this is pretty much like 'read':
      getdirent(fd, buffer, nbytes) -> # actually read */
   PRINT("getdirent64 ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(Word, "getdirent64", UWord, fd, UChar*, buf, UWord, count);
   PRE_MEM_WRITE( "getdirent64(buf)", ARG2, ARG3 );
}
POST(sys_getdirent64)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_getdomainname)
{
   PRINT("getdomainname ( %#lx, %ld )", ARG1, ARG2 );
   PRE_MEM_WRITE( "getdomainname(buf)", ARG1, ARG2 );
}
POST(sys_getdomainname)
{
   POST_MEM_WRITE( ARG1, ARG2 );
}

PRE(sys_getgidx)
{
   PRINT("getgidx ( %ld )", ARG1);
   PRE_REG_READ1(UInt, "getgidx", long, arg1);
}

PRE(sys_getgroups)
{
   PRINT("getgroups ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "getgroups", int, size, gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_WRITE( "getgroups(list)", ARG2, ARG1 * sizeof(gid_t) );
}
POST(sys_getgroups)
{
   vg_assert(SUCCESS);
   if (ARG1 > 0 && RES > 0)
      POST_MEM_WRITE( ARG2, RES * sizeof(gid_t) );
}

PRE(sys_gethostname)
{
   PRINT("gethostname ( %#lx, %ld )", ARG1, ARG2);
   PRE_MEM_WRITE( "gethostname(buf)", ARG1, ARG2 );
}
POST(sys_gethostname)
{
   POST_MEM_WRITE( ARG1, ARG2 );
}

PRE(sys_getpriv)
{
   PRINT("getpriv (UNDOCUMENTED)(%ld, %#lx, %ld)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "getpriv", int, arg1, void*, arg2, int, arg3);
   PRE_MEM_WRITE( "getpriv(arg2)", ARG2, 8 );
}
POST(sys_getpriv)
{
   if (ARG2)
      POST_MEM_WRITE(ARG2, 8);
}

/* Note that this is used for both sys_getprocs and sys_getprocs64.  I
   think that's correct - from the man page, the calling conventions
   look identical. */
PRE(sys_getprocs)
{
   PRINT("getprocs ( %#lx, %ld, %#lx, %ld, %#lx, %ld )",
         ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(int, "getprocs", 
                 void*, processbuffer, long, processize, 
                 void*, filebuffer, long, filesize,
                 void*, indexpointer, long, count);

   /* (processbuffer, processsize, filebuffer, filesize,
      indexpointer, count) */
   PRE_MEM_READ( "getprocs(IndexPointer)", ARG5, sizeof(UInt) );
   if (ARG1)
      PRE_MEM_WRITE( "getprocs(ProcessBuffer)", ARG1, ARG2 * ARG6 );
   if (ARG3)
      PRE_MEM_WRITE( "getprocs(FileBuffer)", ARG3, ARG4 * ARG6 );
}
POST(sys_getprocs)
{
   vg_assert(SUCCESS);
   if (ARG1)
      POST_MEM_WRITE( ARG1, ARG2 * ARG6 );
   if (ARG3)
      POST_MEM_WRITE( ARG3, ARG4 * ARG6 );
}

PRE(sys_getrpid)
{
   PRINT("getrpid ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getrpid", long, arg1, long, arg2, long, arg3);
}

PRE(sys_getsockopt)
{
   PRINT("getsockopt ( %ld, %ld, %ld, %#lx, %#lx )", 
         ARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(int, "getsockopt", int, socket, int, level, 
                                    int, optionname, 
                                    void*, optionval, int*, optionlen);
   if (ARG5) {
      PRE_MEM_READ( "getsockopt(optionlen)", ARG5, sizeof(UInt) );
      PRE_MEM_WRITE( "getsockopt(optionval)", ARG4, *(UInt*)ARG5 );
   }
}
POST(sys_getsockopt)
{
   if (ARG5) {
      POST_MEM_WRITE( ARG5, sizeof(UInt) );
      POST_MEM_WRITE( ARG4, *(UInt*)ARG5 );
   }
}

PRE(sys_gettimerid)
{
   PRINT("gettimerid ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(int, "gettimerid", int, timertype, int, notifytype);
}

PRE(sys_getuidx)
{
   PRINT("getuidx ( %ld )", ARG1);
   PRE_REG_READ1(UInt, "getuidx", UInt, arg1);
}

PRE(sys_incinterval)
{
   PRINT("incinterval ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "incinterval", int, timerid, 
                      struct itimerstruc_t*, value,
                      struct itimerstruc_t*, ovalue);
   if (ARG2)
      PRE_MEM_READ( "incinterval(value)", 
                    ARG2, sizeof(struct itimerstruc_t));
   if (ARG3)
      PRE_MEM_WRITE( "incinterval(value)", 
                     ARG3, sizeof(struct itimerstruc_t));
}
POST(sys_incinterval)
{
   if (ARG3)
      POST_MEM_WRITE( ARG3, sizeof(struct itimerstruc_t));
}

PRE(sys_kfcntl)
{
   *flags |= SfMayBlock;
   switch (ARG2) {
      // These ones ignore ARG3.
      case F_GETFD:
      case F_GETFL:
      case F_GETOWN:
         PRINT("kfcntl ( %ld, %ld )", ARG1,ARG2);
         PRE_REG_READ2(long, "fcntl", unsigned int, fd, unsigned int, cmd);
         break;

      // These ones use ARG3 as "arg".
      case F_DUPFD:
      case F_SETFD:
      case F_SETFL:
      case F_SETOWN:
         PRINT("kfcntl[ARG3=='arg'] ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
         PRE_REG_READ3(long, "fcntl",
                       unsigned int, fd, unsigned int, cmd, unsigned long, arg);
         break;

      // These ones use ARG3 as "lock".
#     if !defined(VGP_ppc64_aix5)
      case F_GETLK:
      case F_SETLK:
      case F_SETLKW:
#     endif
      case F_GETLK64:
      case F_SETLK64:
      case F_SETLKW64:
         PRINT("kfcntl[ARG3=='lock'] ( %ld, %ld, %#lx )", ARG1,ARG2,ARG3);
         PRE_REG_READ3(long, "fcntl",
                       unsigned int, fd, unsigned int, cmd,
                       struct flock64 *, lock);
         if (ARG3 && (ARG2 == F_GETLK || ARG2 == F_GETLK64))
            PRE_MEM_READ( "kfcntl(F_GETLK)", ARG3, sizeof(struct flock64) );
         break;
   }
}
POST(sys_kfcntl)
{
  //  if (ARG2 == VKI_F_DUPFD) {
  //   if (!ML_(fd_allowed)(RES, "fcntl(DUPFD)", tid, True)) {
  //    VG_(close)(RES);
  //    SET_STATUS_Failure( VKI_EMFILE );
  //  } else {
  //    if (VG_(clo_track_fds))
  //	record_fd_open_named(tid, RES);
  //  }
  // }
   if (ARG3 && (ARG2 == F_GETLK || ARG2 == F_GETLK64))
      POST_MEM_WRITE( ARG3, sizeof(struct flock64) );
}

/* COG; can this be moved inside the pre-handler? */
static vki_sigset_t fork_saved_mask; 
PRE(sys_kfork) /* COPY OF GENERIC */
{
   vki_sigset_t mask;

   PRINT("kfork ( )");
   PRE_REG_READ0(long, "fork");

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   VG_(do_atfork_pre)(tid);

   SET_STATUS_from_SysRes( VG_(do_syscall0)(__NR_fork) );

   if (SUCCESS && RES == 0) {
      /* child */
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);

      /* If --child-silent-after-fork=yes was specified, set the
         logging file descriptor to an 'impossible' value.  This is
         noticed by send_bytes_to_logging_sink in m_libcprint.c, which
         duly stops writing any further logging output. */
      if (!VG_(logging_to_socket) && VG_(clo_child_silent_after_fork))
         VG_(clo_log_fd) = -1;
   } 
   else 
   if (SUCCESS && RES > 0) {
      /* parent */
      VG_(do_atfork_parent)(tid);

      PRINT("   fork: process %d created child %lu\n", VG_(getpid)(), RES);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }
}

PRE(sys_kftruncate)
{
   PRINT("kftruncate (BOGUS HANDLER)");
}

PRE(sys_kgetsidx)
{
   PRINT("kgetsidx ( %ld )", ARG1);
   PRE_REG_READ1(Word, "kgetsidx", Word, arg1);
}

PRE(sys_kill)
{
   PRINT("kill ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(int, "kill", int, pid, int, signal);
}

PRE(sys_kioctl)
{
   *flags |= SfMayBlock;
   PRINT("kioctl ( %ld, %#lx, %#lx, %#lx )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(Word, "ioctl", Word, fd, 
                                Word, command, Word, arg, Word, ext);
   switch (ARG2 /* request */) {
      case 0x5800/*TXISATTY*/:
      case 0x5801/*TXTTYNAME*/:
         break;
      case 0x412:/*no idea what any of these are*/
      case 0x430:
      case 0x431:
      case 0x432:
      case 0x441:
      case 0x442:
      case 0x462:
      case 0x480:
      case 0x482:
      case 0x738:
      case 0x736:
      case 0x73B:
      case 0x73C:
      case 0x73D:
      case 0x73E:
      case 0x5401:
      case 0x5403:
      case 0xFF01/*no_idea_at_all_what_this_is*/:
          break;
      /* We don't have any specific information on it, so
         try to do something reasonable based on direction and
         size bits.

         According to Simon Hausmann, _IOC_READ means the kernel
         writes a value to the ioctl value passed from the user
         space and the other way around with _IOC_WRITE. */
      default: {
         UInt dir  = _VKI_IOC_DIR(ARG2);
         UInt size = _VKI_IOC_SIZE(ARG2);
         if (VG_(strstr)(VG_(clo_sim_hints), "lax-ioctls") != NULL) {
            /* 
             * Be very lax about ioctl handling; the only
             * assumption is that the size is correct. Doesn't
             * require the full buffer to be initialized when
             * writing.  Without this, using some device
             * drivers with a large number of strange ioctl
             * commands becomes very tiresome.
             */
         } else if (/* size == 0 || */ dir == _VKI_IOC_NONE) {
            static Int moans = 5;
            if (moans > 0 && !VG_(clo_xml)) {
               moans--;
               VG_(message)(Vg_UserMsg, 
                            "Warning: noted but unhandled ioctl 0x%lx"
                            " with no size/direction hints\n",
                            ARG2); 
               VG_(message)(Vg_UserMsg, 
                            "   This could cause spurious value errors"
                            " to appear.\n");
               VG_(message)(Vg_UserMsg, 
                            "   See README_MISSING_SYSCALL_OR_IOCTL for "
                            "guidance on writing a proper wrapper.\n" );
            }
         } else {
            if ((dir & _VKI_IOC_WRITE) && size > 0)
               PRE_MEM_READ( "ioctl(generic)", ARG3, size);
            if ((dir & _VKI_IOC_READ) && size > 0)
               PRE_MEM_WRITE( "ioctl(generic)", ARG3, size);
         }
         break;
      }
   } /* switch */ 
}
POST(sys_kioctl)
{
   switch (ARG2 /*request*/) {
      case 0xFF01:
         /* 100% kludge.  I have no idea what this ioctl is.  IOCINFO
            ?  But at a guess I'd say it returns some kind of info
            from the kernel. */
         if (ARG3) POST_MEM_WRITE(ARG3, 16);
         break;
      case 0x738: /* Shows up in MPI applications. */
         if (ARG3) POST_MEM_WRITE(ARG3, 4*sizeof(Word));
         break;
      case 0x736: /* Shows up in MPI applications. */
      case 0x73B: /* Shows up in MPI applications. */
      case 0x73C: /* Shows up in MPI applications. */
         if (ARG3) POST_MEM_WRITE(ARG3, 16);
         /* in fact only 4 needed, but being conservative */
         break;

      case 0x5401:
	/* some kind of tty thing */
	if (ARG3) POST_MEM_WRITE(ARG3, 32);
	break;

      case 0x5801/*TXTTYNAME*/:
	/* who knows if this is right.  Presumably an ascii string is
	   written into the buffer specified by ARG3, but how long is
	   that buffer? */
	if (ARG3) POST_MEM_WRITE(ARG3, 16);
        break;

      case 0x412:
      case 0x430:
      case 0x431:
      case 0x432:
      case 0x441:
      case 0x442:
      case 0x462:
      case 0x480:
      case 0x482:
      case 0x73D:
      case 0x73E:
      case 0x5800/*TXISATTY*/:
      case 0x5403:
         break;
      /* We don't have any specific information on it, so
         try to do something reasonable based on direction and
         size bits.

         According to Simon Hausmann, _IOC_READ means the kernel
         writes a value to the ioctl value passed from the user
         space and the other way around with _IOC_WRITE. */
      default: {
         UInt dir  = _VKI_IOC_DIR(ARG2);
         UInt size = _VKI_IOC_SIZE(ARG2);
         if (size > 0 && (dir & _VKI_IOC_READ)
             && RES == 0 
             && ARG3 != (Addr)NULL)
            POST_MEM_WRITE(ARG3, size);
         break;
      }
   }
}

PRE(sys_klseek)
{
   PRINT("klseek ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "klseek", 
                 long, fd, long, offset, long, whence, void*, arg4);
   /* XXX: looks like 4th arg is a pointer to something.  Is it
      read or written by the kernel? */
}

PRE(sys_knlist)
{
   PRINT("knlist (BOGUS HANDLER)");
}

PRE(sys_kpread)
{
   *flags |= SfMayBlock;
   PRINT("sys_kpread ( %ld, %#lx, %llu, %lld )",
         ARG1, ARG2, (ULong)ARG3, (ULong)ARG4);
   PRE_REG_READ4(ssize_t, "kpread",
                 unsigned int, fd, char *, buf,
                 vki_size_t, count, long, offset);
   PRE_MEM_WRITE( "kpread(buf)", ARG2, ARG3 );
}
POST(sys_kpread)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      POST_MEM_WRITE( ARG2, RES );
   }
}

PRE(sys_kread)
{
   *flags |= SfMayBlock;
   PRINT("sys_read ( %ld, %#lx, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "read",
                 unsigned int, fd, char *, buf, vki_size_t, count);
   //zz   if (!ML_(fd_allowed)(ARG1, "read", tid, False))
   //zz      SET_STATUS_Failure( VKI_EBADF );
   //zz   else
      PRE_MEM_WRITE( "read(buf)", ARG2, ARG3 );
}
POST(sys_kread)
{
  vg_assert(SUCCESS);
  POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_kreadv)
{
   Int i;
   struct vki_iovec * vec;
   *flags |= SfMayBlock;
   /* ssize_t readvx ( int fd, struct iovec*, int iovCount, int extension ) */
   PRINT("kreadv ( %ld, %#lx, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(ssize_t, "kreadv",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, iovCount, unsigned long, extension);
   //zz   if (!ML_(fd_allowed)(ARG1, "readv", tid, False)) {
   //zz      SET_STATUS_Failure( VKI_EBADF );
   //zz   } else {
      PRE_MEM_READ( "kreadv(vector)", ARG2, ARG3 * sizeof(struct vki_iovec) );
      if (ARG2 != 0) {
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct vki_iovec *)ARG2;
         for (i = 0; i < (Int)ARG3; i++)
            PRE_MEM_WRITE( "kreadv(vector[...])",
                           (Addr)vec[i].iov_base, vec[i].iov_len );
      }
   //zz }
}
POST(sys_kreadv)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      Int i;
      struct vki_iovec * vec = (struct vki_iovec *)ARG2;
      Int remains = RES;

      /* RES holds the number of bytes read. */
      for (i = 0; i < (Int)ARG3; i++) {
         Int nReadThisBuf = vec[i].iov_len;
         if (nReadThisBuf > remains) nReadThisBuf = remains;
         POST_MEM_WRITE( (Addr)vec[i].iov_base, nReadThisBuf );
         remains -= nReadThisBuf;
         if (remains < 0) VG_(core_panic)("readv: remains < 0");
      }
   }
}

PRE(sys_kthread_ctl)
{
   *flags |= SfMayBlock;
   PRINT("kthread_ctl (BOGUS HANDLER)");
}

PRE(sys_ktruncate)
{
   PRINT("ktruncate( %#lx(%s), %lx, %lx )", ARG1,(Char*)ARG1, ARG2, ARG3 );
   PRE_REG_READ3(int, "ktruncate", char*, path, long, arg2, long, arg3 );
   PRE_MEM_RASCIIZ( "ktruncate(path)", ARG1 );
}

PRE(sys_kwaitpid)
{
   /* Note: args 1 and 2 (status, pid) opposite way round
      from generic handler */
   *flags |= SfMayBlock;
   PRINT("kwaitpid ( %#lx, %ld, %ld, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ3(long, "waitpid", 
                 unsigned int *, status, int, pid, int, options);

   if (ARG1 != (Addr)NULL)
      PRE_MEM_WRITE( "kwaitpid(status)", ARG1, sizeof(int) );
}
POST(sys_kwaitpid)
{
   if (ARG1 != (Addr)NULL)
      POST_MEM_WRITE( ARG1, sizeof(int) );
}

PRE(sys_kwrite)
{
   //zz   Bool ok;
   *flags |= SfMayBlock;
   PRINT("sys_kwrite ( %ld, %#lx, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "kwrite",
                 unsigned int, fd, const char *, buf, vki_size_t, count);
   /* check to see if it is allowed.  If not, try for an exemption from
      --sim-hints=enable-outer (used for self hosting). */
   //zz   ok = ML_(fd_allowed)(ARG1, "write", tid, False);
   //zz   if (!ok && ARG1 == 2/*stderr*/ 
   //zz           && VG_(strstr)(VG_(clo_sim_hints),"enable-outer"))
   //zz      ok = True;
   //zz   if (!ok)
   //zz      SET_STATUS_Failure( VKI_EBADF );
   //zz   else
      PRE_MEM_READ( "write(buf)", ARG2, ARG3 );
}

PRE(sys_kwritev)
{
   PRINT("kwritev (BOGUS HANDLER)");
}

PRE(sys_listen)
{
   PRINT("listen (BOGUS HANDLER)");
}

PRE(sys_loadbind)
{
   PRINT("loadbind( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "loadbind", int, flag,
                      void*, ExportPointer, void*, ImportPointer);
}

PRE(sys_loadquery)
{
   /* loadquery ( int flags, void* buffer, unsigned int bufferlength ) */
   PRINT("loadquery ( %#lx, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_MEM_WRITE( "loadquery(buf)", ARG2, ARG3 );
}
POST(sys_loadquery)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, ARG3 );
}

PRE(sys_lseek)
{
   PRINT("lseek (%ld, %ld, %ld)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "lseek", long, fd, long, offset, long, whence);
}

PRE(sys_mkdir)
{
   PRINT("mkdir (%#lx(%s), %#lx)", ARG1,(Char*)ARG1, ARG2);
   PRE_REG_READ2(int, "mkdir", char*, path, int, mode);
   PRE_MEM_RASCIIZ( "mkdir(path)", ARG1 );
}

PRE(sys_mmap)
{
   PRINT("mmap ( %#lx, %ld, %#lx, %#lx, %ld, %ld )",
         ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(void*, "mmap", void*, addr, int, len, 
                        int, prot, int, flags, int, fd, int, off);
}
POST(sys_mmap)
{
   vg_assert(SUCCESS);
   Addr  addr  = (Addr)RES;
   UWord len   = (UWord)ARG2;
   UWord prot  = (UWord)ARG3;
   UWord flags = (UWord)ARG4;
   Bool r = (prot & VKI_PROT_READ) > 0;
   Bool w = (prot & VKI_PROT_WRITE) > 0;
   Bool x = (prot & VKI_PROT_EXEC) > 0;
   VG_TRACK( new_mem_mmap, addr, len, r,w,x, 0/*di_handle*/ );
   Bool d = VG_(am_notify_client_mmap)( addr, len, prot, flags, 
                                        0/*fake fd*/, 0/*fake offset*/);
   if (d) 
      VG_(discard_translations)( addr, len, "POST(sys_mmap)" );
}

PRE(sys_mntctl)
{
   PRINT("mntctl ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(long, "mntctl", long, command, long, size, char*, buffer);
   PRE_MEM_WRITE( "mntctl(buffer)", ARG3, ARG2 );
}
POST(sys_mntctl)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      /* Buffer too small.  First word is the real required size. */
      POST_MEM_WRITE( ARG3, sizeof(Word) );
   } else {
      /* RES is the number of struct vmount's written to the buf.  But
         these are variable length and to find the end would require
         inspecting each in turn.  So be simple and just mark the
         entire buffer as defined. */
      POST_MEM_WRITE( ARG3, ARG2 );
   }
}

PRE(sys_mprotect)
{
   PRINT("mprotect (BOGUS HANDLER)( %#lx, %ld, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "mprotect", void*, addr, long, len, long, prot);
}
POST(sys_mprotect)
{
   Bool d;
   vg_assert(SUCCESS);
   Addr  addr = ARG1;
   UWord len  = ARG2;
   UWord prot = ARG3;
   d = VG_(am_notify_mprotect)( addr, len, prot );
   if (d)
      VG_(discard_translations)( addr, len, "POST(sys_mprotect)" );
}

PRE(sys_munmap)
{
   PRINT("munmap ( %#lx, %ld )", ARG1, ARG2);
   PRE_REG_READ2(int, "munmap", void*, addr, long, len);
}
POST(sys_munmap)
{
   Bool d;
   vg_assert(SUCCESS);
   Addr  addr = ARG1;
   UWord len  = ARG2;
   VG_TRACK( die_mem_munmap, addr, len );
   d = VG_(am_notify_munmap)( addr, len );
   if (d)
      VG_(discard_translations)( addr, len, "POST(sys_munmap)" );
}

PRE(sys_naccept)
{
   PRINT("naccept (%ld, %#lx, %#lx)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "naccept", int, socket, char*, addr, int*, addrlen);
   PRE_MEM_READ( "naccept(addrlen)", ARG3, sizeof(UInt) );
   PRE_MEM_WRITE( "naccept(addr)", ARG2, *(UInt*)ARG3 );
}
POST(sys_naccept)
{
   POST_MEM_WRITE( ARG3, sizeof(UInt) );
   POST_MEM_WRITE( ARG2, *(UInt*)ARG3 );
}

PRE(sys_ngetpeername)
{
   PRINT("ngetpeername ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "ngetpeername", int, fd, char*, name, int*, namelen);
   PRE_MEM_READ( "ngetpeername(namelen)", ARG3, sizeof(UInt) );
   PRE_MEM_WRITE( "ngetpeername(name)", ARG2, *(UInt*)ARG3 );
}
POST(sys_ngetpeername)
{
   POST_MEM_WRITE( ARG3, sizeof(UInt) );
   POST_MEM_WRITE( ARG2, *(UInt*)ARG3 );
}

PRE(sys_ngetsockname)
{
   PRINT("ngetsockname ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "ngetsockname", int, fd, char*, name, int*, namelen);
   PRE_MEM_READ( "ngetsockname(namelen)", ARG3, sizeof(UInt) );
   PRE_MEM_WRITE( "ngetsockname(name)", ARG2, *(UInt*)ARG3 );
}
POST(sys_ngetsockname)
{
   POST_MEM_WRITE( ARG3, sizeof(UInt) );
   POST_MEM_WRITE( ARG2, *(UInt*)ARG3 );
}

PRE(sys_nrecvfrom)
{
   *flags |= SfMayBlock;
   PRINT("nrecvfrom ( %ld, %#lx, %ld, %ld, %#lx, %#lx )",
         ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(ssize_t, "nrecvfrom",
                 int, s, void*, buf, size_t, len, int, flags,
                 void*, from, UInt*, fromlen);
   PRE_MEM_WRITE( "nrecvfrom(buf)", ARG2, ARG3 );
   if (ARG5) {
      PRE_MEM_READ( "nrecvfrom(fromlen)", ARG6, sizeof(UInt) );
      PRE_MEM_WRITE( "nrecvfrom(from)", ARG5, *(UInt*)ARG6 );
   }
}
POST(sys_nrecvfrom)
{
   POST_MEM_WRITE( ARG2, RES );
   if (ARG5) {
      POST_MEM_WRITE(ARG6, sizeof(UInt));
      POST_MEM_WRITE(ARG5, *(UInt*)ARG6);
   }
}

PRE(sys_nrecvmsg)
{
   *flags |= SfMayBlock;
   PRINT("nrecvmsg(BOGUS HANDLER)( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "nrecvmsg", long, arg1, void*, arg2, long, arg3);
}

PRE(sys_nsendmsg)
{
   *flags |= SfMayBlock;
   PRINT("nsendmsg(BOGUS HANDLER)( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
}

PRE(sys_open) /* XXX CoG */
{
   //zz   HChar  name[30];
   //zz   SysRes sres;

   if (ARG2 & VKI_O_CREAT) {
      // 3-arg version
      PRINT("sys_open ( %#lx(%s), %#lx, %ld )",ARG1,(Char*)ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "open",
                    const char *, filename, int, flags, int, mode);
   } else {
      // 2-arg version
      PRINT("sys_open ( %#lx(%s), %#lx )",ARG1,(Char*)ARG1,ARG2);
      PRE_REG_READ2(long, "open",
                    const char *, filename, int, flags);
   }
   PRE_MEM_RASCIIZ( "open(filename)", ARG1 );

   //zz   /* Handle the case where the open is of /proc/self/cmdline or
   //zz      /proc/<pid>/cmdline, and just give it a copy of the fd for the
   //zz      fake file we cooked up at startup (in m_main).  Also, seek the
   //zz      cloned fd back to the start. */
   //zz
   //zz   VG_(sprintf)(name, "/proc/%d/cmdline", VG_(getpid)());
   //zz   if (ML_(safe_to_deref)( (void*)ARG1, 1 )
   //zz       && (VG_(strcmp)((Char *)ARG1, name) == 0 
   //zz           || VG_(strcmp)((Char *)ARG1, "/proc/self/cmdline") == 0)) {
   //zz      sres = VG_(dup)( VG_(cl_cmdline_fd) );
   //zz      SET_STATUS_from_SysRes( sres );
   //zz      if (!sres.isError) {
   //zz         OffT off = VG_(lseek)( sres.res, 0, VKI_SEEK_SET );
   //zz         if (off < 0)
   //zz            SET_STATUS_Failure( VKI_EMFILE );
   //zz      }
   //zz      return;
   //zz   }

   /* Otherwise handle normally */
   *flags |= SfMayBlock;
}
POST(sys_open)
{
   vg_assert(SUCCESS);
   //zz   if (!ML_(fd_allowed)(RES, "open", tid, True)) {
   //zz      VG_(close)(RES);
   //zz      SET_STATUS_Failure( VKI_EMFILE );
   //zz   } else {
   //zz      if (VG_(clo_track_fds))
   //zz         ML_(record_fd_open_with_given_name)(tid, RES, (Char*)ARG1);
   //zz   }
}

PRE(sys_pipe)
{
   PRINT("sys_pipe ( %#lx )", ARG1);
   PRE_REG_READ1(int, "pipe", int *, filedes);
   PRE_MEM_WRITE( "pipe(filedes)", ARG1, 2*sizeof(int) );
}
POST(sys_pipe)
{
  //zz   Int *p = (Int *)ARG1;

  //zz  if (!ML_(fd_allowed)(p[0], "pipe", tid, True) ||
  //zz      !ML_(fd_allowed)(p[1], "pipe", tid, True)) {
  //zz    VG_(close)(p[0]);
  //zz    VG_(close)(p[1]);
  //zz    SET_STATUS_Failure( VKI_EMFILE );
  //zz  } else {
    POST_MEM_WRITE( ARG1, 2*sizeof(int) );
    //zz    if (VG_(clo_track_fds)) {
    //zz      ML_(record_fd_open_nameless)(tid, p[0]);
    //zz      ML_(record_fd_open_nameless)(tid, p[1]);
    //zz    }
    //zz  }
}

PRE(sys_privcheck)
{
   PRINT("privcheck ( %ld )", ARG1);
   PRE_REG_READ1(int, "privcheck", int, arg1);
}

PRE(sys_readlink)
{
   PRINT("readlink ( 0x%lx(%s),0x%lx,%ld )", ARG1,(Char*)ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "readlink",
                 const char *, path, char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ( "readlink(path)", ARG1 );
   PRE_MEM_WRITE( "readlink(buf)", ARG2,ARG3 );
}
POST(sys_readlink)
{
   POST_MEM_WRITE( ARG2, RES + 1 );
}

PRE(sys_recv)
{
   *flags |= SfMayBlock;
   PRINT("recv ( %ld, %#lx, %ld, %ld )",
         ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "recv", int, fd, void*, buf, int, len, int, flags);
   PRE_MEM_WRITE( "recv(buf)", ARG2, ARG3);
}
POST(sys_recv)
{
   if (RES > 0)
      POST_MEM_WRITE(ARG2, RES);
}

PRE(sys_rename)
{
   *flags |= SfMayBlock;
   PRINT( "rename ( %#lx(%s), %#lx(%s) )", ARG1,(Char*)ARG1, ARG2,(Char*)ARG2 );
   PRE_REG_READ2(int, "rename", char*, frompath, char*, topath);
   PRE_MEM_RASCIIZ( "rename(frompath)", ARG1 );
   PRE_MEM_RASCIIZ( "rename(topath)", ARG2 );
}

PRE(sys_sbrk)
{
   PRINT("sbrk (BOGUS HANDLER)( %#lx )", ARG1);
   PRE_REG_READ1(long, "sbrk", long, arg1);
   /* After a zero sbrk, disallow aspacem from doing sbrk, since libc
      might rely on the value returned by this syscall. */
   /* 1 Oct 06: not currently used (aspacemgr-aix5.c ignores it) */
   VG_(am_aix5_sbrk_allowed) = toBool(ARG1 != 0);
   /* Disallow libc from moving the brk backwards as that might trash
      SkPreAlloc sections acquired by aspacem from previous uses of
      sbrk. */
   if (ARG1 < 0)
      ARG1 = 0;
   /* Do this as a sync syscall, so the sbrk_allowed flag gets turned
      back on ASAP.  Typically libc does sbrk(0) and then sbrk(x > 0)
      in quick succession.  Although surely it should hold some kind
      of lock at that point, else it cannot safely use the result from
      the first sbrk call to influence the second one? */
   *flags &= ~SfMayBlock;
}
POST(sys_sbrk)
{
   vg_assert(SUCCESS);
   handle_sbrk(ARG1);
}

PRE(sys_sched_get_priority_max)
{
   PRINT("sched_get_priority_max ( %ld )", ARG1);
   PRE_REG_READ1(int, "sched_get_priority_max", int, arg1);
}

PRE(sys_sem_destroy)
{
   PRINT("sem_destroy ( %#lx )", ARG1);
   PRE_REG_READ1(int, "sem_destroy", sem_t*, sem);
   PRE_MEM_READ( "sem_destroy(sem)", ARG1, sizeof(sem_t) );
}

PRE(sys_sem_init)
{
   PRINT("sem_init ( %#lx, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "sem_init", sem_t*, sem, int, pshared, int, value);
   PRE_MEM_WRITE( "sem_init(sem)", ARG1, sizeof(sem_t) );
}
POST(sys_sem_init)
{
   POST_MEM_WRITE( ARG1, sizeof(sem_t) );
}

PRE(sys_sem_post)
{
   PRINT("sem_post ( %#lx )", ARG1);
   PRE_REG_READ1(int, "sem_post", sem_t*, sem);
   PRE_MEM_READ("sem_post(sem)", ARG1, sizeof(sem_t));
}
POST(sys_sem_post)
{
   POST_MEM_WRITE(ARG1, sizeof(sem_t));
}

PRE(sys_send)
{
   *flags |= SfMayBlock;
   PRINT("send (BOGUS HANDLER)( %ld, %#lx, %ld, %ld )", 
         ARG1, ARG2, ARG3, ARG4);
}

PRE(sys_setgid)
{
   PRINT("setgid ( %ld )", ARG1);
   PRE_REG_READ1(void, "setgid", int, uid);
}

PRE(sys_setsockopt)
{
   PRINT("setsockopt ( %ld, %ld, %ld, %#lx, %ld )", 
         ARG1,ARG2,ARG3,ARG4,ARG5 );
   PRE_REG_READ5(long, "setsockopt", 
                 long, socket, long, level, long, optionname, 
                 void*, optionvalue, long, optlen);
   if (ARG4)
      PRE_MEM_READ( "setsockopt(optionvalue)", ARG4, ARG5 );
}

PRE(sys_setuid)
{
   PRINT("setuid ( %ld )", ARG1);
   PRE_REG_READ1(void, "setuid", int, uid);
}

static UWord get_shm_size ( Word shmid )
{
   SysRes res;
   struct shmid_ds buf;
   vg_assert(__NR_AIX5_shmctl != __NR_AIX5_UNKNOWN);
   res = VG_(do_syscall3)(__NR_AIX5_shmctl, shmid, IPC_STAT, (UWord)&buf);
   if (0) 
      VG_(printf)("XXX: shm_size(%ld) = %ld %ld\n", shmid, res.res, res.err);
   if (res.isError) {
      if (0)
         VG_(printf)("XXX: shm_size(shmid = %ld): FAILED\n", shmid);
      return 0* 4096;
   } else {
      return buf.shm_segsz;
   }
   /* fails with 22 and 13 (22 = EINVAL, Invalid argument,
      13 = EACCES, Permission denied) */
   /* shmat (4, 0x0, 0x1800) --> Success(0x40000000)
      XXX: shm_size(4) = -1 22
      shmat: seg size = 0
      XXX: shm_size(4) = -1 22

      shmat (5, 0x0, 0x1800) --> Success(0x50000000)
      XXX: shm_size(5) = -1 13
      shmat: seg size = 0
      XXX: shm_size(5) = -1 13

      shmat (4, 0x0, 0x1800) --> Success(0x40000000)
      XXX: shm_size(4) = -1 22
      shmat: seg size = 0
      XXX: shm_size(4) = -1 22
   */
}
PRE(sys_shmat)
{
   UWord segmentSize;
   /* void* shmat ( int shmid, const void* shmaddr, int flags ) */
   PRINT("shmat (%ld, %#lx, %#lx)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(void*, "shmat", int, shmid, void*, shmaddr, int, flags);
   segmentSize = get_shm_size( ARG1 );
   if (0) VG_(printf)("shmat: seg size = %lu\n", segmentSize);
}
POST(sys_shmat)
{
   UInt segmentSize;
   vg_assert(SUCCESS);
   vg_assert(RES != -1L);
   segmentSize = get_shm_size ( ARG1 );
   if ( segmentSize > 0 ) {
      UInt prot = VKI_PROT_READ|VKI_PROT_WRITE;
      Bool d;

      if (ARG2 & SHM_RDONLY)
         prot &= ~VKI_PROT_WRITE;

      d = VG_(am_notify_client_shmat)( RES, VG_PGROUNDUP(segmentSize), prot );

      /* we don't distinguish whether it's read-only or
       * read-write -- it doesn't matter really. */
      VG_TRACK( new_mem_mmap, RES, segmentSize, True, True, False, 0/*di_handle*/ );
      if (d)
         VG_(discard_translations)( (Addr64)RES, 
                                    (ULong)VG_PGROUNDUP(segmentSize),
                                    "ML_(generic_POST_sys_shmat)" );
   }
}

PRE(sys_shmctl)
{
   PRINT("shmctl ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(int, "shmctl", int, shmid, int, command, void*, buffer);
   if (ARG3)
      PRE_MEM_WRITE( "shmctl(buffer)", ARG3, sizeof(struct shmid_ds) );
}
POST(sys_shmctl)
{
   if ((ARG3) && ARG2 == IPC_STAT)
      POST_MEM_WRITE( ARG3, sizeof(struct shmid_ds) );
}

PRE(sys_shmdt)
{
   PRINT("shmdt ( %#lx )", ARG1);
   PRE_REG_READ1(long, "shmdt", void*, address);
}
POST(sys_shmdt)
{
   NSegment const*const s = VG_(am_find_nsegment)(ARG1);

   if (s != NULL) {
      Addr  s_start = s->start;
      SizeT s_len   = s->end+1 - s->start;
      Bool  d;

      vg_assert(s->kind == SkShmC && s->start == ARG1);

      d = VG_(am_notify_munmap)(s_start, s_len);
      /* s is now invalid; do not use after here */
      VG_TRACK( die_mem_munmap, s_start, s_len );
      if (d)
         VG_(discard_translations)( (Addr64)s_start,
                                    (ULong)s_len,
                                    "ML_(generic_POST_sys_shmdt)" );
   }
}

PRE(sys_shmget)
{
   PRINT("shmget ( %ld, %ld, %ld )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(int, "shmget", key_t, key, size_t, size, int, shmFlag);
}

PRE(sys_shutdown)
{
   PRINT("shutdown (BOGUS HANDLER)");
}

PRE(sys_sigcleanup)
{
   PRINT("sigcleanup (UNDOCUMENTED)");
}

PRE(sys_sigprocmask)
{
   PRINT("sigprocmask ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sigprocmask", 
                 int, how, vki_sigset_t *, set, vki_sigset_t *, oldset);
   if (ARG2 != 0)
      PRE_MEM_READ( "sigprocmask(set)", ARG2, sizeof(vki_sigset_t));
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sigprocmask(oldset)", ARG3, sizeof(vki_sigset_t));

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigprocmask) ( tid, ARG1, (vki_sigset_t*)ARG2, 
                                           (vki_sigset_t*)ARG3 )
   );

   if (SUCCESS)
     *flags |= SfPollAfter;
}
POST(sys_sigprocmask)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(vki_sigset_t));
}

PRE(sys_socket)
{
   PRINT("socket ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "socket", int, domain, int, type, int, protocol);
}

PRE(sys_statfs)
{
   PRINT("sys_statfs ( %#lx(%s), %#lx )",ARG1,(Char*)ARG1,ARG2);
   PRE_REG_READ2(long, "statfs", const char *, path, struct statfs *, buf);
   PRE_MEM_RASCIIZ( "statfs(path)", ARG1 );
   PRE_MEM_WRITE( "statfs(buf)", ARG2, sizeof(struct statfs) );
}
POST(sys_statfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct statfs) );
}

PRE(sys_statx)
{
   PRINT("statx ( %#lx(%s), %#lx, %ld, %ld )", ARG1,(Char*)ARG1,ARG2,ARG3,ARG4);
   PRE_MEM_RASCIIZ( "statx(file_name)", ARG1 );
   PRE_REG_READ4(Word, "statx", UWord, fd, void*, buf,
                                UWord, len, UWord, cmd);
   PRE_MEM_WRITE( "statx(buf)", ARG2, ARG3 );
}
POST(sys_statx)
{
   POST_MEM_WRITE( ARG2, ARG3 );
}

PRE(sys_symlink)
{
   PRINT("symlink (BOGUS HANDLER)");
}

PRE(sys_sys_parm)
{
   PRINT("sys_parm (%ld, %ld, %#lx)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "sys_parm", int, cmd, int, cmdflag, 
                      struct vario*, parmp);
   /* this is a bit of a kludge, but if parmp has uninitialised areas
      and we're doing SYSP_SET, lots of errors will be tiresomely
      reported.  Hence just ignore the definedness of the area and
      only check addressability. */
   PRE_MEM_WRITE( "sys_parm(parmp)", ARG3, sizeof(struct vario));
}
POST(sys_sys_parm)
{
   if (ARG1 == SYSP_GET)
      POST_MEM_WRITE( ARG3, sizeof(struct vario) );
}

PRE(sys_sysconfig)
{
   PRINT("sysconfig ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "sysconfig", int, cmd, void*, parmp, int, parmlen);
   /* It may be that the area is read sometimes as well as written,
      but for the same reasons as sys_parm, just check addressibility,
      not definedness. */
   PRE_MEM_WRITE( "sysconfig(parmp)", ARG2, ARG3 );
}
POST(sys_sysconfig)
{
   POST_MEM_WRITE( ARG2, ARG3 );
}

PRE(sys_thread_create)
{
   *flags |= SfMayBlock;
   PRINT("thread_create ( )");
}
POST(sys_thread_create)
{
   vg_assert(SUCCESS);
   if (0) VG_(printf)("new lwpid is %ld\n", RES);

   /* Allocate a new thread slot (which sets it to VgTs_Init), and
      record the lwpid in it, so can later find it again when handling
      sys_thread_setstate for that lwpid. */

   ThreadId     ctid = VG_(alloc_ThreadState)();
   ThreadState* ctst = VG_(get_ThreadState)(ctid);

   vg_assert(ctst->status == VgTs_Init);

   { /* Clear all os_state fields except for the vg stack ones, so any
        existing stack gets reused. */
     Addr v_s_b    = ctst->os_state.valgrind_stack_base;
     Addr v_s_i_SP = ctst->os_state.valgrind_stack_init_SP;
     VG_(memset)(&ctst->os_state, 0, sizeof(ThreadOSstate));
     ctst->os_state.valgrind_stack_base    = v_s_b;
     ctst->os_state.valgrind_stack_init_SP = v_s_i_SP;
   }
   ctst->os_state.lwpid = RES;
}

PRE(sys_thread_init)
{
   *flags |= SfMayBlock;
   PRE_REG_READ2(long, "thread_init", long, arg1, long, arg2);
   PRINT("thread_init (BOGUS HANDLER) ( %#lx, %#lx )", ARG1, ARG2);
}

PRE(sys_thread_kill)
{
   Int target_lwpid, my_lwpid;
   PRINT("thread_kill ( %ld, %ld )", ARG1, ARG2);

   if ( ((Word)ARG1) == (Word)(-1)
        && ARG2 == VKI_SIGSEGV ) {
      /* too difficult to continue; give up. */
      ML_(aix5_set_threadstate_for_emergency_exit)
         (tid, "exiting due to thread_kill(..,SIGSEGV) to process");
      SET_STATUS_Success(0);
      return;
   }

   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;

   target_lwpid = (Int)ARG1;
   my_lwpid     = VG_(gettid)();
   /* we still hold the lock.  Do deadlock-avoidance stuff. */
   if (target_lwpid == my_lwpid) {
      /* sending a signal to myself, which may be fatal.  Therefore
         drop the lock so that if the signal kills me, some other
         thread can pick it up. */
      *flags |= SfMayBlock;
   } else {
      /* sending a signal to some other thread, which may kill it;
         therefore I'd better hold on to the lock to ensure that the
         target doesn't get killed whilst holding it. */
   }
}

/* thread_setmymask_fast is handled on a per platform basis */

PRE(sys_thread_setmystate)
{
   *flags |= SfMayBlock;
   /* args: struct tstate *, struct tstate * 
      I assume: first is new state, if not NULL.  
      Second is place to write the previous state, if not NULL.
      (in the style of sigaction) */
   PRINT("thread_setmystate (BOGUS HANDLER) ( %#lx, %#lx )",
         ARG1, ARG2 );
   PRE_REG_READ2(long, "thread_setmystate", 
                       struct tstate *, newstate, 
                       struct tstate *, oldstate );
   if (ARG1)
      PRE_MEM_READ( "thread_setmystate(arg1)", ARG1, sizeof(struct tstate) );
   if (ARG2)
      PRE_MEM_WRITE( "thread_setmystate(arg2)", ARG2, sizeof(struct tstate) );
   if (1 && VG_(clo_trace_syscalls) && ARG1)
      ML_(aix5debugstuff_show_tstate)(ARG1, "thread_setmystate (NEW)");

   struct tstate* newts  = (struct tstate*)ARG1;
   struct tstate* oldts  = (struct tstate*)ARG2;

   /* Are we just messing with the signal mask?  If so intercept it
      and do it ourselves.  Same idea as handling for
      thread_setmymask_fast in 32-bit mode. */
   if (newts && newts->flags == TSTATE_CHANGE_SIGMASK) {
      vki_sigset_t* newset = newts ? (vki_sigset_t*)&newts->sigmask : NULL;
      vki_sigset_t* oldset = oldts ? (vki_sigset_t*)&oldts->sigmask : NULL;
      SET_STATUS_from_SysRes(
         VG_(do_sys_sigprocmask) ( tid, VKI_SIG_SETMASK, newset, oldset )
      );
      *flags &= ~SfMayBlock;
      return;
   }
}
POST(sys_thread_setmystate)
{
   if (ARG2)
      POST_MEM_WRITE( ARG2, sizeof(struct tstate) );
   if (0 && VG_(clo_trace_syscalls) && ARG2)
      ML_(aix5debugstuff_show_tstate)(ARG2, "thread_setmystate (OLD)");
}

PRE(sys_thread_setmystate_fast)
{
   UWord how = ARG1;
   /* args: ?? */
   PRINT("thread_setmystate_fast (BOGUS HANDLER)"
         "(%#lx,%#lx(%s),%#lx(%s))", 
         ARG1,
         ARG2, ML_(aix5debugstuff_pc_to_fnname)(ARG2),
         ARG3, ML_(aix5debugstuff_pc_to_fnname)(ARG3)
        );
   PRE_REG_READ3(long, "thread_setmystate_fast", 
                       long, arg1, long, arg2, long, arg3);
   if (1 && VG_(clo_trace_syscalls))
      ML_(aix5debugstuff_show_tstate_flags)( how );

   if (how & TSTATE_CHANGE_FLAGS) {
      /* Messing with cancellation type/state.  Pay attention. */
      Bool async    = (how & TSTATE_CANCEL_DEFER) == 0;
      Bool disabled = (how & TSTATE_CANCEL_DISABLE) > 0;
      ThreadState* tst = VG_(get_ThreadState)(tid);
      if (VG_(clo_trace_syscalls))
         VG_(printf)("(cancellation state -> %s %s)",
                     async ? "ASYNC" : "DEFER",
                     disabled ? "DISABLED" : " ENABLED");
      tst->os_state.cancel_async    = async;
      tst->os_state.cancel_disabled = disabled;
      /* If cancellation has been enabled for this thread and there is
         a request outstanding, honour it now. */
      if ((!disabled)
          && tst->os_state.cancel_progress == Canc_Requested) {
         if (VG_(clo_trace_syscalls))
            VG_(printf)("(honouring previous cancellation request)");
         tst->os_state.cancel_progress = Canc_Actioned;
         Bool ok = ML_(aix5_force_thread_into_pthread_exit)(tid);
         if (!ok) {
            /* now at serious risk of deadlock/livelock.  Give up
               rather than continue. */
            ML_(aix5_set_threadstate_for_emergency_exit)
               (tid, "pthread_cancel(case1): "
                     "cannot find pthread_exit; aborting");
            SET_STATUS_Success(0);
            return;
         }
      }
      SET_STATUS_Success(0);
      return;
   }

   /* In all other cases, hand to kernel. */
   *flags |= SfMayBlock;
}

/* thread_setstate is handled in syswrap-ppc{32,64}-aix5.c. */

PRE(sys_thread_terminate_unlock)
{
   ThreadState* tst;
   /* simple; just make this thread exit */
   PRINT("thread_terminate_unlock( %#lx )", ARG1);
   PRE_REG_READ1(void, "thread_terminate_unlock", void*, exitcode);
   tst = VG_(get_ThreadState)(tid);
   /* Drop the lock we were holding, since we're not really going to
      exit the host thread with thread_terminate_unlock. */
   if (0) VG_(printf)("XXXXX dropping lock\n");
   if (1) VG_(do_syscall1)(__NR_AIX5_thread_unlock, ARG1);
   /* Set the thread's status to be exiting, then claim that the
      syscall succeeded. */
   tst->exitreason = VgSrc_ExitThread;
   tst->os_state.exitcode = 0;
   SET_STATUS_Success(0);
}

PRE(sys_thread_tsleep)
{
   *flags |= SfMayBlock;
   PRINT("thread_tsleep (BOGUS HANDLER)( %ld, %#lx, %#lx, %#lx )", 
         ARG1, ARG2, ARG3, ARG4 );
}

PRE(sys_thread_tsleep_event)
{
   *flags |= SfMayBlock;
   PRINT("thread_tsleep_event (UNDOCUMENTED)( %#lx, %#lx, %ld, %#lx )", 
         ARG1, ARG2, ARG3, ARG4 );
}

PRE(sys_thread_twakeup)
{
   *flags |= SfMayBlock;
   PRINT("thread_twakeup (BOGUS HANDLER)( tid=%ld, val=%#lx )", ARG1, ARG2 );
}

PRE(sys_thread_twakeup_event)
{
   *flags |= SfMayBlock;
   PRINT("thread_twakeup_event (BOGUS HANDLER)( %#lx, %ld, %ld )", 
         ARG1, ARG2, ARG3 );
}

PRE(sys_thread_unlock)
{
   *flags |= SfMayBlock;
   PRINT("thread_unlock (BOGUS HANDLER)" );
}

PRE(sys_thread_waitlock)
{
   *flags |= SfMayBlock;
   PRINT("thread_waitlock (BOGUS HANDLER)" );
}

PRE(sys_thread_waitlock_)
{
   *flags |= SfMayBlock;
   PRINT("thread_waitlock_ (BOGUS HANDLER)" );
}

PRE(sys_times)
{
   PRINT("times ( %#lx )", ARG1);
   PRE_REG_READ1(long, "times", struct tms *, buffer);
   PRE_MEM_WRITE("times(buf)", ARG1, sizeof(struct tms) );
}
POST(sys_times)
{
   POST_MEM_WRITE( ARG1, sizeof(struct tms) );
}

PRE(sys_umask)
{
   PRINT("umask (BOGUS HANDLER)");
}

PRE(sys_uname)
{
   PRINT("uname ( %#lx )", ARG1);
   PRE_MEM_WRITE( "uname(Name)", ARG1, sizeof(struct utsname));
}
POST(sys_uname)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG1, sizeof(struct utsname));
}

PRE(sys_unlink)
{
   PRINT("unlink ( %#lx(%s) )", ARG1, (Char*)ARG1 );
   PRE_REG_READ1(int, "unlink", char*, path);
   PRE_MEM_RASCIIZ( "unlink(path)", ARG1 );
}

PRE(sys_utimes)
{
   PRINT("utimes ( %#lx(%s), %#lx )", ARG1,(Char*)ARG1, ARG2);
   PRE_REG_READ2(int, "utimes", char*, path, struct timeval*, times);
   PRE_MEM_RASCIIZ( "utimes(path)", ARG1 );
   PRE_MEM_READ( "utimes(times)", ARG2, 2 * sizeof(struct vki_timeval) );
}

PRE(sys_vmgetinfo)
{
   PRINT("vmgetinfo ( %#lx, %ld, %ld )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(int, "vmgetinfo", void*, out, int, command, int, arg);
   /* It looks like libc's vmgetinfo just hands stuff through to the
      syscall.  The man page says that the interpretation of ARG3(arg)
      depends on ARG2(cmd); nevertheless in all cases basically this
      writes the buffer (ARG1, ARG3). */
   PRE_MEM_WRITE("vmgetinfo(buf)", ARG1, ARG3);
}
POST(sys_vmgetinfo)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE(ARG1, ARG3);
}

PRE(sys_yield)
{
   *flags |= SfMayBlock;
   PRINT("yield ( )");
}

#undef PRE
#undef POST

#endif // defined(VGO_aix5)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
