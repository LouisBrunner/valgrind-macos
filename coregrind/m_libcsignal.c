
/*--------------------------------------------------------------------*/
/*--- Signal-related libc stuff.                    m_libcsignal.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Julian Seward 
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

#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_syscall.h"
#include "pub_core_libcsignal.h"    /* self */

/* sigemptyset, sigfullset, sigaddset and sigdelset return 0 on
   success and -1 on error.  */
/* I believe the indexing scheme in ->sig[] is also correct for
   32- and 64-bit AIX (verified 27 July 06). */

Int VG_(sigfillset)( vki_sigset_t* set )
{
   Int i;
   if (set == NULL)
      return -1;
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      set->sig[i] = ~(UWord)0x0;
   return 0;
}

Int VG_(sigemptyset)( vki_sigset_t* set )
{
   Int i;
   if (set == NULL)
      return -1;
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      set->sig[i] = 0x0;
   return 0;
}

Bool VG_(isemptysigset)( const vki_sigset_t* set )
{
   Int i;
   vg_assert(set != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      if (set->sig[i] != 0x0) return False;
   return True;
}

Bool VG_(isfullsigset)( const vki_sigset_t* set )
{
   Int i;
   vg_assert(set != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      if (set->sig[i] != ~(UWord)0x0) return False;
   return True;
}

Bool VG_(iseqsigset)( const vki_sigset_t* set1, const vki_sigset_t* set2 )
{
   Int i;
   vg_assert(set1 != NULL && set2 != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      if (set1->sig[i] != set2->sig[i]) return False;
   return True;
}


Int VG_(sigaddset)( vki_sigset_t* set, Int signum )
{
   if (set == NULL)
      return -1;
   if (signum < 1 || signum > _VKI_NSIG)
      return -1;
   signum--;
   set->sig[signum / _VKI_NSIG_BPW] |= (1UL << (signum % _VKI_NSIG_BPW));
   return 0;
}

Int VG_(sigdelset)( vki_sigset_t* set, Int signum )
{
   if (set == NULL)
      return -1;
   if (signum < 1 || signum > _VKI_NSIG)
      return -1;
   signum--;
   set->sig[signum / _VKI_NSIG_BPW] &= ~(1UL << (signum % _VKI_NSIG_BPW));
   return 0;
}

Int VG_(sigismember) ( const vki_sigset_t* set, Int signum )
{
   if (set == NULL)
      return 0;
   if (signum < 1 || signum > _VKI_NSIG)
      return 0;
   signum--;
   if (1 & ((set->sig[signum / _VKI_NSIG_BPW]) >> (signum % _VKI_NSIG_BPW)))
      return 1;
   else
      return 0;
}

/* Add all signals in src to dst. */
void VG_(sigaddset_from_set)( vki_sigset_t* dst, vki_sigset_t* src )
{
   Int i;
   vg_assert(dst != NULL && src != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      dst->sig[i] |= src->sig[i];
}

/* Remove all signals in src from dst. */
void VG_(sigdelset_from_set)( vki_sigset_t* dst, vki_sigset_t* src )
{
   Int i;
   vg_assert(dst != NULL && src != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      dst->sig[i] &= ~(src->sig[i]);
}


/* The functions sigaction, sigprocmask, sigpending and sigsuspend
   return 0 on success and -1 on error.  
*/
Int VG_(sigprocmask)( Int how, const vki_sigset_t* set, vki_sigset_t* oldset)
{
   SysRes res = VG_(do_syscall4)(__NR_rt_sigprocmask, 
                                 how, (UWord)set, (UWord)oldset, 
                                 _VKI_NSIG_WORDS * sizeof(UWord));
   return res.isError ? -1 : 0;
}


Int VG_(sigaction) ( Int signum, const struct vki_sigaction* act,  
                     struct vki_sigaction* oldact)
{
   SysRes res = VG_(do_syscall4)(__NR_rt_sigaction,
                                 signum, (UWord)act, (UWord)oldact, 
                                 _VKI_NSIG_WORDS * sizeof(UWord));
   return res.isError ? -1 : 0;
}


Int VG_(kill)( Int pid, Int signo )
{
   SysRes res = VG_(do_syscall2)(__NR_kill, pid, signo);
   return res.isError ? -1 : 0;
}


Int VG_(tkill)( ThreadId tid, Int signo )
{
   SysRes res = VG_(mk_SysRes_Error)(VKI_ENOSYS);
   res = VG_(do_syscall2)(__NR_tkill, tid, signo);
   if (res.isError && res.err == VKI_ENOSYS)
      res = VG_(do_syscall2)(__NR_kill, tid, signo);
   return res.isError ? -1 : 0;
}


/* A cut-down version of POSIX sigtimedwait: poll for pending signals
   mentioned in the sigset_t, and if any are present, select one
   arbitrarily, return its number (which must be > 0), and put
   auxiliary info about it in the siginfo_t, and make it
   not-pending-any-more.  If none are pending, return zero.  The _zero
   refers to the fact that there is zero timeout, so if no signals are
   pending it returns immediately.  Perhaps a better name would be
   'sigpoll'.  Returns -1 on error, 0 if no signals pending, and n > 0
   if signal n was selected. 

   The Linux implementation is trivial: do the corresponding syscall.

   The AIX implementation is horrible and probably broken in a dozen
   obscure ways.  I suspect it's only thread-safe because V forces
   single-threadedness. */

#if defined(VGO_linux)
Int VG_(sigtimedwait_zero)( const vki_sigset_t *set, 
                            vki_siginfo_t *info )
{
   static const struct vki_timespec zero = { 0, 0 };
   SysRes res = VG_(do_syscall4)(__NR_rt_sigtimedwait, (UWord)set, (UWord)info, 
                                 (UWord)&zero, sizeof(*set));
   return res.isError ? -1 : res.res;
}

#elif defined(VGO_aix5)
/* The general idea is:
   - use sigpending to find out which signals are pending
   - choose one
   - temporarily set its handler to sigtimedwait_zero_handler
   - use sigsuspend atomically unblock it and wait for the signal.
     Upon return, sigsuspend restores the signal mask to what it
     was to start with.
   - Restore the handler for the signal to whatever it was before.
*/

/* A signal handler which does nothing (it doesn't need to).  It does
   however check that it's not handing a sync signal for which
   returning is meaningless. */
static void sigtimedwait_zero_handler ( Int sig ) 
{ 
   vg_assert(sig != VKI_SIGILL);
   vg_assert(sig != VKI_SIGSEGV);
   vg_assert(sig != VKI_SIGBUS);
   vg_assert(sig != VKI_SIGTRAP);
   /* do nothing */ 
}

Int VG_(sigtimedwait_zero)( const vki_sigset_t *set, 
                            vki_siginfo_t *info )
{
  Int    i, ir;
  SysRes sr;
  vki_sigset_t pending, blocked, allbutone;
  struct vki_sigaction sa, saved_sa;

  /* Find out what's pending: AIX _sigpending */
  sr = VG_(do_syscall1)(__NR__sigpending, (UWord)&pending);
  vg_assert(!sr.isError);

  /* don't try for signals not in 'set' */
  /* pending = pending `intersect` set */
  for (i = 0; i < _VKI_NSIG_WORDS; i++)
     pending.sig[i] &= set->sig[i];

  /* don't try for signals not blocked at the moment */
  ir = VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &blocked);
  vg_assert(ir == 0);

  /* pending = pending `intersect` blocked */
  for (i = 0; i < _VKI_NSIG_WORDS; i++)
     pending.sig[i] &= blocked.sig[i];

  /* decide which signal we're going to snarf */
  for (i = 1; i < _VKI_NSIG; i++)
     if (VG_(sigismember)(&pending,i))
        break;

  if (i == _VKI_NSIG)
     return 0;

  /* fetch signal i.
     pre: i is blocked and pending
     pre: we are the only thread running 
  */
  /* Set up alternative signal handler */
  VG_(sigfillset)(&allbutone);
  VG_(sigdelset)(&allbutone, i);
  sa.sa_mask     = allbutone;
  sa.ksa_handler = &sigtimedwait_zero_handler;
  sa.sa_flags    = 0;
  ir = VG_(sigaction)(i, &sa, &saved_sa);
  vg_assert(ir == 0);

  /* Switch signal masks and wait for the signal.  This should happen
     immediately, since we've already established it is pending and
     blocked. */
  sr = VG_(do_syscall1)(__NR__sigsuspend, (UWord)&allbutone);
  vg_assert(sr.isError);
  if (0)
     VG_(debugLog)(0, "libcsignal",
                      "sigtimedwait_zero: sigsuspend got res %ld err %ld\n", 
                      sr.res, sr.err);
  vg_assert(sr.res == (UWord)-1);

  /* Restore signal's handler to whatever it was before */
  ir = VG_(sigaction)(i, &saved_sa, NULL);
  vg_assert(ir == 0);

  /* This is bogus - we could get more info from the sighandler. */
  VG_(memset)( info, 0, sizeof(*info) );
  info->si_signo = i;

  return i;
}

#else
#  error Unknown OS
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
