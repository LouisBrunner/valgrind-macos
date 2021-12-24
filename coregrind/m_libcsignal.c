
/*--------------------------------------------------------------------*/
/*--- Signal-related libc stuff.                    m_libcsignal.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_syscall.h"
#include "pub_core_libcsignal.h"    /* self */
#include "pub_core_libcproc.h"

#if !defined(VGO_solaris)
#   define _VKI_MAXSIG (_VKI_NSIG - 1)
#endif
STATIC_ASSERT((_VKI_MAXSIG % _VKI_NSIG_BPW) != 0);

/* IMPORTANT: on Darwin it is essential to use the _nocancel versions
   of syscalls rather than the vanilla version, if a _nocancel version
   is available.  See docs/internals/Darwin-notes.txt for the reason
   why. */

/* sigemptyset, sigfullset, sigaddset and sigdelset return 0 on
   success and -1 on error.  */
/* In the sigset routines below, be aware that _VKI_NSIG_BPW can be
   either 32 or 64, and hence the sig[] words can either be 32- or
   64-bits.  And which they are it doesn't necessarily follow from the
   host word size. */

/* Functions VG_(isemptysigset) and VG_(isfullsigset) check only bits that
   represent valid signals (i.e. signals <= _VKI_MAXSIG).  The same applies
   for the comparison in VG_(iseqsigset).  This is important because when
   a signal set is received from an operating system then bits which represent
   signals > _VKI_MAXSIG can have unexpected values for Valgrind. This is
   mainly specific to the Solaris kernel which clears these bits. */

Int VG_(sigfillset)( vki_sigset_t* set )
{
   Int i;
   if (set == NULL)
      return -1;
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      set->sig[i] = ~0;
   return 0;
}

Int VG_(sigemptyset)( vki_sigset_t* set )
{
   Int i;
   if (set == NULL)
      return -1;
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      set->sig[i] = 0;
   return 0;
}

Bool VG_(isemptysigset)( const vki_sigset_t* set )
{
   Int i;
   vg_assert(set != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++) {
      if (_VKI_NSIG_BPW * (i + 1) <= (_VKI_MAXSIG + 1)) {
         /* Full word check. */
         if (set->sig[i] != 0) return False;
      }
      else {
         /* Partial word check. */
         ULong mask = ((ULong)1UL << (_VKI_MAXSIG % _VKI_NSIG_BPW)) - 1;
         if ((set->sig[i] & mask) != 0) return False;
         break;
      }
   }
   return True;
}

Bool VG_(isfullsigset)( const vki_sigset_t* set )
{
   Int i;
   vg_assert(set != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++) {
      if (_VKI_NSIG_BPW * (i + 1) <= (_VKI_MAXSIG + 1)) {
         /* Full word check. */
         if (set->sig[i] != ~0) return False;
      }
      else {
         /* Partial word check. */
         ULong mask = ((ULong)1UL << (_VKI_MAXSIG % _VKI_NSIG_BPW)) - 1;
         if ((set->sig[i] & mask) != mask) return False;
         break;
      }
   }
   return True;
}

Bool VG_(iseqsigset)( const vki_sigset_t* set1, const vki_sigset_t* set2 )
{
   Int i;
   vg_assert(set1 != NULL && set2 != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++) {
      if (_VKI_NSIG_BPW * (i + 1) <= (_VKI_MAXSIG + 1)) {
         /* Full word comparison. */
         if (set1->sig[i] != set2->sig[i]) return False;
      }
      else {
         /* Partial word comparison. */
         ULong mask = ((ULong)1UL << (_VKI_MAXSIG % _VKI_NSIG_BPW)) - 1;
         if ((set1->sig[i] & mask) != (set2->sig[i] & mask)) return False;
         break;
      }
   }
   return True;
}


Int VG_(sigaddset)( vki_sigset_t* set, Int signum )
{
   if (set == NULL)
      return -1;
   if (signum < 1 || signum > _VKI_NSIG)
      return -1;
   signum--;
   set->sig[signum / _VKI_NSIG_BPW] |= (1ULL << (signum % _VKI_NSIG_BPW));
   return 0;
}

Int VG_(sigdelset)( vki_sigset_t* set, Int signum )
{
   if (set == NULL)
      return -1;
   if (signum < 1 || signum > _VKI_NSIG)
      return -1;
   signum--;
   set->sig[signum / _VKI_NSIG_BPW] &= ~(1ULL << (signum % _VKI_NSIG_BPW));
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
void VG_(sigaddset_from_set)( vki_sigset_t* dst, const vki_sigset_t* src )
{
   Int i;
   vg_assert(dst != NULL && src != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      dst->sig[i] |= src->sig[i];
}

/* Remove all signals in src from dst. */
void VG_(sigdelset_from_set)( vki_sigset_t* dst, const vki_sigset_t* src )
{
   Int i;
   vg_assert(dst != NULL && src != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      dst->sig[i] &= ~(src->sig[i]);
}

/* dst = dst `intersect` src. */
void VG_(sigintersectset)( vki_sigset_t* dst, const vki_sigset_t* src )
{
   Int i;
   vg_assert(dst != NULL && src != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      dst->sig[i] &= src->sig[i];
}

/* dst = ~src */
void VG_(sigcomplementset)( vki_sigset_t* dst, const vki_sigset_t* src )
{
   Int i;
   vg_assert(dst != NULL && src != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      dst->sig[i] = ~ src->sig[i];
}


/* The functions sigaction, sigprocmask, sigpending and sigsuspend
   return 0 on success and -1 on error.  
*/
Int VG_(sigprocmask)( Int how, const vki_sigset_t* set, vki_sigset_t* oldset)
{
#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
#  if defined(__NR_rt_sigprocmask)
   SysRes res = VG_(do_syscall4)(__NR_rt_sigprocmask, 
                                 how, (UWord)set, (UWord)oldset, 
                                 _VKI_NSIG_WORDS * sizeof(UWord));
#  else
   SysRes res = VG_(do_syscall3)(__NR_sigprocmask, 
                                 how, (UWord)set, (UWord)oldset);
#  endif

#  elif defined(VGO_darwin)
   /* On Darwin, __NR_sigprocmask appears to affect the entire
      process, not just this thread.  Hence need to use
      __NR___pthread_sigmask instead. */
   SysRes res =  VG_(do_syscall3)(__NR___pthread_sigmask, 
                                  how, (UWord)set, (UWord)oldset);
#  else
#    error "Unknown OS"
#  endif
   return sr_isError(res) ? -1 : 0;
}


#if defined(VGO_darwin)
/* A helper function for sigaction on Darwin. */
static 
void darwin_signal_demux(void* a1, UWord a2, UWord a3, void* a4, void* a5) {
   VG_(debugLog)(2, "libcsignal",
                    "PRE  demux sig, a2 = %lu, signo = %lu\n", a2, a3);
   if (a2 == 1)
      ((void(*)(int))a1) (a3);
   else
      ((void(*)(int,void*,void*))a1) (a3,a4,a5);
   VG_(debugLog)(2, "libcsignal",
                    "POST demux sig, a2 = %lu, signo = %lu\n", a2, a3);
   VG_(do_syscall2)(__NR_sigreturn, (UWord)a5, 0x1E);
   /* NOTREACHED */
   __asm__ __volatile__("ud2");
}
#endif

Int VG_(sigaction) ( Int signum, 
                     const vki_sigaction_toK_t* act,  
                     vki_sigaction_fromK_t* oldact)
{
#  if defined(VGO_linux)
   /* Normal case: vki_sigaction_toK_t and vki_sigaction_fromK_t are
      identical types. */
   SysRes res = VG_(do_syscall4)(__NR_rt_sigaction,
                                 signum, (UWord)act, (UWord)oldact, 
                                 _VKI_NSIG_WORDS * sizeof(UWord));
   return sr_isError(res) ? -1 : 0;

#  elif defined(VGO_darwin)
   /* If we're passing a new action to the kernel, make a copy of the
      new action, install our own sa_tramp field in it, and ignore
      whatever we were provided with.  This is OK because all the
      sigaction requests come from m_signals, and are not directly
      what the client program requested, so there is no chance that we
      will inadvertently ignore the sa_tramp field requested by the
      client.  (In fact m_signals does ignore it when building signal
      frames for the client, but that's a completely different
      matter).

      If we're receiving an old action from the kernel, be very
      paranoid and make sure the kernel doesn't trash bits of memory
      that we don't expect it to. */
   SysRes res;

   vki_sigaction_toK_t actCopy;
   struct {
     ULong before[2];
     vki_sigaction_fromK_t oa;
     ULong after[2];
   }
   oldactCopy;

   vki_sigaction_toK_t*   real_act;
   vki_sigaction_fromK_t* real_oldact;

   real_act    = act    ? &actCopy       : NULL;
   real_oldact = oldact ? &oldactCopy.oa : NULL;
   VG_(memset)(&oldactCopy, 0x55, sizeof(oldactCopy));
   if (real_act) {
      *real_act = *act;
      real_act->sa_tramp = (void*)&darwin_signal_demux;
   }
   res = VG_(do_syscall3)(__NR_sigaction, 
                          signum, (UWord)real_act, (UWord)real_oldact);
   if (real_oldact) {
      vg_assert(oldactCopy.before[0] == 0x5555555555555555ULL);
      vg_assert(oldactCopy.before[1] == 0x5555555555555555ULL);
      vg_assert(oldactCopy.after[0]  == 0x5555555555555555ULL);
      vg_assert(oldactCopy.after[1]  == 0x5555555555555555ULL);
      *oldact = *real_oldact;
   }
   return sr_isError(res) ? -1 : 0;

#  elif defined(VGO_solaris)
   /* vki_sigaction_toK_t and vki_sigaction_fromK_t are identical types. */
   SysRes res = VG_(do_syscall3)(__NR_sigaction,
                                 signum, (UWord)act, (UWord)oldact);
   return sr_isError(res) ? -1 : 0;
   
#  elif defined(VGO_freebsd)
   SysRes res = VG_(do_syscall3)(__NR_sigaction,
                                 signum, (UWord)act, (UWord)oldact);
   return sr_isError(res) ? -1 : 0;


#  elif defined(VGO_freebsd)
   SysRes res = VG_(do_syscall3)(__NR_sigaction,
                                 signum, (UWord)act, (UWord)oldact);
   return sr_isError(res) ? -1 : 0;


#  else
#    error "Unsupported OS"
#  endif
}


/* See explanation in pub_core_libcsignal.h. */
void 
VG_(convert_sigaction_fromK_to_toK)( const vki_sigaction_fromK_t* fromK,
                                     /*OUT*/vki_sigaction_toK_t* toK )
{
#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
   *toK = *fromK;
#  elif defined(VGO_darwin)
   toK->ksa_handler = fromK->ksa_handler;
   toK->sa_tramp    = NULL; /* the cause of all the difficulty */
   toK->sa_mask     = fromK->sa_mask;
   toK->sa_flags    = fromK->sa_flags;
#  else
#    error "Unsupported OS"
#  endif
}


Int VG_(kill)( Int pid, Int signo )
{
#  if defined(VGO_linux) || defined(VGO_solaris)
   SysRes res = VG_(do_syscall2)(__NR_kill, pid, signo);
#  elif defined(VGO_darwin) || defined(VGO_freebsd)
   SysRes res = VG_(do_syscall3)(__NR_kill,
                                 pid, signo, 1/*posix-compliant*/);
#  else
#    error "Unsupported OS"
#  endif
   return sr_isError(res) ? -1 : 0;
}

Int VG_(tkill)( Int lwpid, Int signo )
{
#  if defined(__NR_tkill)
   SysRes res = VG_(mk_SysRes_Error)(VKI_ENOSYS);
   res = VG_(do_syscall2)(__NR_tkill, lwpid, signo);
   if (sr_isError(res) && sr_Err(res) == VKI_ENOSYS)
      res = VG_(do_syscall2)(__NR_kill, lwpid, signo);
   return sr_isError(res) ? -1 : 0;

#  elif defined(VGO_darwin)
   // Note that the __pthread_kill syscall takes a Mach thread, not a pthread.
   SysRes res;
   res = VG_(do_syscall2)(__NR___pthread_kill, lwpid, signo);
   return sr_isError(res) ? -1 : 0;

#  elif defined(VGO_solaris)
   SysRes res;
#     if defined(SOLARIS_LWP_SIGQUEUE_SYSCALL)
#        if defined(SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID)
            res = VG_(do_syscall6)(__NR_lwp_sigqueue, 0, lwpid, signo,
                                   0, VKI_SI_LWP, 0);
#        else
            res = VG_(do_syscall5)(__NR_lwp_sigqueue, lwpid, signo,
                                   0, VKI_SI_LWP, 0);
#        endif
#     else
         res = VG_(do_syscall2)(__NR_lwp_kill, lwpid, signo);
#     endif
   return sr_isError(res) ? -1 : 0;

#  elif defined(VGO_freebsd)
   SysRes res;
   res = VG_(do_syscall2)(__NR_thr_kill, lwpid, signo);
   return sr_isError(res) ? -1 : 0;

#  else
#    error "Unsupported plat"
#  endif
}

/* ---------------------- sigtimedwait_zero ----------------------- */

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

   The Darwin implementation is horrible and probably broken in a dozen
   obscure ways.  I suspect it's only thread-safe because V forces
   single-threadedness. */

/* ---------- sigtimedwait_zero: Linux ----------- */

#if defined(VGO_linux)
Int VG_(sigtimedwait_zero)( const vki_sigset_t *set,
                            vki_siginfo_t *info )
{
   static const struct vki_timespec zero = { 0, 0 };
   SysRes res = VG_(do_syscall4)(__NR_rt_sigtimedwait, (UWord)set, (UWord)info, 
                                 (UWord)&zero, sizeof(*set));
   return sr_isError(res) ? -1 : sr_Res(res);
}

/* ---------- sigtimedwait_zero: Darwin ----------- */

#elif defined(VGO_darwin)

//static void show_set ( HChar* str, const vki_sigset_t* set ) {
//   Int i;
//   VG_(printf)("%s { ", str);
//   for (i = 1; i <= _VKI_NSIG; i++) {
//     if (VG_(sigismember)(set, i))
//         VG_(printf)("%u ", i);
//   }
//   VG_(printf)("}\n");
//}

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
   /* XXX this is wrong -- get rid of these.  We could
      get _any_ signal here */
   vg_assert(sig != VKI_SIGILL);
   vg_assert(sig != VKI_SIGSEGV);
   vg_assert(sig != VKI_SIGBUS);
   vg_assert(sig != VKI_SIGTRAP);
   /* do nothing */ 
}

Int VG_(sigtimedwait_zero)( const vki_sigset_t *set, 
                            vki_siginfo_t *info )
{
  const Bool debug = False;
  Int    i, ir;
  SysRes sr;
  vki_sigset_t pending, blocked, allbutone;
  vki_sigaction_toK_t   sa, saved_sa2;
  vki_sigaction_fromK_t saved_sa;

  //show_set("STWZ: looking for", set);

  /* Find out what's pending: Darwin sigpending */
  sr = VG_(do_syscall1)(__NR_sigpending, (UWord)&pending);
  vg_assert(!sr_isError(sr));

  /* don't try for signals not in 'set' */
  /* pending = pending `intersect` set */
  VG_(sigintersectset)(&pending, (const vki_sigset_t*)set);

  /* don't try for signals not blocked at the moment */
  ir = VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &blocked);
  vg_assert(ir == 0);

  /* pending = pending `intersect` blocked */
  VG_(sigintersectset)(&pending, &blocked);

  /* decide which signal we're going to snarf */
  for (i = 1; i < _VKI_NSIG; i++)
     if (VG_(sigismember)(&pending,i))
        break;

  if (i == _VKI_NSIG)
     return 0;

  if (debug)
     VG_(debugLog)(0, "libcsignal",
                      "sigtimedwait_zero: snarfing signal %d\n", i );

  /* fetch signal i.
     pre: i is blocked and pending
     pre: we are the only thread running 
  */
  /* Set up alternative signal handler */
  VG_(sigfillset)(&sa.sa_mask);
  sa.ksa_handler = &sigtimedwait_zero_handler;
  sa.sa_flags    = 0;
  ir = VG_(sigaction)(i, &sa, &saved_sa);
  vg_assert(ir == 0);

  /* Switch signal masks and wait for the signal.  This should happen
     immediately, since we've already established it is pending and
     blocked. */
  VG_(sigfillset)(&allbutone);
  VG_(sigdelset)(&allbutone, i);
  /* Note: pass the sig mask by value here, not reference (!) */
  vg_assert(_VKI_NSIG_WORDS == 1);
  sr = VG_(do_syscall3)(__NR_sigsuspend_nocancel,
                        (UWord)allbutone.sig[0], 0,0);
  if (debug)
     VG_(debugLog)(0, "libcsignal",
                      "sigtimedwait_zero: sigsuspend got "
                      "res: %s %#lx\n", 
                      sr_isError(sr) ? "FAIL" : "SUCCESS",
                      sr_isError(sr) ? sr_Err(sr) : sr_Res(sr));
  vg_assert(sr_isError(sr));
  vg_assert(sr_Err(sr) == VKI_EINTR);

  /* Restore signal's handler to whatever it was before */
  VG_(convert_sigaction_fromK_to_toK)( &saved_sa, &saved_sa2 );
  ir = VG_(sigaction)(i, &saved_sa2, NULL);
  vg_assert(ir == 0);

  /* This is bogus - we could get more info from the sighandler. */
  VG_(memset)( info, 0, sizeof(*info) );
  info->si_signo = i;

  return i;
}

#elif defined(VGO_solaris)
Int VG_(sigtimedwait_zero)( const vki_sigset_t *set, vki_siginfo_t *info )
{
   /* Trivial as on Linux. */
   static const struct vki_timespec zero = { 0, 0 };
   SysRes res = VG_(do_syscall3)(__NR_sigtimedwait, (UWord)set, (UWord)info,
                                 (UWord)&zero);
   return sr_isError(res) ? -1 : sr_Res(res);
}

#elif defined(VGO_freebsd)


Int VG_(sigtimedwait_zero)( const vki_sigset_t *set, 
                            vki_siginfo_t *info )
{
   static const struct vki_timespec zero = { 0, 0 };

   SysRes res = VG_(do_syscall3)(__NR_sigtimedwait, (UWord)set, (UWord)info,
                                   (UWord)&zero);
   return sr_isError(res) ? -1 : sr_Res(res);
}

#else
#  error "Unknown OS"
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
