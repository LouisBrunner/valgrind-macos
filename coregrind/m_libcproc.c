
/*--------------------------------------------------------------------*/
/*--- Process-related libc stuff.                     m_libcproc.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward 
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
#include "pub_core_machine.h"    // For VG_(machine_get_VexArchInfo)
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_seqmatch.h"
#include "pub_core_mallocfree.h"
#include "pub_core_syscall.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"

#if defined(VGO_darwin)
/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <mach/mach.h>   /* mach_thread_self */
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */
#endif

/* IMPORTANT: on Darwin it is essential to use the _nocancel versions
   of syscalls rather than the vanilla version, if a _nocancel version
   is available.  See docs/internals/Darwin-notes.txt for the reason
   why. */

/* ---------------------------------------------------------------------
   Command line and environment stuff
   ------------------------------------------------------------------ */

/* As deduced from sp_at_startup, the client's argc, argv[] and
   envp[] as extracted from the client's stack at startup-time. */
HChar** VG_(client_envp) = NULL;

/* Path to library directory */
const HChar *VG_(libdir) = VG_LIBDIR;

const HChar *VG_(LD_PRELOAD_var_name) =
#if defined(VGO_linux)
   "LD_PRELOAD";
#elif defined(VGO_darwin)
   "DYLD_INSERT_LIBRARIES";
#else
#  error Unknown OS
#endif

/* We do getenv without libc's help by snooping around in
   VG_(client_envp) as determined at startup time. */
HChar *VG_(getenv)(const HChar *varname)
{
   Int i, n;
   vg_assert( VG_(client_envp) );
   n = VG_(strlen)(varname);
   for (i = 0; VG_(client_envp)[i] != NULL; i++) {
      HChar* s = VG_(client_envp)[i];
      if (VG_(strncmp)(varname, s, n) == 0 && s[n] == '=') {
         return & s[n+1];
      }
   }
   return NULL;
}

void  VG_(env_unsetenv) ( HChar **env, const HChar *varname )
{
   HChar **from, **to;
   vg_assert(env);
   vg_assert(varname);
   to = NULL;
   Int len = VG_(strlen)(varname);

   for (from = to = env; from && *from; from++) {
      if (!(VG_(strncmp)(varname, *from, len) == 0 && (*from)[len] == '=')) {
	 *to = *from;
	 to++;
      }
   }
   *to = *from;
}

/* set the environment; returns the old env if a new one was allocated */
HChar **VG_(env_setenv) ( HChar ***envp, const HChar* varname,
                          const HChar *val )
{
   HChar **env = (*envp);
   HChar **cpp;
   Int len = VG_(strlen)(varname);
   HChar *valstr = VG_(arena_malloc)(VG_AR_CORE, "libcproc.es.1",
                                     len + VG_(strlen)(val) + 2);
   HChar **oldenv = NULL;

   VG_(sprintf)(valstr, "%s=%s", varname, val);

   for (cpp = env; cpp && *cpp; cpp++) {
      if (VG_(strncmp)(varname, *cpp, len) == 0 && (*cpp)[len] == '=') {
	 *cpp = valstr;
	 return oldenv;
      }
   }

   if (env == NULL) {
      env = VG_(arena_malloc)(VG_AR_CORE, "libcproc.es.2", sizeof(HChar *) * 2);
      env[0] = valstr;
      env[1] = NULL;

      *envp = env;

   }  else {
      Int envlen = (cpp-env) + 2;
      HChar **newenv = VG_(arena_malloc)(VG_AR_CORE, "libcproc.es.3",
                                         envlen * sizeof(HChar *));

      for (cpp = newenv; *env; )
	 *cpp++ = *env++;
      *cpp++ = valstr;
      *cpp++ = NULL;

      oldenv = *envp;

      *envp = newenv;
   }

   return oldenv;
}


/* Walk through a colon-separated environment variable, and remove the
   entries which match remove_pattern.  It slides everything down over
   the removed entries, and pads the remaining space with '\0'.  It
   modifies the entries in place (in the client address space), but it
   shouldn't matter too much, since we only do this just before an
   execve().

   This is also careful to mop up any excess ':'s, since empty strings
   delimited by ':' are considered to be '.' in a path.
*/
static void mash_colon_env(HChar *varp, const HChar *remove_pattern)
{
   HChar *const start = varp;
   HChar *entry_start = varp;
   HChar *output = varp;

   if (varp == NULL)
      return;

   while(*varp) {
      if (*varp == ':') {
	 HChar prev;
	 Bool match;

	 /* This is a bit subtle: we want to match against the entry
	    we just copied, because it may have overlapped with
	    itself, junking the original. */

	 prev = *output;
	 *output = '\0';

	 match = VG_(string_match)(remove_pattern, entry_start);

	 *output = prev;
	 
	 if (match) {
	    output = entry_start;
	    varp++;			/* skip ':' after removed entry */
	 } else
	    entry_start = output+1;	/* entry starts after ':' */
      }

      if (*varp)
         *output++ = *varp++;
   }

   /* make sure last entry is nul terminated */
   *output = '\0';

   /* match against the last entry */
   if (VG_(string_match)(remove_pattern, entry_start)) {
      output = entry_start;
      if (output > start) {
	 /* remove trailing ':' */
	 output--;
	 vg_assert(*output == ':');
      }
   }	 

   /* pad out the left-overs with '\0' */
   while(output < varp)
      *output++ = '\0';
}


// Removes all the Valgrind-added stuff from the passed environment.  Used
// when starting child processes, so they don't see that added stuff.
void VG_(env_remove_valgrind_env_stuff)(HChar** envp)
{

#if defined(VGO_darwin)

   // Environment cleanup is also handled during parent launch 
   // in vg_preloaded.c:vg_cleanup_env().

#endif

   Int i;
   HChar* ld_preload_str = NULL;
   HChar* ld_library_path_str = NULL;
   HChar* dyld_insert_libraries_str = NULL;
   HChar* buf;

   // Find LD_* variables
   // DDD: should probably conditionally compiled some of this:
   // - LD_LIBRARY_PATH is universal?
   // - LD_PRELOAD is on Linux, not on Darwin, not sure about AIX
   // - DYLD_INSERT_LIBRARIES and DYLD_SHARED_REGION are Darwin-only
   for (i = 0; envp[i] != NULL; i++) {
      if (VG_(strncmp)(envp[i], "LD_PRELOAD=", 11) == 0) {
         envp[i] = VG_(arena_strdup)(VG_AR_CORE, "libcproc.erves.1", envp[i]);
         ld_preload_str = &envp[i][11];
      }
      if (VG_(strncmp)(envp[i], "LD_LIBRARY_PATH=", 16) == 0) {
         envp[i] = VG_(arena_strdup)(VG_AR_CORE, "libcproc.erves.2", envp[i]);
         ld_library_path_str = &envp[i][16];
      }
      if (VG_(strncmp)(envp[i], "DYLD_INSERT_LIBRARIES=", 22) == 0) {
         envp[i] = VG_(arena_strdup)(VG_AR_CORE, "libcproc.erves.3", envp[i]);
         dyld_insert_libraries_str = &envp[i][22];
      }
   }

   buf = VG_(arena_malloc)(VG_AR_CORE, "libcproc.erves.4",
                           VG_(strlen)(VG_(libdir)) + 20);

   // Remove Valgrind-specific entries from LD_*.
   VG_(sprintf)(buf, "%s*/vgpreload_*.so", VG_(libdir));
   mash_colon_env(ld_preload_str, buf);
   mash_colon_env(dyld_insert_libraries_str, buf);
   VG_(sprintf)(buf, "%s*", VG_(libdir));
   mash_colon_env(ld_library_path_str, buf);

   // Remove VALGRIND_LAUNCHER variable.
   VG_(env_unsetenv)(envp, VALGRIND_LAUNCHER);

   // Remove DYLD_SHARED_REGION variable.
   VG_(env_unsetenv)(envp, "DYLD_SHARED_REGION");

   // XXX if variable becomes empty, remove it completely?

   VG_(arena_free)(VG_AR_CORE, buf);
}

/* ---------------------------------------------------------------------
   Various important syscall wrappers
   ------------------------------------------------------------------ */

Int VG_(waitpid)(Int pid, Int *status, Int options)
{
#  if defined(VGO_linux)
   SysRes res = VG_(do_syscall4)(__NR_wait4,
                                 pid, (UWord)status, options, 0);
   return sr_isError(res) ? -1 : sr_Res(res);
#  elif defined(VGO_darwin)
   SysRes res = VG_(do_syscall4)(__NR_wait4_nocancel,
                                 pid, (UWord)status, options, 0);
   return sr_isError(res) ? -1 : sr_Res(res);
#  else
#    error Unknown OS
#  endif
}

/* clone the environment */
HChar **VG_(env_clone) ( HChar **oldenv )
{
   HChar **oldenvp;
   HChar **newenvp;
   HChar **newenv;
   Int  envlen;

   vg_assert(oldenv);
   for (oldenvp = oldenv; oldenvp && *oldenvp; oldenvp++);

   envlen = oldenvp - oldenv + 1;
   
   newenv = VG_(arena_malloc)(VG_AR_CORE, "libcproc.ec.1",
                              envlen * sizeof(HChar *));

   oldenvp = oldenv;
   newenvp = newenv;
   
   while (oldenvp && *oldenvp) {
      *newenvp++ = *oldenvp++;
   }
   
   *newenvp = *oldenvp;

   return newenv;
}

void VG_(execv) ( const HChar* filename, HChar** argv )
{
   HChar** envp;
   SysRes res;

   /* restore the DATA rlimit for the child */
   VG_(setrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));

   envp = VG_(env_clone)(VG_(client_envp));
   VG_(env_remove_valgrind_env_stuff)( envp );

   res = VG_(do_syscall3)(__NR_execve,
                          (UWord)filename, (UWord)argv, (UWord)envp);

   VG_(printf)("EXEC failed, errno = %lld\n", (Long)sr_Err(res));
}

/* Return -1 if error, else 0.  NOTE does not indicate return code of
   child! */
Int VG_(system) ( const HChar* cmd )
{
   Int pid;
   if (cmd == NULL)
      return 1;
   pid = VG_(fork)();
   if (pid < 0)
      return -1;
   if (pid == 0) {
      /* child */
      const HChar* argv[4] = { "/bin/sh", "-c", cmd, 0 };
      VG_(execv)(argv[0], (HChar **)argv);

      /* If we're still alive here, execv failed. */
      VG_(exit)(1);
   } else {
      /* parent */
      /* We have to set SIGCHLD to its default behaviour in order that
         VG_(waitpid) works (at least on AIX).  According to the Linux
         man page for waitpid:

         POSIX.1-2001 specifies that if the disposition of SIGCHLD is
         set to SIG_IGN or the SA_NOCLDWAIT flag is set for SIGCHLD
         (see sigaction(2)), then children that terminate do not
         become zombies and a call to wait() or waitpid() will block
         until all children have terminated, and then fail with errno
         set to ECHILD.  (The original POSIX standard left the
         behaviour of setting SIGCHLD to SIG_IGN unspecified.)
      */
      Int ir, zzz;
      vki_sigaction_toK_t sa, sa2;
      vki_sigaction_fromK_t saved_sa;
      VG_(memset)( &sa, 0, sizeof(sa) );
      VG_(sigemptyset)(&sa.sa_mask);
      sa.ksa_handler = VKI_SIG_DFL;
      sa.sa_flags    = 0;
      ir = VG_(sigaction)(VKI_SIGCHLD, &sa, &saved_sa);
      vg_assert(ir == 0);

      zzz = VG_(waitpid)(pid, NULL, 0);

      VG_(convert_sigaction_fromK_to_toK)( &saved_sa, &sa2 );
      ir = VG_(sigaction)(VKI_SIGCHLD, &sa2, NULL);
      vg_assert(ir == 0);
      return zzz == -1 ? -1 : 0;
   }
}

/* ---------------------------------------------------------------------
   Resource limits
   ------------------------------------------------------------------ */

/* Support for getrlimit. */
Int VG_(getrlimit) (Int resource, struct vki_rlimit *rlim)
{
   SysRes res = VG_(mk_SysRes_Error)(VKI_ENOSYS);
   /* res = getrlimit( resource, rlim ); */
#  ifdef __NR_ugetrlimit
   res = VG_(do_syscall2)(__NR_ugetrlimit, resource, (UWord)rlim);
#  endif
   if (sr_isError(res) && sr_Err(res) == VKI_ENOSYS)
      res = VG_(do_syscall2)(__NR_getrlimit, resource, (UWord)rlim);
   return sr_isError(res) ? -1 : sr_Res(res);
}


/* Support for setrlimit. */
Int VG_(setrlimit) (Int resource, const struct vki_rlimit *rlim)
{
   SysRes res;
   /* res = setrlimit( resource, rlim ); */
   res = VG_(do_syscall2)(__NR_setrlimit, resource, (UWord)rlim);
   return sr_isError(res) ? -1 : sr_Res(res);
}

/* Support for prctl. */
Int VG_(prctl) (Int option, 
                ULong arg2, ULong arg3, ULong arg4, ULong arg5)
{
   SysRes res = VG_(mk_SysRes_Error)(VKI_ENOSYS);
#  if defined(VGO_linux)
   /* res = prctl( option, arg2, arg3, arg4, arg5 ); */
   res = VG_(do_syscall5)(__NR_prctl, (UWord) option,
                          (UWord) arg2, (UWord) arg3, (UWord) arg4,
                          (UWord) arg5);
#  endif

   return sr_isError(res) ? -1 : sr_Res(res);
}

/* ---------------------------------------------------------------------
   pids, etc
   ------------------------------------------------------------------ */

Int VG_(gettid)(void)
{
#  if defined(VGO_linux)
   SysRes res = VG_(do_syscall0)(__NR_gettid);

   if (sr_isError(res) && sr_Res(res) == VKI_ENOSYS) {
      HChar pid[16];      
      /*
       * The gettid system call does not exist. The obvious assumption
       * to make at this point would be that we are running on an older
       * system where the getpid system call actually returns the ID of
       * the current thread.
       *
       * Unfortunately it seems that there are some systems with a kernel
       * where getpid has been changed to return the ID of the thread group
       * leader but where the gettid system call has not yet been added.
       *
       * So instead of calling getpid here we use readlink to see where
       * the /proc/self link is pointing...
       */

#     if defined(VGP_arm64_linux)
      res = VG_(do_syscall4)(__NR_readlinkat, VKI_AT_FDCWD,
                             (UWord)"/proc/self",
                             (UWord)pid, sizeof(pid));
#     else
      res = VG_(do_syscall3)(__NR_readlink, (UWord)"/proc/self",
                             (UWord)pid, sizeof(pid));
#     endif
      if (!sr_isError(res) && sr_Res(res) > 0) {
         HChar* s;
         pid[sr_Res(res)] = '\0';
         res = VG_(mk_SysRes_Success)(  VG_(strtoll10)(pid, &s) );
         if (*s != '\0') {
            VG_(message)(Vg_DebugMsg, 
               "Warning: invalid file name linked to by /proc/self: %s\n",
               pid);
         }
      }
   }

   return sr_Res(res);

#  elif defined(VGO_darwin)
   // Darwin's gettid syscall is something else.
   // Use Mach thread ports for lwpid instead.
   return mach_thread_self();

#  else
#    error "Unknown OS"
#  endif
}

/* You'd be amazed how many places need to know the current pid. */
Int VG_(getpid) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
   return sr_Res( VG_(do_syscall0)(__NR_getpid) );
}

Int VG_(getpgrp) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
   return sr_Res( VG_(do_syscall0)(__NR_getpgrp) );
}

Int VG_(getppid) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
   return sr_Res( VG_(do_syscall0)(__NR_getppid) );
}

Int VG_(geteuid) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
#  if defined(__NR_geteuid32)
   // We use the 32-bit version if it's supported.  Otherwise, IDs greater
   // than 65536 cause problems, as bug #151209 showed.
   return sr_Res( VG_(do_syscall0)(__NR_geteuid32) );
#  else
   return sr_Res( VG_(do_syscall0)(__NR_geteuid) );
#  endif
}

Int VG_(getegid) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
#  if defined(__NR_getegid32)
   // We use the 32-bit version if it's supported.  Otherwise, IDs greater
   // than 65536 cause problems, as bug #151209 showed.
   return sr_Res( VG_(do_syscall0)(__NR_getegid32) );
#  else
   return sr_Res( VG_(do_syscall0)(__NR_getegid) );
#  endif
}

/* Get supplementary groups into list[0 .. size-1].  Returns the
   number of groups written, or -1 if error.  Note that in order to be
   portable, the groups are 32-bit unsigned ints regardless of the
   platform. */
Int VG_(getgroups)( Int size, UInt* list )
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_mips64_linux)
   Int    i;
   SysRes sres;
   UShort list16[64];
   if (size < 0) return -1;
   if (size > 64) size = 64;
   sres = VG_(do_syscall2)(__NR_getgroups, size, (Addr)list16);
   if (sr_isError(sres))
      return -1;
   if (sr_Res(sres) > size)
      return -1;
   for (i = 0; i < sr_Res(sres); i++)
      list[i] = (UInt)list16[i];
   return sr_Res(sres);

#  elif defined(VGP_amd64_linux) || defined(VGP_ppc64_linux)  \
        || defined(VGP_arm_linux)                             \
        || defined(VGO_darwin) || defined(VGP_s390x_linux)    \
        || defined(VGP_mips32_linux) || defined(VGP_arm64_linux)
   SysRes sres;
   sres = VG_(do_syscall2)(__NR_getgroups, size, (Addr)list);
   if (sr_isError(sres))
      return -1;
   return sr_Res(sres);

#  else
#     error "VG_(getgroups): needs implementation on this platform"
#  endif
}

/* ---------------------------------------------------------------------
   Process tracing
   ------------------------------------------------------------------ */

Int VG_(ptrace) ( Int request, Int pid, void *addr, void *data )
{
   SysRes res;
   res = VG_(do_syscall4)(__NR_ptrace, request, pid, (UWord)addr, (UWord)data);
   if (sr_isError(res))
      return -1;
   return sr_Res(res);
}

/* ---------------------------------------------------------------------
   Fork
   ------------------------------------------------------------------ */

Int VG_(fork) ( void )
{
#  if defined(VGO_linux)
   SysRes res;
   res = VG_(do_syscall0)(__NR_fork);
   if (sr_isError(res))
      return -1;
   return sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall0)(__NR_fork); /* __NR_fork is UX64 */
   if (sr_isError(res))
      return -1;
   /* on success: wLO = child pid; wHI = 1 for child, 0 for parent */
   if (sr_ResHI(res) != 0) {
      return 0;  /* this is child: return 0 instead of child pid */
   }
   return sr_Res(res);

#  else
#    error "Unknown OS"
#  endif
}

/* ---------------------------------------------------------------------
   Timing stuff
   ------------------------------------------------------------------ */

UInt VG_(read_millisecond_timer) ( void )
{
   /* 'now' and 'base' are in microseconds */
   static ULong base = 0;
   ULong  now;

#  if defined(VGO_linux)
   { SysRes res;
     struct vki_timespec ts_now;
     res = VG_(do_syscall2)(__NR_clock_gettime, VKI_CLOCK_MONOTONIC,
                            (UWord)&ts_now);
     if (sr_isError(res) == 0) {
        now = ts_now.tv_sec * 1000000ULL + ts_now.tv_nsec / 1000;
     } else {
       struct vki_timeval tv_now;
       res = VG_(do_syscall2)(__NR_gettimeofday, (UWord)&tv_now, (UWord)NULL);
       vg_assert(! sr_isError(res));
       now = tv_now.tv_sec * 1000000ULL + tv_now.tv_usec;
     }
   }

#  elif defined(VGO_darwin)
   // Weird: it seems that gettimeofday() doesn't fill in the timeval, but
   // rather returns the tv_sec as the low 32 bits of the result and the
   // tv_usec as the high 32 bits of the result.  (But the timeval cannot be
   // NULL!)  See bug 200990.
   { SysRes res;
     struct vki_timeval tv_now = { 0, 0 };
     res = VG_(do_syscall2)(__NR_gettimeofday, (UWord)&tv_now, (UWord)NULL);
     vg_assert(! sr_isError(res));
     now = sr_Res(res) * 1000000ULL + sr_ResHI(res);
   }

#  else
#    error "Unknown OS"
#  endif

   /* COMMON CODE */  
   if (base == 0)
      base = now;

   return (now - base) / 1000;
}


/* ---------------------------------------------------------------------
   atfork()
   ------------------------------------------------------------------ */

struct atfork {
   vg_atfork_t  pre;
   vg_atfork_t  parent;
   vg_atfork_t  child;
};

#define VG_MAX_ATFORK 10

static struct atfork atforks[VG_MAX_ATFORK];
static Int n_atfork = 0;

void VG_(atfork)(vg_atfork_t pre, vg_atfork_t parent, vg_atfork_t child)
{
   Int i;

   for (i = 0; i < n_atfork; i++) {
      if (atforks[i].pre == pre &&
          atforks[i].parent == parent &&
          atforks[i].child == child)
         return;
   }

   if (n_atfork >= VG_MAX_ATFORK)
      VG_(core_panic)(
         "Too many VG_(atfork) handlers requested: raise VG_MAX_ATFORK");

   atforks[n_atfork].pre    = pre;
   atforks[n_atfork].parent = parent;
   atforks[n_atfork].child  = child;

   n_atfork++;
}

void VG_(do_atfork_pre)(ThreadId tid)
{
   Int i;

   for (i = 0; i < n_atfork; i++)
      if (atforks[i].pre != NULL)
         (*atforks[i].pre)(tid);
}

void VG_(do_atfork_parent)(ThreadId tid)
{
   Int i;

   for (i = 0; i < n_atfork; i++)
      if (atforks[i].parent != NULL)
         (*atforks[i].parent)(tid);
}

void VG_(do_atfork_child)(ThreadId tid)
{
   Int i;

   for (i = 0; i < n_atfork; i++)
      if (atforks[i].child != NULL)
         (*atforks[i].child)(tid);
}


/* ---------------------------------------------------------------------
   icache invalidation
   ------------------------------------------------------------------ */

void VG_(invalidate_icache) ( void *ptr, SizeT nbytes )
{
   if (nbytes == 0) return;    // nothing to do

   // Get cache info
   VexArchInfo vai;
   VG_(machine_get_VexArchInfo)(NULL, &vai);

   // If I-caches are coherent, nothing needs to be done here
   if (vai.hwcache_info.icaches_maintain_coherence) return;

#  if defined(VGA_ppc32) || defined(VGA_ppc64)
   Addr startaddr = (Addr) ptr;
   Addr endaddr   = startaddr + nbytes;
   Addr cls;
   Addr addr;

   VG_(machine_get_VexArchInfo)( NULL, &vai );
   cls = vai.ppc_icache_line_szB;

   /* Stay sane .. */
   vg_assert(cls == 16 || cls == 32 || cls == 64 || cls == 128);

   startaddr &= ~(cls - 1);
   for (addr = startaddr; addr < endaddr; addr += cls) {
      __asm__ __volatile__("dcbst 0,%0" : : "r" (addr));
   }
   __asm__ __volatile__("sync");
   for (addr = startaddr; addr < endaddr; addr += cls) {
      __asm__ __volatile__("icbi 0,%0" : : "r" (addr));
   }
   __asm__ __volatile__("sync; isync");

#  elif defined(VGP_arm_linux)
   /* ARM cache flushes are privileged, so we must defer to the kernel. */
   Addr startaddr = (Addr) ptr;
   Addr endaddr   = startaddr + nbytes;
   VG_(do_syscall2)(__NR_ARM_cacheflush, startaddr, endaddr);

#  elif defined(VGP_arm64_linux)
   // This arm64_linux section of this function VG_(invalidate_icache)
   // is copied from
   // https://github.com/armvixl/vixl/blob/master/src/a64/cpu-a64.cc
   // which has the following copyright notice:
   /*
   Copyright 2013, ARM Limited
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:
   
   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.
   * Neither the name of ARM Limited nor the names of its contributors may be
     used to endorse or promote products derived from this software without
     specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS CONTRIBUTORS "AS IS" AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   */

   // Ask what the I and D line sizes are
   UInt cache_type_register;
   // Copy the content of the cache type register to a core register.
   __asm__ __volatile__ ("mrs %[ctr], ctr_el0" // NOLINT
                         : [ctr] "=r" (cache_type_register));

   const Int kDCacheLineSizeShift = 16;
   const Int kICacheLineSizeShift = 0;
   const UInt kDCacheLineSizeMask = 0xf << kDCacheLineSizeShift;
   const UInt kICacheLineSizeMask = 0xf << kICacheLineSizeShift;

   // The cache type register holds the size of the I and D caches as a power of
   // two.
   const UInt dcache_line_size_power_of_two =
       (cache_type_register & kDCacheLineSizeMask) >> kDCacheLineSizeShift;
   const UInt icache_line_size_power_of_two =
       (cache_type_register & kICacheLineSizeMask) >> kICacheLineSizeShift;

   const UInt dcache_line_size_ = 4 * (1 << dcache_line_size_power_of_two);
   const UInt icache_line_size_ = 4 * (1 << icache_line_size_power_of_two);

   Addr start = (Addr)ptr;
   // Sizes will be used to generate a mask big enough to cover a pointer.
   Addr dsize = (Addr)dcache_line_size_;
   Addr isize = (Addr)icache_line_size_;

   // Cache line sizes are always a power of 2.
   Addr dstart = start & ~(dsize - 1);
   Addr istart = start & ~(isize - 1);
   Addr end    = start + nbytes;

   __asm__ __volatile__ (
     // Clean every line of the D cache containing the target data.
     "0: \n\t"
     // dc : Data Cache maintenance
     // c : Clean
     // va : by (Virtual) Address
     // u : to the point of Unification
     // The point of unification for a processor is the point by which the
     // instruction and data caches are guaranteed to see the same copy of a
     // memory location. See ARM DDI 0406B page B2-12 for more information.
     "dc cvau, %[dline] \n\t"
     "add %[dline], %[dline], %[dsize] \n\t"
     "cmp %[dline], %[end] \n\t"
     "b.lt 0b \n\t"
     // Barrier to make sure the effect of the code above is visible to the rest
     // of the world.
     // dsb : Data Synchronisation Barrier
     // ish : Inner SHareable domain
     // The point of unification for an Inner Shareable shareability domain is
     // the point by which the instruction and data caches of all the processors
     // in that Inner Shareable shareability domain are guaranteed to see the
     // same copy of a memory location. See ARM DDI 0406B page B2-12 for more
     // information.
     "dsb ish \n\t"
     // Invalidate every line of the I cache containing the target data.
     "1: \n\t"
     // ic : instruction cache maintenance
     // i : invalidate
     // va : by address
     // u : to the point of unification
     "ic ivau, %[iline] \n\t"
     "add %[iline], %[iline], %[isize] \n\t"
     "cmp %[iline], %[end] \n\t"
     "b.lt 1b \n\t"
     // Barrier to make sure the effect of the code above is visible to the rest
     // of the world.
     "dsb ish \n\t"
     // Barrier to ensure any prefetching which happened before this code is
     // discarded.
     // isb : Instruction Synchronisation Barrier
     "isb \n\t"
     : [dline] "+r" (dstart),
       [iline] "+r" (istart)
     : [dsize] "r" (dsize),
       [isize] "r" (isize),
       [end] "r" (end)
     // This code does not write to memory but without the dependency gcc might
     // move this code before the code is generated.
     : "cc", "memory"
   );

#  elif defined(VGA_mips32) || defined(VGA_mips64)
   SysRes sres = VG_(do_syscall3)(__NR_cacheflush, (UWord) ptr,
                                 (UWord) nbytes, (UWord) 3);
   vg_assert( sres._isError == 0 );

#  endif
}


/* ---------------------------------------------------------------------
   dcache flushing
   ------------------------------------------------------------------ */

void VG_(flush_dcache) ( void *ptr, SizeT nbytes )
{
   /* Currently this is only required on ARM64. */
#  if defined(VGA_arm64)
   Addr startaddr = (Addr) ptr;
   Addr endaddr   = startaddr + nbytes;
   Addr cls;
   Addr addr;

   ULong ctr_el0;
   __asm__ __volatile__ ("mrs %0, ctr_el0" : "=r"(ctr_el0));
   cls = 4 * (1ULL << (0xF & (ctr_el0 >> 16)));

   /* Stay sane .. */
   vg_assert(cls == 64);

   startaddr &= ~(cls - 1);
   for (addr = startaddr; addr < endaddr; addr += cls) {
      __asm__ __volatile__("dc cvau, %0" : : "r" (addr));
   }
   __asm__ __volatile__("dsb ish");
#  endif
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
