
/*--------------------------------------------------------------------*/
/*--- Process-related libc stuff.                     m_libcproc.c ---*/
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
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_mallocfree.h"
#include "pub_core_syscall.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"

/* ---------------------------------------------------------------------
   Command line and environment stuff
   ------------------------------------------------------------------ */

/* As deduced from sp_at_startup, the client's argc, argv[] and
   envp[] as extracted from the client's stack at startup-time. */
Char** VG_(client_envp);

/* Path to library directory */
const Char *VG_(libdir) = VG_LIBDIR;

/* We do getenv without libc's help by snooping around in
   VG_(client_envp) as determined at startup time. */
Char *VG_(getenv)(Char *varname)
{
   Int i, n;
   n = VG_(strlen)(varname);
   for (i = 0; VG_(client_envp)[i] != NULL; i++) {
      Char* s = VG_(client_envp)[i];
      if (VG_(strncmp)(varname, s, n) == 0 && s[n] == '=') {
         return & s[n+1];
      }
   }
   return NULL;
}

void  VG_(env_unsetenv) ( Char **env, const Char *varname )
{
   Char **from;
   Char **to = NULL;
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
Char **VG_(env_setenv) ( Char ***envp, const Char* varname, const Char *val )
{
   Char **env = (*envp);
   Char **cpp;
   Int len = VG_(strlen)(varname);
   Char *valstr = VG_(arena_malloc)(VG_AR_CORE, len + VG_(strlen)(val) + 2);
   Char **oldenv = NULL;

   VG_(sprintf)(valstr, "%s=%s", varname, val);

   for (cpp = env; cpp && *cpp; cpp++) {
      if (VG_(strncmp)(varname, *cpp, len) == 0 && (*cpp)[len] == '=') {
	 *cpp = valstr;
	 return oldenv;
      }
   }

   if (env == NULL) {
      env = VG_(arena_malloc)(VG_AR_CORE, sizeof(Char **) * 2);
      env[0] = valstr;
      env[1] = NULL;

      *envp = env;

   }  else {
      Int envlen = (cpp-env) + 2;
      Char **newenv = VG_(arena_malloc)(VG_AR_CORE, envlen * sizeof(Char **));

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
static void mash_colon_env(Char *varp, const Char *remove_pattern)
{
   Char *const start = varp;
   Char *entry_start = varp;
   Char *output = varp;

   if (varp == NULL)
      return;

   while(*varp) {
      if (*varp == ':') {
	 Char prev;
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

      *output++ = *varp++;
   }

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
void VG_(env_remove_valgrind_env_stuff)(Char** envp)
{
   Int i;
   Char* ld_preload_str = NULL;
   Char* ld_library_path_str = NULL;
   Char* buf;

   // Find LD_* variables
   for (i = 0; envp[i] != NULL; i++) {
      if (VG_(strncmp)(envp[i], "LD_PRELOAD=", 11) == 0)
         ld_preload_str = &envp[i][11];
      if (VG_(strncmp)(envp[i], "LD_LIBRARY_PATH=", 16) == 0)
         ld_library_path_str = &envp[i][16];
   }

   buf = VG_(arena_malloc)(VG_AR_CORE, VG_(strlen)(VG_(libdir)) + 20);

   // Remove Valgrind-specific entries from LD_*.
   VG_(sprintf)(buf, "%s*/vgpreload_*.so", VG_(libdir));
   mash_colon_env(ld_preload_str, buf);
   VG_(sprintf)(buf, "%s*", VG_(libdir));
   mash_colon_env(ld_library_path_str, buf);

   // Remove VALGRIND_LAUNCHER variable.
   VG_(env_unsetenv)(envp, VALGRIND_LAUNCHER);

   // XXX if variable becomes empty, remove it completely?

   VG_(arena_free)(VG_AR_CORE, buf);
}

/* ---------------------------------------------------------------------
   Various important syscall wrappers
   ------------------------------------------------------------------ */

Int VG_(waitpid)(Int pid, Int *status, Int options)
{
#  if defined(VGO_linux)
   SysRes res = VG_(do_syscall4)(__NR_wait4, pid, (UWord)status, options, 0);
   return res.isError ? -1 : res.res;
#  elif defined(VGO_aix5)
   /* magic number 4 obtained by truss-ing a C program doing
      'waitpid'.  Note status and pid args opposite way round from
      POSIX. */
   SysRes res = VG_(do_syscall5)(__NR_AIX5_kwaitpid, 
                                 (UWord)status, pid, 4 | options,0,0);
   if (0) VG_(printf)("waitpid: got 0x%x 0x%x\n", res.res, res.err);
   return res.isError ? -1 : res.res;
#  else
#    error Unknown OS
#  endif
}

/* clone the environment */
Char **VG_(env_clone) ( Char **oldenv )
{
   Char **oldenvp;
   Char **newenvp;
   Char **newenv;
   Int  envlen;

   for (oldenvp = oldenv; oldenvp && *oldenvp; oldenvp++);

   envlen = oldenvp - oldenv + 1;
   
   newenv = VG_(arena_malloc)(VG_AR_CORE, envlen * sizeof(Char **));

   oldenvp = oldenv;
   newenvp = newenv;
   
   while (oldenvp && *oldenvp) {
      *newenvp++ = *oldenvp++;
   }
   
   *newenvp = *oldenvp;

   return newenv;
}

/* Return -1 if error, else 0.  NOTE does not indicate return code of
   child! */
Int VG_(system) ( Char* cmd )
{
   Int    pid;
   SysRes res;
   if (cmd == NULL)
      return 1;
   res = VG_(do_syscall0)(__NR_fork);
   if (res.isError)
      return -1;
   pid = res.res;
   if (pid == 0) {
      /* child */
      static Char** envp = NULL;
      Char* argv[4];

      /* restore the DATA rlimit for the child */
      VG_(setrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));

      envp = VG_(env_clone)(VG_(client_envp));
      VG_(env_remove_valgrind_env_stuff)( envp ); 

      argv[0] = "/bin/sh";
      argv[1] = "-c";
      argv[2] = cmd;
      argv[3] = 0;

      (void)VG_(do_syscall3)(__NR_execve, 
                             (UWord)"/bin/sh", (UWord)argv, (UWord)envp);

      /* If we're still alive here, execve failed. */
      VG_(exit)(1);
   } else {
      /* parent */
      Int ir, zzz;
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
      struct vki_sigaction sa, saved_sa;
      VG_(memset)( &sa, 0, sizeof(struct vki_sigaction) );
      VG_(sigemptyset)(&sa.sa_mask);
      sa.ksa_handler = VKI_SIG_DFL;
      sa.sa_flags    = 0;
      ir = VG_(sigaction)(VKI_SIGCHLD, &sa, &saved_sa);
      vg_assert(ir == 0);

      zzz = VG_(waitpid)(pid, NULL, 0);

      ir = VG_(sigaction)(VKI_SIGCHLD, &saved_sa, NULL);
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
   if (res.isError && res.err == VKI_ENOSYS)
      res = VG_(do_syscall2)(__NR_getrlimit, resource, (UWord)rlim);
   return res.isError ? -1 : res.res;
}


/* Support for setrlimit. */
Int VG_(setrlimit) (Int resource, const struct vki_rlimit *rlim)
{
   SysRes res;
   /* res = setrlimit( resource, rlim ); */
   res = VG_(do_syscall2)(__NR_setrlimit, resource, (UWord)rlim);
   return res.isError ? -1 : res.res;
}

/* ---------------------------------------------------------------------
   pids, etc
   ------------------------------------------------------------------ */

Int VG_(gettid)(void)
{
#  if defined(VGO_aix5)
   SysRes res;
   Int    r;
   vg_assert(__NR_AIX5__thread_self != __NR_AIX5_UNKNOWN);
   res = VG_(do_syscall0)(__NR_AIX5__thread_self);
   r = res.res;
   return r;

#  else
   SysRes res = VG_(do_syscall0)(__NR_gettid);

   if (res.isError && res.res == VKI_ENOSYS) {
      Char pid[16];      
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

      res = VG_(do_syscall3)(__NR_readlink, (UWord)"/proc/self",
                             (UWord)pid, sizeof(pid));
      if (!res.isError && res.res > 0) {
         pid[res.res] = '\0';
         res.res = VG_(atoll)(pid);
      }
   }

   return res.res;
#  endif
}

/* You'd be amazed how many places need to know the current pid. */
Int VG_(getpid) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
   return VG_(do_syscall0)(__NR_getpid) . res;
}

Int VG_(getpgrp) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
   return VG_(do_syscall0)(__NR_getpgrp) . res;
}

Int VG_(getppid) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
   return VG_(do_syscall0)(__NR_getppid) . res;
}

Int VG_(geteuid) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
#  if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   return VG_(do_syscall1)(__NR_AIX5_getuidx, 1) . res;
#  else
   return VG_(do_syscall0)(__NR_geteuid) . res;
#  endif
}

Int VG_(getegid) ( void )
{
   /* ASSUMES SYSCALL ALWAYS SUCCEEDS */
#  if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   return VG_(do_syscall1)(__NR_AIX5_getgidx, 1) . res;
#  else
   return VG_(do_syscall0)(__NR_getegid) . res;
#  endif
}

/* Get supplementary groups into list[0 .. size-1].  Returns the
   number of groups written, or -1 if error.  Note that in order to be
   portable, the groups are 32-bit unsigned ints regardless of the
   platform. */
Int VG_(getgroups)( Int size, UInt* list )
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux)
   Int    i;
   SysRes sres;
   UShort list16[64];
   if (size < 0) return -1;
   if (size > 64) size = 64;
   sres = VG_(do_syscall2)(__NR_getgroups, size, (Addr)list16);
   if (sres.isError)
      return -1;
   if (sres.res > size)
      return -1;
   for (i = 0; i < sres.res; i++)
      list[i] = (UInt)list16[i];
   return sres.res;

#  elif defined(VGP_amd64_linux) || defined(VGP_ppc64_linux) \
        || defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   SysRes sres;
   sres = VG_(do_syscall2)(__NR_getgroups, size, (Addr)list);
   if (sres.isError)
      return -1;
   return sres.res;

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
   if (res.isError)
      return -1;
   return res.res;
}

/* ---------------------------------------------------------------------
   Fork
   ------------------------------------------------------------------ */

Int VG_(fork) ( void )
{
   SysRes res;
   res = VG_(do_syscall0)(__NR_fork);
   if (res.isError)
      return -1;
   return res.res;
}

/* ---------------------------------------------------------------------
   Timing stuff
   ------------------------------------------------------------------ */

UInt VG_(read_millisecond_timer) ( void )
{
   /* 'now' and 'base' are in microseconds */
   static ULong base = 0;
   ULong  now;

#  if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   /* AIX requires a totally different implementation since
      sys_gettimeofday doesn't exist.  We use the POWER real-time
      register facility.  This will SIGILL on PowerPC 970 on AIX,
      since PowerPC doesn't support these instructions. */
   UWord nsec, sec1, sec2;
   while (1) {
      __asm__ __volatile__ ("\n"
         "\tmfspr %0,4\n" /* 4==RTCU */
         "\tmfspr %1,5\n" /* 5==RTCL */
         "\tmfspr %2,4\n" /* 4==RTCU */
         : "=b" (sec1), "=b" (nsec), "=b" (sec2)
      );
      if (sec1 == sec2) break;
   }
   vg_assert(nsec < 1000*1000*1000);
   now  = ((ULong)sec1) * 1000000ULL;
   now += (ULong)(nsec / 1000);
#  else

   struct vki_timeval tv_now;
   SysRes res;
   res = VG_(do_syscall2)(__NR_gettimeofday, (UWord)&tv_now, (UWord)NULL);
   now = tv_now.tv_sec * 1000000ULL + tv_now.tv_usec;
#  endif
   
   if (base == 0)
      base = now;

   return (now - base) / 1000;
}

/* ---------------------------------------------------------------------
   A trivial atfork() facility for Valgrind's internal use
   ------------------------------------------------------------------ */

// Trivial because it only supports a single post-fork child action, which
// is all we need.

static vg_atfork_t atfork_child = NULL;

void VG_(atfork_child)(vg_atfork_t child)
{
   if (NULL != atfork_child)
      VG_(core_panic)("More than one atfork_child handler requested");

   atfork_child = child;
}

void VG_(do_atfork_child)(ThreadId tid)
{
   if (NULL != atfork_child)
      (*atfork_child)(tid);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
