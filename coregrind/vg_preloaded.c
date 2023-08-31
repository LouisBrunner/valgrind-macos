
/*--------------------------------------------------------------------*/
/*--- Client-space code for the core.               vg_preloaded.c ---*/
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


/* ---------------------------------------------------------------------
   ALL THE CODE IN THIS FILE RUNS ON THE SIMULATED CPU. 

   These functions are not called directly - they're the targets of code
   redirection or load notifications (see pub_core_redir.h for info).
   They're named weirdly so that the intercept code can find them when the
   shared object is initially loaded.

   Note that this filename has the "vg_" prefix because it can appear
   in stack traces, and the "vg_" makes it a little clearer that it
   originates from Valgrind.
   ------------------------------------------------------------------ */

#include "pub_core_basics.h"
#include "pub_core_clreq.h"
#include "pub_core_debuginfo.h"  // Needed for pub_core_redir.h
#include "pub_core_redir.h"      // For VG_NOTIFY_ON_LOAD

#ifdef HAVE_HEADER_FEATURES_H
#include <features.h>
#endif

#if !defined(VGO_darwin)
/* Instruct GDB via a .debug_gdb_scripts section to load the valgrind and tool
   front-end commands.  */
/* Note: The "MS" section flags are to remove duplicates.  */
#define DEFINE_GDB_PY_SCRIPT(script_name) \
  asm("\
.pushsection \".debug_gdb_scripts\", \"MS\",@progbits,1\n\
.byte 1 /* Python */\n\
.asciz \"" script_name "\"\n\
.popsection \n\
");

#ifdef VG_GDBSCRIPTS_DIR
DEFINE_GDB_PY_SCRIPT(VG_GDBSCRIPTS_DIR "/valgrind-monitor.py")
#endif
#endif

#if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)

/* ---------------------------------------------------------------------
   Hook for running __gnu_cxx::__freeres() and __libc_freeres() once
   the program exits.
   ------------------------------------------------------------------ */

void VG_NOTIFY_ON_LOAD(freeres)(Vg_FreeresToRun to_run);
void VG_NOTIFY_ON_LOAD(freeres)(Vg_FreeresToRun to_run)
{
#  if !defined(__UCLIBC__) \
      && !defined(VGPV_arm_linux_android) \
      && !defined(VGPV_x86_linux_android) \
      && !defined(VGPV_mips32_linux_android) \
      && !defined(VGPV_arm64_linux_android)

   /* g++ mangled __gnu_cxx::__freeres yields -> _ZN9__gnu_cxx9__freeresEv */
   extern void _ZN9__gnu_cxx9__freeresEv(void) __attribute__((weak));
   if (((to_run & VG_RUN__GNU_CXX__FREERES) != 0) &&
       (_ZN9__gnu_cxx9__freeresEv != NULL)) {
      _ZN9__gnu_cxx9__freeresEv();
   }

#  endif

#  if !defined(__UCLIBC__) && !defined(MUSL_LIBC) \
      && !defined(VGPV_arm_linux_android) \
      && !defined(VGPV_x86_linux_android) \
      && !defined(VGPV_mips32_linux_android) \
      && !defined(VGPV_arm64_linux_android)

   extern void __libc_freeres(void) __attribute__((weak));
   if (((to_run & VG_RUN__LIBC_FREERES) != 0) &&
       (__libc_freeres != NULL)) {
      __libc_freeres();
   }

#  endif

   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__FREERES_DONE, 0, 0, 0, 0, 0);
   /*NOTREACHED*/
   *(volatile int *)0 = 'x';
}

#endif // VGO_linux || VGO_solaris

#if defined(VGO_linux)

/* ---------------------------------------------------------------------
   Wrapper for indirect functions which need to be redirected.
   ------------------------------------------------------------------ */

void * VG_NOTIFY_ON_LOAD(ifunc_wrapper) (void);
void * VG_NOTIFY_ON_LOAD(ifunc_wrapper) (void)
{
    OrigFn fn;
    Addr result = 0;
    Addr fnentry;

    /* Call the original indirect function and get it's result */
    VALGRIND_GET_ORIG_FN(fn);
    CALL_FN_W_v(result, fn);

#if defined(VGP_ppc64be_linux)
   /* ppc64be uses function descriptors, so get the actual function entry
      address for the client request, but return the function descriptor
      from this function. 
      result points to the function descriptor, which starts with the
      function entry. */
    fnentry = *(Addr*)result;
#else
    fnentry = result;
#endif

    /* Ask the valgrind core running on the real CPU (as opposed to this
       code which runs on the emulated CPU) to update the redirection that
       led to this function. This client request eventually gives control to
       the function VG_(redir_add_ifunc_target) in m_redir.c  */
    VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__ADD_IFUNC_TARGET,
                                    fn.nraddr, fnentry, 0, 0, 0);
    return (void*)result;
}

#elif defined(VGO_darwin)

#include "config.h" /* VERSION */

/* ---------------------------------------------------------------------
   Darwin crash log hints
   ------------------------------------------------------------------ */

/* This string will be inserted into crash logs, so crashes while 
   running under Valgrind can be distinguished from other crashes. */
__private_extern__ const char *__crashreporter_info__ = "Instrumented by Valgrind " VERSION;

/* ---------------------------------------------------------------------
   Darwin environment cleanup
   ------------------------------------------------------------------ */

/* Scrubbing DYLD_INSERT_LIBRARIES from envp during exec is insufficient, 
   as there are other ways to launch a process with environment that 
   valgrind can't catch easily (i.e. launchd). 
   Instead, scrub DYLD_INSERT_LIBRARIES from the parent process once 
   dyld is done loading vg_preload.so.
*/
#include <string.h>
#include <crt_externs.h>

// GrP fixme copied from m_libcproc
static void env_unsetenv ( HChar **env, const HChar *varname )
{
   HChar **from;
   HChar **to = NULL;
   Int len = strlen(varname);

   for (from = to = env; from && *from; from++) {
      if (!(strncmp(varname, *from, len) == 0 && (*from)[len] == '=')) {
	 *to = *from;
	 to++;
      }
   }
   *(to++) = *(from++);
   /* fix the 4th "char* apple" pointer (aka. executable path pointer) */
   *(to++) = *(from++);
   *to = NULL;
}

static void vg_cleanup_env(void)  __attribute__((constructor));
static void vg_cleanup_env(void)
{
    HChar **envp = (HChar**)*_NSGetEnviron();
    env_unsetenv(envp, "VALGRIND_LAUNCHER");
    env_unsetenv(envp, "DYLD_SHARED_REGION");
    // GrP fixme should be more like mash_colon_env()
    env_unsetenv(envp, "DYLD_INSERT_LIBRARIES");
}   

/* ---------------------------------------------------------------------
   Darwin arc4random (rdar://6166275)
   ------------------------------------------------------------------ */

#include <fcntl.h>
#include <unistd.h>

int VG_REPLACE_FUNCTION_ZU(libSystemZdZaZddylib, arc4random)(void);
int VG_REPLACE_FUNCTION_ZU(libSystemZdZaZddylib, arc4random)(void)
{
    static int rnd = -1;
    int result;

    if (rnd < 0) rnd = open("/dev/random", O_RDONLY);

    read(rnd, &result, sizeof(result));
    return result;
}

void VG_REPLACE_FUNCTION_ZU(libSystemZdZaZddylib, arc4random_stir)(void);
void VG_REPLACE_FUNCTION_ZU(libSystemZdZaZddylib, arc4random_stir)(void)
{
    // do nothing
}

void VG_REPLACE_FUNCTION_ZU(libSystemZdZaZddylib, arc4random_addrandom)(unsigned char *dat, int datlen);
void VG_REPLACE_FUNCTION_ZU(libSystemZdZaZddylib, arc4random_addrandom)(unsigned char *dat, int datlen)
{
    // do nothing
    // GrP fixme ought to check [dat..dat+datlen) is defined
    // but don't care if it's initialized
}

#elif defined(VGO_freebsd)

#if (FREEBSD_VERS >= FREEBSD_14)

void * VG_NOTIFY_ON_LOAD(ifunc_wrapper) (void);
void * VG_NOTIFY_ON_LOAD(ifunc_wrapper) (void)
{
    OrigFn fn;
    Addr result = 0;
    Addr fnentry;

    /* Call the original indirect function and get it's result */
    VALGRIND_GET_ORIG_FN(fn);
    CALL_FN_W_v(result, fn);

    fnentry = result;

    VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__ADD_IFUNC_TARGET,
                                    fn.nraddr, fnentry, 0, 0, 0);
    return (void*)result;
}

#endif

#elif defined(VGO_solaris)

/* Declare the errno and environ symbols weakly in case the client is not
   linked against libc. In such a case it also cannot run replacement
   functions for set_error() and spawnveg() where these two variables are
   needed so this is ok. */
__attribute__((weak)) extern int errno;
__attribute__((weak)) extern char **environ;

#include <assert.h>
#include <errno.h>
#include <spawn.h>
#include <sys/syscall.h>
#include <sys/signal.h>
#include <unistd.h>

/* Replace function block_all_signals() from libc. When the client program is
   not running under valgrind, the function blocks all signals by setting
   sc_sigblock flag in the schedctl control block. When run under Valgrind
   this would bypass Valgrind's syscall and signal machinery.
   Valgrind's signal machinery needs to retain control over which signals are
   blocked and which not (see m_signals.c and m_scheduler/scheduler.c for more
   information - typically synchronous signals should not be blocked).
   Therefore this function replacement emulates lwp_sigmask syscall.
*/
void VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, block_all_signals)(/*ulwp_t*/ void *self);
void VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, block_all_signals)(/*ulwp_t*/ void *self)
{
   syscall(SYS_lwp_sigmask, SIG_SETMASK, ~0U, ~0U, ~0U, ~0U);
}

/* Replace functions get_error() and set_error() in libc. These functions are
   internal to the library and are used to work with an error value returned
   by posix_spawn() (when it is implemented using vfork()). A child calls
   set_error() to set an error code and the parent then calls get_error() to
   read it. Accessor functions are used so these trivial store+load operations
   are not changed by the compiler in any way.

   Since Valgrind translates vfork() to a normal fork(), calling set_error()
   by the child would have no effect on the error value in the parent so
   something must be done to fix this problem.

   A pipe is created between a child and its parent in the forksys pre-wrapper
   when a vfork() is encountered. The child's end of the pipe is closed when
   the child exits or execs (because close-on-exec is set on the file
   descriptor). Valgrind (the parent) waits on the child's end of the pipe to
   be closed which preserves the vfork() behaviour that the parent process is
   suspended while the child is using its resources.

   The pipe is then used to send an eventual error code set by the child in
   posix_spawn() to the parent. If there is any error Valgrind returns it as
   an error from the vfork() syscall. This means the syscall can return errors
   that it would normally never return but this is not a problem in practice
   because any error is directly propagated as a return code from
   posix_spawn().

   Address of vg_vfork_fildes is found by Valgrind when debug information for
   vgpreload_core.so is being processed. A value of this variable is set in
   the forksys pre-wrapper before a fork() call is made and set back to -1
   before returning from the wrapper by the parent.

   Newer Solaris versions introduce the spawn syscall and posix_spawn() is
   implemented using it. The redirect is not needed for these versions.
*/
int vg_vfork_fildes = -1;

int VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, get_error)(int *errp);
int VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, get_error)(int *errp)
{
   /* Always return 0 when the parent tries to call get_error(). Any error
      from the child is returned directly as an error from the vfork child.
      Value pointed by errp is initialized only by the child so not
      redirecting this function would mean that the parent gets an
      uninitialized/garbage value when it calls this function. */
   return 0;
}

int VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, set_error)(int *errp, int err);
int VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, set_error)(int *errp, int err)
{
   *errp = err;

   /* Libc should always call set_error() only after doing a vfork() syscall
      in posix_spawn(). The forksys pre-wrapper saves a descriptor of the
      child's end of the pipe in vg_vfork_fildes so it is an error if it is
      not a valid file descriptor at this point. */
   assert(vg_vfork_fildes >= 0);
   /* Current protocol between this function and the forksys pre-wrapper
      allows to send only errors in range [0, 255] (one byte values). */
   assert(err >= 0 && err <= 0xff);

   if (err != 0) {
      unsigned char w = (unsigned char)(err & 0xff);
      ssize_t res;
      do {
         res = write(vg_vfork_fildes, &w, 1);
         assert(res == 1 || (errno == EINTR || errno == ERESTART));
      } while (res != 1);
   }

   return err;
}

/* Replace spawnveg() in libast.so.1. This function is used by ksh to spawn
   new processes. The library has a build time option to select between
   several variants of this function based on behaviour of vfork() and
   posix_spawn() on the system for which the library is being compiled.
   Unfortunately, Solaris and illumos use the real vfork() variant which does
   not work correctly with the vfork() -> fork() translation done by Valgrind
   (see the forksys pre-wrapper for details). Therefore the function is
   replaced here with an implementation that uses posix_spawn(). This
   replacement can be removed when a configuration of libast in Solaris and
   illumos is changed to use the posix_spawn() implementation.
*/
pid_t VG_REPLACE_FUNCTION_ZU(libastZdsoZd1, spawnveg)(const char *command,
                                                      char **argv,
                                                      char **envv,
                                                      pid_t pgid);
pid_t VG_REPLACE_FUNCTION_ZU(libastZdsoZd1, spawnveg)(const char *command,
                                                      char **argv,
                                                      char **envp,
                                                      pid_t pgid)
{
   int err = 0;
   pid_t pid;
   posix_spawnattr_t attr;
   int attr_init_done = 0;

   err = posix_spawnattr_init(&attr);
   if (err != 0)
      goto out;
   attr_init_done = 1;

   err = posix_spawnattr_init(&attr);
   if (err != 0)
      goto out;

   if (pgid != 0) {
      if (pgid <= 1)
         pgid = 0;
      err = posix_spawnattr_setpgroup(&attr, pgid);
      if (err != 0)
         goto out;
      err = posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETPGROUP);
      if (err != 0)
         goto out;
   }

   err = posix_spawn(&pid, command, NULL, &attr, argv, envp ? envp : environ);

out:
   if (attr_init_done)
      posix_spawnattr_destroy(&attr);
   if (err != 0) {
      errno = err;
      return -1;
   }
   return pid;
}

#else
#  error Unknown OS
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
