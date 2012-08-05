
/*--------------------------------------------------------------------*/
/*--- Client-space code for the core.               vg_preloaded.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward 
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

#if defined(VGO_linux)

/* ---------------------------------------------------------------------
   Hook for running __libc_freeres once the program exits.
   ------------------------------------------------------------------ */

void VG_NOTIFY_ON_LOAD(freeres)( void );
void VG_NOTIFY_ON_LOAD(freeres)( void )
{
#  if !defined(__UCLIBC__) \
   && !defined(VGPV_arm_linux_android) && !defined(VGPV_x86_linux_android)
   extern void __libc_freeres(void);
   __libc_freeres();
#  endif
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__LIBC_FREERES_DONE, 
                                   0, 0, 0, 0, 0);
   /*NOTREACHED*/
   *(volatile int *)0 = 'x';
}

/* ---------------------------------------------------------------------
   Wrapper for indirect functions which need to be redirected.
   ------------------------------------------------------------------ */

void * VG_NOTIFY_ON_LOAD(ifunc_wrapper) (void);
void * VG_NOTIFY_ON_LOAD(ifunc_wrapper) (void)
{
    OrigFn fn;
    Addr result = 0;

    /* Call the original indirect function and get it's result */
    VALGRIND_GET_ORIG_FN(fn);
    CALL_FN_W_v(result, fn);

    /* Ask the valgrind core running on the real CPU (as opposed to this
       code which runs on the emulated CPU) to update the redirection that
       led to this function. This client request eventually gives control to
       the function VG_(redir_add_ifunc_target) in m_redir.c  */
    VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__ADD_IFUNC_TARGET,
                                    fn.nraddr, result, 0, 0, 0);
    return (void*)result;
}

#elif defined(VGO_darwin)

#include "config.h" /* VERSION */

/* ---------------------------------------------------------------------
   Darwin crash log hints
   ------------------------------------------------------------------ */

/* This string will be inserted into crash logs, so crashes while 
   running under Valgrind can be distinguished from other crashes. */
__private_extern__ char *__crashreporter_info__ = "Instrumented by Valgrind " VERSION;

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
static void env_unsetenv ( Char **env, const Char *varname )
{
   Char **from;
   Char **to = NULL;
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
    Char **envp = (Char**)*_NSGetEnviron();
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

#else

#  error Unknown OS
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
