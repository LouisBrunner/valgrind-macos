
/*--------------------------------------------------------------------*/
/*--- Intercepts for various libc functions we want to capture     ---*/
/*--- (mostly for threading purposes).              vg_intercept.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
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
   ALL THE CODE IN THIS FILE RUNS ON THE SIMULATED CPU.  It is
   intended for various reasons as drop-in replacements for libc
   functions.  These functions are not called directly - they're the
   targets of code redirection.  They're named the same as the library
   functions they replace so that messages printing their names are
   sensible, but the we don't really require the dynamic linker to find
   them.
   ------------------------------------------------------------------ */

#include "valgrind.h"
#include "vg_include.h"
#include <unistd.h>
#include <signal.h>

static void init(void) __attribute__((constructor));
static int init_done;

int raise(int sig)
{
   if (!init_done)
      init();

   return kill(getpid(), sig);
}
int __libc_raise(int) __attribute__((alias("raise"), visibility("protected")));
int __GI_raise(int) __attribute__((alias("raise"), visibility("protected")));

/* Don't alias, so there's no chance that "gsignal" will appear in a
   message instead of "raise" */
int gsignal(int sig)
{
   raise(sig);
}

/* ---------------------------------------------------------------------
   Hook for running __libc_freeres once the program exits.
   ------------------------------------------------------------------ */

static void VGINJ_(__libc_freeres_wrapper)( void )
{
   int res;
#ifndef __UCLIBC__
   extern void __libc_freeres(void);
   __libc_freeres();
#endif
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__LIBC_FREERES_DONE, 0, 0, 0, 0);
   /*NOTREACHED*/
   *(int *)0 = 'x';
}

static const struct {
   const char *fromlib, *fromsym;
   const void *toaddr;
} redirects[] = {
#define _S(x)	#x
#define S(x)	_S(x)
#define E(l, pfx, s)	{ "soname:" l, pfx #s, (void *)s }
#define R(l, s)						\
   E(l, "", s),						\
   E(l, "__", s),					\
   E(l, "__libc_", s),					\
   E(l, "__GI_", s)

   R("libc.so.6", raise),
   R("libc.so.6", gsignal),
#undef R
};

static void init(void)
{
   int i;
   int res;

   if (init_done)
      return;
   init_done = 1;

   VALGRIND_MAGIC_SEQUENCE(res, -1, VG_USERREQ__REGISTER_LIBC_FREERES,
			   (Addr)VGINJ_(__libc_freeres_wrapper), 0, 0, 0);

   for(i = 0; i < sizeof(redirects)/sizeof(*redirects); i++) {
      VALGRIND_MAGIC_SEQUENCE(res, -1, VG_USERREQ__REGISTER_REDIRECT_ADDR,
			      redirects[i].fromlib, redirects[i].fromsym,
			      redirects[i].toaddr, 0);
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                           vg_intercept.c ---*/
/*--------------------------------------------------------------------*/

