
/*--------------------------------------------------------------------*/
/*--- A header file defining structures and constants which are    ---*/
/*--- important at the kernel boundary for this platform.          ---*/
/*---                                             vg_kerneliface.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org
      Julian_Seward@muraroa.demon.co.uk

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

#ifndef __VG_KERNELIFACE_H
#define __VG_KERNELIFACE_H

/* This file is ONLY to be included into vg_include.h.  Do not include
   it directly into valgrind source .c files.  This file defines types
   and constants for the kernel interface, and to make that clear
   everything is prefixed VKI. */

/*--- All the following stuff is correct for Linux kernels 2.2.X and
      2.4.X. 
---*/

/* Should really get this from an include file somewhere. */
#define VKI_BYTES_PER_PAGE_BITS 12
#define VKI_BYTES_PER_PAGE (1 << VKI_BYTES_PER_PAGE_BITS)

#define VKI_BYTES_PER_WORD 4
#define VKI_WORDS_PER_PAGE (VKI_BYTES_PER_PAGE / VKI_BYTES_PER_WORD)


/* For system call numbers __NR_... */
#include <asm/unistd.h>

/* An implementation of signal sets.  These are the same as the sigset
   implementations in the relevant Linux kernels.  Note carefully that
   this has nothing to do with glibc's signal sets.  We work entirely
   at the kernel boundary, so the libc stuff is invisible and
   irrelevant.  */

/* The following is copied from
   /usr/src/linux-2.4.9-13/include/asm-i386/signal.h */
#define VKI_KNSIG       64  /* true for linux 2.2.X and 2.4.X */
#define VKI_KNSIG_BPW   32  /* since we're using UInts */
#define VKI_KNSIG_WORDS (VKI_KNSIG / VKI_KNSIG_BPW)

typedef 
   struct { 
      UInt ws[VKI_KNSIG_WORDS]; 
   }
   vki_ksigset_t;

typedef
   struct {
      void*         ksa_handler;
      unsigned long ksa_flags;
      void (*ksa_restorer)(void);
      vki_ksigset_t ksa_mask;
   }
   vki_ksigaction;

typedef 
   struct {
      void* ss_sp;
      Int   ss_flags;
      UInt  ss_size;
   } 
   vki_kstack_t;


#define VKI_SIG_BLOCK          0    /* for blocking signals */
#define VKI_SIG_UNBLOCK        1    /* for unblocking signals */
#define VKI_SIG_SETMASK        2    /* for setting the signal mask */

#define VKI_SIG_DFL ((void*)0)     /* default signal handling */
#define VKI_SIG_IGN ((void*)1)     /* ignore signal */
#define VKI_SIG_ERR ((void*)-1)    /* error return from signal */

#define VKI_SA_ONSTACK      0x08000000
#define VKI_SA_RESTART      0x10000000
#if 0
#define VKI_SA_NOCLDSTOP    0x00000001
#define VKI_SA_NOCLDWAIT    0x00000002 /* not supported yet */
#define VKI_SA_SIGINFO      0x00000004
#define VKI_SA_NODEFER      0x40000000
#define VKI_SA_RESETHAND    0x80000000
#define VKI_SA_NOMASK       SA_NODEFER
#define VKI_SA_ONESHOT      SA_RESETHAND
#define VKI_SA_INTERRUPT    0x20000000 /* dummy -- ignored */
#define VKI_SA_RESTORER     0x04000000
#endif

#define VKI_SIGABRT          6
#define VKI_SIGSEGV         11
#define VKI_SIGBUS           7
#define VKI_SIGILL           4
#define VKI_SIGFPE           8
#define VKI_SIGKILL          9
#define VKI_SIGABRT          6
#define VKI_SIGSTOP         19
#define VKI_SIGTERM         15

/* The following are copied from /usr/include/bits/mman.h, which in
   turn claims to have got them from the kernel headers. */

#define VKI_PROT_READ      0x1             /* Page can be read.  */
#define VKI_PROT_WRITE     0x2             /* Page can be written.  */
#define VKI_PROT_EXEC      0x4             /* Page can be executed.  */
#define VKI_MAP_ANONYMOUS  0x20            /* Don't use a file.  */
#define VKI_MAP_PRIVATE    0x02            /* Changes are private.  */


/* Gawd ... hack ... */

typedef struct vki__user_cap_header_struct {
        UInt version;
        int pid;
} vki_cap_user_header_t;
 
typedef struct vki__user_cap_data_struct {
        UInt effective;
        UInt permitted;
        UInt inheritable;
} vki_cap_user_data_t;
  

/* "Byrial Jensen" <byrial@image.dk> says:
               [various] ioctls take a pointer to a "struct
               termios" but this is another and shorter "struct
               termios" than the one defined in <termios.h> and used
               by tcgetattr(3) and tcsetattr(3) and other library
               functions. GNU libc translate between its library
               termios and the kernel termios. 
*/

#define VKI_SIZEOF_STRUCT_TERMIOS 36


#endif /* ndef __VG_KERNELIFACE_H */

/*--------------------------------------------------------------------*/
/*--- end                                         vg_kerneliface.h ---*/
/*--------------------------------------------------------------------*/
