
/*--------------------------------------------------------------------*/
/*--- A header file defining structures and constants which are    ---*/
/*--- important at the kernel boundary for this platform.          ---*/
/*---                                             vg_kerneliface.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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

   The GNU General Public License is contained in the file COPYING.
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


/* sigaltstack controls */
#define VKI_SS_ONSTACK      1
#define VKI_SS_DISABLE      2

#define VKI_MINSIGSTKSZ     2048
#define VKI_SIGSTKSZ        8192



#define VKI_SIG_BLOCK          0    /* for blocking signals */
#define VKI_SIG_UNBLOCK        1    /* for unblocking signals */
#define VKI_SIG_SETMASK        2    /* for setting the signal mask */

#define VKI_SIG_DFL ((void*)0)     /* default signal handling */
#define VKI_SIG_IGN ((void*)1)     /* ignore signal */
#define VKI_SIG_ERR ((void*)-1)    /* error return from signal */

#define VKI_SA_ONSTACK      0x08000000
#define VKI_SA_RESTART      0x10000000
#define VKI_SA_NOCLDSTOP    0x00000001
#define VKI_SA_RESETHAND    0x80000000
#define VKI_SA_ONESHOT      VKI_SA_RESETHAND
#define VKI_SA_NODEFER      0x40000000
#define VKI_SA_NOMASK       VKI_SA_NODEFER
#if 0
#define VKI_SA_NOCLDWAIT    0x00000002 /* not supported yet */
#define VKI_SA_SIGINFO      0x00000004
#define VKI_SA_INTERRUPT    0x20000000 /* dummy -- ignored */
#define VKI_SA_RESTORER     0x04000000
#endif

#define VKI_SIGSEGV         11
#define VKI_SIGBUS           7
#define VKI_SIGILL           4
#define VKI_SIGFPE           8
#define VKI_SIGKILL          9
#define VKI_SIGSTOP         19
#define VKI_SIGTERM         15
#define VKI_SIGUSR1         10

/* The following are copied from include/asm-i386/mman.h .*/

#define VKI_PROT_READ      0x1             /* Page can be read.  */
#define VKI_PROT_WRITE     0x2             /* Page can be written.  */
#define VKI_PROT_EXEC      0x4             /* Page can be executed.  */
#define VKI_MAP_ANONYMOUS  0x20            /* Don't use a file.  */
#define VKI_MAP_PRIVATE    0x02            /* Changes are private.  */
#define VKI_MAP_FIXED      0x10            /* Interpret addr exactly */

/* Copied from linux-2.4.19/include/asm-i386/fcntl.h */

#define VKI_O_RDONLY             00
#define VKI_O_WRONLY             01
#define VKI_O_RDWR               02
#define VKI_O_CREAT            0100 /* not fcntl */
#define VKI_O_EXCL             0200 /* not fcntl */
#define VKI_O_TRUNC           01000 /* not fcntl */
#define VKI_O_APPEND          02000
#define VKI_O_NONBLOCK        04000
#define VKI_O_SYNC           010000
#define VKI_FASYNC           020000 /* fcntl, for BSD compatibility */
#define VKI_O_DIRECT         040000 /* direct disk access hint */
#define VKI_O_LARGEFILE     0100000
#define VKI_O_DIRECTORY     0200000 /* must be a directory */
#define VKI_O_NOFOLLOW      0400000 /* don't follow links */

/* Copied from linux-2.4.19/include/linux/stat.h */

#define VKI_S_IRWXU 00700
#define VKI_S_IRUSR 00400
#define VKI_S_IWUSR 00200
#define VKI_S_IXUSR 00100

#define VKI_S_IRWXG 00070
#define VKI_S_IRGRP 00040
#define VKI_S_IWGRP 00020
#define VKI_S_IXGRP 00010

#define VKI_S_IRWXO 00007
#define VKI_S_IROTH 00004
#define VKI_S_IWOTH 00002
#define VKI_S_IXOTH 00001


/* Copied from /usr/src/linux-2.4.9-13/include/asm/errno.h */

#define VKI_EPERM            1      /* Operation not permitted */
#define VKI_EINTR            4      /* Interrupted system call */
#define VKI_EINVAL          22      /* Invalid argument */
#define VKI_ENOMEM          12      /* Out of memory */
#define	VKI_EFAULT          14      /* Bad address */
#define VKI_ESRCH            3      /* No such process */
#define VKI_ENOSYS          38      /* Function not implemented */

#define VKI_EWOULDBLOCK     VKI_EAGAIN  /* Operation would block */
#define VKI_EAGAIN          11      /* Try again */


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

/* Adam Gundy <arg@cyberscience.com>, 20 Mar 2002, says: */
#define VKI_SIZEOF_STRUCT_TERMIO 17


/* File descriptor sets, for doing select().  Copied from
   /usr/src/linux-2.4.9-31/include/linux/posix_types.h 
*/
/*
 * This allows for 1024 file descriptors: if NR_OPEN is ever grown
 * beyond that you'll have to change this too. But 1024 fd's seem to be
 * enough even for such "real" unices like OSF/1, so hopefully this is
 * one limit that doesn't have to be changed [again].
 *
 * Note that POSIX wants the FD_CLEAR(fd,fdsetp) defines to be in
 * <sys/time.h> (and thus <linux/time.h>) - but this is a more logical
 * place for them. Solved by having dummy defines in <sys/time.h>.
 */

/*
 * Those macros may have been defined in <gnu/types.h>. But we always
 * use the ones here. 
 */
#undef VKI_NFDBITS
#define VKI_NFDBITS       (8 * sizeof(unsigned long))

#undef VKI_FD_SETSIZE
#define VKI_FD_SETSIZE    1024

#undef VKI_FDSET_LONGS
#define VKI_FDSET_LONGS   (VKI_FD_SETSIZE/VKI_NFDBITS)

#undef VKI_FDELT
#define VKI_FDELT(d)      ((d) / VKI_NFDBITS)

#undef VKI_FDMASK
#define VKI_FDMASK(d)     (1UL << ((d) % VKI_NFDBITS))

typedef struct {
        unsigned long vki_fds_bits [VKI_FDSET_LONGS];
} vki_fd_set;


/* Gawd ...
   Copied from /usr/src/linux-2.4.9-31/./include/asm-i386/posix_types.h
*/
#undef  VKI_FD_SET
#define VKI_FD_SET(fd,fdsetp) \
                __asm__ __volatile__("btsl %1,%0": \
                        "=m" (*(vki_fd_set *) (fdsetp)):"r" ((int) (fd)))

#undef  VKI_FD_CLR
#define VKI_FD_CLR(fd,fdsetp) \
                __asm__ __volatile__("btrl %1,%0": \
                        "=m" (*(vki_fd_set *) (fdsetp)):"r" ((int) (fd)))

#undef  VKI_FD_ISSET
#define VKI_FD_ISSET(fd,fdsetp) (__extension__ ({ \
                unsigned char __result; \
                __asm__ __volatile__("btl %1,%2 ; setb %0" \
                        :"=q" (__result) :"r" ((int) (fd)), \
                        "m" (*(vki_fd_set *) (fdsetp))); \
                __result; }))

#undef  VKI_FD_ZERO
#define VKI_FD_ZERO(fdsetp) \
do { \
        int __d0, __d1; \
        __asm__ __volatile__("cld ; rep ; stosl" \
                        :"=m" (*(vki_fd_set *) (fdsetp)), \
                          "=&c" (__d0), "=&D" (__d1) \
                        :"a" (0), "1" (VKI_FDSET_LONGS), \
                        "2" ((vki_fd_set *) (fdsetp)) : "memory"); \
} while (0)



/* 
./include/asm-i386/posix_types.h:typedef long           __kernel_suseconds_t;
./include/linux/types.h:typedef __kernel_suseconds_t    suseconds_t;

./include/asm-i386/posix_types.h:typedef long           __kernel_time_t;
./include/linux/types.h:typedef __kernel_time_t         time_t;
*/

struct vki_timeval {
        /* time_t */ long         tv_sec;         /* seconds */
        /* suseconds_t */ long    tv_usec;        /* microseconds */
};



/* For fcntl on fds ..
   from ./include/asm-i386/fcntl.h */
#define VKI_F_GETFL         3       /* get file->f_flags */
#define VKI_F_SETFL         4       /* set file->f_flags */

#define VKI_O_NONBLOCK        04000

/* For nanosleep ... 
   from ./include/linux/time.h */
struct vki_timespec {
        /* time_t */ long tv_sec;         /* seconds */
        long    tv_nsec;        /* nanoseconds */
};


/* STAT stuff 
   from /usr/src/linux-2.4.9-31/include/asm-i386/stat.h */
struct vki_stat {
        unsigned short st_dev;
        unsigned short __pad1;
        unsigned long st_ino;
        unsigned short st_mode;
        unsigned short st_nlink;
        unsigned short st_uid;
        unsigned short st_gid;
        unsigned short st_rdev;
        unsigned short __pad2;
        unsigned long  st_size;
        unsigned long  st_blksize;
        unsigned long  st_blocks;
        unsigned long  st_atime;
        unsigned long  __unused1;
        unsigned long  st_mtime;
        unsigned long  __unused2;
        unsigned long  st_ctime;
        unsigned long  __unused3;
        unsigned long  __unused4;
        unsigned long  __unused5;
};


/* To do with the ELF frame constructed by the kernel on a process'
   stack just before it transfers control to the program's interpreter
   (to use the ELF parlance).
   Constants from /usr/src/linux-2.4.9-31/include/linux/elf.h
   Logic from     /usr/src/linux-2.4.9-31/fs/binfmt_elf.c
                  and its counterpart in the 2.2.14 kernel sources 
                  in Red Hat 6.2.  */
#define VKI_AT_CLKTCK 17    /* frequency at which times() increments */
#define VKI_AT_HWCAP  16    /* arch dependent hints at CPU capabilities */
#define VKI_AT_BASE   7     /* base address of interpreter */
#define VKI_AT_PAGESZ 6     /* system page size */
#define VKI_AT_PHNUM  5     /* number of program headers */
#define VKI_AT_PHENT  4     /* size of program header entry */
#define VKI_AT_PHDR   3     /* program headers for program */
#define VKI_AT_USER_AUX_SEGMENT 23  /* tell glibc what address segment
                                       0x3B points to.  (Needed for
                                       Red Hat Limbo, 7.3.92) */

/* Including <linux/module.h> leads to loads of hassle because then we
   need <asm/atomic.h> sometimes (RedHat 7.3) and that is a
   kernel-only header which deliberately #errors on gcc-3.1.  Mucho
   hassle considering that we only want to know sizeof(struct module).
   Hence ...
 
   #include <stdio.h>
   #include <asm/atomic.h>
   #include <linux/module.h>

   int main ( void )
   {
      printf ("sizeof(struct module) = %d\n", sizeof(struct module) );
      return 0;
    }
*/

#define VKI_SIZEOF_STRUCT_MODULE 96


/* This is the structure passed to the modify_ldt syscall.  Just so as
   to confuse and annoy everyone, this is _not_ the same as an
   VgLdtEntry and has to be translated into such.  The logic for doing
   so, in vg_ldt.c, is copied from the kernel sources. */
/*
 * ldt.h
 *
 * Definitions of structures used with the modify_ldt system call.
 */
typedef struct vki_modify_ldt_ldt_s {
        unsigned int  entry_number;
        unsigned long base_addr;
        unsigned int  limit;
        unsigned int  seg_32bit:1;
        unsigned int  contents:2;
        unsigned int  read_exec_only:1;
        unsigned int  limit_in_pages:1;
        unsigned int  seg_not_present:1;
        unsigned int  useable:1;
} vki_modify_ldt_t;

#define VKI_MODIFY_LDT_CONTENTS_DATA        0
#define VKI_MODIFY_LDT_CONTENTS_STACK       1
#define VKI_MODIFY_LDT_CONTENTS_CODE        2



#endif /* ndef __VG_KERNELIFACE_H */

/*--------------------------------------------------------------------*/
/*--- end                                         vg_kerneliface.h ---*/
/*--------------------------------------------------------------------*/
