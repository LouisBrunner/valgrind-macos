
/*--------------------------------------------------------------------*/
/*--- A header file defining structures and constants which are    ---*/
/*--- important at the kernel boundary for this platform.          ---*/
/*---                                             vg_kerneliface.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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

#define SI_MAX_SIZE	128
#define SI_PAD_SIZE	((SI_MAX_SIZE/sizeof(int)) - 3)

union vki_sigval {
   Int sival_int;
   void *sival_ptr;
};

typedef
   struct {
      Int si_signo;
      Int si_errno;
      Int si_code;

      union {
	 Int _pad[SI_PAD_SIZE];

	 /* kill() */
	 struct {
	    Int _pid;		/* sender's pid */
	    Short _uid;		/* sender's uid */
	 } _kill;

	 /* POSIX.1b timers */
	 struct {
	    UInt _timer1;
	    UInt _timer2;
	 } _timer;

	 /* POSIX.1b signals */
	 struct {
	    Int _pid;		/* sender's pid */
	    UShort _uid;		/* sender's uid */
	    union vki_sigval _sigval;
	 } _rt;

	 /* SIGCHLD */
	 struct {
	    Int _pid;		/* which child */
	    UShort _uid;		/* sender's uid */
	    Int _status;		/* exit code */
	    Int _utime;
	    Int _stime;
	 } _sigchld;

	 /* SIGILL, SIGFPE, SIGSEGV, SIGBUS */
	 struct {
	    void *_addr; /* faulting insn/memory ref. */
	 } _sigfault;

	 /* SIGPOLL */
	 struct {
	    Int _band;	/* POLL_IN, POLL_OUT, POLL_MSG */
	    Int _fd;
	 } _sigpoll;
      } _sifields;
   } vki_ksiginfo_t;

/* linux-2.6/include/asm-generic/siginfo.h */

#define VKI_SI_USER	0
#define VKI_SI_QUEUE	-1
#define VKI_SI_TKILL	-6

struct vki_fpreg {
	UShort significand[4];
	UShort exponent;
};

struct vki_fpxreg {
	UShort significand[4];
	UShort exponent;
	UShort padding[3];
};

struct vki_xmmreg {
	UInt element[4];
};

struct vki_fpstate {
	/* Regular FPU environment */
	unsigned long 	cw;
	unsigned long	sw;
	unsigned long	tag;
	unsigned long	ipoff;
	unsigned long	cssel;
	unsigned long	dataoff;
	unsigned long	datasel;
	struct vki_fpreg	_st[8];
	unsigned short	status;
	unsigned short	magic;		/* 0xffff = regular FPU data only */

	/* FXSR FPU environment */
	unsigned long	_fxsr_env[6];	/* FXSR FPU env is ignored */
	unsigned long	mxcsr;
	unsigned long	reserved;
	struct vki_fpxreg	_fxsr_st[8];	/* FXSR FPU reg data is ignored */
	struct vki_xmmreg	_xmm[8];
	unsigned long	padding[56];
};

#define X86_FXSR_MAGIC		0x0000

struct vki_sigcontext {
	UShort	gs, __gsh;
	UShort	fs, __fsh;
	UShort	es, __esh;
	UShort	ds, __dsh;
	UInt	edi;
	UInt	esi;
	UInt	ebp;
	UInt	esp;
	UInt	ebx;
	UInt	edx;
	UInt	ecx;
	UInt	eax;
	UInt	trapno;
	UInt	err;
	UInt	eip;
	UShort	cs, __csh;
	UInt	eflags;
	UInt	esp_at_signal;
	UShort	ss, __ssh;
	struct	vki_fpstate * fpstate;
	UInt	oldmask;
	UInt	cr2;
};

struct vki_ucontext {
	UInt		  uc_flags;
	struct vki_ucontext  *uc_link;
	vki_kstack_t	  uc_stack;
	struct vki_sigcontext uc_mcontext;
	vki_ksigset_t	  uc_sigmask;	/* mask last for extensibility */
};


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
#define VKI_SA_SIGINFO      0x00000004
#define VKI_SA_RESETHAND    0x80000000
#define VKI_SA_ONESHOT      VKI_SA_RESETHAND
#define VKI_SA_NODEFER      0x40000000
#define VKI_SA_NOMASK       VKI_SA_NODEFER
#define VKI_SA_NOCLDWAIT    0x00000002
#if 0
#define VKI_SA_INTERRUPT    0x20000000 /* dummy -- ignored */
#define VKI_SA_RESTORER     0x04000000
#endif

/* extra wait flags */
#define	VKI_WNOHANG	1		/* Don't block waiting.  */
#define	VKI_WUNTRACED	2		/* Report status of stopped children.  */
#define VKI__WALL	0x40000000	/* Wait for any child.  */
#define VKI__WCLONE	0x80000000	/* Wait for cloned process.  */

#define	VKI_SIGHUP	1	/* Hangup (POSIX).  */
#define	VKI_SIGINT	2	/* Interrupt (ANSI).  */
#define	VKI_SIGQUIT	3	/* Quit (POSIX).  */
#define	VKI_SIGILL	4	/* Illegal instruction (ANSI).  */
#define	VKI_SIGTRAP	5	/* Trace trap (POSIX).  */
#define	VKI_SIGABRT	6	/* Abort (ANSI).  */
#define	VKI_SIGIOT	6	/* IOT trap (4.2 BSD).  */
#define	VKI_SIGBUS	7	/* BUS error (4.2 BSD).  */
#define	VKI_SIGFPE	8	/* Floating-point exception (ANSI).  */
#define	VKI_SIGKILL	9	/* Kill, unblockable (POSIX).  */
#define	VKI_SIGUSR1	10	/* User-defined signal 1 (POSIX).  */
#define	VKI_SIGSEGV	11	/* Segmentation violation (ANSI).  */
#define	VKI_SIGUSR2	12	/* User-defined signal 2 (POSIX).  */
#define	VKI_SIGPIPE	13	/* Broken pipe (POSIX).  */
#define	VKI_SIGALRM	14	/* Alarm clock (POSIX).  */
#define	VKI_SIGTERM	15	/* Termination (ANSI).  */
#define	VKI_SIGSTKFLT	16	/* Stack fault.  */
#define	VKI_SIGCLD	SIGCHLD	/* Same as SIGCHLD (System V).  */
#define	VKI_SIGCHLD	17	/* Child status has changed (POSIX).  */
#define	VKI_SIGCONT	18	/* Continue (POSIX).  */
#define	VKI_SIGSTOP	19	/* Stop, unblockable (POSIX).  */
#define	VKI_SIGTSTP	20	/* Keyboard stop (POSIX).  */
#define	VKI_SIGTTIN	21	/* Background read from tty (POSIX).  */
#define	VKI_SIGTTOU	22	/* Background write to tty (POSIX).  */
#define	VKI_SIGURG	23	/* Urgent condition on socket (4.2 BSD).  */
#define	VKI_SIGXCPU	24	/* CPU limit exceeded (4.2 BSD).  */
#define	VKI_SIGXFSZ	25	/* File size limit exceeded (4.2 BSD).  */
#define	VKI_SIGVTALRM	26	/* Virtual alarm clock (4.2 BSD).  */
#define	VKI_SIGPROF	27	/* Profiling alarm clock (4.2 BSD).  */
#define	VKI_SIGWINCH	28	/* Window size change (4.3 BSD, Sun).  */
#define	VKI_SIGPOLL	SIGIO	/* Pollable event occurred (System V).  */
#define	VKI_SIGIO	29	/* I/O now possible (4.2 BSD).  */
#define	VKI_SIGPWR	30	/* Power failure restart (System V).  */
#define	VKI_SIGSYS	31	/* Bad system call.  */
#define	VKI_SIGUNUSED	31

#define VKI_SIGRTMIN	    32
#define VKI_SIGRTMAX	    63

#define VKI_SIGVGINT	    (VKI_SIGRTMIN+0) /* signal for internal use - interrupt */
#define VKI_SIGVGKILL	    (VKI_SIGRTMIN+1) /* signal for internal use - kill */
#define VKI_SIGRTUSERMIN    (VKI_SIGRTMIN+2) /* first user-usable RT signal */

/* The following are copied from include/asm-i386/mman.h .*/

#define VKI_PROT_NONE      0x0		   /* No page permissions */
#define VKI_PROT_READ      0x1             /* Page can be read.  */
#define VKI_PROT_WRITE     0x2             /* Page can be written.  */
#define VKI_PROT_EXEC      0x4             /* Page can be executed.  */
#define VKI_MAP_ANONYMOUS  0x20            /* Don't use a file.  */
#define VKI_MAP_SHARED	   0x01		   /* Share changes.  */
#define VKI_MAP_PRIVATE    0x02            /* Changes are private.  */
#define VKI_MAP_FIXED      0x10            /* Interpret addr exactly */
#define VKI_MAP_NOSYMS     0x40000000	   /* internal pseudo-flag to disable symbol loading */
#define VKI_MAP_CLIENT     0x80000000	   /* internal pseudo-flag to distinguish client mappings */

/* linux/mman.h */
#define VKI_MREMAP_MAYMOVE	1
#define VKI_MREMAP_FIXED	2

/* Copied from linux-2.4.19/include/asm-i386/fcntl.h */

#define VKI_O_ACCMODE	       0003
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

#define VKI_SEEK_SET              0
#define VKI_SEEK_CUR              1
#define VKI_SEEK_END              2

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
#define VKI_ENOENT	     2	    /* No such file or directory */
#define VKI_ESRCH            3      /* No such process */
#define VKI_EINTR            4      /* Interrupted system call */
#define VKI_EBADF            9      /* Bad file number */
#define VKI_ENOMEM          12      /* Out of memory */
#define VKI_EWOULDBLOCK     VKI_EAGAIN  /* Operation would block */
#define VKI_EAGAIN          11      /* Try again */
#define	VKI_EACCES	    13      /* Permission denied */
#define	VKI_EFAULT          14      /* Bad address */
#define VKI_EEXIST	    17	    /* File exists */
#define VKI_EINVAL          22      /* Invalid argument */
#define VKI_EMFILE	    24	    /* Too many open files */
#define VKI_ENOSYS          38      /* Function not implemented */

#define VKI_ERESTARTSYS	    512	    /* Restart the syscall */

/* Copied from linux/isdn.h */

#define VKI_IIOCGETCPS  _IO( 'I',21 )
#define VKI_IIOCNETGPN  _IO( 'I',34 )

#define ISDN_MSNLEN          32

typedef struct {
    char name[ 10 ];
    char phone[ ISDN_MSNLEN ];
    int  outgoing;
} isdn_net_ioctl_phone;


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



struct vki_pollfd {
   Int		fd;
   Short	events;
   Short	revents;
};

/* asm/poll.h */
#define VKI_POLLIN          0x0001
#define VKI_POLLPRI         0x0002
#define VKI_POLLOUT         0x0004
#define VKI_POLLERR         0x0008
#define VKI_POLLHUP         0x0010
#define VKI_POLLNVAL        0x0020



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
#define VKI_F_DUPFD	    0	    /* dup */
#define VKI_F_GETFD	    1	    /* get close_on_exec */
#define VKI_F_SETFD	    2	    /* set/clear close_on_exec */
#define VKI_F_GETFL         3       /* get file->f_flags */
#define VKI_F_SETFL         4       /* set file->f_flags */

/* for F_[GET|SET]FL */
#define VKI_FD_CLOEXEC	    1	    /* actually anything with low bit set goes */

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
#define VKI_AT_NULL   0
#define VKI_AT_SYSINFO 32   /* address of system info page */
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
        unsigned int  reserved:25;
} vki_modify_ldt_t;

#define VKI_MODIFY_LDT_CONTENTS_DATA        0
#define VKI_MODIFY_LDT_CONTENTS_STACK       1
#define VKI_MODIFY_LDT_CONTENTS_CODE        2

#define VKI_GDT_TLS_ENTRIES 3
#define VKI_GDT_TLS_MIN     6
#define VKI_GDT_TLS_MAX     (VKI_GDT_TLS_MIN + VKI_GDT_TLS_ENTRIES)

/* Flags for clone() */
/* linux/sched.h */
#define VKI_CSIGNAL		0x000000ff	/* signal mask to be sent at exit */
#define VKI_CLONE_VM		0x00000100	/* set if VM shared between processes */
#define VKI_CLONE_FS		0x00000200	/* set if fs info shared between processes */
#define VKI_CLONE_FILES		0x00000400	/* set if open files shared between processes */
#define VKI_CLONE_SIGHAND	0x00000800	/* set if signal handlers and blocked signals shared */
#define VKI_CLONE_IDLETASK	0x00001000	/* set if new pid should be 0 (kernel only)*/
#define VKI_CLONE_PTRACE	0x00002000	/* set if we want to let tracing continue on the child too */
#define VKI_CLONE_VFORK		0x00004000	/* set if the parent wants the child to wake it up on mm_release */
#define VKI_CLONE_PARENT	0x00008000	/* set if we want to have the same parent as the cloner */
#define VKI_CLONE_THREAD	0x00010000	/* Same thread group? */
#define VKI_CLONE_NEWNS		0x00020000	/* New namespace group? */
#define VKI_CLONE_SYSVSEM	0x00040000	/* share system V SEM_UNDO semantics */
#define VKI_CLONE_SETTLS	0x00080000	/* create a new TLS for the child */
#define VKI_CLONE_PARENT_SETTID	0x00100000	/* set the TID in the parent */
#define VKI_CLONE_CHILD_CLEARTID	0x00200000	/* clear the TID in the child */
#define VKI_CLONE_DETACHED	0x00400000	/* parent wants no child-exit signal */
#define VKI_CLONE_UNTRACED	0x00800000	/* set if the tracing process can't force VKI_CLONE_PTRACE on this clone */
#define VKI_CLONE_CHILD_SETTID	0x01000000	/* set the TID in the child */

/* This is the structure passed to the getdents syscall. */
/*
 * linux/dirent.h
 */
typedef struct vki_dirent {
	long           d_ino;
	long           d_off;
	unsigned short d_reclen;
	char           d_name[256];
} vki_dirent;



/* This is the structure passed to the getrlimit syscall. */
/*
 * bits/resource.h
 */
typedef struct vki_rlimit {
	unsigned long rlim_cur;
	unsigned long rlim_max;
} vki_rlimit;

#define VKI_RLIMIT_NOFILE 7

/* Socket stuff. */
/*
 * sys/socket.h
 */
typedef unsigned short vki_sa_family_t;
struct vki_sockaddr {
  vki_sa_family_t sa_family;     /* Address family. */
  char            sa_data[14];   /* Address data. */
};

/* statfs structs */
/*
 * bits/statfs.h
 */

struct statfs {
        unsigned int f_type;
        unsigned int f_bsize;
        unsigned int f_blocks;
        unsigned int f_bfree;
        unsigned int f_bavail;
        unsigned int f_files;
        unsigned int f_ffree;
        int f_fsid[ 2 ];
        unsigned int f_namelen;
        unsigned int f_frsize;
        unsigned int f_spare[5];
};

struct statfs64 {
        unsigned int f_type;
        unsigned int f_bsize;
        unsigned long long f_blocks;
        unsigned long long f_bfree;
        unsigned long long f_bavail;
        unsigned long long f_files;
        unsigned long long f_ffree;
        int f_fsid[ 2 ];
        unsigned int f_namelen;
        unsigned int f_frsize;
        unsigned int f_spare[5];
};

/* 
 * linux/futex.h
 */

#define VKI_FUTEX_WAIT    0
#define VKI_FUTEX_WAKE    1
#define VKI_FUTEX_FD      2
#define VKI_FUTEX_REQUEUE 3


#endif /*  __VG_KERNELIFACE_H */

/*--------------------------------------------------------------------*/
/*--- end                                         vg_kerneliface.h ---*/
/*--------------------------------------------------------------------*/
