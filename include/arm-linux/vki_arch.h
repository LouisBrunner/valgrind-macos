
/*--------------------------------------------------------------------*/
/*--- ARM/Linux-specific kernel interface.    arm-linux/vki_arch.h ---*/
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

#ifndef __ARM_LINUX_VKI_ARCH_H
#define __ARM_LINUX_VKI_ARCH_H

// ARM can be big or little endian;  we're only supporting little endian.  
#define VKI_LITTLE_ENDIAN  1

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/types.h
//----------------------------------------------------------------------

typedef unsigned char __vki_u8;

typedef __signed__ short __vki_s16;
typedef unsigned short __vki_u16;

typedef unsigned int __vki_u32;

typedef __signed__ long long __vki_s64;
typedef unsigned long long __vki_u64;

typedef unsigned short vki_u16;

typedef unsigned int vki_u32;

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/page.h
//----------------------------------------------------------------------

#define VKI_PAGE_SHIFT		12
#define VKI_PAGE_SIZE		(1UL << VKI_PAGE_SHIFT)

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/signal.h
//----------------------------------------------------------------------

#define _VKI_NSIG	64
#define _VKI_NSIG_BPW	32
#define _VKI_NSIG_WORDS	(_VKI_NSIG / _VKI_NSIG_BPW)

typedef unsigned long vki_old_sigset_t;		/* at least 32 bits */

typedef struct {
	unsigned long sig[_VKI_NSIG_WORDS];
} vki_sigset_t;

#define VKI_SIGHUP		 1
#define VKI_SIGINT		 2
#define VKI_SIGQUIT		 3
#define VKI_SIGILL		 4
#define VKI_SIGTRAP		 5
#define VKI_SIGABRT		 6
//#define VKI_SIGIOT		 6
#define VKI_SIGBUS		 7
#define VKI_SIGFPE		 8
#define VKI_SIGKILL		 9
#define VKI_SIGUSR1		10
#define VKI_SIGSEGV		11
#define VKI_SIGUSR2		12
#define VKI_SIGPIPE		13
#define VKI_SIGALRM		14
#define VKI_SIGTERM		15
#define VKI_SIGSTKFLT	16
#define VKI_SIGCHLD		17
#define VKI_SIGCONT		18
#define VKI_SIGSTOP		19
#define VKI_SIGTSTP		20
#define VKI_SIGTTIN		21
#define VKI_SIGTTOU		22
#define VKI_SIGURG		23
#define VKI_SIGXCPU		24
#define VKI_SIGXFSZ		25
#define VKI_SIGVTALRM	26
#define VKI_SIGPROF		27
#define VKI_SIGWINCH	28
#define VKI_SIGIO		29
#define VKI_SIGPWR		30
#define VKI_SIGSYS		31
#define	VKI_SIGUNUSED	31

/* These should not be considered constants from userland.  */
#define VKI_SIGRTMIN	32
#define VKI_SIGRTMAX	_VKI_NSIG

#define VKI_SIGSWI		32

#define VKI_SA_NOCLDSTOP	0x00000001
#define VKI_SA_NOCLDWAIT	0x00000002
#define VKI_SA_SIGINFO	0x00000004
//#define VKI_SA_THIRTYTWO	0x02000000
#define VKI_SA_RESTORER	0x04000000
#define VKI_SA_ONSTACK	0x08000000
#define VKI_SA_RESTART	0x10000000
#define VKI_SA_NODEFER	0x40000000
#define VKI_SA_RESETHAND	0x80000000

#define VKI_SA_NOMASK	VKI_SA_NODEFER
#define VKI_SA_ONESHOT	VKI_SA_RESETHAND
//#define VKI_SA_INTERRUPT	0x20000000 /* dummy -- ignored */


#define VKI_SS_ONSTACK	1
#define VKI_SS_DISABLE	2

#define VKI_MINSIGSTKSZ	2048

#define VKI_SIG_BLOCK          0	/* for blocking signals */
#define VKI_SIG_UNBLOCK        1	/* for unblocking signals */
#define VKI_SIG_SETMASK        2	/* for setting the signal mask */

/* Type of a signal handler.  */
typedef void __vki_signalfn_t(int);
typedef __vki_signalfn_t __user *__vki_sighandler_t;

typedef void __vki_restorefn_t(void);
typedef __vki_restorefn_t __user *__vki_sigrestore_t;

#define VKI_SIG_DFL	((__vki_sighandler_t)0)	/* default signal handling */
#define VKI_SIG_IGN	((__vki_sighandler_t)1)	/* ignore signal */

struct vki_old_sigaction {
        // [[Nb: a 'k' prefix is added to "sa_handler" because
        // bits/sigaction.h (which gets dragged in somehow via signal.h)
        // #defines it as something else.  Since that is done for glibc's
        // purposes, which we don't care about here, we use our own name.]]
	__vki_sighandler_t ksa_handler;
	vki_old_sigset_t sa_mask;
	unsigned long sa_flags;
	__vki_sigrestore_t sa_restorer;
};

struct vki_sigaction {
        // [[See comment about extra 'k' above]]
	__vki_sighandler_t ksa_handler;
	unsigned long sa_flags;
	__vki_sigrestore_t sa_restorer;
	vki_sigset_t sa_mask;		/* mask last for extensibility */
};

typedef struct vki_sigaltstack {
	void __user *ss_sp;
	int ss_flags;
	vki_size_t ss_size;
} vki_stack_t;

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/sigcontext.h
//----------------------------------------------------------------------

struct vki_sigcontext {
	unsigned long trap_no;
	unsigned long error_code;
	unsigned long oldmask;
	unsigned long arm_r0;
	unsigned long arm_r1;
	unsigned long arm_r2;
	unsigned long arm_r3;
	unsigned long arm_r4;
	unsigned long arm_r5;
	unsigned long arm_r6;
	unsigned long arm_r7;
	unsigned long arm_r8;
	unsigned long arm_r9;
	unsigned long arm_r10;
	unsigned long arm_fp;
	unsigned long arm_ip;
	unsigned long arm_sp;
	unsigned long arm_lr;
	unsigned long arm_pc;
	unsigned long arm_cpsr;
	unsigned long fault_address;
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/mman.h
//----------------------------------------------------------------------

#define VKI_PROT_READ	0x1		/* page can be read */
#define VKI_PROT_WRITE	0x2		/* page can be written */
#define VKI_PROT_EXEC	0x4		/* page can be executed */
//#define VKI_PROT_SEM	0x8		/* page may be used for atomic ops */
//#define VKI_PROT_NONE	0x0		/* page can not be accessed */

#define VKI_MAP_SHARED	0x01		/* Share changes */
#define VKI_MAP_PRIVATE	0x02		/* Changes are private */
#define VKI_MAP_TYPE	0x0f		/* Mask for type of mapping */
#define VKI_MAP_FIXED	0x10		/* Interpret addr exactly */
#define VKI_MAP_ANONYMOUS	0x20	/* don't use a file */

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/fcntl.h
//----------------------------------------------------------------------

#define VKI_O_RDONLY	     00
#define VKI_O_WRONLY	     01
#define VKI_O_CREAT	   0100	/* not fcntl */
#define VKI_O_EXCL	   0200	/* not fcntl */
#define VKI_O_TRUNC	  01000	/* not fcntl */
#define VKI_O_NONBLOCK	  04000

#define VKI_F_DUPFD	0	/* dup */
#define VKI_F_GETFD	1	/* get close_on_exec */
#define VKI_F_SETFD	2	/* set/clear close_on_exec */
#define VKI_F_GETFL	3	/* get file->f_flags */
#define VKI_F_SETFL	4	/* set file->f_flags */
#define VKI_F_GETLK	5
#define VKI_F_SETLK	6
#define VKI_F_SETLKW	7

#define VKI_F_SETOWN	8	/*  for sockets. */
#define VKI_F_GETOWN	9	/*  for sockets. */
#define VKI_F_SETSIG	10	/*  for sockets. */
#define VKI_F_GETSIG	11	/*  for sockets. */

#define VKI_F_GETLK64	12	/*  using 'struct flock64' */
#define VKI_F_SETLK64	13
#define VKI_F_SETLKW64	14

/* for F_[GET|SET]FL */
#define VKI_FD_CLOEXEC	1	/* actually anything with low bit set goes */

#define VKI_F_LINUX_SPECIFIC_BASE	1024

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/resource.h
//----------------------------------------------------------------------

#define VKI_RLIMIT_DATA		2	/* max data size */
#define VKI_RLIMIT_STACK	3	/* max stack size */
#define VKI_RLIMIT_CORE		4	/* max core file size */
#define VKI_RLIMIT_NOFILE	7	/* max number of open files */

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/socket.h
//----------------------------------------------------------------------

#define VKI_SOL_SOCKET	1

#define VKI_SO_TYPE	3

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/sockios.h
//----------------------------------------------------------------------

#define VKI_SIOCSPGRP	0x8902
#define VKI_SIOCGPGRP	0x8904
#define VKI_SIOCGSTAMP	0x8906		/* Get stamp */

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/stat.h
//----------------------------------------------------------------------

// [[Nb: resolved some #ifdefs by assuming __ARMEB__ is false, ie. that
// we're not big-endian.]]
struct vki_stat {
	unsigned long  st_dev;
	unsigned long  st_ino;
	unsigned short st_mode;
	unsigned short st_nlink;
	unsigned short st_uid;
	unsigned short st_gid;
	unsigned long  st_rdev;
	unsigned long  st_size;
	unsigned long  st_blksize;
	unsigned long  st_blocks;
	unsigned long  st_atime;
	unsigned long  st_atime_nsec;
	unsigned long  st_mtime;
	unsigned long  st_mtime_nsec;
	unsigned long  st_ctime;
	unsigned long  st_ctime_nsec;
	unsigned long  __unused4;
	unsigned long  __unused5;
};

struct vki_stat64 {
	unsigned long long	st_dev;
	unsigned char   __pad0[4];

#define STAT64_HAS_BROKEN_ST_INO	1
	unsigned long	__st_ino;
	unsigned int	st_mode;
	unsigned int	st_nlink;

	unsigned long	st_uid;
	unsigned long	st_gid;

	unsigned long long	st_rdev;
	unsigned char   __pad3[4];

	long long	st_size;
	unsigned long	st_blksize;

	unsigned long   st_blocks;	/* Number 512-byte blocks allocated. */
	unsigned long   __pad4;		/* Future possible st_blocks hi bits */

	unsigned long	st_atime;
	unsigned long	st_atime_nsec;

	unsigned long	st_mtime;
	unsigned long	st_mtime_nsec;

	unsigned long	st_ctime;
	unsigned long	st_ctime_nsec;

	unsigned long long	st_ino;
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/statfs.h
//----------------------------------------------------------------------

// [[Nb: asm-arm/statfs.h just #include asm-generic/statfs.h directly]]
struct vki_statfs {
	__vki_u32 f_type;
	__vki_u32 f_bsize;
	__vki_u32 f_blocks;
	__vki_u32 f_bfree;
	__vki_u32 f_bavail;
	__vki_u32 f_files;
	__vki_u32 f_ffree;
	__vki_kernel_fsid_t f_fsid;
	__vki_u32 f_namelen;
	__vki_u32 f_frsize;
	__vki_u32 f_spare[5];
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/termios.h
//----------------------------------------------------------------------

struct vki_winsize {
	unsigned short ws_row;
	unsigned short ws_col;
	unsigned short ws_xpixel;
	unsigned short ws_ypixel;
};

#define VKI_NCC 8
struct vki_termio {
	unsigned short c_iflag;		/* input mode flags */
	unsigned short c_oflag;		/* output mode flags */
	unsigned short c_cflag;		/* control mode flags */
	unsigned short c_lflag;		/* local mode flags */
	unsigned char c_line;		/* line discipline */
	unsigned char c_cc[VKI_NCC];	/* control characters */
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/termbits.h
//----------------------------------------------------------------------

typedef unsigned char   vki_cc_t;
typedef unsigned int    vki_tcflag_t;

#define VKI_NCCS 19
struct vki_termios {
	vki_tcflag_t c_iflag;		/* input mode flags */
	vki_tcflag_t c_oflag;		/* output mode flags */
	vki_tcflag_t c_cflag;		/* control mode flags */
	vki_tcflag_t c_lflag;		/* local mode flags */
	vki_cc_t c_line;		/* line discipline */
	vki_cc_t c_cc[VKI_NCCS];	/* control characters */
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/ioctl.h
//----------------------------------------------------------------------

#define _VKI_IOC_NRBITS		8
#define _VKI_IOC_TYPEBITS	8
#define _VKI_IOC_SIZEBITS	14
#define _VKI_IOC_DIRBITS	2

#define _VKI_IOC_SIZEMASK	((1 << _VKI_IOC_SIZEBITS)-1)
#define _VKI_IOC_DIRMASK	((1 << _VKI_IOC_DIRBITS)-1)

#define _VKI_IOC_NRSHIFT	0
#define _VKI_IOC_TYPESHIFT	(_VKI_IOC_NRSHIFT+_VKI_IOC_NRBITS)
#define _VKI_IOC_SIZESHIFT	(_VKI_IOC_TYPESHIFT+_VKI_IOC_TYPEBITS)
#define _VKI_IOC_DIRSHIFT	(_VKI_IOC_SIZESHIFT+_VKI_IOC_SIZEBITS)

#define _VKI_IOC_NONE	0U
#define _VKI_IOC_WRITE	1U
#define _VKI_IOC_READ	2U

#define _VKI_IOC(dir,type,nr,size) \
	(((dir)  << _VKI_IOC_DIRSHIFT) | \
	 ((type) << _VKI_IOC_TYPESHIFT) | \
	 ((nr)   << _VKI_IOC_NRSHIFT) | \
	 ((size) << _VKI_IOC_SIZESHIFT))

/* used to create numbers */
#define _VKI_IO(type,nr)	_VKI_IOC(_VKI_IOC_NONE,(type),(nr),0)
#define _VKI_IOR(type,nr,size)	_VKI_IOC(_VKI_IOC_READ,(type),(nr),sizeof(size))
#define _VKI_IOW(type,nr,size)	_VKI_IOC(_VKI_IOC_WRITE,(type),(nr),sizeof(size))
#define _VKI_IOWR(type,nr,size)	_VKI_IOC(_VKI_IOC_READ|_VKI_IOC_WRITE,(type),(nr),sizeof(size))

/* used to decode ioctl numbers.. */
#define _VKI_IOC_DIR(nr)	(((nr) >> _VKI_IOC_DIRSHIFT) & _VKI_IOC_DIRMASK)
#define _VKI_IOC_SIZE(nr)	(((nr) >> _VKI_IOC_SIZESHIFT) & _VKI_IOC_SIZEMASK)

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/ioctls.h
//----------------------------------------------------------------------

#define VKI_TCGETS	0x5401
#define VKI_TCSETS	0x5402
#define VKI_TCSETSW	0x5403
#define VKI_TCSETSF	0x5404
#define VKI_TCGETA	0x5405
#define VKI_TCSETA	0x5406
#define VKI_TCSETAW	0x5407
#define VKI_TCSETAF	0x5408
#define VKI_TCSBRK	0x5409
#define VKI_TCXONC	0x540A
#define VKI_TCFLSH	0x540B
#define VKI_TIOCSCTTY	0x540E
#define VKI_TIOCGPGRP	0x540F
#define VKI_TIOCSPGRP	0x5410
#define VKI_TIOCOUTQ	0x5411
#define VKI_TIOCGWINSZ	0x5413
#define VKI_TIOCSWINSZ	0x5414
#define VKI_TIOCMBIS	0x5416
#define VKI_TIOCMBIC	0x5417
#define VKI_TIOCMSET	0x5418
#define VKI_FIONREAD	0x541B
#define VKI_TIOCLINUX	0x541C
#define VKI_FIONBIO	0x5421
#define VKI_TCSBRKP	0x5425	/* Needed for POSIX tcsendbreak() */
#define VKI_TIOCGPTN	_VKI_IOR('T',0x30, unsigned int) /* Get Pty Number (of pty-mux device) */
#define VKI_TIOCSPTLCK	_VKI_IOW('T',0x31, int)  /* Lock/unlock Pty */

#define VKI_FIOASYNC	0x5452

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/poll.h
//----------------------------------------------------------------------

#define VKI_POLLIN	0x0001

struct vki_pollfd {
	int fd;
	short events;
	short revents;
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/user.h
//----------------------------------------------------------------------

// XXX: For x86, had here:
//   struct vki_user_i387_struct
//   struct vki_user_fxsr_struct
//   struct vki_user_regs_struct

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/ptrace.h
//----------------------------------------------------------------------

struct vki_pt_regs {
	long uregs[18];
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/elf.h
//----------------------------------------------------------------------

typedef unsigned long vki_elf_greg_t;

#define VKI_ELF_NGREG (sizeof (struct vki_pt_regs) / sizeof(vki_elf_greg_t))
typedef vki_elf_greg_t vki_elf_gregset_t[VKI_ELF_NGREG];

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/ucontext.h
//----------------------------------------------------------------------

struct vki_ucontext {
	unsigned long		uc_flags;
	struct vki_ucontext    *uc_link;
	vki_stack_t		uc_stack;
	struct vki_sigcontext	uc_mcontext;
	vki_sigset_t		uc_sigmask;	/* mask last for extensibility */
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/ipcbuf.h
//----------------------------------------------------------------------

struct vki_ipc64_perm
{
	__vki_kernel_key_t	key;
	__vki_kernel_uid32_t	uid;
	__vki_kernel_gid32_t	gid;
	__vki_kernel_uid32_t	cuid;
	__vki_kernel_gid32_t	cgid;
	__vki_kernel_mode_t	mode;
	unsigned short		__pad1;
	unsigned short		seq;
	unsigned short		__pad2;
	unsigned long		__unused1;
	unsigned long		__unused2;
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/sembuf.h
//----------------------------------------------------------------------

struct vki_semid64_ds {
	struct vki_ipc64_perm sem_perm;		/* permissions .. see ipc.h */
	__vki_kernel_time_t	sem_otime;		/* last semop time */
	unsigned long	__unused1;
	__vki_kernel_time_t	sem_ctime;		/* last change time */
	unsigned long	__unused2;
	unsigned long	sem_nsems;		/* no. of semaphores in array */
	unsigned long	__unused3;
	unsigned long	__unused4;
};


//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/msgbuf.h
//----------------------------------------------------------------------

struct vki_msqid64_ds {
	struct vki_ipc64_perm msg_perm;
	__vki_kernel_time_t msg_stime;	/* last msgsnd time */
	unsigned long	__unused1;
	__vki_kernel_time_t msg_rtime;	/* last msgrcv time */
	unsigned long	__unused2;
	__vki_kernel_time_t msg_ctime;	/* last change time */
	unsigned long	__unused3;
	unsigned long  msg_cbytes;	/* current number of bytes on queue */
	unsigned long  msg_qnum;	/* number of messages in queue */
	unsigned long  msg_qbytes;	/* max number of bytes on queue */
	__vki_kernel_pid_t msg_lspid;	/* pid of last msgsnd */
	__vki_kernel_pid_t msg_lrpid;	/* last receive pid */
	unsigned long  __unused4;
	unsigned long  __unused5;
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/ipc.h
//----------------------------------------------------------------------

struct vki_ipc_kludge {
	struct vki_msgbuf __user *msgp;
	long msgtyp;
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/asm-arm/shmbuf.h
//----------------------------------------------------------------------

struct vki_shmid64_ds {
	struct vki_ipc64_perm	shm_perm;	/* operation perms */
	vki_size_t		shm_segsz;	/* size of segment (bytes) */
	__vki_kernel_time_t	shm_atime;	/* last attach time */
	unsigned long		__unused1;
	__vki_kernel_time_t	shm_dtime;	/* last detach time */
	unsigned long		__unused2;
	__vki_kernel_time_t	shm_ctime;	/* last change time */
	unsigned long		__unused3;
	__vki_kernel_pid_t	shm_cpid;	/* pid of creator */
	__vki_kernel_pid_t	shm_lpid;	/* pid of last operator */
	unsigned long		shm_nattch;	/* no. of current attaches */
	unsigned long		__unused4;
	unsigned long		__unused5;
};

struct vki_shminfo64 {
	unsigned long	shmmax;
	unsigned long	shmmin;
	unsigned long	shmmni;
	unsigned long	shmseg;
	unsigned long	shmall;
	unsigned long	__unused1;
	unsigned long	__unused2;
	unsigned long	__unused3;
	unsigned long	__unused4;
};

//----------------------------------------------------------------------
// And that's it!
//----------------------------------------------------------------------

#endif // __ARM_LINUX_VKI_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
