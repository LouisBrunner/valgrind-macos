
/*--------------------------------------------------------------------*/
/*--- riscv64/Linux-specific kernel interface. vki-riscv64-linux.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2020-2023 Petr Pavlu
      petr.pavlu@dagobah.cz

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

#ifndef __VKI_RISCV64_LINUX_H
#define __VKI_RISCV64_LINUX_H

// riscv64 is little-endian.
#define VKI_LITTLE_ENDIAN  1

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/int-ll64.h
//----------------------------------------------------------------------

typedef unsigned char __vki_u8;

typedef __signed__ short __vki_s16;
typedef unsigned short __vki_u16;

typedef __signed__ int __vki_s32;
typedef unsigned int __vki_u32;

typedef __signed__ long long __vki_s64;
typedef unsigned long long __vki_u64;

typedef unsigned short vki_u16;

typedef unsigned int vki_u32;

//----------------------------------------------------------------------
// From linux-6.0/arch/riscv/include/asm/page.h
//----------------------------------------------------------------------

#define VKI_PAGE_SHIFT	(12)
#define VKI_PAGE_SIZE	(1UL << VKI_PAGE_SHIFT)
#define VKI_MAX_PAGE_SHIFT	VKI_PAGE_SHIFT
#define VKI_MAX_PAGE_SIZE	VKI_PAGE_SIZE

//----------------------------------------------------------------------
// From linux-6.0/include/asm-generic/shmparam.h
//----------------------------------------------------------------------

#define VKI_SHMLBA VKI_PAGE_SIZE	/* attach addr a multiple of this */

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/signal-defs.h
//----------------------------------------------------------------------

#define VKI_SIG_BLOCK          0	/* for blocking signals */
#define VKI_SIG_UNBLOCK        1	/* for unblocking signals */
#define VKI_SIG_SETMASK        2	/* for setting the signal mask */

typedef void __vki_signalfn_t(int);
typedef __vki_signalfn_t __user *__vki_sighandler_t;

#define VKI_SIG_DFL	((__vki_sighandler_t)0)	/* default signal handling */
#define VKI_SIG_IGN	((__vki_sighandler_t)1)	/* ignore signal */

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/signal.h
//----------------------------------------------------------------------

#define _VKI_NSIG	64
#define _VKI_NSIG_BPW	64
#define _VKI_NSIG_WORDS	(_VKI_NSIG / _VKI_NSIG_BPW)

typedef unsigned long vki_old_sigset_t;

typedef struct {
	unsigned long sig[_VKI_NSIG_WORDS];
} vki_sigset_t;

#define VKI_SIGHUP		 1
#define VKI_SIGINT		 2
#define VKI_SIGQUIT		 3
#define VKI_SIGILL		 4
#define VKI_SIGTRAP		 5
#define VKI_SIGABRT		 6
#define VKI_SIGBUS		 7
#define VKI_SIGFPE		 8
#define VKI_SIGKILL		 9
#define VKI_SIGUSR1		10
#define VKI_SIGSEGV		11
#define VKI_SIGUSR2		12
#define VKI_SIGPIPE		13
#define VKI_SIGALRM		14
#define VKI_SIGTERM		15
#define VKI_SIGSTKFLT		16
#define VKI_SIGCHLD		17
#define VKI_SIGCONT		18
#define VKI_SIGSTOP		19
#define VKI_SIGTSTP		20
#define VKI_SIGTTIN		21
#define VKI_SIGTTOU		22
#define VKI_SIGURG		23
#define VKI_SIGXCPU		24
#define VKI_SIGXFSZ		25
#define VKI_SIGVTALRM		26
#define VKI_SIGPROF		27
#define VKI_SIGWINCH		28
#define VKI_SIGIO		29
#define VKI_SIGPWR		30
#define VKI_SIGSYS		31
#define	VKI_SIGUNUSED		31

#define VKI_SIGRTMIN		32
#define VKI_SIGRTMAX		_VKI_NSIG

#define VKI_SA_NOCLDSTOP	0x00000001
#define VKI_SA_NOCLDWAIT	0x00000002
#define VKI_SA_SIGINFO		0x00000004
#define VKI_SA_ONSTACK		0x08000000
#define VKI_SA_RESTART		0x10000000
#define VKI_SA_NODEFER		0x40000000
#define VKI_SA_RESETHAND	0x80000000

#define VKI_SA_NOMASK	VKI_SA_NODEFER
#define VKI_SA_ONESHOT	VKI_SA_RESETHAND

#define VKI_MINSIGSTKSZ	2048

struct vki_sigaction_base {
	__vki_sighandler_t ksa_handler;
	unsigned long sa_flags;
	vki_sigset_t sa_mask;
};

/* On Linux we use the same type for passing sigactions to
   and from the kernel.  Hence: */
typedef  struct vki_sigaction_base  vki_sigaction_toK_t;
typedef  struct vki_sigaction_base  vki_sigaction_fromK_t;

typedef struct vki_sigaltstack {
	void __user *ss_sp;
	int ss_flags;
	vki_size_t ss_size;
} vki_stack_t;

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/linux/signal.h
//----------------------------------------------------------------------

#define VKI_SS_ONSTACK	1
#define VKI_SS_DISABLE	2

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/mman-common.h
//----------------------------------------------------------------------

#define VKI_PROT_READ	0x1		/* page can be read */
#define VKI_PROT_WRITE	0x2		/* page can be written */
#define VKI_PROT_EXEC	0x4		/* page can be executed */
#define VKI_PROT_NONE	0x0		/* page can not be accessed */
#define VKI_PROT_GROWSDOWN	0x01000000	/* mprotect flag: extend change to start of growsdown vma */
#define VKI_PROT_GROWSUP	0x02000000	/* mprotect flag: extend change to end of growsup vma */

#define VKI_MAP_FIXED	0x10		/* Interpret addr exactly */
#define VKI_MAP_ANONYMOUS	0x20	/* don't use a file */

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/mman.h
//----------------------------------------------------------------------

#define VKI_MAP_NORESERVE       0x4000  /* don't check for reservations */

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/linux/mman.h
//----------------------------------------------------------------------

#define VKI_MAP_SHARED	0x01		/* Share changes */
#define VKI_MAP_PRIVATE	0x02		/* Changes are private */

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/fcntl.h
//----------------------------------------------------------------------

#define VKI_O_ACCMODE	     03
#define VKI_O_RDONLY	     00
#define VKI_O_WRONLY	     01
#define VKI_O_RDWR	     02
#define VKI_O_CREAT	   0100	/* not fcntl */
#define VKI_O_EXCL	   0200	/* not fcntl */
#define VKI_O_TRUNC	  01000	/* not fcntl */
#define VKI_O_APPEND	  02000
#define VKI_O_NONBLOCK	  04000
#define VKI_O_LARGEFILE	0100000
#define VKI_O_DIRECT   00040000

#define VKI_F_DUPFD		0	/* dup */
#define VKI_F_GETFD		1	/* get close_on_exec */
#define VKI_F_SETFD		2	/* set/clear close_on_exec */
#define VKI_F_GETFL		3	/* get file->f_flags */
#define VKI_F_SETFL		4	/* set file->f_flags */
#define VKI_F_GETLK		5
#define VKI_F_SETLK		6
#define VKI_F_SETLKW		7

#define VKI_F_SETOWN		8	/*  for sockets. */
#define VKI_F_GETOWN		9	/*  for sockets. */
#define VKI_F_SETSIG		10	/*  for sockets. */
#define VKI_F_GETSIG		11	/*  for sockets. */

#define VKI_F_SETOWN_EX		15
#define VKI_F_GETOWN_EX		16

#define VKI_F_OFD_GETLK		36
#define VKI_F_OFD_SETLK		37
#define VKI_F_OFD_SETLKW	38

struct vki_f_owner_ex {
	int	type;
	__vki_kernel_pid_t	pid;
};

#define VKI_FD_CLOEXEC	1	/* actually anything with low bit set goes */

#define VKI_F_LINUX_SPECIFIC_BASE	1024

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/linux/fcntl.h
//----------------------------------------------------------------------

#define VKI_AT_FDCWD		-100

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/resource.h
//----------------------------------------------------------------------

#define VKI_RLIMIT_DATA		2	/* max data size */
#define VKI_RLIMIT_STACK	3	/* max stack size */
#define VKI_RLIMIT_CORE		4	/* max core file size */
#define VKI_RLIMIT_NOFILE	7	/* max number of open files */

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/socket.h
//----------------------------------------------------------------------

#define VKI_SOL_SOCKET	1

#define VKI_SO_TYPE	3

#define VKI_SO_ATTACH_FILTER	26

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/sockios.h
//----------------------------------------------------------------------

#define VKI_SIOCSPGRP		0x8902
#define VKI_SIOCGPGRP		0x8904
#define VKI_SIOCATMARK		0x8905
#define VKI_SIOCGSTAMP		0x8906		/* Get stamp (timeval) */
#define VKI_SIOCGSTAMPNS	0x8907		/* Get stamp (timespec) */

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/stat.h
//----------------------------------------------------------------------

struct vki_stat {
	unsigned long	st_dev;		/* Device.  */
	unsigned long	st_ino;		/* File serial number.  */
	unsigned int	st_mode;	/* File mode.  */
	unsigned int	st_nlink;	/* Link count.  */
	unsigned int	st_uid;		/* User ID of the file's owner.  */
	unsigned int	st_gid;		/* Group ID of the file's group. */
	unsigned long	st_rdev;	/* Device number, if device.  */
	unsigned long	__pad1;
	long		st_size;	/* Size of file, in bytes.  */
	int		st_blksize;	/* Optimal block size for I/O.  */
	int		__pad2;
	long		st_blocks;	/* Number 512-byte blocks allocated. */
	long		st_atime;	/* Time of last access.  */
	unsigned long	st_atime_nsec;
	long		st_mtime;	/* Time of last modification.  */
	unsigned long	st_mtime_nsec;
	long		st_ctime;	/* Time of last status change.  */
	unsigned long	st_ctime_nsec;
	unsigned int	__unused4;
	unsigned int	__unused5;
};

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/statfs.h
//----------------------------------------------------------------------

struct vki_statfs {
	long f_type;
	long f_bsize;
	long f_blocks;
	long f_bfree;
	long f_bavail;
	long f_files;
	long f_ffree;
	__vki_kernel_fsid_t f_fsid;
	long f_namelen;
	long f_frsize;
	long f_flags;
	long f_spare[4];
};

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/termios.h
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
// From linux-6.0/include/uapi/asm-generic/termbits.h
//----------------------------------------------------------------------

typedef unsigned char	vki_cc_t;
typedef unsigned int	vki_tcflag_t;

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
// From linux-6.0/include/uapi/asm-generic/ioctl.h
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

#define _VKI_IO(type,nr)	_VKI_IOC(_VKI_IOC_NONE,(type),(nr),0)
#define _VKI_IOR(type,nr,size)	_VKI_IOC(_VKI_IOC_READ,(type),(nr),sizeof(size))
#define _VKI_IOW(type,nr,size)	_VKI_IOC(_VKI_IOC_WRITE,(type),(nr),sizeof(size))
#define _VKI_IOWR(type,nr,size)	_VKI_IOC(_VKI_IOC_READ|_VKI_IOC_WRITE,(type),(nr),sizeof(size))

#define _VKI_IOC_DIR(nr)		(((nr) >> _VKI_IOC_DIRSHIFT) & _VKI_IOC_DIRMASK)
#define _VKI_IOC_SIZE(nr)		(((nr) >> _VKI_IOC_SIZESHIFT) & _VKI_IOC_SIZEMASK)

//----------------------------------------------------------------------
// From linux-3.10.5/include/uapi/asm-generic/ioctls.h
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
#define VKI_TIOCMGET	0x5415
#define VKI_TIOCMBIS	0x5416
#define VKI_TIOCMBIC	0x5417
#define VKI_TIOCMSET	0x5418
#define VKI_FIONREAD	0x541B
#define VKI_TIOCLINUX	0x541C
#define VKI_TIOCGSERIAL	0x541E
#define VKI_TIOCSSERIAL	0x541F
#define VKI_FIONBIO	0x5421
#define VKI_TIOCNOTTY	0x5422
#define VKI_TCSBRKP	0x5425	/* Needed for POSIX tcsendbreak() */
#define VKI_TIOCGPTN	_VKI_IOR('T',0x30, unsigned int) /* Get Pty Number (of pty-mux device) */
#define VKI_TIOCSPTLCK	_VKI_IOW('T',0x31, int) /* Lock/unlock Pty */

#define VKI_FIONCLEX    0x5450
#define VKI_FIOCLEX     0x5451
#define VKI_FIOASYNC	0x5452
#define VKI_TIOCSERGETLSR   0x5459 /* Get line status register */

#define VKI_TIOCGICOUNT	0x545D	/* read serial port inline interrupt counts */

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/poll.h
//----------------------------------------------------------------------

#define VKI_POLLIN		0x0001

struct vki_pollfd {
	int fd;
	short events;
	short revents;
};

//----------------------------------------------------------------------
// From linux-6.0/arch/riscv/include/uapi/asm/ptrace.h
//----------------------------------------------------------------------

struct vki_user_regs_struct {
	unsigned long pc;
	unsigned long ra;
	unsigned long sp;
	unsigned long gp;
	unsigned long tp;
	unsigned long t0;
	unsigned long t1;
	unsigned long t2;
	unsigned long s0;
	unsigned long s1;
	unsigned long a0;
	unsigned long a1;
	unsigned long a2;
	unsigned long a3;
	unsigned long a4;
	unsigned long a5;
	unsigned long a6;
	unsigned long a7;
	unsigned long s2;
	unsigned long s3;
	unsigned long s4;
	unsigned long s5;
	unsigned long s6;
	unsigned long s7;
	unsigned long s8;
	unsigned long s9;
	unsigned long s10;
	unsigned long s11;
	unsigned long t3;
	unsigned long t4;
	unsigned long t5;
	unsigned long t6;
};

struct __vki_riscv_f_ext_state {
	__vki_u32 f[32];
	__vki_u32 fcsr;
};

struct __vki_riscv_d_ext_state {
	__vki_u64 f[32];
	__vki_u32 fcsr;
};

struct __vki_riscv_q_ext_state {
	__vki_u64 f[64] __attribute__((aligned(16)));
	__vki_u32 fcsr;
	__vki_u32 reserved[3];
};

union __vki_riscv_fp_state {
	struct __vki_riscv_f_ext_state f;
	struct __vki_riscv_d_ext_state d;
	struct __vki_riscv_q_ext_state q;
};

//----------------------------------------------------------------------
// From linux-6.0/arch/riscv/include/uapi/asm/sigcontext.h
//----------------------------------------------------------------------

struct vki_sigcontext {
	struct vki_user_regs_struct sc_regs;
	union __vki_riscv_fp_state sc_fpregs;
};

//----------------------------------------------------------------------
// From linux-6.0/arch/riscv/include/uapi/asm/elf.h
//----------------------------------------------------------------------

typedef unsigned long vki_elf_greg_t;
typedef struct vki_user_regs_struct vki_elf_gregset_t;
#define VKI_ELF_NGREG (sizeof (struct vki_elf_gregset_t) / sizeof(vki_elf_greg_t))

typedef union __vki_riscv_fp_state vki_elf_fpregset_t;

//----------------------------------------------------------------------
// From linux-6.0/arch/riscv/include/uapi/asm/ucontext.h
//----------------------------------------------------------------------

struct vki_ucontext {
	unsigned long		uc_flags;
	struct vki_ucontext	*uc_link;
	vki_stack_t		uc_stack;
	vki_sigset_t		uc_sigmask;
	__vki_u8		__unused[1024 / 8 - sizeof(vki_sigset_t)];
	struct vki_sigcontext	uc_mcontext;
};

typedef char vki_modify_ldt_t;

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/ipcbuf.h
//----------------------------------------------------------------------

struct vki_ipc64_perm {
	__vki_kernel_key_t	key;
	__vki_kernel_uid32_t	uid;
	__vki_kernel_gid32_t	gid;
	__vki_kernel_uid32_t	cuid;
	__vki_kernel_gid32_t	cgid;
	__vki_kernel_mode_t	mode;
        unsigned char           __pad1[4 - sizeof(__vki_kernel_mode_t)];
	unsigned short		seq;
	unsigned short		__pad2;
	unsigned long		__unused1;
	unsigned long		__unused2;
};

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/sembuf.h
//----------------------------------------------------------------------

struct vki_semid64_ds {
	struct vki_ipc64_perm sem_perm;		/* permissions .. see ipc.h */
	__vki_kernel_time_t	sem_otime;		/* last semop time */
	__vki_kernel_time_t	sem_ctime;		/* last change time */
	unsigned long	sem_nsems;		/* no. of semaphores in array */
	unsigned long	__unused3;
	unsigned long	__unused4;
};

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/msgbuf.h
//----------------------------------------------------------------------

struct vki_msqid64_ds {
	struct vki_ipc64_perm msg_perm;
	__vki_kernel_time_t msg_stime;	/* last msgsnd time */
	__vki_kernel_time_t msg_rtime;	/* last msgrcv time */
	__vki_kernel_time_t msg_ctime;	/* last change time */
	unsigned long  msg_cbytes;	/* current number of bytes on queue */
	unsigned long  msg_qnum;	/* number of messages in queue */
	unsigned long  msg_qbytes;	/* max number of bytes on queue */
	__vki_kernel_pid_t msg_lspid;	/* pid of last msgsnd */
	__vki_kernel_pid_t msg_lrpid;	/* last receive pid */
	unsigned long  __unused4;
	unsigned long  __unused5;
};

//----------------------------------------------------------------------
// From linux-6.0/include/uapi/asm-generic/shmbuf.h
//----------------------------------------------------------------------

struct vki_shmid64_ds {
	struct vki_ipc64_perm	shm_perm;	/* operation perms */
	vki_size_t		shm_segsz;	/* size of segment (bytes) */
	__vki_kernel_time_t	shm_atime;	/* last attach time */
	__vki_kernel_time_t	shm_dtime;	/* last detach time */
	__vki_kernel_time_t	shm_ctime;	/* last change time */
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
// From linux-6.0/include/uapi/asm-generic/errno.h
//----------------------------------------------------------------------

#define	VKI_ENOSYS		38	/* Invalid system call number */
#define	VKI_EOVERFLOW		75	/* Value too large for defined data type */

#endif // __VKI_RISCV64_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                      vki-riscv64-linux.h ---*/
/*--------------------------------------------------------------------*/
