
/*--------------------------------------------------------------------*/
/*--- Linux-specific kernel interface.                 vki-linux.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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

/* This file defines types and constants for the kernel interface, and to
   make that clear everything is prefixed VKI_/vki_.

   All code is copied verbatim from kernel source files, except that:
   - VKI_/vki_ prefixes are added
   - some extra explanatory comments are included;  they are all within
     "[[ ]]"
   - for some types, we only care about the size;  for a few of them (big
     ones that are painful to fully drag in here), a VKI_SIZEOF_* constant
     is used.
   
   The files the code is taken from is indicated.

   Note especially that the types are not the glibc versions, many of which
   are different to those in here. 

   Also note that this file contains all the generic header info, ie. that
   from linux/include/linux/ *.h.  The arch-specific header info, eg. that
   from linux/include/asm-i386/ *.h, is in vki-$PLATFORM.h and
   vki_posixtypes-$PLATFORM.h.  (Two files are required to avoid
   circular dependencies between the generic VKI header and the
   arch-specific VKI header.  It's possible in the future, as more stuff
   gets pulled in, that we might have to split files up some more to avoid
   further circular dependencies.)
   
   Finally, note that it is assumed that __KERNEL__ is set for all these
   definitions, which affects some of them.
*/

/* The structure is (aiui, jrs 20060504):

     #include plat-specific posix types (vki-posixtypes-ARCH-linux.h)

     Lots more types, structs, consts, in this file

     #include other plat-specific stuff (vki-ARCH-linux.h)

     Even more types, structs, consts, in this file

   The system call numbers are dealt with by
   pub_{core,tool}_vkiscnums.h, not via pub_{core,tool}_vki.h, which
   is what this file is part of.
*/

#ifndef __VKI_LINUX_H
#define __VKI_LINUX_H

//----------------------------------------------------------------------
// Arch-specific POSIX types
//----------------------------------------------------------------------

#if defined(VGA_x86)
#  include "vki-posixtypes-x86-linux.h"
#elif defined(VGA_amd64)
#  include "vki-posixtypes-amd64-linux.h"
#elif defined(VGA_ppc32)
#  include "vki-posixtypes-ppc32-linux.h"
#elif defined(VGA_ppc64)
#  include "vki-posixtypes-ppc64-linux.h"
#elif defined(VGA_arm)
#  include "vki-posixtypes-arm-linux.h"
#elif defined(VGA_s390x)
#  include "vki-posixtypes-s390x-linux.h"
#else
#  error Unknown platform
#endif

//----------------------------------------------------------------------
// VKI_STATIC_ASSERT(). Inspired by BUILD_BUG_ON() from
// linux-2.6.34/include/linux/kernel.h
//----------------------------------------------------------------------

/*
 * Evaluates to zero if 'expr' is true and forces a compilation error if
 * 'expr' is false. Can be used in a context where no comma expressions
 * are allowed.
 */
#ifdef __cplusplus
template <bool b> struct vki_static_assert { int m_bitfield:(2*b-1); };
#define VKI_STATIC_ASSERT(expr)                         \
    (sizeof(vki_static_assert<(expr)>) - sizeof(int))
#else
#define VKI_STATIC_ASSERT(expr) (sizeof(struct { int:-!(expr); }))
#endif

//----------------------------------------------------------------------
// Based on _IOC_TYPECHECK() from linux-2.6.34/asm-generic/ioctl.h
//----------------------------------------------------------------------

/* provoke compile error for invalid uses of size argument */
#define _VKI_IOC_TYPECHECK(t)                                           \
    (VKI_STATIC_ASSERT((sizeof(t) == sizeof(t[1])                       \
                        && sizeof(t) < (1 << _VKI_IOC_SIZEBITS)))       \
     + sizeof(t))

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/compiler.h
//----------------------------------------------------------------------

# define __user

# define __attribute_const__    /* unimplemented */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/posix_types.h
//----------------------------------------------------------------------

#undef __VKI_NFDBITS
#define __VKI_NFDBITS	(8 * sizeof(unsigned long))

#undef __VKI_FD_SETSIZE
#define __VKI_FD_SETSIZE	1024

#undef __VKI_FDSET_LONGS
#define __VKI_FDSET_LONGS	(__VKI_FD_SETSIZE/__VKI_NFDBITS)

#undef __VKI_FDELT
#define	__VKI_FDELT(d)	((d) / __VKI_NFDBITS)

#undef __VKI_FDMASK
#define	__VKI_FDMASK(d)	(1UL << ((d) % __VKI_NFDBITS))

typedef struct {
	unsigned long fds_bits [__VKI_FDSET_LONGS];
} __vki_kernel_fd_set;

typedef int __vki_kernel_key_t;
typedef int __vki_kernel_mqd_t;

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/types.h
//----------------------------------------------------------------------

typedef __vki_kernel_fd_set	vki_fd_set;
typedef __vki_kernel_mode_t	vki_mode_t;
typedef __vki_kernel_off_t	vki_off_t;
typedef __vki_kernel_pid_t	vki_pid_t;
typedef __vki_kernel_key_t	vki_key_t;
typedef __vki_kernel_suseconds_t	vki_suseconds_t;
typedef __vki_kernel_timer_t	vki_timer_t;
typedef __vki_kernel_clockid_t	vki_clockid_t;
typedef __vki_kernel_mqd_t	vki_mqd_t;

// [[Nb: it's a bit unclear due to a #ifdef, but I think this is right. --njn]]
typedef __vki_kernel_uid32_t	vki_uid_t;
typedef __vki_kernel_gid32_t	vki_gid_t;

typedef __vki_kernel_old_uid_t	vki_old_uid_t;
typedef __vki_kernel_old_gid_t	vki_old_gid_t;

typedef __vki_kernel_loff_t	vki_loff_t;

typedef __vki_kernel_size_t	vki_size_t;
typedef __vki_kernel_time_t	vki_time_t;
typedef __vki_kernel_clock_t	vki_clock_t;
typedef __vki_kernel_caddr_t	vki_caddr_t;

typedef unsigned long           vki_u_long;

typedef unsigned int	        vki_uint;

//----------------------------------------------------------------------
// Now the rest of the arch-specific stuff
//----------------------------------------------------------------------

#if defined(VGA_x86)
#  include "vki-x86-linux.h"
#elif defined(VGA_amd64)
#  include "vki-amd64-linux.h"
#elif defined(VGA_ppc32)
#  include "vki-ppc32-linux.h"
#elif defined(VGA_ppc64)
#  include "vki-ppc64-linux.h"
#elif defined(VGA_arm)
#  include "vki-arm-linux.h"
#elif defined(VGA_s390x)
#  include "vki-s390x-linux.h"
#else
#  error Unknown platform
#endif

//----------------------------------------------------------------------
// From linux-2.6.20.1/include/linux/types.h
//----------------------------------------------------------------------

typedef		__vki_s32	vki_int32_t;

typedef		__vki_u8	vki_uint8_t;
typedef		__vki_u16	vki_uint16_t;
typedef		__vki_u32	vki_uint32_t;

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/limits.h
//----------------------------------------------------------------------

#define VKI_PATH_MAX       4096	/* # chars in a path name including nul */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/kernel.h
//----------------------------------------------------------------------

struct vki_sysinfo {
	long uptime;			/* Seconds since boot */
	unsigned long loads[3];		/* 1, 5, and 15 minute load averages */
	unsigned long totalram;		/* Total usable main memory size */
	unsigned long freeram;		/* Available memory size */
	unsigned long sharedram;	/* Amount of shared memory */
	unsigned long bufferram;	/* Memory used by buffers */
	unsigned long totalswap;	/* Total swap space size */
	unsigned long freeswap;		/* swap space still available */
	unsigned short procs;		/* Number of current processes */
	unsigned short pad;		/* explicit padding for m68k */
	unsigned long totalhigh;	/* Total high memory size */
	unsigned long freehigh;		/* Available high memory size */
	unsigned int mem_unit;		/* Memory unit size in bytes */
	char _f[20-2*sizeof(long)-sizeof(int)];	/* Padding: libc5 uses this.. */
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/time.h
//----------------------------------------------------------------------

#define VKI_CLOCK_REALTIME            0
#define VKI_CLOCK_MONOTONIC           1
#define VKI_CLOCK_PROCESS_CPUTIME_ID  2
#define VKI_CLOCK_THREAD_CPUTIME_ID   3

struct vki_timespec {
	vki_time_t	tv_sec;		/* seconds */
	long		tv_nsec;	/* nanoseconds */
};

struct vki_timeval {
	vki_time_t	tv_sec;		/* seconds */
	vki_suseconds_t	tv_usec;	/* microseconds */
};

struct vki_timezone {
	int	tz_minuteswest;	/* minutes west of Greenwich */
	int	tz_dsttime;	/* type of dst correction */
};

struct  vki_itimerspec {
        struct  vki_timespec it_interval;    /* timer period */
        struct  vki_timespec it_value;       /* timer expiration */
};

struct	vki_itimerval {
	struct	vki_timeval it_interval;	/* timer interval */
	struct	vki_timeval it_value;	/* current value */
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/timex.h
//----------------------------------------------------------------------

struct vki_timex {
	unsigned int modes;	/* mode selector */
	long offset;		/* time offset (usec) */
	long freq;		/* frequency offset (scaled ppm) */
	long maxerror;		/* maximum error (usec) */
	long esterror;		/* estimated error (usec) */
	int status;		/* clock command/status */
	long constant;		/* pll time constant */
	long precision;		/* clock precision (usec) (read only) */
	long tolerance;		/* clock frequency tolerance (ppm)
				 * (read only)
				 */
	struct vki_timeval time;	/* (read only) */
	long tick;		/* (modified) usecs between clock ticks */

	long ppsfreq;           /* pps frequency (scaled ppm) (ro) */
	long jitter;            /* pps jitter (us) (ro) */
	int shift;              /* interval duration (s) (shift) (ro) */
	long stabil;            /* pps stability (scaled ppm) (ro) */
	long jitcnt;            /* jitter limit exceeded (ro) */
	long calcnt;            /* calibration intervals (ro) */
	long errcnt;            /* calibration errors (ro) */
	long stbcnt;            /* stability limit exceeded (ro) */

	int  :32; int  :32; int  :32; int  :32;
	int  :32; int  :32; int  :32; int  :32;
	int  :32; int  :32; int  :32; int  :32;
};

#define VKI_ADJ_OFFSET			0x0001	/* time offset */
#define VKI_ADJ_FREQUENCY		0x0002	/* frequency offset */
#define VKI_ADJ_MAXERROR		0x0004	/* maximum time error */
#define VKI_ADJ_ESTERROR		0x0008	/* estimated time error */
#define VKI_ADJ_STATUS			0x0010	/* clock status */
#define VKI_ADJ_TIMECONST		0x0020	/* pll time constant */
#define VKI_ADJ_TAI			0x0080	/* set TAI offset */
#define VKI_ADJ_TICK			0x4000	/* tick value */
#define VKI_ADJ_ADJTIME			0x8000	/* switch between adjtime/adjtimex modes */
//#define VKI_ADJ_OFFSET_SINGLESHOT	0x8001	/* old-fashioned adjtime */
#define VKI_ADJ_OFFSET_READONLY		0x2000	/* read-only adjtime */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/times.h
//----------------------------------------------------------------------

struct vki_tms {
	vki_clock_t tms_utime;
	vki_clock_t tms_stime;
	vki_clock_t tms_cutime;
	vki_clock_t tms_cstime;
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/utime.h
//----------------------------------------------------------------------

struct vki_utimbuf {
	vki_time_t actime;
	vki_time_t modtime;
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/sched.h
//----------------------------------------------------------------------

#define VKI_CSIGNAL		0x000000ff	/* signal mask to be sent at exit */
#define VKI_CLONE_VM		0x00000100	/* set if VM shared between processes */
#define VKI_CLONE_FS		0x00000200	/* set if fs info shared between processes */
#define VKI_CLONE_FILES		0x00000400	/* set if open files shared between processes */
#define VKI_CLONE_SIGHAND	0x00000800	/* set if signal handlers and blocked signals shared */
#define VKI_CLONE_VFORK		0x00004000	/* set if the parent wants the child to wake it up on mm_release */
#define VKI_CLONE_PARENT	0x00008000	/* set if we want to have the same parent as the cloner */
#define VKI_CLONE_THREAD	0x00010000	/* Same thread group? */
#define VKI_CLONE_SYSVSEM	0x00040000	/* share system V SEM_UNDO semantics */
#define VKI_CLONE_SETTLS	0x00080000	/* create a new TLS for the child */
#define VKI_CLONE_PARENT_SETTID	0x00100000	/* set the TID in the parent */
#define VKI_CLONE_CHILD_CLEARTID	0x00200000	/* clear the TID in the child */
#define VKI_CLONE_DETACHED	0x00400000	/* Unused, ignored */
#define VKI_CLONE_CHILD_SETTID	0x01000000	/* set the TID in the child */

struct vki_sched_param {
	int sched_priority;
};

#define VKI_TASK_COMM_LEN 16

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/asm-generic/siginfo.h
//----------------------------------------------------------------------

typedef union vki_sigval {
	int sival_int;
	void __user *sival_ptr;
} vki_sigval_t;

#ifndef __VKI_ARCH_SI_PREAMBLE_SIZE
#define __VKI_ARCH_SI_PREAMBLE_SIZE	(3 * sizeof(int))
#endif

#define VKI_SI_MAX_SIZE	128

#ifndef VKI_SI_PAD_SIZE
#define VKI_SI_PAD_SIZE	((VKI_SI_MAX_SIZE - __VKI_ARCH_SI_PREAMBLE_SIZE) / sizeof(int))
#endif

#ifndef __VKI_ARCH_SI_UID_T
#define __VKI_ARCH_SI_UID_T	vki_uid_t
#endif

#ifndef __VKI_ARCH_SI_BAND_T
#define __VKI_ARCH_SI_BAND_T long
#endif

// [[Nb: this type changed between 2.4 and 2.6, but not in a way that
// affects Valgrind.]]
typedef struct vki_siginfo {
	int si_signo;
	int si_errno;
	int si_code;

	union {
		int _pad[VKI_SI_PAD_SIZE];

		/* kill() */
		struct {
			vki_pid_t _pid;		/* sender's pid */
			__VKI_ARCH_SI_UID_T _uid;	/* sender's uid */
		} _kill;

		/* POSIX.1b timers */
		struct {
			vki_timer_t _tid;		/* timer id */
			int _overrun;		/* overrun count */
			char _pad[sizeof( __VKI_ARCH_SI_UID_T) - sizeof(int)];
			vki_sigval_t _sigval;	/* same as below */
			int _sys_private;       /* not to be passed to user */
		} _timer;

		/* POSIX.1b signals */
		struct {
			vki_pid_t _pid;		/* sender's pid */
			__VKI_ARCH_SI_UID_T _uid;	/* sender's uid */
			vki_sigval_t _sigval;
		} _rt;

		/* SIGCHLD */
		struct {
			vki_pid_t _pid;		/* which child */
			__VKI_ARCH_SI_UID_T _uid;	/* sender's uid */
			int _status;		/* exit code */
			vki_clock_t _utime;
			vki_clock_t _stime;
		} _sigchld;

		/* SIGILL, SIGFPE, SIGSEGV, SIGBUS */
		struct {
			void __user *_addr; /* faulting insn/memory ref. */
#ifdef __ARCH_SI_TRAPNO
			int _trapno;	/* TRAP # which caused the signal */
#endif
		} _sigfault;

		/* SIGPOLL */
		struct {
			__VKI_ARCH_SI_BAND_T _band;	/* POLL_IN, POLL_OUT, POLL_MSG */
			int _fd;
		} _sigpoll;
	} _sifields;
} vki_siginfo_t;

#define __VKI_SI_FAULT	0

/*
 * si_code values
 * Digital reserves positive values for kernel-generated signals.
 */
#define VKI_SI_USER	0		/* sent by kill, sigsend, raise */
#define VKI_SI_TKILL	-6		/* sent by tkill system call */

/*
 * SIGILL si_codes
 */
#define VKI_ILL_ILLOPC	(__VKI_SI_FAULT|1)	/* illegal opcode */
#define VKI_ILL_ILLOPN	(__VKI_SI_FAULT|2)	/* illegal operand */
#define VKI_ILL_ILLADR	(__VKI_SI_FAULT|3)	/* illegal addressing mode */
#define VKI_ILL_ILLTRP	(__VKI_SI_FAULT|4)	/* illegal trap */
#define VKI_ILL_PRVOPC	(__VKI_SI_FAULT|5)	/* privileged opcode */
#define VKI_ILL_PRVREG	(__VKI_SI_FAULT|6)	/* privileged register */
#define VKI_ILL_COPROC	(__VKI_SI_FAULT|7)	/* coprocessor error */
#define VKI_ILL_BADSTK	(__VKI_SI_FAULT|8)	/* internal stack error */

/*
 * SIGFPE si_codes
 */
#define VKI_FPE_INTDIV	(__VKI_SI_FAULT|1)	/* integer divide by zero */
#define VKI_FPE_INTOVF	(__VKI_SI_FAULT|2)	/* integer overflow */
#define VKI_FPE_FLTDIV	(__VKI_SI_FAULT|3)	/* floating point divide by zero */
#define VKI_FPE_FLTOVF	(__VKI_SI_FAULT|4)	/* floating point overflow */
#define VKI_FPE_FLTUND	(__VKI_SI_FAULT|5)	/* floating point underflow */
#define VKI_FPE_FLTRES	(__VKI_SI_FAULT|6)	/* floating point inexact result */
#define VKI_FPE_FLTINV	(__VKI_SI_FAULT|7)	/* floating point invalid operation */
#define VKI_FPE_FLTSUB	(__VKI_SI_FAULT|8)	/* subscript out of range */

/*
 * SIGSEGV si_codes
 */
#define VKI_SEGV_MAPERR	(__VKI_SI_FAULT|1)	/* address not mapped to object */
#define VKI_SEGV_ACCERR	(__VKI_SI_FAULT|2)	/* invalid permissions for mapped object */

/*
 * SIGBUS si_codes
 */
#define VKI_BUS_ADRALN	(__VKI_SI_FAULT|1)	/* invalid address alignment */
#define VKI_BUS_ADRERR	(__VKI_SI_FAULT|2)	/* non-existant physical address */
#define VKI_BUS_OBJERR	(__VKI_SI_FAULT|3)	/* object specific hardware error */

/*
 * SIGTRAP si_codes
 */
#define VKI_TRAP_BRKPT      (__VKI_SI_FAULT|1)  /* process breakpoint */
#define VKI_TRAP_TRACE      (__VKI_SI_FAULT|2)  /* process trace trap */

/*
 * This works because the alignment is ok on all current architectures
 * but we leave open this being overridden in the future
 */
#ifndef VKI___ARCH_SIGEV_PREAMBLE_SIZE
#define VKI___ARCH_SIGEV_PREAMBLE_SIZE	(sizeof(int) * 2 + sizeof(vki_sigval_t))
#endif

#define VKI_SIGEV_MAX_SIZE	64
#define VKI_SIGEV_PAD_SIZE	((VKI_SIGEV_MAX_SIZE - VKI___ARCH_SIGEV_PREAMBLE_SIZE) \
		/ sizeof(int))

typedef struct vki_sigevent {
	vki_sigval_t sigev_value;
	int sigev_signo;
	int sigev_notify;
	union {
		int _pad[VKI_SIGEV_PAD_SIZE];
		 int _tid;

		struct {
			void (*_function)(vki_sigval_t);
			void *_attribute;	/* really pthread_attr_t */
		} _sigev_thread;
	} _sigev_un;
} vki_sigevent_t;

//----------------------------------------------------------------------
// From elsewhere...
//----------------------------------------------------------------------

// [[The kernel actually uses the numbers 0,1,2 directly here, believe it or
// not.  So we introduce our own constants, based on the glibc ones.]]
#define VKI_SEEK_SET              0
#define VKI_SEEK_CUR              1
#define VKI_SEEK_END              2

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/net.h
//----------------------------------------------------------------------

#define VKI_SYS_SOCKET		1	/* sys_socket(2)		*/
#define VKI_SYS_BIND		2	/* sys_bind(2)			*/
#define VKI_SYS_CONNECT		3	/* sys_connect(2)		*/
#define VKI_SYS_LISTEN		4	/* sys_listen(2)		*/
#define VKI_SYS_ACCEPT		5	/* sys_accept(2)		*/
#define VKI_SYS_GETSOCKNAME	6	/* sys_getsockname(2)		*/
#define VKI_SYS_GETPEERNAME	7	/* sys_getpeername(2)		*/
#define VKI_SYS_SOCKETPAIR	8	/* sys_socketpair(2)		*/
#define VKI_SYS_SEND		9	/* sys_send(2)			*/
#define VKI_SYS_RECV		10	/* sys_recv(2)			*/
#define VKI_SYS_SENDTO		11	/* sys_sendto(2)		*/
#define VKI_SYS_RECVFROM	12	/* sys_recvfrom(2)		*/
#define VKI_SYS_SHUTDOWN	13	/* sys_shutdown(2)		*/
#define VKI_SYS_SETSOCKOPT	14	/* sys_setsockopt(2)		*/
#define VKI_SYS_GETSOCKOPT	15	/* sys_getsockopt(2)		*/
#define VKI_SYS_SENDMSG		16	/* sys_sendmsg(2)		*/
#define VKI_SYS_RECVMSG		17	/* sys_recvmsg(2)		*/
#define VKI_SYS_ACCEPT4		18	/* sys_accept4(2)		*/

enum vki_sock_type {
	VKI_SOCK_STREAM	= 1,
	// [[others omitted]]
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/uio.h
//----------------------------------------------------------------------

struct vki_iovec
{
	void __user *iov_base;	/* BSD uses caddr_t (1003.1g requires void *) */
	__vki_kernel_size_t iov_len; /* Must be size_t (1003.1g) */
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/socket.h
//----------------------------------------------------------------------

// [[Resolved arbitrarily;  doesn't really matter whether it's '__inline__'
//   or 'inline']]
#define __KINLINE static __inline__

typedef unsigned short	vki_sa_family_t;

struct vki_sockaddr {
	vki_sa_family_t	sa_family;	/* address family, AF_xxx	*/
	char		sa_data[14];	/* 14 bytes of protocol address	*/
};

struct vki_msghdr {
	void	*	msg_name;	/* Socket name			*/
	int		msg_namelen;	/* Length of name		*/
	struct vki_iovec *	msg_iov;	/* Data blocks			*/
	__vki_kernel_size_t	msg_iovlen;	/* Number of blocks		*/
	void 	*	msg_control;	/* Per protocol magic (eg BSD file descriptor passing) */
	__vki_kernel_size_t	msg_controllen;	/* Length of cmsg list */
	unsigned	msg_flags;
};

struct vki_cmsghdr {
	__vki_kernel_size_t	cmsg_len;	/* data byte count, including hdr */
        int		cmsg_level;	/* originating protocol */
        int		cmsg_type;	/* protocol-specific type */
};

#define __VKI_CMSG_NXTHDR(ctl, len, cmsg) __vki_cmsg_nxthdr((ctl),(len),(cmsg))
#define VKI_CMSG_NXTHDR(mhdr, cmsg) vki_cmsg_nxthdr((mhdr), (cmsg))

#define VKI_CMSG_ALIGN(len) ( ((len)+sizeof(long)-1) & ~(sizeof(long)-1) )

#define VKI_CMSG_DATA(cmsg)	((void *)((char *)(cmsg) + VKI_CMSG_ALIGN(sizeof(struct vki_cmsghdr))))

#define __VKI_CMSG_FIRSTHDR(ctl,len) ((len) >= sizeof(struct vki_cmsghdr) ? \
				  (struct vki_cmsghdr *)(ctl) : \
				  (struct vki_cmsghdr *)NULL)
#define VKI_CMSG_FIRSTHDR(msg)	__VKI_CMSG_FIRSTHDR((msg)->msg_control, (msg)->msg_controllen)

// [[Urgh, this is revolting...]
__KINLINE struct vki_cmsghdr * __vki_cmsg_nxthdr(void *__ctl, __vki_kernel_size_t __size,
					       struct vki_cmsghdr *__cmsg)
{
	struct vki_cmsghdr * __ptr;

	__ptr = (struct vki_cmsghdr*)(((unsigned char *) __cmsg) +  VKI_CMSG_ALIGN(__cmsg->cmsg_len));
	if ((unsigned long)((char*)(__ptr+1) - (char *) __ctl) > __size)
		return (struct vki_cmsghdr *)0;

	return __ptr;
}

__KINLINE struct vki_cmsghdr * vki_cmsg_nxthdr (struct vki_msghdr *__msg, struct vki_cmsghdr *__cmsg)
{
	return __vki_cmsg_nxthdr(__msg->msg_control, __msg->msg_controllen, __cmsg);
}

#define	VKI_SCM_RIGHTS	0x01		/* rw: access rights (array of int) */

#define VKI_AF_UNIX	1	/* Unix domain sockets 		*/
#define VKI_AF_INET	2	/* Internet IP Protocol		*/
#define VKI_AF_INET6	10	/* IP version 6			*/

#define VKI_MSG_NOSIGNAL	0x4000	/* Do not generate SIGPIPE */

#define VKI_SOL_SCTP	132

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/in.h
//----------------------------------------------------------------------

struct vki_in_addr {
	__vki_u32	s_addr;
};

/* Structure describing an Internet (IP) socket address. */
#define __VKI_SOCK_SIZE__	16	/* sizeof(struct sockaddr)	*/
struct vki_sockaddr_in {
  vki_sa_family_t	sin_family;	/* Address family		*/
  unsigned short int	sin_port;	/* Port number			*/
  struct vki_in_addr	sin_addr;	/* Internet address		*/

  /* Pad to size of `struct sockaddr'. */
  unsigned char		__pad[__VKI_SOCK_SIZE__ - sizeof(short int) -
			sizeof(unsigned short int) - sizeof(struct vki_in_addr)];
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/in6.h
//----------------------------------------------------------------------

struct vki_in6_addr
{
	union 
	{
		__vki_u8	u6_addr8[16];
		__vki_u16	u6_addr16[8];
		__vki_u32	u6_addr32[4];
	} vki_in6_u;
#define vki_s6_addr		vki_in6_u.u6_addr8
#define vki_s6_addr16		vki_in6_u.u6_addr16
#define vki_s6_addr32		vki_in6_u.u6_addr32
};

struct vki_sockaddr_in6 {
	unsigned short int	sin6_family;    /* AF_INET6 */
	__vki_u16		sin6_port;      /* Transport layer port # */
	__vki_u32		sin6_flowinfo;  /* IPv6 flow information */
	struct vki_in6_addr	sin6_addr;      /* IPv6 address */
	__vki_u32		sin6_scope_id;  /* scope id (new in RFC2553) */
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/un.h
//----------------------------------------------------------------------

#define VKI_UNIX_PATH_MAX	108

struct vki_sockaddr_un {
	vki_sa_family_t sun_family;	/* AF_UNIX */
	char sun_path[VKI_UNIX_PATH_MAX];	/* pathname */
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/if.h
//----------------------------------------------------------------------

#define	VKI_IFNAMSIZ	16

struct vki_ifmap 
{
	unsigned long mem_start;
	unsigned long mem_end;
	unsigned short base_addr; 
	unsigned char irq;
	unsigned char dma;
	unsigned char port;
	/* 3 bytes spare */
};

struct vki_if_settings
{
	unsigned int type;	/* Type of physical device or protocol */
	unsigned int size;	/* Size of the data allocated by the caller */
	union {
                // [[Nb: converted these all to void* to avoid pulling in
                //   unnecessary headers]]]
		/* {atm/eth/dsl}_settings anyone ? */
		void /*raw_hdlc_proto		*/__user *raw_hdlc;
		void /*cisco_proto		*/__user *cisco;
		void /*fr_proto			*/__user *fr;
		void /*fr_proto_pvc		*/__user *fr_pvc;
		void /*fr_proto_pvc_info	*/__user *fr_pvc_info;

		/* interface settings */
		void /*sync_serial_settings	*/__user *sync;
		void /*te1_settings		*/__user *te1;
	} ifs_ifsu;
};

struct vki_ifreq 
{
#define VKI_IFHWADDRLEN	6
	union
	{
		char	ifrn_name[VKI_IFNAMSIZ];		/* if name, e.g. "en0" */
	} ifr_ifrn;
	
	union {
		struct	vki_sockaddr ifru_addr;
		struct	vki_sockaddr ifru_dstaddr;
		struct	vki_sockaddr ifru_broadaddr;
		struct	vki_sockaddr ifru_netmask;
		struct  vki_sockaddr ifru_hwaddr;
		short	ifru_flags;
		int	ifru_ivalue;
		int	ifru_mtu;
		struct  vki_ifmap ifru_map;
		char	ifru_slave[VKI_IFNAMSIZ];	/* Just fits the size */
		char	ifru_newname[VKI_IFNAMSIZ];
		void __user *	ifru_data;
		struct	vki_if_settings ifru_settings;
	} ifr_ifru;
};

#define vki_ifr_name	ifr_ifrn.ifrn_name	/* interface name 	*/
#define ifr_hwaddr	ifr_ifru.ifru_hwaddr	/* MAC address 		*/
#define	ifr_addr	ifr_ifru.ifru_addr	/* address		*/
#define	ifr_dstaddr	ifr_ifru.ifru_dstaddr	/* other end of p-p lnk	*/
#define	ifr_broadaddr	ifr_ifru.ifru_broadaddr	/* broadcast address	*/
#define	ifr_netmask	ifr_ifru.ifru_netmask	/* interface net mask	*/
#define	vki_ifr_flags	ifr_ifru.ifru_flags	/* flags		*/
#define	vki_ifr_metric	ifr_ifru.ifru_ivalue	/* metric		*/
#define	vki_ifr_mtu		ifr_ifru.ifru_mtu	/* mtu			*/
#define ifr_map		ifr_ifru.ifru_map	/* device map		*/
#define ifr_slave	ifr_ifru.ifru_slave	/* slave device		*/
#define	vki_ifr_data	ifr_ifru.ifru_data	/* for use by interface	*/
#define vki_ifr_ifindex	ifr_ifru.ifru_ivalue	/* interface index	*/
#define ifr_bandwidth	ifr_ifru.ifru_ivalue    /* link bandwidth	*/
#define ifr_qlen	ifr_ifru.ifru_ivalue	/* Queue length 	*/
#define ifr_newname	ifr_ifru.ifru_newname	/* New name		*/
#define ifr_settings	ifr_ifru.ifru_settings	/* Device/proto settings*/

struct vki_ifconf 
{
	int	ifc_len;			/* size of buffer	*/
	union 
	{
		char __user *ifcu_buf;
		struct vki_ifreq __user *ifcu_req;
	} ifc_ifcu;
};
#define	vki_ifc_buf	ifc_ifcu.ifcu_buf	/* buffer address	*/

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/if_arp.h
//----------------------------------------------------------------------

struct vki_arpreq {
  struct vki_sockaddr	arp_pa;		/* protocol address		*/
  struct vki_sockaddr	arp_ha;		/* hardware address		*/
  int			arp_flags;	/* flags			*/
  struct vki_sockaddr   arp_netmask;    /* netmask (only for proxy arps) */
  char			arp_dev[16];
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/route.h
//----------------------------------------------------------------------

struct vki_rtentry 
{
	unsigned long	rt_pad1;
	struct vki_sockaddr	rt_dst;		/* target address		*/
	struct vki_sockaddr	rt_gateway;	/* gateway addr (RTF_GATEWAY)	*/
	struct vki_sockaddr	rt_genmask;	/* target network mask (IP)	*/
	unsigned short	rt_flags;
	short		rt_pad2;
	unsigned long	rt_pad3;
	void		*rt_pad4;
	short		rt_metric;	/* +1 for binary compatibility!	*/
	char __user	*rt_dev;	/* forcing the device at add	*/
	unsigned long	rt_mtu;		/* per route MTU/Window 	*/
// [[Not important for Valgrind]]
//#ifndef __KERNEL__
//#define rt_mss	rt_mtu		/* Compatibility :-(            */
//#endif
	unsigned long	rt_window;	/* Window clamping 		*/
	unsigned short	rt_irtt;	/* Initial RTT			*/
};

//----------------------------------------------------------------------
// From linux-2.6.13-rc5/include/net/sctp/user.h
//----------------------------------------------------------------------

typedef __vki_s32 vki_sctp_assoc_t;

enum vki_sctp_optname {
	VKI_SCTP_RTOINFO,
#define VKI_SCTP_RTOINFO VKI_SCTP_RTOINFO
	VKI_SCTP_ASSOCINFO,
#define VKI_SCTP_ASSOCINFO VKI_SCTP_ASSOCINFO
	VKI_SCTP_INITMSG,
#define VKI_SCTP_INITMSG VKI_SCTP_INITMSG
	VKI_SCTP_NODELAY, 	/* Get/set nodelay option. */
#define VKI_SCTP_NODELAY	VKI_SCTP_NODELAY
	VKI_SCTP_AUTOCLOSE,
#define VKI_SCTP_AUTOCLOSE VKI_SCTP_AUTOCLOSE
	VKI_SCTP_SET_PEER_PRIMARY_ADDR, 
#define VKI_SCTP_SET_PEER_PRIMARY_ADDR VKI_SCTP_SET_PEER_PRIMARY_ADDR
	VKI_SCTP_PRIMARY_ADDR,
#define VKI_SCTP_PRIMARY_ADDR VKI_SCTP_PRIMARY_ADDR
	VKI_SCTP_ADAPTION_LAYER,      
#define VKI_SCTP_ADAPTION_LAYER VKI_SCTP_ADAPTION_LAYER
	VKI_SCTP_DISABLE_FRAGMENTS,
#define VKI_SCTP_DISABLE_FRAGMENTS VKI_SCTP_DISABLE_FRAGMENTS
	VKI_SCTP_PEER_ADDR_PARAMS,
#define VKI_SCTP_PEER_ADDR_PARAMS VKI_SCTP_PEER_ADDR_PARAMS
	VKI_SCTP_DEFAULT_SEND_PARAM,
#define VKI_SCTP_DEFAULT_SEND_PARAM VKI_SCTP_DEFAULT_SEND_PARAM
	VKI_SCTP_EVENTS,
#define VKI_SCTP_EVENTS VKI_SCTP_EVENTS
	VKI_SCTP_I_WANT_MAPPED_V4_ADDR,  /* Turn on/off mapped v4 addresses  */
#define VKI_SCTP_I_WANT_MAPPED_V4_ADDR VKI_SCTP_I_WANT_MAPPED_V4_ADDR
	VKI_SCTP_MAXSEG, 	/* Get/set maximum fragment. */
#define VKI_SCTP_MAXSEG 	VKI_SCTP_MAXSEG
	VKI_SCTP_STATUS,
#define VKI_SCTP_STATUS VKI_SCTP_STATUS
	VKI_SCTP_GET_PEER_ADDR_INFO,
#define VKI_SCTP_GET_PEER_ADDR_INFO VKI_SCTP_GET_PEER_ADDR_INFO

	/* Internal Socket Options. Some of the sctp library functions are 
	 * implemented using these socket options.
	 */
	VKI_SCTP_SOCKOPT_BINDX_ADD = 100,/* BINDX requests for adding addresses. */
#define VKI_SCTP_SOCKOPT_BINDX_ADD	VKI_SCTP_SOCKOPT_BINDX_ADD
	VKI_SCTP_SOCKOPT_BINDX_REM, /* BINDX requests for removing addresses. */
#define VKI_SCTP_SOCKOPT_BINDX_REM	VKI_SCTP_SOCKOPT_BINDX_REM
	VKI_SCTP_SOCKOPT_PEELOFF, 	/* peel off association. */
#define VKI_SCTP_SOCKOPT_PEELOFF	VKI_SCTP_SOCKOPT_PEELOFF
	VKI_SCTP_GET_PEER_ADDRS_NUM, 	/* Get number of peer addresss. */
#define VKI_SCTP_GET_PEER_ADDRS_NUM	VKI_SCTP_GET_PEER_ADDRS_NUM
	VKI_SCTP_GET_PEER_ADDRS, 	/* Get all peer addresss. */
#define VKI_SCTP_GET_PEER_ADDRS	VKI_SCTP_GET_PEER_ADDRS
	VKI_SCTP_GET_LOCAL_ADDRS_NUM, 	/* Get number of local addresss. */
#define VKI_SCTP_GET_LOCAL_ADDRS_NUM	VKI_SCTP_GET_LOCAL_ADDRS_NUM
	VKI_SCTP_GET_LOCAL_ADDRS, 	/* Get all local addresss. */
#define VKI_SCTP_GET_LOCAL_ADDRS	VKI_SCTP_GET_LOCAL_ADDRS
	VKI_SCTP_SOCKOPT_CONNECTX, /* CONNECTX requests. */
#define VKI_SCTP_SOCKOPT_CONNECTX	VKI_SCTP_SOCKOPT_CONNECTX
};

struct vki_sctp_getaddrs {
	vki_sctp_assoc_t        assoc_id;
	int			addr_num;
	struct vki_sockaddr	*addrs;
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/resource.h
//----------------------------------------------------------------------

struct	vki_rusage {
	struct vki_timeval ru_utime;	/* user time used */
	struct vki_timeval ru_stime;	/* system time used */
	long	ru_maxrss;		/* maximum resident set size */
	long	ru_ixrss;		/* integral shared memory size */
	long	ru_idrss;		/* integral unshared data size */
	long	ru_isrss;		/* integral unshared stack size */
	long	ru_minflt;		/* page reclaims */
	long	ru_majflt;		/* page faults */
	long	ru_nswap;		/* swaps */
	long	ru_inblock;		/* block input operations */
	long	ru_oublock;		/* block output operations */
	long	ru_msgsnd;		/* messages sent */
	long	ru_msgrcv;		/* messages received */
	long	ru_nsignals;		/* signals received */
	long	ru_nvcsw;		/* voluntary context switches */
	long	ru_nivcsw;		/* involuntary " */
};

struct vki_rlimit {
	unsigned long	rlim_cur;
	unsigned long	rlim_max;
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/elfcore.h
//----------------------------------------------------------------------

struct vki_elf_siginfo
{
	int	si_signo;			/* signal number */
	int	si_code;			/* extra code */
	int	si_errno;			/* errno */
};

// [[Removed some commented out lines here]]
struct vki_elf_prstatus
{
	struct vki_elf_siginfo pr_info;	/* Info associated with signal */
	short	pr_cursig;		/* Current signal */
	unsigned long pr_sigpend;	/* Set of pending signals */
	unsigned long pr_sighold;	/* Set of held signals */
	vki_pid_t	pr_pid;
	vki_pid_t	pr_ppid;
	vki_pid_t	pr_pgrp;
	vki_pid_t	pr_sid;
	struct vki_timeval pr_utime;	/* User time */
	struct vki_timeval pr_stime;	/* System time */
	struct vki_timeval pr_cutime;	/* Cumulative user time */
	struct vki_timeval pr_cstime;	/* Cumulative system time */
	vki_elf_gregset_t pr_reg;	/* GP registers */
	int pr_fpvalid;		/* True if math co-processor being used.  */
};

#define VKI_ELF_PRARGSZ	(80)	/* Number of chars for args */

struct vki_elf_prpsinfo
{
	char	pr_state;	/* numeric process state */
	char	pr_sname;	/* char for pr_state */
	char	pr_zomb;	/* zombie */
	char	pr_nice;	/* nice val */
	unsigned long pr_flag;	/* flags */
	__vki_kernel_uid_t	pr_uid;
	__vki_kernel_gid_t	pr_gid;
	vki_pid_t	pr_pid, pr_ppid, pr_pgrp, pr_sid;
	/* Lots missing */
	char	pr_fname[16];	/* filename of executable */
	char	pr_psargs[VKI_ELF_PRARGSZ];	/* initial part of arg list */
};

//----------------------------------------------------------------------
// From linux-2.6.12.1/include/linux/eventpoll.h
//----------------------------------------------------------------------

/* Valid opcodes to issue to sys_epoll_ctl() */
#define VKI_EPOLL_CTL_ADD 1
#define VKI_EPOLL_CTL_DEL 2
#define VKI_EPOLL_CTL_MOD 3

#ifdef __x86_64__
#define VKI_EPOLL_PACKED __attribute__((packed))
#else
#define VKI_EPOLL_PACKED
#endif

struct vki_epoll_event {
	__vki_u32 events;
	__vki_u64 data;
} VKI_EPOLL_PACKED;


//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/mqueue.h
//----------------------------------------------------------------------

struct vki_mq_attr {
	long	mq_flags;	/* message queue flags			*/
	long	mq_maxmsg;	/* maximum number of messages		*/
	long	mq_msgsize;	/* maximum message size			*/
	long	mq_curmsgs;	/* number of messages currently queued	*/
	long	__reserved[4];	/* ignored for input, zeroed for output */
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/utsname.h
//----------------------------------------------------------------------

struct vki_new_utsname {
	char sysname[65];
	char nodename[65];
	char release[65];
	char version[65];
	char machine[65];
	char domainname[65];
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/mii.h
//----------------------------------------------------------------------

/* This structure is used in all SIOCxMIIxxx ioctl calls */
struct vki_mii_ioctl_data {
	vki_u16		phy_id;
	vki_u16		reg_num;
	vki_u16		val_in;
	vki_u16		val_out;
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/capability.h
//----------------------------------------------------------------------

// [[capget()/capset() man page says this, ominously:
//
//   The kernel API is likely to change and use of these functions  (in
//   particular the format of the cap_user_*_t types) is subject to
//   change with each kernel revision.
//
// However, the format hasn't changed since at least Linux 2.4.6.]]

typedef struct __vki_user_cap_header_struct {
	__vki_u32 version;
	int pid;
} __user *vki_cap_user_header_t;
 
typedef struct __vki_user_cap_data_struct {
        __vki_u32 effective;
        __vki_u32 permitted;
        __vki_u32 inheritable;
} __user *vki_cap_user_data_t;
  

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/module.h
//----------------------------------------------------------------------

// [[We do a VKI_SIZEOF_* here because this type is so big, and its size
//   depends on the word size, so see vki_arch.h]]

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/ipc.h
//----------------------------------------------------------------------

/* Obsolete, used only for backwards compatibility and libc5 compiles */
struct vki_ipc_perm
{
	__vki_kernel_key_t	key;
	__vki_kernel_uid_t	uid;
	__vki_kernel_gid_t	gid;
	__vki_kernel_uid_t	cuid;
	__vki_kernel_gid_t	cgid;
	__vki_kernel_mode_t	mode; 
	unsigned short	seq;
};

#define VKI_IPC_CREAT  00001000   /* create if key is nonexistent */
#define VKI_IPC_EXCL   00002000   /* fail if key exists */
#define VKI_IPC_NOWAIT 00004000   /* return error on wait */

//#define VKI_IPC_RMID 0     /* remove resource */
#define VKI_IPC_SET  1     /* set ipc_perm options */
#define VKI_IPC_STAT 2     /* get ipc_perm options */
#define VKI_IPC_INFO 3     /* see ipcs */

#define VKI_IPC_64  0x0100  /* New version (support 32-bit UIDs, bigger
			       message sizes, etc. */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/sem.h
//----------------------------------------------------------------------

#define VKI_GETALL  13       /* get all semval's */
#define VKI_SETVAL  16       /* set semval */
#define VKI_SETALL  17       /* set all semval's */

#define VKI_SEM_STAT 18
#define VKI_SEM_INFO 19

/* Obsolete, used only for backwards compatibility and libc5 compiles */
struct vki_semid_ds {
	struct vki_ipc_perm	sem_perm;		/* permissions .. see ipc.h */
	__vki_kernel_time_t	sem_otime;		/* last semop time */
	__vki_kernel_time_t	sem_ctime;		/* last change time */
        // [[Use void* to avoid excess header copying]]
	void/*struct sem	*/*sem_base;		/* ptr to first semaphore in array */
	void/*struct sem_queue */*sem_pending;		/* pending operations to be processed */
	void/*struct sem_queue */**sem_pending_last;	/* last pending operation */
	void/*struct sem_undo	*/*undo;			/* undo requests on this array */
	unsigned short	sem_nsems;		/* no. of semaphores in array */
};

struct vki_sembuf {
	unsigned short  sem_num;	/* semaphore index in array */
	short		sem_op;		/* semaphore operation */
	short		sem_flg;	/* operation flags */
};

union vki_semun {
	int val;			/* value for SETVAL */
	struct vki_semid_ds __user *buf;	/* buffer for IPC_STAT & IPC_SET */
	unsigned short __user *array;	/* array for GETALL & SETALL */
	struct vki_seminfo __user *__buf;	/* buffer for IPC_INFO */
	void __user *__pad;
};

struct  vki_seminfo {
	int semmap;
	int semmni;
	int semmns;
	int semmnu;
	int semmsl;
	int semopm;
	int semume;
	int semusz;
	int semvmx;
	int semaem;
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/asm-generic/errno-base.h
//----------------------------------------------------------------------

#define	VKI_EPERM		 1	/* Operation not permitted */
#define	VKI_ENOENT		 2	/* No such file or directory */
#define	VKI_ESRCH		 3	/* No such process */
#define	VKI_EINTR		 4	/* Interrupted system call */
#define VKI_ENOEXEC              8      /* Exec format error */
#define	VKI_EBADF		 9	/* Bad file number */
#define VKI_ECHILD              10      /* No child processes */
#define VKI_EAGAIN		11	/* Try again */
#define VKI_EWOULDBLOCK		VKI_EAGAIN
#define	VKI_ENOMEM		12	/* Out of memory */
#define	VKI_EACCES		13	/* Permission denied */
#define	VKI_EFAULT		14	/* Bad address */
#define	VKI_EEXIST		17	/* File exists */
#define	VKI_EINVAL		22	/* Invalid argument */
#define	VKI_EMFILE		24	/* Too many open files */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/asm-generic/errno.h
//----------------------------------------------------------------------

#define	VKI_ENOSYS		38	/* Function not implemented */
#define	VKI_EOVERFLOW		75	/* Value too large for defined data type */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/wait.h
//----------------------------------------------------------------------

#define VKI_WNOHANG	0x00000001

#define __VKI_WALL	0x40000000	/* Wait on all children, regardless of type */
#define __VKI_WCLONE	0x80000000	/* Wait only on non-SIGCHLD children */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/mman.h
//----------------------------------------------------------------------

#define VKI_MREMAP_MAYMOVE	1
#define VKI_MREMAP_FIXED	2

//----------------------------------------------------------------------
// From linux-2.6.31-rc4/include/linux/futex.h
//----------------------------------------------------------------------

#define VKI_FUTEX_WAIT (0)
#define VKI_FUTEX_WAKE (1)
#define VKI_FUTEX_FD (2)
#define VKI_FUTEX_REQUEUE (3)
#define VKI_FUTEX_CMP_REQUEUE (4)
#define VKI_FUTEX_WAKE_OP (5)
#define VKI_FUTEX_LOCK_PI (6)
#define VKI_FUTEX_UNLOCK_PI (7)
#define VKI_FUTEX_TRYLOCK_PI (8)
#define VKI_FUTEX_WAIT_BITSET (9)
#define VKI_FUTEX_WAKE_BITSET (10)
#define VKI_FUTEX_WAIT_REQUEUE_PI (11)
#define VKI_FUTEX_CMP_REQUEUE_PI (12)
#define VKI_FUTEX_PRIVATE_FLAG (128)
#define VKI_FUTEX_CLOCK_REALTIME (256)

struct vki_robust_list {
	struct vki_robust_list __user *next;
};

struct vki_robust_list_head {
	/*
	 * The head of the list. Points back to itself if empty:
	 */
	struct vki_robust_list list;

	/*
	 * This relative offset is set by user-space, it gives the kernel
	 * the relative position of the futex field to examine. This way
	 * we keep userspace flexible, to freely shape its data-structure,
	 * without hardcoding any particular offset into the kernel:
	 */
	long futex_offset;

	/*
	 * The death of the thread may race with userspace setting
	 * up a lock's links. So to handle this race, userspace first
	 * sets this field to the address of the to-be-taken lock,
	 * then does the lock acquire, and then adds itself to the
	 * list, and then clears this field. Hence the kernel will
	 * always have full knowledge of all locks that the thread
	 * _might_ have taken. We check the owner TID in any case,
	 * so only truly owned locks will be handled.
	 */
	struct vki_robust_list __user *list_op_pending;
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/errno.h
//----------------------------------------------------------------------

#define VKI_ERESTARTSYS	512

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/stat.h
//----------------------------------------------------------------------

#define VKI_S_IFMT  00170000
#define VKI_S_IFSOCK 0140000
#define VKI_S_IFLNK  0120000
#define VKI_S_IFREG  0100000
#define VKI_S_IFBLK  0060000
#define VKI_S_IFDIR  0040000
#define VKI_S_IFCHR  0020000
#define VKI_S_IFIFO  0010000
#define VKI_S_ISUID  0004000
#define VKI_S_ISGID  0002000
#define VKI_S_ISVTX  0001000

#define VKI_S_ISLNK(m)	(((m) & VKI_S_IFMT) == VKI_S_IFLNK)
#define VKI_S_ISREG(m)	(((m) & VKI_S_IFMT) == VKI_S_IFREG)
#define VKI_S_ISDIR(m)	(((m) & VKI_S_IFMT) == VKI_S_IFDIR)
#define VKI_S_ISCHR(m)	(((m) & VKI_S_IFMT) == VKI_S_IFCHR)
#define VKI_S_ISBLK(m)	(((m) & VKI_S_IFMT) == VKI_S_IFBLK)
#define VKI_S_ISFIFO(m)	(((m) & VKI_S_IFMT) == VKI_S_IFIFO)
#define VKI_S_ISSOCK(m)	(((m) & VKI_S_IFMT) == VKI_S_IFSOCK)

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

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/dirent.h
//----------------------------------------------------------------------

struct vki_dirent {
	long		d_ino;
	__vki_kernel_off_t	d_off;
	unsigned short	d_reclen;
	char		d_name[256]; /* We must not include limits.h! */
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/fcntl.h
//----------------------------------------------------------------------

#define VKI_F_SETLEASE      (VKI_F_LINUX_SPECIFIC_BASE + 0)
#define VKI_F_GETLEASE      (VKI_F_LINUX_SPECIFIC_BASE + 1)

#define VKI_F_CANCELLK      (VKI_F_LINUX_SPECIFIC_BASE + 5)

#define VKI_F_DUPFD_CLOEXEC (VKI_F_LINUX_SPECIFIC_BASE + 6)

#define VKI_F_NOTIFY        (VKI_F_LINUX_SPECIFIC_BASE + 2)

#define VKI_F_SETPIPE_SZ    (VKI_F_LINUX_SPECIFIC_BASE + 7)
#define VKI_F_GETPIPE_SZ    (VKI_F_LINUX_SPECIFIC_BASE + 8)

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/sysctl.h
//----------------------------------------------------------------------

struct __vki_sysctl_args {
	int __user *name;
	int nlen;
	void __user *oldval;
	vki_size_t __user *oldlenp;
	void __user *newval;
	vki_size_t newlen;
	unsigned long __unused[4];
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/aio_abi.h
//----------------------------------------------------------------------

typedef unsigned long	vki_aio_context_t;

enum {
	VKI_IOCB_CMD_PREAD = 0,
	VKI_IOCB_CMD_PWRITE = 1,
	VKI_IOCB_CMD_FSYNC = 2,
	VKI_IOCB_CMD_FDSYNC = 3,
	VKI_IOCB_CMD_PREADV = 7,
	VKI_IOCB_CMD_PWRITEV = 8,
};

/* read() from /dev/aio returns these structures. */
struct vki_io_event {
	__vki_u64	data;		/* the data field from the iocb */
	__vki_u64	obj;		/* what iocb this event came from */
        // [[Nb: These fields renamed from 'res' and 'res2' because 'res' is
        //   a macro in vg_syscalls.c!]]
	__vki_s64	result;		/* result code for this event */
	__vki_s64	result2;	/* secondary result */
};

#if defined(VKI_LITTLE_ENDIAN)
#  define VKI_PADDED(x,y)	x, y
#elif defined(VKI_BIG_ENDIAN)
#  define VKI_PADDED(x,y)	y, x
#else
#error edit for your odd byteorder.
#endif

struct vki_iocb {
	/* these are internal to the kernel/libc. */
	__vki_u64	aio_data;	/* data to be returned in event's data */
	__vki_u32	VKI_PADDED(aio_key, aio_reserved1);
				/* the kernel sets aio_key to the req # */

	/* common fields */
	__vki_u16	aio_lio_opcode;	/* see IOCB_CMD_ above */
	__vki_s16	aio_reqprio;
	__vki_u32	aio_fildes;

	__vki_u64	aio_buf;
	__vki_u64	aio_nbytes;
	__vki_s64	aio_offset;

	/* extra parameters */
	__vki_u64	aio_reserved2;	/* TODO: use this for a (struct sigevent *) */
	__vki_u64	aio_reserved3;
}; /* 64 bytes */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/aio.h
//----------------------------------------------------------------------

struct vki_aio_ring {
	unsigned	id;	/* kernel internal index number */
	unsigned	nr;	/* number of io_events */
	unsigned	head;
	unsigned	tail;

	unsigned	magic;
	unsigned	compat_features;
	unsigned	incompat_features;
	unsigned	header_length;	/* size of aio_ring */

	struct vki_io_event		io_events[0];
}; /* 128 bytes + ring size */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/msg.h
//----------------------------------------------------------------------

#define VKI_MSG_STAT 11
#define VKI_MSG_INFO 12

struct vki_msqid_ds {
	struct vki_ipc_perm msg_perm;
	struct vki_msg *msg_first;		/* first message on queue,unused  */
	struct vki_msg *msg_last;		/* last message in queue,unused */
	__vki_kernel_time_t msg_stime;	/* last msgsnd time */
	__vki_kernel_time_t msg_rtime;	/* last msgrcv time */
	__vki_kernel_time_t msg_ctime;	/* last change time */
	unsigned long  msg_lcbytes;	/* Reuse junk fields for 32 bit */
	unsigned long  msg_lqbytes;	/* ditto */
	unsigned short msg_cbytes;	/* current number of bytes on queue */
	unsigned short msg_qnum;	/* number of messages in queue */
	unsigned short msg_qbytes;	/* max number of bytes on queue */
	__vki_kernel_ipc_pid_t msg_lspid;	/* pid of last msgsnd */
	__vki_kernel_ipc_pid_t msg_lrpid;	/* last receive pid */
};

struct vki_msgbuf {
	long mtype;         /* type of message */
	char mtext[1];      /* message text */
};

struct vki_msginfo {
	int msgpool;
	int msgmap; 
	int msgmax; 
	int msgmnb; 
	int msgmni; 
	int msgssz; 
	int msgtql; 
	unsigned short  msgseg; 
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/shm.h
//----------------------------------------------------------------------

struct vki_shmid_ds {
	struct vki_ipc_perm		shm_perm;	/* operation perms */
	int			shm_segsz;	/* size of segment (bytes) */
	__vki_kernel_time_t		shm_atime;	/* last attach time */
	__vki_kernel_time_t		shm_dtime;	/* last detach time */
	__vki_kernel_time_t		shm_ctime;	/* last change time */
	__vki_kernel_ipc_pid_t	shm_cpid;	/* pid of creator */
	__vki_kernel_ipc_pid_t	shm_lpid;	/* pid of last operator */
	unsigned short		shm_nattch;	/* no. of current attaches */
	unsigned short 		shm_unused;	/* compatibility */
	void 			*shm_unused2;	/* ditto - used by DIPC */
	void			*shm_unused3;	/* unused */
};

#define VKI_SHM_RDONLY  010000  /* read-only access */
#define VKI_SHM_RND     020000  /* round attach address to SHMLBA boundary */

#define VKI_SHM_STAT 	13
#define VKI_SHM_INFO 	14

/* Obsolete, used only for backwards compatibility */
struct	vki_shminfo {
	int shmmax;
	int shmmin;
	int shmmni;
	int shmseg;
	int shmall;
};

struct vki_shm_info {
	int used_ids;
	unsigned long shm_tot;	/* total allocated shm */
	unsigned long shm_rss;	/* total resident shm */
	unsigned long shm_swp;	/* total swapped shm */
	unsigned long swap_attempts;
	unsigned long swap_successes;
};

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/rtc.h
//----------------------------------------------------------------------

struct vki_rtc_time {
	int tm_sec;
	int tm_min;
	int tm_hour;
	int tm_mday;
	int tm_mon;
	int tm_year;
	int tm_wday;
	int tm_yday;
	int tm_isdst;
};

#define VKI_RTC_AIE_ON	_VKI_IO('p', 0x01)	/* Alarm int. enable on	*/
#define VKI_RTC_AIE_OFF	_VKI_IO('p', 0x02)	/* ... off		*/
#define VKI_RTC_UIE_ON	_VKI_IO('p', 0x03)	/* Update int. enable on*/
#define VKI_RTC_UIE_OFF	_VKI_IO('p', 0x04)	/* ... off		*/
#define VKI_RTC_PIE_ON	_VKI_IO('p', 0x05)	/* Periodic int. enable on*/
#define VKI_RTC_PIE_OFF	_VKI_IO('p', 0x06)	/* ... off		*/

#define VKI_RTC_ALM_SET		_VKI_IOW('p', 0x07, struct vki_rtc_time) /* Set alarm time  */
#define VKI_RTC_ALM_READ	_VKI_IOR('p', 0x08, struct vki_rtc_time) /* Read alarm time */
#define VKI_RTC_RD_TIME		_VKI_IOR('p', 0x09, struct vki_rtc_time) /* Read RTC time   */
//#define RTC_SET_TIME	_IOW('p', 0x0a, struct rtc_time) /* Set RTC time    */
#define VKI_RTC_IRQP_READ	_VKI_IOR('p', 0x0b, unsigned long)	 /* Read IRQ rate   */
#define VKI_RTC_IRQP_SET	_VKI_IOW('p', 0x0c, unsigned long)	 /* Set IRQ rate    */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/isdn.h
//----------------------------------------------------------------------

// [[Nb: Resolved this for the common case where CONFIG_COBALT_MICRO_SERVER
//   is not defined]]
#define VKI_ISDN_MAX_CHANNELS   64

#define VKI_IIOCGETCPS  _VKI_IO('I',21)

#define VKI_IIOCNETGPN  _VKI_IO('I',34)

#define VKI_ISDN_MSNLEN          32

typedef struct {
  char name[10];
  char phone[VKI_ISDN_MSNLEN];
  int  outgoing;
} vki_isdn_net_ioctl_phone;

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/sockios.h
//----------------------------------------------------------------------

#define VKI_SIOCOUTQ		VKI_TIOCOUTQ

#define VKI_SIOCADDRT		0x890B	/* add routing table entry	*/
#define VKI_SIOCDELRT		0x890C	/* delete routing table entry	*/

#define VKI_SIOCGIFNAME		0x8910	/* get iface name		*/
#define VKI_SIOCGIFCONF		0x8912	/* get iface list		*/
#define VKI_SIOCGIFFLAGS	0x8913	/* get flags			*/
#define VKI_SIOCSIFFLAGS	0x8914	/* set flags			*/
#define VKI_SIOCGIFADDR		0x8915	/* get PA address		*/
#define VKI_SIOCSIFADDR		0x8916	/* set PA address		*/
#define VKI_SIOCGIFDSTADDR	0x8917	/* get remote PA address	*/
#define VKI_SIOCSIFDSTADDR	0x8918	/* set remote PA address	*/
#define VKI_SIOCGIFBRDADDR	0x8919	/* get broadcast PA address	*/
#define VKI_SIOCSIFBRDADDR	0x891a	/* set broadcast PA address	*/
#define VKI_SIOCGIFNETMASK	0x891b	/* get network PA mask		*/
#define VKI_SIOCSIFNETMASK	0x891c	/* set network PA mask		*/
#define VKI_SIOCGIFMETRIC	0x891d	/* get metric			*/
#define VKI_SIOCSIFMETRIC	0x891e	/* set metric			*/
#define VKI_SIOCGIFMTU		0x8921	/* get MTU size			*/
#define VKI_SIOCSIFMTU		0x8922	/* set MTU size			*/
#define	VKI_SIOCSIFHWADDR	0x8924	/* set hardware address 	*/
#define VKI_SIOCGIFHWADDR	0x8927	/* Get hardware address		*/
#define VKI_SIOCGIFINDEX	0x8933	/* name -> if_index mapping	*/

#define VKI_SIOCGIFTXQLEN	0x8942	/* Get the tx queue length	*/
#define VKI_SIOCSIFTXQLEN	0x8943	/* Set the tx queue length 	*/

#define VKI_SIOCGMIIPHY		0x8947	/* Get address of MII PHY in use. */
#define VKI_SIOCGMIIREG		0x8948	/* Read MII PHY register.	*/
#define VKI_SIOCSMIIREG		0x8949	/* Write MII PHY register.	*/

#define VKI_SIOCDARP		0x8953	/* delete ARP table entry	*/
#define VKI_SIOCGARP		0x8954	/* get ARP table entry		*/
#define VKI_SIOCSARP		0x8955	/* set ARP table entry		*/

#define VKI_SIOCDRARP		0x8960	/* delete RARP table entry	*/
#define VKI_SIOCGRARP		0x8961	/* get RARP table entry		*/
#define VKI_SIOCSRARP		0x8962	/* set RARP table entry		*/

#define VKI_SIOCGIFMAP		0x8970	/* Get device parameters	*/
#define VKI_SIOCSIFMAP		0x8971	/* Set device parameters	*/

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/ppdev.h
//----------------------------------------------------------------------

#define VKI_PP_MAJOR	99

#define VKI_PP_IOCTL	'p'

/* Set mode for read/write (e.g. IEEE1284_MODE_EPP) */
#define VKI_PPSETMODE	_VKI_IOW(VKI_PP_IOCTL, 0x80, int)

/* Read status */
#define VKI_PPRSTATUS	_VKI_IOR(VKI_PP_IOCTL, 0x81, unsigned char)
//#define PPWSTATUS	OBSOLETE__IOW(PP_IOCTL, 0x82, unsigned char)

/* Read/write control */
#define VKI_PPRCONTROL	_VKI_IOR(VKI_PP_IOCTL, 0x83, unsigned char)
#define VKI_PPWCONTROL	_VKI_IOW(VKI_PP_IOCTL, 0x84, unsigned char)

struct vki_ppdev_frob_struct {
	unsigned char mask;
	unsigned char val;
};
#define VKI_PPFCONTROL      _VKI_IOW(VKI_PP_IOCTL, 0x8e, struct vki_ppdev_frob_struct)

/* Read/write data */
#define VKI_PPRDATA		_VKI_IOR(VKI_PP_IOCTL, 0x85, unsigned char)
#define VKI_PPWDATA		_VKI_IOW(VKI_PP_IOCTL, 0x86, unsigned char)

/* Claim the port to start using it */
#define VKI_PPCLAIM		_VKI_IO(VKI_PP_IOCTL, 0x8b)

/* Release the port when you aren't using it */
#define VKI_PPRELEASE	_VKI_IO(VKI_PP_IOCTL, 0x8c)

/* Yield the port (release it if another driver is waiting,
 * then reclaim) */
#define VKI_PPYIELD		_VKI_IO(VKI_PP_IOCTL, 0x8d)

/* Register device exclusively (must be before PPCLAIM). */
#define VKI_PPEXCL		_VKI_IO(VKI_PP_IOCTL, 0x8f)

/* Data line direction: non-zero for input mode. */
#define VKI_PPDATADIR	_VKI_IOW(VKI_PP_IOCTL, 0x90, int)

/* Negotiate a particular IEEE 1284 mode. */
#define VKI_PPNEGOT	_VKI_IOW(VKI_PP_IOCTL, 0x91, int)

/* Set control lines when an interrupt occurs. */
#define VKI_PPWCTLONIRQ	_VKI_IOW(VKI_PP_IOCTL, 0x92, unsigned char)

/* Clear (and return) interrupt count. */
#define VKI_PPCLRIRQ	_VKI_IOR(VKI_PP_IOCTL, 0x93, int)

/* Set the IEEE 1284 phase that we're in (e.g. IEEE1284_PH_FWD_IDLE) */
#define VKI_PPSETPHASE	_VKI_IOW(VKI_PP_IOCTL, 0x94, int)

/* Set and get port timeout (struct timeval's) */
#define VKI_PPGETTIME	_VKI_IOR(VKI_PP_IOCTL, 0x95, struct vki_timeval)
#define VKI_PPSETTIME	_VKI_IOW(VKI_PP_IOCTL, 0x96, struct vki_timeval)

#define VKI_PPGETMODES	_VKI_IOR(VKI_PP_IOCTL, 0x97, unsigned int)

#define VKI_PPGETMODE	_VKI_IOR(VKI_PP_IOCTL, 0x98, int)
#define VKI_PPGETPHASE	_VKI_IOR(VKI_PP_IOCTL, 0x99, int)

#define VKI_PPGETFLAGS	_VKI_IOR(VKI_PP_IOCTL, 0x9a, int)
#define VKI_PPSETFLAGS	_VKI_IOW(VKI_PP_IOCTL, 0x9b, int)

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/fs.h
//----------------------------------------------------------------------

#define VKI_BLKROSET   _VKI_IO(0x12,93)	/* set device read-only (0 = read-write) */
#define VKI_BLKROGET   _VKI_IO(0x12,94)	/* get read-only status (0 = read_write) */
#define VKI_BLKGETSIZE _VKI_IO(0x12,96) /* return device size /512 (long *arg) */
#define VKI_BLKRASET   _VKI_IO(0x12,98)	/* set read ahead for block device */
#define VKI_BLKRAGET   _VKI_IO(0x12,99)	/* get current read ahead setting */
#define VKI_BLKFRASET  _VKI_IO(0x12,100)/* set filesystem (mm/filemap.c) read-ahead */
#define VKI_BLKFRAGET  _VKI_IO(0x12,101)/* get filesystem (mm/filemap.c) read-ahead */
#define VKI_BLKSECTGET _VKI_IO(0x12,103)/* get max sectors per request (ll_rw_blk.c) */
#define VKI_BLKSSZGET  _VKI_IO(0x12,104)/* get block device sector size */
#define VKI_BLKBSZGET  _VKI_IOR(0x12,112,vki_size_t)
#define VKI_BLKBSZSET  _VKI_IOW(0x12,113,vki_size_t)
#define VKI_BLKGETSIZE64 _VKI_IOR(0x12,114,vki_size_t) /* return device size in bytes (u64 *arg) */

#define VKI_FIBMAP	_VKI_IO(0x00,1)	/* bmap access */
#define VKI_FIGETBSZ    _VKI_IO(0x00,2)	/* get the block size used for bmap */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/scsi/sg.h
//----------------------------------------------------------------------

typedef struct vki_sg_io_hdr
{
    int interface_id;           /* [i] 'S' for SCSI generic (required) */
    int dxfer_direction;        /* [i] data transfer direction  */
    unsigned char cmd_len;      /* [i] SCSI command length ( <= 16 bytes) */
    unsigned char mx_sb_len;    /* [i] max length to write to sbp */
    unsigned short iovec_count; /* [i] 0 implies no scatter gather */
    unsigned int dxfer_len;     /* [i] byte count of data transfer */
    void __user *dxferp;	/* [i], [*io] points to data transfer memory
					      or scatter gather list */
    unsigned char __user *cmdp; /* [i], [*i] points to command to perform */
    void __user *sbp;		/* [i], [*o] points to sense_buffer memory */
    unsigned int timeout;       /* [i] MAX_UINT->no timeout (unit: millisec) */
    unsigned int flags;         /* [i] 0 -> default, see SG_FLAG... */
    int pack_id;                /* [i->o] unused internally (normally) */
    void __user * usr_ptr;      /* [i->o] unused internally */
    unsigned char status;       /* [o] scsi status */
    unsigned char masked_status;/* [o] shifted, masked scsi status */
    unsigned char msg_status;   /* [o] messaging level data (optional) */
    unsigned char sb_len_wr;    /* [o] byte count actually written to sbp */
    unsigned short host_status; /* [o] errors from host adapter */
    unsigned short driver_status;/* [o] errors from software driver */
    int resid;                  /* [o] dxfer_len - actual_transferred */
    unsigned int duration;      /* [o] time taken by cmd (unit: millisec) */
    unsigned int info;          /* [o] auxiliary information */
} vki_sg_io_hdr_t;  /* 64 bytes long (on i386) */

typedef struct vki_sg_scsi_id { /* used by SG_GET_SCSI_ID ioctl() */
    int host_no;        /* as in "scsi<n>" where 'n' is one of 0, 1, 2 etc */
    int channel;
    int scsi_id;        /* scsi id of target device */
    int lun;
    int scsi_type;      /* TYPE_... defined in scsi/scsi.h */
    short h_cmd_per_lun;/* host (adapter) maximum commands per lun */
    short d_queue_depth;/* device (or adapter) maximum queue length */
    int unused[2];      /* probably find a good use, set 0 for now */
} vki_sg_scsi_id_t; /* 32 bytes long on i386 */

#define VKI_SG_EMULATED_HOST 0x2203 /* true for emulated host adapter (ATAPI) */

#define VKI_SG_SET_RESERVED_SIZE 0x2275  /* request a new reserved buffer size */
#define VKI_SG_GET_RESERVED_SIZE 0x2272  /* actual size of reserved buffer */

#define VKI_SG_GET_SCSI_ID 0x2276   /* Yields fd's bus, chan, dev, lun + type */

#define VKI_SG_GET_SG_TABLESIZE 0x227F  /* 0 implies can't do scatter gather */

#define VKI_SG_GET_VERSION_NUM 0x2282 /* Example: version 2.1.34 yields 20134 */

#define VKI_SG_IO 0x2285   /* similar effect as write() followed by read() */

#define VKI_SG_SET_TIMEOUT 0x2201  /* unit: jiffies (10ms on i386) */
#define VKI_SG_GET_TIMEOUT 0x2202  /* yield timeout as _return_ value */

//#define SG_GET_COMMAND_Q 0x2270   /* Yields 0 (queuing off) or 1 (on) */
#define VKI_SG_SET_COMMAND_Q 0x2271   /* Change queuing state with 0 or 1 */

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/cdrom.h
//----------------------------------------------------------------------

#define VKI_CDROMPLAYMSF	0x5303 /* Play Audio MSF (struct cdrom_msf) */
#define VKI_CDROMREADTOCHDR	0x5305 /* Read TOC header 
                                           (struct cdrom_tochdr) */
#define VKI_CDROMREADTOCENTRY	0x5306 /* Read TOC entry 
                                           (struct cdrom_tocentry) */
#define VKI_CDROMSUBCHNL	0x530b /* Read subchannel data 
                                           (struct cdrom_subchnl) */
#define VKI_CDROMREADMODE2	0x530c /* Read CDROM mode 2 data (2336 Bytes) 
                                           (struct cdrom_read) */
#define VKI_CDROMREADAUDIO	0x530e /* (struct cdrom_read_audio) */
#define VKI_CDROMMULTISESSION	0x5310 /* Obtain the start-of-last-session 
                                           address of multi session disks 
                                           (struct cdrom_multisession) */
#define VKI_CDROM_GET_MCN	0x5311 /* Obtain the "Universal Product Code" 
                                           if available (struct cdrom_mcn) */
#define VKI_CDROMVOLREAD	0x5313 /* Get the drive's volume setting
                                          (struct cdrom_volctrl) */
#define VKI_CDROMREADRAW	0x5314	/* read data in raw mode (2352 Bytes)
                                           (struct cdrom_read) */
#define VKI_CDROM_CLEAR_OPTIONS	0x5321  /* Clear behavior options */
#define VKI_CDROM_DRIVE_STATUS	0x5326  /* Get tray position, etc. */

#define VKI_CDROM_SEND_PACKET	0x5393	/* send a packet to the drive */

struct vki_cdrom_msf0		
{
	__vki_u8	minute;
	__vki_u8	second;
	__vki_u8	frame;
};

union vki_cdrom_addr		
{
	struct vki_cdrom_msf0	msf;
	int			lba;
};

struct vki_cdrom_msf 
{
	__vki_u8	cdmsf_min0;	/* start minute */
	__vki_u8	cdmsf_sec0;	/* start second */
	__vki_u8	cdmsf_frame0;	/* start frame */
	__vki_u8	cdmsf_min1;	/* end minute */
	__vki_u8	cdmsf_sec1;	/* end second */
	__vki_u8	cdmsf_frame1;	/* end frame */
};

struct vki_cdrom_tochdr 	
{
	__vki_u8	cdth_trk0;	/* start track */
	__vki_u8	cdth_trk1;	/* end track */
};

struct vki_cdrom_volctrl
{
	__vki_u8	channel0;
	__vki_u8	channel1;
	__vki_u8	channel2;
	__vki_u8	channel3;
};

struct vki_cdrom_subchnl 
{
	__vki_u8	cdsc_format;
	__vki_u8	cdsc_audiostatus;
	__vki_u8	cdsc_adr:	4;
	__vki_u8	cdsc_ctrl:	4;
	__vki_u8	cdsc_trk;
	__vki_u8	cdsc_ind;
	union vki_cdrom_addr cdsc_absaddr;
	union vki_cdrom_addr cdsc_reladdr;
};

struct vki_cdrom_tocentry 
{
	__vki_u8	cdte_track;
	__vki_u8	cdte_adr	:4;
	__vki_u8	cdte_ctrl	:4;
	__vki_u8	cdte_format;
	union vki_cdrom_addr cdte_addr;
	__vki_u8	cdte_datamode;
};

struct vki_cdrom_read      
{
	int	cdread_lba;
	char 	*cdread_bufaddr;
	int	cdread_buflen;
};

struct vki_cdrom_read_audio
{
	union vki_cdrom_addr addr; /* frame address */
	__vki_u8 addr_format;      /* CDROM_LBA or CDROM_MSF */
	int nframes;           /* number of 2352-byte-frames to read at once */
	__vki_u8 __user *buf;      /* frame buffer (size: nframes*2352 bytes) */
};

struct vki_cdrom_multisession
{
	union vki_cdrom_addr addr; /* frame address: start-of-last-session 
	                           (not the new "frame 16"!).  Only valid
	                           if the "xa_flag" is true. */
	__vki_u8 xa_flag;        /* 1: "is XA disk" */
	__vki_u8 addr_format;    /* CDROM_LBA or CDROM_MSF */
};

struct vki_cdrom_mcn 
{
  __vki_u8 medium_catalog_number[14]; /* 13 ASCII digits, null-terminated */
};

#define VKI_CDROM_PACKET_SIZE	12

struct vki_cdrom_generic_command
{
	unsigned char 		cmd[VKI_CDROM_PACKET_SIZE];
	unsigned char		__user *buffer;
	unsigned int 		buflen;
	int			stat;
        // [[replace with void* to reduce inclusion amounts]]
	void/*struct vki_request_sense	*/__user *sense;
	unsigned char		data_direction;
	int			quiet;
	int			timeout;
	void			__user *reserved[1];	/* unused, actually */
};

#define VKI_CD_SYNC_SIZE         12 /* 12 sync bytes per raw data frame */
#define VKI_CD_HEAD_SIZE          4 /* header (address) bytes per raw data frame */
#define VKI_CD_FRAMESIZE_RAW   2352 /* bytes per frame, "raw" mode */
#define VKI_CD_FRAMESIZE_RAW0 (VKI_CD_FRAMESIZE_RAW-VKI_CD_SYNC_SIZE-VKI_CD_HEAD_SIZE) /*2336*/

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/soundcard.h
//----------------------------------------------------------------------

#ifndef _VKI_SIOWR
#if defined(_VKI_IOWR) && (defined(_AIX) || (!defined(sun) && !defined(sparc) && !defined(__sparc__) && !defined(__INCioctlh) && !defined(__Lynx__)))
/* Use already defined ioctl defines if they exist (except with Sun or Sparc) */
#define	_VKI_SIO		_VKI_IO
#define	_VKI_SIOR		_VKI_IOR
#define	_VKI_SIOW		_VKI_IOW
#define	_VKI_SIOWR		_VKI_IOWR
#else
// [[Valgrind: Install this case if/when necessary]
#error Valgrind: Cannot handle sparc/sun case yet...
#  endif /* _IOWR */
#endif  /* !_VKI_SIOWR */

#define VKI_SNDCTL_SEQ_CTRLRATE		_VKI_SIOWR('Q', 3, int)	/* Set/get timer resolution (HZ) */
#define VKI_SNDCTL_SEQ_GETOUTCOUNT	_VKI_SIOR ('Q', 4, int)
#define VKI_SNDCTL_SEQ_GETINCOUNT	_VKI_SIOR ('Q', 5, int)
#define VKI_SNDCTL_SEQ_PERCMODE		_VKI_SIOW ('Q', 6, int)
#define VKI_SNDCTL_SEQ_TESTMIDI		_VKI_SIOW ('Q', 8, int)
#define VKI_SNDCTL_SEQ_RESETSAMPLES	_VKI_SIOW ('Q', 9, int)
#define VKI_SNDCTL_SEQ_NRSYNTHS		_VKI_SIOR ('Q',10, int)
#define VKI_SNDCTL_SEQ_NRMIDIS		_VKI_SIOR ('Q',11, int)
#define VKI_SNDCTL_SEQ_GETTIME		_VKI_SIOR ('Q',19, int)

#define VKI_SNDCTL_TMR_TIMEBASE		_VKI_SIOWR('T', 1, int)
#define VKI_SNDCTL_TMR_TEMPO		_VKI_SIOWR('T', 5, int)
#define VKI_SNDCTL_TMR_SOURCE		_VKI_SIOWR('T', 6, int)

#define VKI_SNDCTL_MIDI_PRETIME		_VKI_SIOWR('m', 0, int)
#define VKI_SNDCTL_MIDI_MPUMODE		_VKI_SIOWR('m', 1, int)

#define VKI_SNDCTL_DSP_RESET		_VKI_SIO  ('P', 0)
#define VKI_SNDCTL_DSP_SYNC		_VKI_SIO  ('P', 1)
#define VKI_SNDCTL_DSP_SPEED		_VKI_SIOWR('P', 2, int)
#define VKI_SNDCTL_DSP_STEREO		_VKI_SIOWR('P', 3, int)
#define VKI_SNDCTL_DSP_GETBLKSIZE	_VKI_SIOWR('P', 4, int)
#define VKI_SNDCTL_DSP_CHANNELS		_VKI_SIOWR('P', 6, int)
#define VKI_SOUND_PCM_WRITE_FILTER	_VKI_SIOWR('P', 7, int)
#define VKI_SNDCTL_DSP_POST		_VKI_SIO  ('P', 8)
#define VKI_SNDCTL_DSP_SUBDIVIDE	_VKI_SIOWR('P', 9, int)
#define VKI_SNDCTL_DSP_SETFRAGMENT	_VKI_SIOWR('P',10, int)

#define VKI_SNDCTL_DSP_GETFMTS		_VKI_SIOR ('P',11, int) /* Returns a mask */
#define VKI_SNDCTL_DSP_SETFMT		_VKI_SIOWR('P', 5, int) /* Selects ONE fmt */

typedef struct vki_audio_buf_info {
			int fragments;	/* # of available fragments (partially usend ones not counted) */
			int fragstotal;	/* Total # of fragments allocated */
			int fragsize;	/* Size of a fragment in bytes */

			int bytes;	/* Available space in bytes (includes partially used fragments) */
			/* Note! 'bytes' could be more than fragments*fragsize */
		} vki_audio_buf_info;

#define VKI_SNDCTL_DSP_GETOSPACE	_VKI_SIOR ('P',12, vki_audio_buf_info)
#define VKI_SNDCTL_DSP_GETISPACE	_VKI_SIOR ('P',13, vki_audio_buf_info)
#define VKI_SNDCTL_DSP_NONBLOCK		_VKI_SIO  ('P',14)
#define VKI_SNDCTL_DSP_GETCAPS		_VKI_SIOR ('P',15, int)

#define VKI_SNDCTL_DSP_GETTRIGGER	_VKI_SIOR ('P',16, int)
#define VKI_SNDCTL_DSP_SETTRIGGER	_VKI_SIOW ('P',16, int)

#define VKI_SNDCTL_DSP_SETSYNCRO	_VKI_SIO  ('P', 21)
#define VKI_SNDCTL_DSP_SETDUPLEX	_VKI_SIO  ('P', 22)
#define VKI_SNDCTL_DSP_GETODELAY	_VKI_SIOR ('P', 23, int)

#define VKI_SNDCTL_DSP_GETCHANNELMASK	_VKI_SIOWR('P', 64, int)
#define VKI_SNDCTL_DSP_BIND_CHANNEL	_VKI_SIOWR('P', 65, int)

#define VKI_SNDCTL_DSP_SETSPDIF		_VKI_SIOW ('P', 66, int)
#define VKI_SNDCTL_DSP_GETSPDIF		_VKI_SIOR ('P', 67, int)

#define VKI_SOUND_PCM_READ_RATE		_VKI_SIOR ('P', 2, int)
#define VKI_SOUND_PCM_READ_CHANNELS	_VKI_SIOR ('P', 6, int)
#define VKI_SOUND_PCM_READ_BITS		_VKI_SIOR ('P', 5, int)
#define VKI_SOUND_PCM_READ_FILTER	_VKI_SIOR ('P', 7, int)


//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/hdreg.h
//----------------------------------------------------------------------

struct vki_hd_geometry {
      unsigned char heads;
      unsigned char sectors;
      unsigned short cylinders;
      unsigned long start;
};

#define VKI_HDIO_GETGEO		0x0301	/* get device geometry */
#define VKI_HDIO_GET_DMA	0x030b	/* get use-dma flag */
#define VKI_HDIO_GET_IDENTITY	0x030d	/* get IDE identification info */

// [[Nb: done like this because the original type is a huge struct that will
//   always be the same size.]]
#define VKI_SIZEOF_STRUCT_HD_DRIVEID   512

//----------------------------------------------------------------------
// From linux-2.6.8.1/include/linux/fb.h
//----------------------------------------------------------------------

#define VKI_FBIOGET_VSCREENINFO	0x4600
#define VKI_FBIOGET_FSCREENINFO	0x4602

struct vki_fb_fix_screeninfo {
	char id[16];			/* identification string eg "TT Builtin" */
	unsigned long smem_start;	/* Start of frame buffer mem */
					/* (physical address) */
	__vki_u32 smem_len;			/* Length of frame buffer mem */
	__vki_u32 type;			/* see FB_TYPE_*		*/
	__vki_u32 type_aux;		/* Interleave for interleaved Planes */
	__vki_u32 visual;		/* see FB_VISUAL_*		*/ 
	__vki_u16 xpanstep;		/* zero if no hardware panning  */
	__vki_u16 ypanstep;		/* zero if no hardware panning  */
	__vki_u16 ywrapstep;		/* zero if no hardware ywrap    */
	__vki_u32 line_length;		/* length of a line in bytes    */
	unsigned long mmio_start;	/* Start of Memory Mapped I/O   */
					/* (physical address) */
	__vki_u32 mmio_len;		/* Length of Memory Mapped I/O  */
	__vki_u32 accel;		/* Indicate to driver which	*/
					/*  specific chip/card we have	*/
	__vki_u16 reserved[3];		/* Reserved for future compatibility */
};

struct vki_fb_bitfield {
	__vki_u32 offset;		/* beginning of bitfield	*/
	__vki_u32 length;		/* length of bitfield		*/
	__vki_u32 msb_right;		/* != 0 : Most significant bit is */ 
					/* right */ 
};

struct vki_fb_var_screeninfo {
	__vki_u32 xres;			/* visible resolution		*/
	__vki_u32 yres;
	__vki_u32 xres_virtual;		/* virtual resolution		*/
	__vki_u32 yres_virtual;
	__vki_u32 xoffset;		/* offset from virtual to visible */
	__vki_u32 yoffset;		/* resolution			*/

	__vki_u32 bits_per_pixel;	/* guess what			*/
	__vki_u32 grayscale;		/* != 0 Graylevels instead of colors */

	struct vki_fb_bitfield red;	/* bitfield in fb mem if true color, */
	struct vki_fb_bitfield green;	/* else only length is significant */
	struct vki_fb_bitfield blue;
	struct vki_fb_bitfield transp;	/* transparency			*/	

	__vki_u32 nonstd;		/* != 0 Non standard pixel format */

	__vki_u32 activate;		/* see FB_ACTIVATE_*		*/

	__vki_u32 height;		/* height of picture in mm    */
	__vki_u32 width;		/* width of picture in mm     */

	__vki_u32 accel_flags;		/* (OBSOLETE) see fb_info.flags */

	/* Timing: All values in pixclocks, except pixclock (of course) */
	__vki_u32 pixclock;		/* pixel clock in ps (pico seconds) */
	__vki_u32 left_margin;		/* time from sync to picture	*/
	__vki_u32 right_margin;		/* time from picture to sync	*/
	__vki_u32 upper_margin;		/* time from sync to picture	*/
	__vki_u32 lower_margin;
	__vki_u32 hsync_len;		/* length of horizontal sync	*/
	__vki_u32 vsync_len;		/* length of vertical sync	*/
	__vki_u32 sync;			/* see FB_SYNC_*		*/
	__vki_u32 vmode;		/* see FB_VMODE_*		*/
	__vki_u32 rotate;		/* angle we rotate counter clockwise */
	__vki_u32 reserved[5];		/* Reserved for future compatibility */
};

//----------------------------------------------------------------------
// From linux-2.6.9/include/linux/kd.h
//----------------------------------------------------------------------

#define VKI_GIO_FONT       0x4B60  /* gets font in expanded form */
#define VKI_PIO_FONT       0x4B61  /* use font in expanded form */

#define VKI_GIO_FONTX      0x4B6B  /* get font using struct consolefontdesc */
#define VKI_PIO_FONTX      0x4B6C  /* set font using struct consolefontdesc */
struct vki_consolefontdesc {
	unsigned short charcount;	/* characters in font (256 or 512) */
	unsigned short charheight;	/* scan lines per character (1-32) */
	char __user *chardata;		/* font data in expanded form */
};

#define VKI_PIO_FONTRESET  0x4B6D  /* reset to default font */

#define VKI_GIO_CMAP       0x4B70  /* gets colour palette on VGA+ */
#define VKI_PIO_CMAP       0x4B71  /* sets colour palette on VGA+ */

#define VKI_KIOCSOUND      0x4B2F  /* start sound generation (0 for off) */
#define VKI_KDMKTONE       0x4B30  /* generate tone */

#define VKI_KDGETLED       0x4B31  /* return current led state */
#define VKI_KDSETLED       0x4B32  /* set led state [lights, not flags] */

#define VKI_KDGKBTYPE      0x4B33  /* get keyboard type */

#define VKI_KDADDIO        0x4B34  /* add i/o port as valid */
#define VKI_KDDELIO        0x4B35  /* del i/o port as valid */
#define VKI_KDENABIO       0x4B36  /* enable i/o to video board */
#define VKI_KDDISABIO      0x4B37  /* disable i/o to video board */

#define VKI_KDSETMODE      0x4B3A  /* set text/graphics mode */
#define VKI_KDGETMODE      0x4B3B  /* get current mode */

#define VKI_KDMAPDISP      0x4B3C  /* map display into address space */
#define VKI_KDUNMAPDISP    0x4B3D  /* unmap display from address space */

#define		VKI_E_TABSZ		256
#define VKI_GIO_SCRNMAP    0x4B40  /* get screen mapping from kernel */
#define VKI_PIO_SCRNMAP	   0x4B41  /* put screen mapping table in kernel */
#define VKI_GIO_UNISCRNMAP 0x4B69  /* get full Unicode screen mapping */
#define VKI_PIO_UNISCRNMAP 0x4B6A  /* set full Unicode screen mapping */

#define VKI_GIO_UNIMAP     0x4B66  /* get unicode-to-font mapping from kernel */
struct vki_unipair {
	unsigned short unicode;
	unsigned short fontpos;
};
struct vki_unimapdesc {
	unsigned short entry_ct;
	struct vki_unipair __user *entries;
};
#define VKI_PIO_UNIMAP     0x4B67  /* put unicode-to-font mapping in kernel */
#define VKI_PIO_UNIMAPCLR  0x4B68  /* clear table, possibly advise hash algorithm */
struct vki_unimapinit {
	unsigned short advised_hashsize;  /* 0 if no opinion */
	unsigned short advised_hashstep;  /* 0 if no opinion */
	unsigned short advised_hashlevel; /* 0 if no opinion */
};

#define VKI_KDGKBMODE      0x4B44  /* gets current keyboard mode */
#define VKI_KDSKBMODE      0x4B45  /* sets current keyboard mode */

#define VKI_KDGKBMETA      0x4B62  /* gets meta key handling mode */
#define VKI_KDSKBMETA      0x4B63  /* sets meta key handling mode */

#define VKI_KDGKBLED       0x4B64  /* get led flags (not lights) */
#define VKI_KDSKBLED       0x4B65  /* set led flags (not lights) */

struct vki_kbentry {
	unsigned char kb_table;
	unsigned char kb_index;
	unsigned short kb_value;
};
#define VKI_KDGKBENT       0x4B46  /* gets one entry in translation table */
#define VKI_KDSKBENT       0x4B47  /* sets one entry in translation table */

struct vki_kbsentry {
	unsigned char kb_func;
	unsigned char kb_string[512];
};
#define VKI_KDGKBSENT      0x4B48  /* gets one function key string entry */
#define VKI_KDSKBSENT      0x4B49  /* sets one function key string entry */

struct vki_kbdiacr {
        unsigned char diacr, base, result;
};
struct vki_kbdiacrs {
        unsigned int kb_cnt;    /* number of entries in following array */
	struct vki_kbdiacr kbdiacr[256];    /* MAX_DIACR from keyboard.h */
};
#define VKI_KDGKBDIACR     0x4B4A  /* read kernel accent table */
#define VKI_KDSKBDIACR     0x4B4B  /* write kernel accent table */

struct vki_kbkeycode {
	unsigned int scancode, keycode;
};
#define VKI_KDGETKEYCODE   0x4B4C  /* read kernel keycode table entry */
#define VKI_KDSETKEYCODE   0x4B4D  /* write kernel keycode table entry */

#define VKI_KDSIGACCEPT    0x4B4E  /* accept kbd generated signals */

struct vki_kbd_repeat {
	int delay;	/* in msec; <= 0: don't change */
	int period;	/* in msec; <= 0: don't change */
			/* earlier this field was misnamed "rate" */
};
#define VKI_KDKBDREP       0x4B52  /* set keyboard delay/repeat rate;
                                    * actually used values are returned */

#define VKI_KDFONTOP       0x4B72  /* font operations */

struct vki_console_font_op {
	unsigned int op;	/* operation code KD_FONT_OP_* */
	unsigned int flags;	/* KD_FONT_FLAG_* */
	unsigned int width, height;	/* font size */
	unsigned int charcount;
	unsigned char __user *data;	/* font data with height fixed to 32 */
};

#define VKI_KD_FONT_OP_SET		0	/* Set font */
#define VKI_KD_FONT_OP_GET		1	/* Get font */
#define VKI_KD_FONT_OP_SET_DEFAULT	2	/* Set font to default, data points to name / NULL */
#define VKI_KD_FONT_OP_COPY		3	/* Copy from another console */

//----------------------------------------------------------------------
// From linux-2.6.9/include/linux/kb.h
//----------------------------------------------------------------------

typedef __vki_kernel_uid32_t vki_qid_t; /* Type in which we store ids in memory */

//----------------------------------------------------------------------
// From linux-2.6.20.1/include/linux/ptrace.h
//----------------------------------------------------------------------

#define VKI_PTRACE_TRACEME         0
#define VKI_PTRACE_PEEKTEXT	   1
#define VKI_PTRACE_PEEKDATA	   2
#define VKI_PTRACE_PEEKUSR	   3
#define VKI_PTRACE_POKEUSR	   6

#define VKI_PTRACE_DETACH         17

#define VKI_PTRACE_GETEVENTMSG	0x4201
#define VKI_PTRACE_GETSIGINFO	0x4202
#define VKI_PTRACE_SETSIGINFO	0x4203

//----------------------------------------------------------------------
// From linux-2.6.14/include/sound/asound.h
//----------------------------------------------------------------------

enum {
	VKI_SNDRV_PCM_IOCTL_HW_FREE = _VKI_IO('A', 0x12),
	VKI_SNDRV_PCM_IOCTL_HWSYNC = _VKI_IO('A', 0x22),
	VKI_SNDRV_PCM_IOCTL_PREPARE = _VKI_IO('A', 0x40),
	VKI_SNDRV_PCM_IOCTL_RESET = _VKI_IO('A', 0x41),
	VKI_SNDRV_PCM_IOCTL_START = _VKI_IO('A', 0x42),
	VKI_SNDRV_PCM_IOCTL_DROP = _VKI_IO('A', 0x43),
	VKI_SNDRV_PCM_IOCTL_DRAIN = _VKI_IO('A', 0x44),
	VKI_SNDRV_PCM_IOCTL_PAUSE = _VKI_IOW('A', 0x45, int),
	VKI_SNDRV_PCM_IOCTL_RESUME = _VKI_IO('A', 0x47),
	VKI_SNDRV_PCM_IOCTL_XRUN = _VKI_IO('A', 0x48),
	VKI_SNDRV_PCM_IOCTL_LINK = _VKI_IOW('A', 0x60, int),
	VKI_SNDRV_PCM_IOCTL_UNLINK = _VKI_IO('A', 0x61),
};

enum {
	VKI_SNDRV_TIMER_IOCTL_START = _VKI_IO('T', 0xa0),
	VKI_SNDRV_TIMER_IOCTL_STOP = _VKI_IO('T', 0xa1),
	VKI_SNDRV_TIMER_IOCTL_CONTINUE = _VKI_IO('T', 0xa2),
	VKI_SNDRV_TIMER_IOCTL_PAUSE = _VKI_IO('T', 0xa3),
};

//----------------------------------------------------------------------
// From linux-2.6.15.4/include/linux/serial.h
//----------------------------------------------------------------------

struct vki_serial_icounter_struct {
	int cts, dsr, rng, dcd;
	int rx, tx;
	int frame, overrun, parity, brk;
	int buf_overrun;
	int reserved[9];
};

//----------------------------------------------------------------------
// From linux-2.6.16/include/linux/vt.h
//----------------------------------------------------------------------

#define VKI_VT_OPENQRY	0x5600	/* find available vt */

struct vki_vt_mode {
	char mode;		/* vt mode */
	char waitv;		/* if set, hang on writes if not active */
	short relsig;		/* signal to raise on release req */
	short acqsig;		/* signal to raise on acquisition */
	short frsig;		/* unused (set to 0) */
};
#define VKI_VT_GETMODE	0x5601	/* get mode of active vt */
#define VKI_VT_SETMODE	0x5602	/* set mode of active vt */

struct vki_vt_stat {
	unsigned short v_active;	/* active vt */
	unsigned short v_signal;	/* signal to send */
	unsigned short v_state;		/* vt bitmask */
};
#define VKI_VT_GETSTATE	0x5603	/* get global vt state info */
#define VKI_VT_SENDSIG	0x5604	/* signal to send to bitmask of vts */

#define VKI_VT_RELDISP	0x5605	/* release display */

#define VKI_VT_ACTIVATE	0x5606	/* make vt active */
#define VKI_VT_WAITACTIVE	0x5607	/* wait for vt active */
#define VKI_VT_DISALLOCATE	0x5608  /* free memory associated to vt */

struct vki_vt_sizes {
	unsigned short v_rows;		/* number of rows */
	unsigned short v_cols;		/* number of columns */
	unsigned short v_scrollsize;	/* number of lines of scrollback */
};
#define VKI_VT_RESIZE	0x5609	/* set kernel's idea of screensize */

struct vki_vt_consize {
	unsigned short v_rows;	/* number of rows */
	unsigned short v_cols;	/* number of columns */
	unsigned short v_vlin;	/* number of pixel rows on screen */
	unsigned short v_clin;	/* number of pixel rows per character */
	unsigned short v_vcol;	/* number of pixel columns on screen */
	unsigned short v_ccol;	/* number of pixel columns per character */
};
#define VKI_VT_RESIZEX      0x560A  /* set kernel's idea of screensize + more */
#define VKI_VT_LOCKSWITCH   0x560B  /* disallow vt switching */
#define VKI_VT_UNLOCKSWITCH 0x560C  /* allow vt switching */

//----------------------------------------------------------------------
// From linux-2.6.19/include/linux/prctl.h
//----------------------------------------------------------------------

#define VKI_PR_SET_PDEATHSIG  1  /* Second arg is a signal */
#define VKI_PR_GET_PDEATHSIG  2  /* Second arg is a ptr to return the signal */

#define VKI_PR_GET_DUMPABLE   3
#define VKI_PR_SET_DUMPABLE   4

#define VKI_PR_GET_UNALIGN	  5
#define VKI_PR_SET_UNALIGN	  6
# define VKI_PR_UNALIGN_NOPRINT	1	/* silently fix up unaligned user accesses */
# define VKI_PR_UNALIGN_SIGBUS	2	/* generate SIGBUS on unaligned user access */

#define VKI_PR_GET_KEEPCAPS   7
#define VKI_PR_SET_KEEPCAPS   8

#define VKI_PR_GET_FPEMU  9
#define VKI_PR_SET_FPEMU 10
# define VKI_PR_FPEMU_NOPRINT	1	/* silently emulate fp operations accesses */
# define VKI_PR_FPEMU_SIGFPE	2	/* don't emulate fp operations, send SIGFPE instead */

#define VKI_PR_GET_FPEXC	11
#define VKI_PR_SET_FPEXC	12
# define VKI_PR_FP_EXC_SW_ENABLE	0x80	/* Use FPEXC for FP exception enables */
# define VKI_PR_FP_EXC_DIV		0x010000	/* floating point divide by zero */
# define VKI_PR_FP_EXC_OVF		0x020000	/* floating point overflow */
# define VKI_PR_FP_EXC_UND		0x040000	/* floating point underflow */
# define VKI_PR_FP_EXC_RES		0x080000	/* floating point inexact result */
# define VKI_PR_FP_EXC_INV		0x100000	/* floating point invalid operation */
# define VKI_PR_FP_EXC_DISABLED	0	/* FP exceptions disabled */
# define VKI_PR_FP_EXC_NONRECOV	1	/* async non-recoverable exc. mode */
# define VKI_PR_FP_EXC_ASYNC	2	/* async recoverable exception mode */
# define VKI_PR_FP_EXC_PRECISE	3	/* precise exception mode */

#define VKI_PR_GET_TIMING   13
#define VKI_PR_SET_TIMING   14
# define VKI_PR_TIMING_STATISTICAL  0       /* Normal, traditional,
                                                   statistical process timing */
# define VKI_PR_TIMING_TIMESTAMP    1       /* Accurate timestamp based
                                                   process timing */

#define VKI_PR_SET_NAME    15		/* Set process name */
#define VKI_PR_GET_NAME    16		/* Get process name */

#define VKI_PR_GET_ENDIAN	19
#define VKI_PR_SET_ENDIAN	20
# define VKI_PR_ENDIAN_BIG		0
# define VKI_PR_ENDIAN_LITTLE	1	/* True little endian mode */
# define VKI_PR_ENDIAN_PPC_LITTLE	2	/* "PowerPC" pseudo little endian */

//----------------------------------------------------------------------
// From linux-2.6.19/include/linux/usbdevice_fs.h
//----------------------------------------------------------------------

struct vki_usbdevfs_ctrltransfer {
	__vki_u8 bRequestType;
	__vki_u8 bRequest;
	__vki_u16 wValue;
	__vki_u16 wIndex;
	__vki_u16 wLength;
	__vki_u32 timeout;  /* in milliseconds */
 	void __user *data;
};

struct vki_usbdevfs_bulktransfer {
	unsigned int ep;
	unsigned int len;
	unsigned int timeout; /* in milliseconds */
	void __user *data;
};

#define VKI_USBDEVFS_MAXDRIVERNAME 255

struct vki_usbdevfs_getdriver {
	unsigned int interface;
	char driver[VKI_USBDEVFS_MAXDRIVERNAME + 1];
};

struct vki_usbdevfs_connectinfo {
	unsigned int devnum;
	unsigned char slow;
};

struct vki_usbdevfs_iso_packet_desc {
	unsigned int length;
	unsigned int actual_length;
	unsigned int status;
};

struct vki_usbdevfs_urb {
	unsigned char type;
	unsigned char endpoint;
	int status;
	unsigned int flags;
	void __user *buffer;
	int buffer_length;
	int actual_length;
	int start_frame;
	int number_of_packets;
	int error_count;
	unsigned int signr;  /* signal to be sent on error, -1 if none should be sent */
	void *usercontext;
	struct vki_usbdevfs_iso_packet_desc iso_frame_desc[0];
};

struct vki_usbdevfs_ioctl {
	int	ifno;		/* interface 0..N ; negative numbers reserved */
	int	ioctl_code;	/* MUST encode size + direction of data so the
				 * macros in <asm/ioctl.h> give correct values */
	void __user *data;	/* param buffer (in, or out) */
};

#define VKI_USBDEVFS_CONTROL           _VKI_IOWR('U', 0, struct vki_usbdevfs_ctrltransfer)
#define VKI_USBDEVFS_BULK              _VKI_IOWR('U', 2, struct vki_usbdevfs_bulktransfer)
#define VKI_USBDEVFS_GETDRIVER         _VKI_IOW('U', 8, struct vki_usbdevfs_getdriver)
#define VKI_USBDEVFS_SUBMITURB         _VKI_IOR('U', 10, struct vki_usbdevfs_urb)
#define VKI_USBDEVFS_DISCARDURB        _VKI_IO('U', 11)
#define VKI_USBDEVFS_REAPURB           _VKI_IOW('U', 12, void *)
#define VKI_USBDEVFS_REAPURBNDELAY     _VKI_IOW('U', 13, void *)
#define VKI_USBDEVFS_CONNECTINFO       _VKI_IOW('U', 17, struct vki_usbdevfs_connectinfo)
#define VKI_USBDEVFS_IOCTL             _VKI_IOWR('U', 18, struct vki_usbdevfs_ioctl)
#define VKI_USBDEVFS_RESET             _VKI_IO('U', 20)

#define VKI_USBDEVFS_URB_TYPE_ISO              0
#define VKI_USBDEVFS_URB_TYPE_INTERRUPT        1
#define VKI_USBDEVFS_URB_TYPE_CONTROL          2
#define VKI_USBDEVFS_URB_TYPE_BULK             3

// [[this is missing in usbdevice_fs.h]]
struct vki_usbdevfs_setuppacket {
       __vki_u8 bRequestType;
       __vki_u8 bRequest;
       __vki_u16 wValue;
       __vki_u16 wIndex;
       __vki_u16 wLength;
};

//----------------------------------------------------------------------
// From linux-2.6.20.1/include/linux/i2c.h
//----------------------------------------------------------------------

#define VKI_I2C_SLAVE		0x0703	/* Change slave address			*/
					/* Attn.: Slave address is 7 or 10 bits */
#define VKI_I2C_SLAVE_FORCE	0x0706	/* Change slave address			*/
					/* Attn.: Slave address is 7 or 10 bits */
					/* This changes the address, even if it */
					/* is already taken!			*/
#define VKI_I2C_TENBIT		0x0704	/* 0 for 7 bit addrs, != 0 for 10 bit	*/
#define VKI_I2C_FUNCS		0x0705	/* Get the adapter functionality */
#define VKI_I2C_PEC		0x0708	/* != 0 for SMBus PEC                   */

//----------------------------------------------------------------------
// From linux-2.6.20.1/include/linux/keyctl.h
//----------------------------------------------------------------------

/* keyctl commands */
#define VKI_KEYCTL_GET_KEYRING_ID	0	/* ask for a keyring's ID */
#define VKI_KEYCTL_JOIN_SESSION_KEYRING	1	/* join or start named session keyring */
#define VKI_KEYCTL_UPDATE		2	/* update a key */
#define VKI_KEYCTL_REVOKE		3	/* revoke a key */
#define VKI_KEYCTL_CHOWN		4	/* set ownership of a key */
#define VKI_KEYCTL_SETPERM		5	/* set perms on a key */
#define VKI_KEYCTL_DESCRIBE		6	/* describe a key */
#define VKI_KEYCTL_CLEAR		7	/* clear contents of a keyring */
#define VKI_KEYCTL_LINK			8	/* link a key into a keyring */
#define VKI_KEYCTL_UNLINK		9	/* unlink a key from a keyring */
#define VKI_KEYCTL_SEARCH		10	/* search for a key in a keyring */
#define VKI_KEYCTL_READ			11	/* read a key or keyring's contents */
#define VKI_KEYCTL_INSTANTIATE		12	/* instantiate a partially constructed key */
#define VKI_KEYCTL_NEGATE		13	/* negate a partially constructed key */
#define VKI_KEYCTL_SET_REQKEY_KEYRING	14	/* set default request-key keyring */
#define VKI_KEYCTL_SET_TIMEOUT		15	/* set key timeout */
#define VKI_KEYCTL_ASSUME_AUTHORITY	16	/* assume request_key() authorisation */

/*--------------------------------------------------------------------*/
// From linux-2.6.20.1/include/linux/key.h
/*--------------------------------------------------------------------*/

/* key handle serial number */
typedef vki_int32_t vki_key_serial_t;

/* key handle permissions mask */
typedef vki_uint32_t vki_key_perm_t;

//----------------------------------------------------------------------
// From linux-2.6.24.7/include/linux/wireless.h
// (wireless extensions version 22, 2007-03-16)
//----------------------------------------------------------------------

/*
 * [[Wireless extensions ioctls.]]
 */

/* Wireless Identification */
#define VKI_SIOCSIWCOMMIT	0x8B00	/* Commit pending changes to driver */
#define VKI_SIOCGIWNAME		0x8B01	/* get name == wireless protocol */

/* Basic operations */
#define VKI_SIOCSIWNWID		0x8B02	/* set network id (pre-802.11) */
#define VKI_SIOCGIWNWID		0x8B03	/* get network id (the cell) */
#define VKI_SIOCSIWFREQ		0x8B04	/* set channel/frequency (Hz) */
#define VKI_SIOCGIWFREQ		0x8B05	/* get channel/frequency (Hz) */
#define VKI_SIOCSIWMODE		0x8B06	/* set operation mode */
#define VKI_SIOCGIWMODE		0x8B07	/* get operation mode */
#define VKI_SIOCSIWSENS		0x8B08	/* set sensitivity (dBm) */
#define VKI_SIOCGIWSENS		0x8B09	/* get sensitivity (dBm) */

/* Informative stuff */
#define VKI_SIOCSIWRANGE	0x8B0A	/* Unused */
#define VKI_SIOCGIWRANGE	0x8B0B	/* Get range of parameters */
#define VKI_SIOCSIWPRIV		0x8B0C	/* Unused */
#define VKI_SIOCGIWPRIV		0x8B0D	/* get private ioctl interface info */
#define VKI_SIOCSIWSTATS	0x8B0E	/* Unused */
#define VKI_SIOCGIWSTATS	0x8B0F	/* Get /proc/net/wireless stats */

/* Spy support (statistics per MAC address - used for Mobile IP support) */
#define VKI_SIOCSIWSPY		0x8B10	/* set spy addresses */
#define VKI_SIOCGIWSPY		0x8B11	/* get spy info (quality of link) */
#define VKI_SIOCSIWTHRSPY	0x8B12	/* set spy threshold (spy event) */
#define VKI_SIOCGIWTHRSPY	0x8B13	/* get spy threshold */

/* Access Point manipulation */
#define VKI_SIOCSIWAP		0x8B14	/* set access point MAC addresses */
#define VKI_SIOCGIWAP		0x8B15	/* get access point MAC addresses */
#define VKI_SIOCGIWAPLIST	0x8B17	/* Deprecated in favor of scanning */
#define VKI_SIOCSIWSCAN         0x8B18	/* trigger scanning (list cells) */
#define VKI_SIOCGIWSCAN         0x8B19	/* get scanning results */

/* 802.11 specific support */
#define VKI_SIOCSIWESSID	0x8B1A	/* set ESSID (network name) */
#define VKI_SIOCGIWESSID	0x8B1B	/* get ESSID */
#define VKI_SIOCSIWNICKN	0x8B1C	/* set node name/nickname */
#define VKI_SIOCGIWNICKN	0x8B1D	/* get node name/nickname */

/* Other parameters useful in 802.11 and some other devices */
#define VKI_SIOCSIWRATE		0x8B20	/* set default bit rate (bps) */
#define VKI_SIOCGIWRATE		0x8B21	/* get default bit rate (bps) */
#define VKI_SIOCSIWRTS		0x8B22	/* set RTS/CTS threshold (bytes) */
#define VKI_SIOCGIWRTS		0x8B23	/* get RTS/CTS threshold (bytes) */
#define VKI_SIOCSIWFRAG		0x8B24	/* set fragmentation thr (bytes) */
#define VKI_SIOCGIWFRAG		0x8B25	/* get fragmentation thr (bytes) */
#define VKI_SIOCSIWTXPOW	0x8B26	/* set transmit power (dBm) */
#define VKI_SIOCGIWTXPOW	0x8B27	/* get transmit power (dBm) */
#define VKI_SIOCSIWRETRY	0x8B28	/* set retry limits and lifetime */
#define VKI_SIOCGIWRETRY	0x8B29	/* get retry limits and lifetime */

/* Encoding stuff (scrambling, hardware security, WEP...) */
#define VKI_SIOCSIWENCODE	0x8B2A	/* set encoding token & mode */
#define VKI_SIOCGIWENCODE	0x8B2B	/* get encoding token & mode */

/* Power saving stuff (power management, unicast and multicast) */
#define VKI_SIOCSIWPOWER	0x8B2C	/* set Power Management settings */
#define VKI_SIOCGIWPOWER	0x8B2D	/* get Power Management settings */

/* WPA : Generic IEEE 802.11 informatiom element (e.g., for WPA/RSN/WMM). */
#define VKI_SIOCSIWGENIE	0x8B30		/* set generic IE */
#define VKI_SIOCGIWGENIE	0x8B31		/* get generic IE */

/* WPA : IEEE 802.11 MLME requests */
#define VKI_SIOCSIWMLME		0x8B16	/* request MLME operation; uses
					 * struct iw_mlme */
/* WPA : Authentication mode parameters */
#define VKI_SIOCSIWAUTH		0x8B32	/* set authentication mode params */
#define VKI_SIOCGIWAUTH		0x8B33	/* get authentication mode params */

/* WPA : Extended version of encoding configuration */
#define VKI_SIOCSIWENCODEEXT	0x8B34	/* set encoding token & mode */
#define VKI_SIOCGIWENCODEEXT	0x8B35	/* get encoding token & mode */

/* WPA2 : PMKSA cache management */
#define VKI_SIOCSIWPMKSA	0x8B36	/* PMKSA cache operation */

/*
 * [[Payload for the wireless extensions ioctls.]]
 */

struct	vki_iw_param
{
  __vki_s32	value;		/* The value of the parameter itself */
  __vki_u8	fixed;		/* Hardware should not use auto select */
  __vki_u8	disabled;	/* Disable the feature */
  __vki_u16	flags;		/* Various specifc flags (if any) */
};

struct	vki_iw_point
{
  void __user	*pointer;	/* Pointer to the data  (in user space) */
  __vki_u16	length;		/* number of fields or size in bytes */
  __vki_u16	flags;		/* Optional params */
};

struct	vki_iw_freq
{
	__vki_s32	m;		/* Mantissa */
	__vki_s16	e;		/* Exponent */
	__vki_u8	i;		/* List index (when in range struct) */
	__vki_u8	flags;		/* Flags (fixed/auto) */
};

struct	vki_iw_quality
{
	__vki_u8	qual;		/* link quality (%retries, SNR,
					   %missed beacons or better...) */
	__vki_u8	level;		/* signal level (dBm) */
	__vki_u8	noise;		/* noise level (dBm) */
	__vki_u8	updated;	/* Flags to know if updated */
};

union	vki_iwreq_data
{
	/* Config - generic */
	char		name[VKI_IFNAMSIZ];
	/* Name : used to verify the presence of  wireless extensions.
	 * Name of the protocol/provider... */

	struct vki_iw_point	essid;	/* Extended network name */
	struct vki_iw_param	nwid;	/* network id (or domain - the cell) */
	struct vki_iw_freq	freq;	/* frequency or channel :
					 * 0-1000 = channel
					 * > 1000 = frequency in Hz */

	struct vki_iw_param	sens;	/* signal level threshold */
	struct vki_iw_param	bitrate;/* default bit rate */
	struct vki_iw_param	txpower;/* default transmit power */
	struct vki_iw_param	rts;	/* RTS threshold threshold */
	struct vki_iw_param	frag;	/* Fragmentation threshold */
	__vki_u32		mode;	/* Operation mode */
	struct vki_iw_param	retry;	/* Retry limits & lifetime */

	struct vki_iw_point	encoding; /* Encoding stuff : tokens */
	struct vki_iw_param	power;	/* PM duration/timeout */
	struct vki_iw_quality	qual;	/* Quality part of statistics */

	struct vki_sockaddr ap_addr;	/* Access point address */
	struct vki_sockaddr addr;	/* Destination address (hw/mac) */

	struct vki_iw_param	param;	/* Other small parameters */
	struct vki_iw_point	data;	/* Other large parameters */
};

struct	vki_iwreq 
{
	union
	{
		char ifrn_name[VKI_IFNAMSIZ];	/* if name, e.g. "eth0" */
	} ifr_ifrn;

	/* Data part (defined just above) */
	union	vki_iwreq_data	u;
};

/*--------------------------------------------------------------------*/
// From linux-2.6.31.5/include/linux/perf_counter.h
/*--------------------------------------------------------------------*/

struct vki_perf_counter_attr {

	/*
	 * Major type: hardware/software/tracepoint/etc.
	 */
	__vki_u32			type;

	/*
	 * Size of the attr structure, for fwd/bwd compat.
	 */
	__vki_u32			size;

	/*
	 * Type specific configuration information.
	 */
	__vki_u64			config;

	union {
		__vki_u64		sample_period;
		__vki_u64		sample_freq;
	};

	__vki_u64			sample_type;
	__vki_u64			read_format;

	__vki_u64			disabled       :  1, /* off by default        */
					inherit	       :  1, /* children inherit it   */
					pinned	       :  1, /* must always be on PMU */
					exclusive      :  1, /* only group on PMU     */
					exclude_user   :  1, /* don't count user      */
					exclude_kernel :  1, /* ditto kernel          */
					exclude_hv     :  1, /* ditto hypervisor      */
					exclude_idle   :  1, /* don't count when idle */
					mmap           :  1, /* include mmap data     */
					comm	       :  1, /* include comm data     */
					freq           :  1, /* use freq, not period  */
					inherit_stat   :  1, /* per task counts       */
					enable_on_exec :  1, /* next exec enables     */
					task           :  1, /* trace fork/exit       */

					__reserved_1   : 50;

	__vki_u32			wakeup_events;	/* wakeup every n events */
	__vki_u32			__reserved_2;

	__vki_u64			__reserved_3;
};

/*--------------------------------------------------------------------*/
// From linux-2.6.32.4/include/linux/getcpu.h
/*--------------------------------------------------------------------*/

struct vki_getcpu_cache {
	unsigned long blob[128 / sizeof(long)];
};

//----------------------------------------------------------------------
// From linux-2.6.33.3/include/linux/input.h
//----------------------------------------------------------------------

/*
 * IOCTLs (0x00 - 0x7f)
 */

#define VKI_EVIOCGNAME(len)	_VKI_IOC(_VKI_IOC_READ, 'E', 0x06, len)		/* get device name */
#define VKI_EVIOCGPHYS(len)	_VKI_IOC(_VKI_IOC_READ, 'E', 0x07, len)		/* get physical location */
#define VKI_EVIOCGUNIQ(len)	_VKI_IOC(_VKI_IOC_READ, 'E', 0x08, len)		/* get unique identifier */

#define VKI_EVIOCGKEY(len)	_VKI_IOC(_VKI_IOC_READ, 'E', 0x18, len)		/* get global keystate */
#define VKI_EVIOCGLED(len)	_VKI_IOC(_VKI_IOC_READ, 'E', 0x19, len)		/* get all LEDs */
#define VKI_EVIOCGSND(len)	_VKI_IOC(_VKI_IOC_READ, 'E', 0x1a, len)		/* get all sounds status */
#define VKI_EVIOCGSW(len)	_VKI_IOC(_VKI_IOC_READ, 'E', 0x1b, len)		/* get all switch states */

#define VKI_EVIOCGBIT(ev,len)	_VKI_IOC(_VKI_IOC_READ, 'E', 0x20 + ev, len)	/* get event bits */

/*
 * Event types
 */

#define VKI_EV_SYN		0x00
#define VKI_EV_KEY		0x01
#define VKI_EV_REL		0x02
#define VKI_EV_ABS		0x03
#define VKI_EV_MSC		0x04
#define VKI_EV_SW		0x05
#define VKI_EV_LED		0x11
#define VKI_EV_SND		0x12
#define VKI_EV_REP		0x14
#define VKI_EV_FF		0x15
#define VKI_EV_PWR		0x16
#define VKI_EV_FF_STATUS	0x17
#define VKI_EV_MAX		0x1f
#define VKI_EV_CNT		(VKI_EV_MAX+1)

#endif // __VKI_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
