
/*--------------------------------------------------------------------*/
/*--- A header file for making sense of syscalls.  Unsafe in the   ---*/
/*--- sense that we don't call any functions mentioned herein.     ---*/
/*---                                                  vg_unsafe.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

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

   The GNU General Public License is contained in the file LICENSE.
*/


/* These includes are only used for making sense of the args for
   system calls. */
#include <asm/unistd.h>   /* for system call numbers */
#include <sys/mman.h>     /* for PROT_* */
#include <sys/utsname.h>  /* for uname */
#include <sys/time.h>     /* for struct timeval & struct timezone */
#include <linux/net.h>    /* for the SYS_* constants */
#include <sys/resource.h> /* for struct rlimit */
#include <linux/shm.h>    /* for struct shmid_ds & struct ipc_perm */
#include <sys/socket.h>   /* for struct msghdr */
#include <sys/un.h>       /* for sockaddr_un */
#include <net/if.h>       /* for struct ifreq et al */
#include <net/if_arp.h>   /* for struct arpreq */
#include <net/route.h>    /* for struct rtentry */
#include <asm/ipc.h>      /* for struct ipc_kludge */
#include <linux/msg.h>    /* for struct msgbuf */
#include <linux/sem.h>    /* for struct sembuf */

#include <linux/isdn.h>   /* for ISDN ioctls */
#include <linux/module.h> /* for struct module */
#include <scsi/sg.h>      /* for the SG_* ioctls */
#include <sched.h>        /* for struct sched_param */
#include <linux/sysctl.h> /* for struct __sysctl_args */
#include <linux/cdrom.h>  /* for cd-rom ioctls */

#define __USE_LARGEFILE64
#include <sys/stat.h>     /* for struct stat */
#undef __USE_LARGEFILE64

#include <asm/ioctls.h>   /* for stuff for dealing with ioctl :( */
#include <sys/soundcard.h> /* for various soundcard ioctl constants :( */

#ifndef GLIBC_2_1
#  include <linux/rtc.h>   /* for RTC_* ioctls */
#endif

#include <termios.h>
#include <pty.h>

/* 2.2 stuff ... */
#include <sys/uio.h>

/* Both */
#include <utime.h>
#include <sys/times.h>    /* for struct tms */

/* 2.0 at least, for gid_t and loff_t */
#include <sys/types.h>

#include <sys/statfs.h>

#include <sys/sysinfo.h>

#include <sys/poll.h>


/*--------------------------------------------------------------------*/
/*--- end                                              vg_unsafe.h ---*/
/*--------------------------------------------------------------------*/
