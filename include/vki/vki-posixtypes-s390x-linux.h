
/*--------------------------------------------------------------------*/
/*--- s390x/Linux-specific kernel interface: posix types.          ---*/
/*---                                 vki-posixtypes-s390x-linux.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2017

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Contributed by Florian Krohm and Volker Sameske */

#ifndef __VKI_POSIXTYPES_S390X_LINUX_H
#define __VKI_POSIXTYPES_S390X_LINUX_H

//----------------------------------------------------------------------
// From linux-2.6.16.60/include/asm-s390/posix_types.h
//----------------------------------------------------------------------

typedef long            __vki_kernel_off_t;
typedef int             __vki_kernel_pid_t;
typedef unsigned long   __vki_kernel_size_t;
typedef long            __vki_kernel_time_t;
typedef long            __vki_kernel_suseconds_t;
typedef long            __vki_kernel_clock_t;
typedef int		__vki_kernel_timer_t;
typedef int		__vki_kernel_clockid_t;
typedef int             __vki_kernel_daddr_t;
typedef char *          __vki_kernel_caddr_t;
typedef unsigned short	__vki_kernel_uid16_t;
typedef unsigned short	__vki_kernel_gid16_t;
typedef long long       __vki_kernel_loff_t;

typedef unsigned int    __vki_kernel_ino_t;
typedef unsigned int    __vki_kernel_mode_t;
typedef unsigned int    __vki_kernel_nlink_t;
typedef int             __vki_kernel_ipc_pid_t;
typedef unsigned int    __vki_kernel_uid_t;
typedef unsigned int    __vki_kernel_gid_t;
typedef long            __vki_kernel_ssize_t;
typedef long            __vki_kernel_ptrdiff_t;
typedef unsigned long   __vki_kernel_sigset_t;      /* at least 32 bits */
typedef __vki_kernel_uid_t __vki_kernel_old_uid_t;
typedef __vki_kernel_gid_t __vki_kernel_old_gid_t;
typedef __vki_kernel_uid_t __vki_kernel_uid32_t;
typedef __vki_kernel_gid_t __vki_kernel_gid32_t;
typedef unsigned short __vki_kernel_old_dev_t;

typedef struct {
   int val[2];
} __vki_kernel_fsid_t;

#endif // __VKI_POSIXTYPES_S390X_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
