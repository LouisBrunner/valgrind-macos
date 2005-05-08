
/*--------------------------------------------------------------------*/
/*--- AMD64/Linux-specific syscall stuff.                          ---*/
/*---                                  priv_syscalls-amd64-linux.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn@valgrind.org

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

#ifndef __PRIV_SYSCALLS_AMD64_LINUX_H
#define __PRIV_SYSCALLS_AMD64_LINUX_H

// Accessors for the ThreadArchState
#define VGP_SYSCALL_NUM       guest_RAX
#define VGP_SYSCALL_ARG1      guest_RDI
#define VGP_SYSCALL_ARG2      guest_RSI
#define VGP_SYSCALL_ARG3      guest_RDX
#define VGP_SYSCALL_ARG4      guest_R10
#define VGP_SYSCALL_ARG5      guest_R8
#define VGP_SYSCALL_ARG6      guest_R9
#define VGP_SYSCALL_RET       guest_RAX

// Setting a syscall result
#define VGP_SET_SYSCALL_RESULT(regs, val)    ((regs).vex.guest_RAX = (val))

// For informing tools that a syscall result has been set.
#define VGP_TRACK_SYSCALL_RETVAL(zztid) \
   VG_TRACK( post_reg_write, Vg_CoreSysCall, zztid, O_SYSCALL_RET, sizeof(UWord) );

#endif   // __PRIV_SYSCALLS_AMD64_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/


