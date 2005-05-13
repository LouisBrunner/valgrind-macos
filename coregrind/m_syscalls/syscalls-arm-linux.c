
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.       arm-linux/syscalls.c ---*/
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

#include "core.h"


// See the comment accompanying the declaration of VGA_(thread_syscall)() in
// coregrind/core.h for an explanation of what this does, and why.
//
// XXX: this function and these variables should be assembly code!  See the
// x86 version.
const Addr VGA_(sys_before), VGA_(sys_restarted),
           VGA_(sys_after),  VGA_(sys_done);
void VGA_(do_thread_syscall)(UWord sys,
                             UWord arg1, UWord arg2, UWord arg3,
                             UWord arg4, UWord arg5, UWord arg6,
                             UWord *result, /*enum PXState*/Int *statep,
                             /*enum PXState*/Int poststate)
{
   I_die_here;
}


// Back up to restart a system call.
static void restart_syscall(ThreadArchState *arch)
{
   I_die_here;
#if 0
   arch->vex.guest_EIP -= 2;             // sizeof(int $0x80)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80 
   */
   {
      UChar *p = (UChar *)arch->vex.guest_EIP;
      
      if (p[0] != 0xcd || p[1] != 0x80)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x\n",
                      arch->vex.guest_EIP, p[0], p[1]); 

      vg_assert(p[0] == 0xcd && p[1] == 0x80);
   }
#endif
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for ARM/Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// coregrind/vg_syscalls.c for notes about how they work.

#define PRE(name, f)     PRE_TEMPLATE(static, arm_linux, name, f)
#define POST(name)      POST_TEMPLATE(static, arm_linux, name)

PRE(sys_syscall, Special)
{
   // Nb!!!
   //
   // __NR_syscall is a "higher-order syscall" on ARM;  it's all a bit
   // strange.  To implement this, you'll need to shuffle the args down, do
   // the same for the shadow args, and maybe some other stuff.
   VG_(printf)("__NR_syscall detected!");
   I_die_here;
}

PRE(sys_clone, Special)
{
   I_die_here;
   // XXX: maybe this clone stuff could be factored out
#if 0
   PRINT("sys_clone ( %d, %p, %p, %p, %p )",ARG1,ARG2,ARG3,ARG4,ARG5);
   // XXX: really not sure about the last two args... if they are really
   // there, we should do PRE_MEM_READs for both of them...
   PRE_REG_READ4(int, "clone",
                 unsigned long, flags, void *, child_stack,
                 int *, parent_tidptr, int *, child_tidptr);

   if (ARG2 == 0 &&
       (ARG1 == (VKI_CLONE_CHILD_CLEARTID|VKI_CLONE_CHILD_SETTID|VKI_SIGCHLD)
     || ARG1 == (VKI_CLONE_PARENT_SETTID|VKI_SIGCHLD))) 
   {
      VGA_(gen_sys_fork_before)(tid, tst);
      SET_RESULT( VG_(do_syscall5)(SYSNO, ARG1, ARG2, ARG3, ARG4, ARG5) );
      VGA_(gen_sys_fork_after) (tid, tst);
   } else {
      VG_(unimplemented)
         ("clone(): not supported by Valgrind.\n   "
          "We do support programs linked against\n   "
          "libpthread.so, though.  Re-run with -v and ensure that\n   "
          "you are picking up Valgrind's implementation of libpthread.so.");
   }
#endif
}

PRE(sys_ipc, Special)
{
   // XXX: the situation is complicated by the fact that ARM's ipc
   // super-syscall, which encompasses shmdt, shmat, getsem, etc, seems to
   // be the same (or at least similar?) to x86's, and so we want to avoid
   // duplicating the x86 wrapper here, since it's so big...
   I_die_here;
}

POST(sys_ipc)
{
   I_die_here;
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The ARM/Linux syscall table
   ------------------------------------------------------------------ */

// Macros for adding ARM/Linux-specific wrappers to the syscall table.  Note
// that ARM syscall numbers start at __NR_SYSCALL_BASE.
#define PLAX_(const, name) \
   SYS_WRAPPER_ENTRY_X_(arm_linux, const - __NR_SYSCALL_BASE, name) 
#define PLAXY(const, name) \
   SYS_WRAPPER_ENTRY_XY(arm_linux, const - __NR_SYSCALL_BASE, name) 

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-arm/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on ARM (as per sys_call_table in linux/arch/arm/kernel/entry.S).
//
// XXX: look at the x86-linux one to see how to do it.

const struct SyscallTableEntry VGA_(syscall_table)[] = {
   //   (restart_syscall)                             // 0
   GENX_(__NR_exit,              sys_exit),           // 1
   LINX_(__NR_mount,             sys_mount),          // 21
   PLAX_(__NR_syscall,           sys_syscall),        // 113
   PLAXY(__NR_ipc,               sys_ipc),            // 117
   PLAX_(__NR_clone,             sys_clone),          // 120
};

const UInt VGA_(syscall_table_size) = 
            sizeof(VGA_(syscall_table)) / sizeof(VGA_(syscall_table)[0]);

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
