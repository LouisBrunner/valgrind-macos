
/*--------------------------------------------------------------------*/
/*--- ARM/Linux-specific syscalls, etc.       arm-linux/syscalls.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Nicholas Nethercote
      njn25@cam.ac.uk

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
void VGA_(restart_syscall)(ThreadArchState *arch)
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

#define PRE(x,f) \
   static UInt arm_linux_##x##_flags = f; \
   static void arm_linux_##x##_before(ThreadId tid, ThreadState *tst)
#define POST(x) \
   static void arm_linux_##x##_after (ThreadId tid, ThreadState *tst)

#define SYSNO	PLATFORM_SYSCALL_NUM(tst->arch)    // in PRE(x)
#define res	PLATFORM_SYSCALL_RET(tst->arch)	   // in POST(x)
#define arg1	PLATFORM_SYSCALL_ARG1(tst->arch)
#define arg2	PLATFORM_SYSCALL_ARG2(tst->arch)
#define arg3	PLATFORM_SYSCALL_ARG3(tst->arch)
#define arg4	PLATFORM_SYSCALL_ARG4(tst->arch)
#define arg5	PLATFORM_SYSCALL_ARG5(tst->arch)
#define arg6	PLATFORM_SYSCALL_ARG6(tst->arch)

#define set_result(val) PLATFORM_SET_SYSCALL_RESULT(tst->arch, (val))

#define PRINT(format, args...)  \
   if (VG_(clo_trace_syscalls))        \
      VG_(printf)(format, ## args)

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
   PRINT("sys_clone ( %d, %p, %p, %p, %p )",arg1,arg2,arg3,arg4,arg5);
   // XXX: really not sure about the last two args... if they are really
   // there, we should do PRE_MEM_READs for both of them...
   PRE_REG_READ4(int, "clone",
                 unsigned long, flags, void *, child_stack,
                 int *, parent_tidptr, int *, child_tidptr);

   if (arg2 == 0 &&
       (arg1 == (VKI_CLONE_CHILD_CLEARTID|VKI_CLONE_CHILD_SETTID|VKI_SIGCHLD)
     || arg1 == (VKI_CLONE_PARENT_SETTID|VKI_SIGCHLD))) 
   {
      VGA_(gen_sys_fork_before)(tid, tst);
      set_result( VG_(do_syscall)(SYSNO, arg1, arg2, arg3, arg4, arg5) );
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

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The ARM/Linux syscall table
   ------------------------------------------------------------------ */

#define GENX_(const, name) \
   [const] = { &VGA_(gen_##name##_flags), VGA_(gen_##name##_before), NULL }
#define GENXY(const, name) \
   [const] = { &VGA_(gen_##name##_flags), VGA_(gen_##name##_before), \
                                          VGA_(gen_##name##_after) }

#define LINX_(const, name) \
   [const] = { &VGA_(linux_##name##_flags), VGA_(linux_##name##_before), NULL }
#define LINXY(const, name) \
   [const] = { &VGA_(linux_##name##_flags), VGA_(linux_##name##_before), \
                                            VGA_(linux_##name##_after) }
#define PLAX_(const, name) \
   [const] = { &arm_linux_##name##_flags, arm_linux_##name##_before, NULL }
#define PLAXY(const, name) \
   [const] = { &arm_linux_##name##_flags, arm_linux_##name##_before, \
                                          arm_linux_##name##_after }

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-arm/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on ARM (as per sys_call_table in linux/arch/arm/kernel/entry.S).
//
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

const struct SyscallTableEntry VGA_(syscall_table)[] = {
   //   (restart_syscall)                             // 0
   PLAX_(__NR_syscall,           sys_syscall),        // 113
   PLAX_(__NR_clone,             sys_clone),          // 120
};

const UInt VGA_(syscall_table_size) = 
            sizeof(VGA_(syscall_table)) / sizeof(VGA_(syscall_table)[0]);

#undef GENX_
#undef GENXY

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
