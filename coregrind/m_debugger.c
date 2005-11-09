
/*--------------------------------------------------------------------*/
/*--- Attaching a debugger.                           m_debugger.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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

#include "pub_core_basics.h"
#include "pub_core_threadstate.h"
#include "pub_core_clientstate.h"
#include "pub_core_debugger.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_options.h"


#define WIFSTOPPED(status) (((status) & 0xff) == 0x7f)
#define WSTOPSIG(status) (((status) & 0xff00) >> 8)

static Int ptrace_setregs(Int pid, VexGuestArchState* vex)
{
   struct vki_user_regs_struct regs;
#if defined(VGA_x86)
   regs.cs     = vex->guest_CS;
   regs.ss     = vex->guest_SS;
   regs.ds     = vex->guest_DS;
   regs.es     = vex->guest_ES;
   regs.fs     = vex->guest_FS;
   regs.gs     = vex->guest_GS;
   regs.eax    = vex->guest_EAX;
   regs.ebx    = vex->guest_EBX;
   regs.ecx    = vex->guest_ECX;
   regs.edx    = vex->guest_EDX;
   regs.esi    = vex->guest_ESI;
   regs.edi    = vex->guest_EDI;
   regs.ebp    = vex->guest_EBP;
   regs.esp    = vex->guest_ESP;
   regs.eflags = LibVEX_GuestX86_get_eflags(vex);
   regs.eip    = vex->guest_EIP;

   return VG_(ptrace)(VKI_PTRACE_SETREGS, pid, NULL, &regs);
#elif defined(VGA_amd64)
   regs.rax    = vex->guest_RAX;
   regs.rbx    = vex->guest_RBX;
   regs.rcx    = vex->guest_RCX;
   regs.rdx    = vex->guest_RDX;
   regs.rsi    = vex->guest_RSI;
   regs.rdi    = vex->guest_RDI;
   regs.rbp    = vex->guest_RBP;
   regs.rsp    = vex->guest_RSP;
   regs.r8     = vex->guest_R8;
   regs.r9     = vex->guest_R9;
   regs.r10    = vex->guest_R10;
   regs.r11    = vex->guest_R11;
   regs.r12    = vex->guest_R12;
   regs.r13    = vex->guest_R13;
   regs.r14    = vex->guest_R14;
   regs.r15    = vex->guest_R15;
   regs.eflags = LibVEX_GuestAMD64_get_rflags(vex);
   regs.rip    = vex->guest_RIP;

   return VG_(ptrace)(VKI_PTRACE_SETREGS, pid, NULL, &regs);
#elif defined(VGA_ppc32)
   Int rc = 0;

   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R0 * 4, vex->guest_GPR0);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R1 * 4, vex->guest_GPR1);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R2 * 4, vex->guest_GPR2);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R3 * 4, vex->guest_GPR3);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R4 * 4, vex->guest_GPR4);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R5 * 4, vex->guest_GPR5);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R6 * 4, vex->guest_GPR6);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R7 * 4, vex->guest_GPR7);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R8 * 4, vex->guest_GPR8);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R9 * 4, vex->guest_GPR9);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R10 * 4, vex->guest_GPR10);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R11 * 4, vex->guest_GPR11);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R12 * 4, vex->guest_GPR12);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R13 * 4, vex->guest_GPR13);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R14 * 4, vex->guest_GPR14);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R15 * 4, vex->guest_GPR15);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R16 * 4, vex->guest_GPR16);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R17 * 4, vex->guest_GPR17);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R18 * 4, vex->guest_GPR18);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R19 * 4, vex->guest_GPR19);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R20 * 4, vex->guest_GPR20);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R21 * 4, vex->guest_GPR21);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R22 * 4, vex->guest_GPR22);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R23 * 4, vex->guest_GPR23);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R24 * 4, vex->guest_GPR24);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R25 * 4, vex->guest_GPR25);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R26 * 4, vex->guest_GPR26);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R27 * 4, vex->guest_GPR27);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R28 * 4, vex->guest_GPR28);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R29 * 4, vex->guest_GPR29);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R30 * 4, vex->guest_GPR30);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_R31 * 4, vex->guest_GPR31);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_NIP * 4, vex->guest_CIA);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_CCR * 4,
                     LibVEX_GuestPPC32_get_CR(vex));
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_LNK * 4, vex->guest_LR);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_CTR * 4, vex->guest_CTR);
   rc |= VG_(ptrace)(VKI_PTRACE_POKEUSR, pid, VKI_PT_XER * 4,
                     LibVEX_GuestPPC32_get_XER(vex));
   return rc;
#else
#  error Unknown arch
#endif
}

/* Start debugger and get it to attach to this process.  Called if the
   user requests this service after an error has been shown, so she can
   poke around and look at parameters, memory, etc.  You can't
   meaningfully get the debugger to continue the program, though; to
   continue, quit the debugger.  */
void VG_(start_debugger) ( ThreadId tid )
{
  Int pid;

  if ((pid = VG_(fork)()) == 0) {
      VG_(ptrace)(VKI_PTRACE_TRACEME, 0, NULL, NULL);
      VG_(kill)(VG_(getpid)(), VKI_SIGSTOP);

   } else if (pid > 0) {
      Int status;
      Int res;

      if ((res = VG_(waitpid)(pid, &status, 0)) == pid &&
          WIFSTOPPED(status) && WSTOPSIG(status) == VKI_SIGSTOP &&
          ptrace_setregs(pid, &(VG_(threads)[tid].arch.vex)) == 0 &&
          VG_(kill)(pid, VKI_SIGSTOP) == 0 &&
          VG_(ptrace)(VKI_PTRACE_DETACH, pid, NULL, 0) == 0)
      {
         Char pidbuf[15];
         Char file[30];
         Char buf[100];
         Char *bufptr;
         Char *cmdptr;
         
         VG_(sprintf)(pidbuf, "%d", pid);
         VG_(sprintf)(file, "/proc/%d/fd/%d", pid, VG_(cl_exec_fd));
 
         bufptr = buf;
         cmdptr = VG_(clo_db_command);
         
         while (*cmdptr) {
            switch (*cmdptr) {
               case '%':
                  switch (*++cmdptr) {
                     case 'f':
                        VG_(memcpy)(bufptr, file, VG_(strlen)(file));
                        bufptr += VG_(strlen)(file);
                        cmdptr++;
                        break;
                  case 'p':
                     VG_(memcpy)(bufptr, pidbuf, VG_(strlen)(pidbuf));
                     bufptr += VG_(strlen)(pidbuf);
                     cmdptr++;
                     break;
                  default:
                     *bufptr++ = *cmdptr++;
                     break;
                  }
                  break;
               default:
                  *bufptr++ = *cmdptr++;
                  break;
            }
         }
         
         *bufptr++ = '\0';
  
         VG_(message)(Vg_UserMsg, "starting debugger with cmd: %s", buf);
         res = VG_(system)(buf);
         if (res == 0) {      
            VG_(message)(Vg_UserMsg, "");
            VG_(message)(Vg_UserMsg, 
                         "Debugger has detached.  Valgrind regains control.  We continue.");
         } else {
            VG_(message)(Vg_UserMsg, "Apparently failed!");
            VG_(message)(Vg_UserMsg, "");
         }
      }

      VG_(kill)(pid, VKI_SIGKILL);
      VG_(waitpid)(pid, &status, 0);
   }
}



/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
