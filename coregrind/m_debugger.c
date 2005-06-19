
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
#include "pub_core_debugger.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_options.h"

// We can remove these easily by implementing our own VG_(ptrace)() and
// VG_(fork)().
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <unistd.h>

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

   return ptrace(PTRACE_SETREGS, pid, NULL, &regs);
#elif defined(VGA_amd64)
   I_die_here;
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

   if ((pid = fork()) == 0) {
      ptrace(PTRACE_TRACEME, 0, NULL, NULL);
      VG_(kill)(VG_(getpid)(), VKI_SIGSTOP);

   } else if (pid > 0) {
      Int status;
      Int res;

      if ((res = VG_(waitpid)(pid, &status, 0)) == pid &&
          WIFSTOPPED(status) && WSTOPSIG(status) == SIGSTOP &&
          ptrace_setregs(pid, &(VG_(threads)[tid].arch.vex)) == 0 &&
          VG_(kill)(pid, SIGSTOP) == 0 &&
          ptrace(PTRACE_DETACH, pid, NULL, 0) == 0)
      {
         Char pidbuf[15];
         Char file[30];
         Char buf[100];
         Char *bufptr;
         Char *cmdptr;
         
         VG_(sprintf)(pidbuf, "%d", pid);
         VG_(sprintf)(file, "/proc/%d/fd/%d", pid, VG_(clexecfd));
 
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
