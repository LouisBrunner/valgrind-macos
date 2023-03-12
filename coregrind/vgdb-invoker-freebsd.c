/*--------------------------------------------------------------------*/
/*--- Implementation of vgdb invoker subsystem via ptrace() calls. ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Philippe Waroquiers
   Copyright (C) 2021-2022 Paul Floyd
      pjfloyd@wanadoo.fr

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

#include "config.h"

#include "vgdb.h"
#include "pub_core_threadstate.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/ptrace.h>
#include <sys/time.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <sys/uio.h>
#include <elf.h>
#include <sys/procfs.h>

/*
 * This file is largely a copy of vgdb-invoker-ptrace.c
 *
 * Sadly, though ptrace exists on most unix-like systems,
 * no two versions are the same.
 *
 * The main two differences are that
 * - FreeBSD ptrace works at the process level, not lwps
 *   so just attaching to the main  pid stops everything
 *   (no need to read memory to get lwpids and stop all of the
 *   threads)
 * - Reading registers is a lot simpler.
 *
 * Known limitations:
 * gdbserver_tests/nlcontrolc has different behaviour
 * becase attaching causes the select() syscall of main
 * to be interrupted. This seems to be a "feature" of
 * ptrace on FreeBSD so I doubt it can be fixed.
 *
 */

// 32-bit or 64-bit wide, depending on primary architecture.
typedef Addr  CORE_ADDR;
typedef int  PTRACE_XFER_TYPE;
typedef caddr_t PTRACE_ARG3_TYPE;

// if > 0, pid for which registers have to be restored.
// if == 0, means we have not yet called setregs (or have already
// restored the registers).
static int pid_of_save_regs = 0;
/* True if we have continued pid_of_save_regs after PT_ATTACH. */
static Bool pid_of_save_regs_continued = False;
// When setregs has been called to change the registers of pid_of_save_regs,
// vgdb cannot transmit the signals intercepted during ptrace.
// So, we queue them, and will deliver them when detaching.
// See function waitstopped for more info.
static int signal_queue_sz = 0;
static siginfo_t *signal_queue;

/* True when loss of connection indicating that the Valgrind
   process is dying. */
static Bool dying = False;

/* Copy LEN bytes of data from vgdb memory at MYADDR
   to valgrind memory at MEMADDR.
   On failure (cannot write the valgrind memory)
   returns the value of errno.  */
__attribute__((unused)) /* not used on all platforms */
static
int ptrace_write_memory (pid_t inferior_pid, CORE_ADDR memaddr,
                         const void *myaddr, size_t len)
{
   size_t i;
   /* Round starting address down to longword boundary.  */
   CORE_ADDR addr = memaddr & -(CORE_ADDR) sizeof (PTRACE_XFER_TYPE);
   /* Round ending address up; get number of longwords that makes.  */
   size_t count
      = (((memaddr + len) - addr) + sizeof (PTRACE_XFER_TYPE) - 1)
      / sizeof (PTRACE_XFER_TYPE);
   /* Allocate buffer of that many longwords.  */
   PTRACE_XFER_TYPE *buffer
      = (PTRACE_XFER_TYPE *) alloca (count * sizeof (PTRACE_XFER_TYPE));

   if (debuglevel >= 1) {
      DEBUG (1, "Writing ");
      for (i = 0; i < len; i++) {
         PDEBUG (1, "%02x", ((const unsigned char*)myaddr)[i]);
      }
      PDEBUG(1, " to %p\n", (void *) memaddr);
   }

   /* Fill start and end extra bytes of buffer with existing memory data.  */

   buffer[0] = ptrace (PT_READ_I, inferior_pid,
                       (PTRACE_ARG3_TYPE) addr, 0);

   if (count > 1) {
      buffer[count - 1]
         = ptrace (PT_READ_I, inferior_pid,
                   (PTRACE_ARG3_TYPE) (addr + (count - 1)
                                       * sizeof (PTRACE_XFER_TYPE)),
                   0);
   }

   /* Copy data to be written over corresponding part of buffer */

   memcpy ((char *) buffer + (memaddr & (sizeof (PTRACE_XFER_TYPE) - 1)),
           myaddr, len);

   /* Write the entire buffer.  */

   for (i = 0; i < count; i++, addr += sizeof (PTRACE_XFER_TYPE)) {
      errno = 0;
      ptrace (PT_WRITE_I, inferior_pid,
              (PTRACE_ARG3_TYPE) addr, buffer[i]);
      if (errno) {
         return errno;
      }
   }

   return 0;
}

static
char *status_image (int status)
{
   static char result[256];  // large enough
   int sz = 0;
#define APPEND(...) sz += snprintf (result+sz, 256 - sz - 1, __VA_ARGS__)

   result[0] = 0;

   if (WIFEXITED(status)) {
      APPEND ("WIFEXITED %d ", WEXITSTATUS(status));
   }

   if (WIFSIGNALED(status)) {
      APPEND ("WIFSIGNALED %d ", WTERMSIG(status));
      if (WCOREDUMP(status)) {
         APPEND ("WCOREDUMP ");
      }
   }

   if (WIFSTOPPED(status))
      APPEND ("WIFSTOPPED %d ", WSTOPSIG(status));

#ifdef WIFCONTINUED
   if (WIFCONTINUED(status)) {
      APPEND ("WIFCONTINUED ");
   }
#endif

   return result;
#undef APPEND
}

/* Wait till the process pid is reported as stopped with signal_expected.
   If other signal(s) than signal_expected are received, waitstopped
   will pass them to pid, waiting for signal_expected to stop pid.
   Returns True when process is in stopped state with signal_expected.
   Returns False if a problem was encountered while waiting for pid
   to be stopped.

   If pid is reported as being dead/exited, waitstopped will return False.
*/
static
Bool waitstopped (pid_t pid, int signal_expected, const char *msg)
{
   pid_t p;
   int status = 0;
   int signal_received;
   int res;

   while (1) {
      DEBUG(1, "waitstopped %s before waitpid signal_expected %d\n",
            msg, signal_expected);
      p = waitpid(pid, &status, 0); /* PJF options was __WALL */
      DEBUG(1, "after waitpid pid %d p %d status 0x%x %s\n", pid, p,
            status, status_image (status));
      if (p != pid) {
         ERROR(errno, "%s waitpid pid %d in waitstopped %d status 0x%x %s\n",
               msg, pid, p, status, status_image (status));
         return False;
      }

      /* The process either exited or was terminated by a (fatal) signal. */
      if (WIFEXITED(status) || WIFSIGNALED(status)) {
         shutting_down = True;
         return False;
      }

      assert (WIFSTOPPED(status));
      signal_received = WSTOPSIG(status);
      if (signal_received == signal_expected) {
         break;
      }

      /* pid received a signal which is not the signal we are waiting for.
         If we have not (yet) changed the registers of the inferior
         or we have (already) reset them, we can transmit the signal.

         If we have already set the registers of the inferior, we cannot
         transmit the signal, as this signal would arrive when the
         gdbserver code runs. And valgrind only expects signals to
         arrive in a small code portion around
         client syscall logic, where signal are unmasked (see e.g.
         m_syswrap/syscall-x86-linux.S ML_(do_syscall_for_client_WRK).

         As ptrace is forcing a call to gdbserver by jumping
         'out of this region', signals are not masked, but
         will arrive outside of the allowed/expected code region.
         So, if we have changed the registers of the inferior, we
         rather queue the signal to transmit them when detaching,
         after having restored the registers to the initial values. */
      if (pid_of_save_regs) {
         siginfo_t *newsiginfo;
         struct ptrace_lwpinfo new_lwpinfo;

         // realloc a bigger queue, and store new signal at the end.
         // This is not very efficient but we assume not many sigs are queued.
         if (signal_queue_sz >= 64) {
            DEBUG(0, "too many queued signals while waiting for SIGSTOP\n");
            return False;
         }
         signal_queue_sz++;
         signal_queue = vrealloc(signal_queue,
                                 sizeof(siginfo_t) * signal_queue_sz);
         newsiginfo = signal_queue + (signal_queue_sz - 1);

         res = ptrace (PT_LWPINFO, pid, (caddr_t)&new_lwpinfo, sizeof(new_lwpinfo));
         *newsiginfo = new_lwpinfo.pl_siginfo;
         if (res != 0) {
            ERROR(errno, "PT_LWPINFO failed: signal lost !!!!\n");
            signal_queue_sz--;
         } else {
            DEBUG(1, "waitstopped PTRACE_CONT, queuing signal %d"
                  " si_signo %d si_pid %d\n",
                  signal_received, newsiginfo->si_signo, newsiginfo->si_pid);
         }
         res = ptrace (PT_CONTINUE, pid, (caddr_t)1, 0);
      } else {
         DEBUG(1, "waitstopped PT_CONTINUE with signal %d\n", signal_received);
         res = ptrace (PT_CONTINUE, pid, (caddr_t)1, signal_received);
      }
      if (res != 0) {
         ERROR(errno, "waitstopped PTRACE_CONT\n");
         return False;
      }
   }

   return True;
}

/* Stops the given pid, wait for the process to be stopped.
   Returns True if successful, False otherwise.
   msg is used in tracing and error reporting. */
static
Bool stop (pid_t pid, const char *msg)
{
   long res;

   DEBUG(1, "%s SIGSTOP pid %d\n", msg, pid);
   res = kill (pid, SIGSTOP);
   if (res != 0) {
      ERROR(errno, "%s SIGSTOP pid %d %ld\n", msg, pid, res);
      return False;
   }

   return waitstopped (pid, SIGSTOP, msg);

}

/* Attaches to given pid, wait for the process to be stopped.
   Returns True if successful, False otherwise.
   msg is used in tracing and error reporting. */
static
Bool attach (pid_t pid, const char *msg)
{
   int res;
   static Bool output_error = True;
   static Bool initial_attach = True;
   // For a ptrace_scope protected system, we do not want to output
   // repetitively attach error. We will output once an error
   // for the initial_attach. Once the 1st attach has succeeded, we
   // again show all errors.

   DEBUG(1, "%s PT_ATTACH pid %d\n", msg, pid);
   res = ptrace (PT_ATTACH, pid, 0, 0);
   if (res != 0) {
      if (output_error || debuglevel > 0) {
         ERROR(errno, "%s PT_ATTACH pid %d %d\n", msg, pid, res);
         if (initial_attach) {
            output_error = False;
         }
      }
      return False;
   }

   initial_attach = False;
   output_error = True;
   return waitstopped(pid, SIGSTOP, msg);
}

static
void detach_from_all_threads (pid_t pid)
{
   long res = ptrace (PT_DETACH, pid, NULL, 0);

   if (res != 0) {
      ERROR(errno, "PT_DETACH pid %d res %ld\n",
            pid, res);
   }
}

static struct reg reg_save;

/* Get the registers from pid into regs.
   Returns True if all ok, otherwise False. */
static
Bool getregs (pid_t pid, struct reg *regs)
{
   if (ptrace(PT_GETREGS, pid, (caddr_t)regs, 0) < 0) {
       return False;
   }
   return True;
}

/* Set the registers of pid to regs.
   Returns True if all ok, otherwise False. */
static
Bool setregs (pid_t pid, struct reg *regs)
{
    if (ptrace(PT_SETREGS, pid, (caddr_t)regs, 0) < 0) {
        return False;
    }
    return True;
}

/* Restore the registers to the saved value, then detaches from all threads */
static
void restore_and_detach (pid_t pid)
{
   int res;

   DEBUG(1, "restore_and_detach pid %d pid_of_save_regs %d\n",
         pid, pid_of_save_regs);

   if (pid_of_save_regs) {
      /* In case the 'main pid' has been continued, we need to stop it
         before resetting the registers. */
      if (pid_of_save_regs_continued) {
         pid_of_save_regs_continued = False;
         if (!stop(pid_of_save_regs, "sigstop before reset regs")) {
            DEBUG(0, "Could not sigstop before reset");
         }
      }

      DEBUG(1, "setregs restore registers pid %d\n", pid_of_save_regs);
      if (!setregs(pid_of_save_regs, &reg_save)) {
         ERROR(errno, "setregs restore registers pid %d after cont\n",
               pid_of_save_regs);
      }

      /* Now, we transmit all the signals we have queued. */
      if (signal_queue_sz > 0) {
         int i;
         for (i = 0; i < signal_queue_sz; i++) {
            DEBUG(1, "PTRACE_CONT to transmit queued signal %d\n",
                  signal_queue[i].si_signo);
            res = ptrace (PT_CONTINUE, pid_of_save_regs, (caddr_t)1,
                          signal_queue[i].si_signo);
            if (res != 0) {
               ERROR(errno, "PT_CONTINUE with signal %d\n",
                     signal_queue[i].si_signo);
            }
            if (!stop(pid_of_save_regs, "sigstop after transmit sig")) {
               DEBUG(0, "Could not sigstop after transmit sig");
            }
         }
         free (signal_queue);
         signal_queue = NULL;
         signal_queue_sz = 0;
      }
      pid_of_save_regs = 0;
   } else {
      DEBUG(1, "PTRACE_SETREGS restore registers: no pid\n");
   }
   if (signal_queue) {
      ERROR (0, "One or more signals queued were not delivered. "
             "First signal: %d\n", signal_queue[0].si_signo);
   }
   detach_from_all_threads(pid);
}

Bool invoker_invoke_gdbserver (pid_t pid)
{
   long res;
   Bool stopped;
   struct reg reg_mod;
   Addr sp __attribute__((unused)); // Not used on all platforms.

   /* A specific int value is passed to invoke_gdbserver, to check
      everything goes according to the plan. */
   const int check = 0x8BADF00D; // ate bad food.

   const Addr bad_return = 0;
   // A bad return address will be pushed on the stack.
   // The function invoke_gdbserver cannot return. If ever it returns, a NULL
   // address pushed on the stack should ensure this is detected.

   /* Not yet attached. If problem, vgdb can abort,
      no cleanup needed. */

   DEBUG(1, "attach to 'main' pid %d\n", pid);
   if (!attach(pid, "attach main pid")) {
      ERROR(0, "error attach main pid %d\n", pid);
      return False;
   }

   /* Now, we are attached. If problem, detach and return. */

   DEBUG(1, "calling getregs\n");

   if (!getregs(pid, &reg_mod)) {
      detach_from_all_threads(pid);
      return False;
   }
   reg_save = reg_mod;

   DEBUG(1, "getregs call succeeded\n");

#if defined(VGA_x86)
   sp = reg_mod.r_esp;
#elif defined(VGA_amd64)
   sp = reg_mod.r_rsp;
   if (shared32 != NULL) {
     /* 64bit vgdb speaking with a 32bit executable.
        To have system call restart properly, we need to sign extend rax.
        For more info:
        web search '[patch] Fix syscall restarts for amd64->i386 biarch'
        e.g. http://sourceware.org/ml/gdb-patches/2009-11/msg00592.html */
     *(long *)&reg_save.r_rax = *(int*)&reg_save.r_rax;
     DEBUG(1, "Sign extending %8.8lx to %8.8lx\n",
           reg_mod.r_rax, reg_save.r_rax);
   }
#else
   I_die_here : (sp) architecture missing in vgdb-invoker-freebsd.c
#endif


   // the magic below is derived from spying what gdb sends to
   // the (classical) gdbserver when invoking a C function.
   if (shared32 != NULL) {
      // vgdb speaking with a 32bit executable.
#if   defined(VGA_x86) || defined(VGA_amd64)
      const int regsize = 4;
      int rw;
      /* push check arg on the stack */
      sp = sp - regsize;
      DEBUG(1, "push check arg ptrace_write_memory\n");
      assert(regsize == sizeof(check));
      rw = ptrace_write_memory(pid, sp,
                               &check,
                               regsize);
      if (rw != 0) {
         ERROR(rw, "push check arg ptrace_write_memory\n");
         detach_from_all_threads(pid);
         return False;
      }

      sp = sp - regsize;
      DEBUG(1, "push bad_return return address ptrace_write_memory\n");
      // Note that for a 64 bits vgdb, only 4 bytes of NULL bad_return
      // are written.
      rw = ptrace_write_memory(pid, sp,
                               &bad_return,
                               regsize);
      if (rw != 0) {
         ERROR(rw, "push bad_return return address ptrace_write_memory\n");
         detach_from_all_threads(pid);
         return False;
      }
#if   defined(VGA_x86)
      /* set ebp, esp, eip and orig_eax to invoke gdbserver */
      // compiled in 32bits, speaking with a 32bits exe
      reg_mod.r_ebp = sp; // bp set to sp
      reg_mod.r_esp = sp;
      reg_mod.r_eip = shared32->invoke_gdbserver;
#elif defined(VGA_amd64)
      /* set ebp, esp, eip and orig_eax to invoke gdbserver */
      // compiled in 64bits, speaking with a 32bits exe
      reg_mod.r_rbp = sp; // bp set to sp
      reg_mod.r_rsp = sp;
      reg_mod.r_rip = shared32->invoke_gdbserver;
#else
      I_die_here : not x86 or amd64 in x86/amd64 section/
#endif

#else
      I_die_here : architecture missing in vgdb-invoker-freebsd.c
#endif
      }

   else if (shared64 != NULL) {
#if defined(VGA_x86)
      assert(0); // cannot vgdb a 64 bits executable with a 32 bits exe
#elif defined(VGA_amd64)
      // vgdb speaking with a 64 bit executable.
      const int regsize = 8;
      int rw;

      /* give check arg in rdi */
      reg_mod.r_rdi = check;

      /* push return address on stack : return to breakaddr */
      sp &= ~0xf; // keep the stack aligned on 16 bytes ...
      //sp = sp - 128; // do not touch the amd64 redzone
      sp = sp - regsize;
      DEBUG(1, "push bad_return return address ptrace_write_memory\n");
      rw = ptrace_write_memory(pid, sp,
                               &bad_return,
                               sizeof(bad_return));
      if (rw != 0) {
         ERROR(rw, "push bad_return return address ptrace_write_memory\n");
         detach_from_all_threads(pid);
         return False;
      }

      /* set rbp, rsp, rip and orig_rax to invoke gdbserver */
      reg_mod.r_rbp = sp; // bp set to sp
      reg_mod.r_rsp = sp;
      reg_mod.r_rip = shared64->invoke_gdbserver;

#else
      I_die_here: architecture missing in vgdb-invoker-freebsd.c
#endif
   }
   else {
      assert(0);
   }

   DEBUG(1, "calling setregs\n");

   if (!setregs(pid, &reg_mod)) {
      detach_from_all_threads(pid);
      return False;
   }

   DEBUG(1, "setregs succeeded\n");

   /* Now that we have modified the registers, we set
      pid_of_save_regs to indicate that restore_and_detach
      must restore the registers in case of cleanup. */
   pid_of_save_regs = pid;
   pid_of_save_regs_continued = False;


   /* We PTRACE_CONT-inue pid.
      Either gdbserver will be invoked directly (if all
      threads are interruptible) or gdbserver will be
      called soon by the scheduler. In the first case,
      pid will stop on the break inserted above when
      gdbserver returns. In the 2nd case, the break will
      be encountered directly. */
   DEBUG(1, "PT_CONTINUE to invoke\n");
   /* an address of 1 means continue without modifying IP
      since we already set IP above there is no need to set it here */
   res = ptrace (PT_CONTINUE, pid, (caddr_t)1, 0);
   if (res != 0) {
      ERROR(errno, "PT_CONTINUE\n");
      restore_and_detach(pid);
      return False;
   }
   pid_of_save_regs_continued = True;
   /* Wait for SIGSTOP generated by m_gdbserver.c give_control_back_to_vgdb */
   stopped = waitstopped (pid, SIGSTOP,
                          "waitpid status after PTRACE_CONT to invoke");
   if (stopped) {
      /* Here pid has properly stopped on the break. */
      pid_of_save_regs_continued = False;
      restore_and_detach(pid);
      return True;
   }
   /* Whatever kind of problem happened. We shutdown. */
   shutting_down = True;
   return False;
}

void invoker_cleanup_restore_and_detach(void *v_pid)
{
   DEBUG(1, "invoker_cleanup_restore_and_detach dying: %d\n", dying);
   if (!dying) {
      restore_and_detach(*(int*)v_pid);
   }
}

void invoker_restrictions_msg(void)
{
}

void invoker_valgrind_dying(void)
{
   /* Avoid messing up with registers of valgrind when it is dying. */
   pid_of_save_regs_continued = False;
   dying = True;
}
