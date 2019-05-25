/*--------------------------------------------------------------------*/
/*--- Implementation of vgdb invoker subsystem on Solaris             */
/*                      via /proc filesystem and control messages. ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2014-2017 Ivo Raisr <ivosh@ivosh.net>

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

/* This module implements vgdb-invoker subsystem as per vgdb.h
   on Solaris. It differs significantly from the other ptrace-based
   implementation found in vgdb-invoker-ptrace.c. However the goal
   is the same - to work on the following scenario:

   - A valgrind process (referred to also as an inferior process)
     is remotely debugged with gdb.
   - All threads of the inferior process are stuck in blocking
     syscalls.
   - Therefore no thread can process packets received from gdb.

   When module vgdb.c detects this situation then it calls
   function invoker_invoke_gdbserver() within the context of
   invoke_gdbserver_in_valgrind_thread thread. The steps of
   interaction between vgdb and m_gdbserver module are as follows:

   1. Function invoker_invoke_gdbserver() attaches to the inferior
      process and stops all threads.
   2. It gets registers of the first thread and modifies them
      and the stack so that a call to "invoke_gdbserver" function
      is arranged along with a function parameter.
   3. Then it creates an agent thread within the inferior process
      with these modified registers and waits until the agent thread
      exits.
   4. Meanwhile in the inferior process function
      VG_(invoke_gdbserver)() is invoked within the context of the
      agent thread; all other threads are still stopped.
   5. The agent thread processes packets from gdb relayed by vgdb.
   6. Eventually processing is finished and the agent thread exits
      in function give_control_back_to_vgdb().
   7. vgdb then detaches from the inferior process and thus resumes
      all the stopped threads.
 */

#include "vgdb.h"

#include <assert.h>
#include <errno.h>
#include <string.h>

typedef Addr CORE_ADDR;

typedef struct {
   long cmd;
   union {
       long flags;
       prgregset_t regs;
   } arg;
} ctl_t;

/* Process control file /proc/<pid>/ctl.
   Once this file is closed, PR_RLC flag takes effect and
   inferior process resumes automatically. */
static int ctl_fd = -1;

/* Copy LEN bytes of data from vgdb memory at MYADDR
   to valgrind memory at MEMADDR.
   On failure (cannot write the valgrind memory)
   returns the value of errno. */
static int write_memory(pid_t pid, CORE_ADDR memaddr,
                        const void *myaddr, size_t len)
{
   char procname[PATH_MAX];
   snprintf(procname, sizeof(procname), "/proc/%d/as", pid);

   /* Open the process address-space file. */
   int as_fd = open(procname, O_WRONLY, 0);
   if (as_fd < 0) {
      int error = errno;
      ERROR(error, "Failed to open %s.\n", procname);
      return error;
   }

   if (debuglevel >= 1) {
      DEBUG(1, "Writing bytes '");
      size_t i;
      for (i = 0; i < len; i++)
         PDEBUG(1, "%02x", ((const unsigned char *) myaddr)[i]);
      PDEBUG(1, "' to address %#lx.\n", memaddr);
   }

   ssize_t written = pwrite(as_fd, myaddr, len, memaddr);
   if ((written < 0) || (written != len)) {
      int error = errno;
      ERROR(error, "Failed to write to file %s, memory block of %zu"
            " bytes at %#lx to %#lx.\n",
            procname, len, (Addr) myaddr, memaddr);
      close(as_fd);
      return error;
   }

   DEBUG(1, "Written ok.\n");
   close(as_fd);
   return 0;
}

/* Attaches to a process identified by pid and stops all threads. */
static Bool attach(pid_t pid)
{
   char procname[PATH_MAX];
   snprintf(procname, sizeof(procname), "/proc/%d/ctl", pid);

   DEBUG(1, "Attaching to pid %d.\n", pid);

   /* Open the process control file. */
   ctl_fd = open(procname, O_WRONLY, 0);
   if (ctl_fd < 0) {
      ERROR(errno, "Failed to open %s.\n", procname);
      return False;
   }

   DEBUG(1, "Setting run-on-last-close-flag (PR_RLC) to pid %d.\n", pid);

   /* Set run-on-last-close flag. */
   ctl_t ctl;
   ctl.cmd = PCSET;
   ctl.arg.flags = PR_RLC;
   size_t bytes = sizeof(ctl.cmd) + sizeof(ctl.arg.flags);
   ssize_t written = write(ctl_fd, (void *) &ctl, bytes);
   if ((written < 0) || (written != bytes)) {
      ERROR(errno, "Failed to write to ctl_fd: PCSET + PR_RLC.\n");
      return False;
   }

   DEBUG(1, "Stopping process %d.\n", pid);

   /* Stop the whole process - all threads. */
   ctl.cmd = PCSTOP;
   bytes = sizeof(ctl.cmd);
   written = write(ctl_fd, (void *) &ctl, bytes);
   if ((written < 0) || (written != bytes)) {
      ERROR(errno, "Failed to write to ctl_fd: PCSTOP.\n");
      return False;
   }

   DEBUG(1, "Process %d stopped.\n", pid);

   /* Now confirm it is actually the case. */
   snprintf(procname, sizeof(procname), "/proc/%d/status", pid);
   int status_fd = open(procname, O_RDONLY, 0);
   if (status_fd < 0) {
      ERROR(errno, "Failed to open %s.\n", procname);
      return False;
   }

   pstatus_t pstatus;
   ssize_t nread = read(status_fd, &pstatus, sizeof(pstatus));
   if ((nread < 0) || (nread != sizeof(pstatus))) {
      ERROR(errno, "Failed to read from %s.\n", procname);
      close(status_fd);
      return False;
   }

   if (pstatus.pr_flags & PR_RLC) {
      DEBUG(2, "Process %d has run-on-last-close flag set. Good.\n", pid);
   } else {
      ERROR(0, "Process %d does not have run-on-last-close flag set!\n", pid);
      close(status_fd);
      return False;
   }

   if (pstatus.pr_lwp.pr_flags & PR_STOPPED) {
      DEBUG(3, "Process %d seems to be stopped. Good.\n", pid);
   } else {
      ERROR(0, "Process %d is not stopped!\n", pid);
      close(status_fd);
      return False;
   }

   close(status_fd);
   return True;
}

static void detach(pid_t pid)
{
   if (ctl_fd != -1) {
      close(ctl_fd);
      ctl_fd = -1;
   }

   DEBUG(1, "Detached from pid %d.\n", pid);
}

/* Gets the registers of the first thread. */
static Bool get_regs(pid_t pid, prgregset_t *regs)
{
   char procname[PATH_MAX];
   snprintf(procname, sizeof(procname), "/proc/%d/lwp/1/lwpstatus", pid);

   DEBUG(1, "Getting registers from the first thread of process %d.\n", pid);

   /* Open the first thread's status file. */
   int status_fd = open(procname, O_RDONLY, 0);
   if (status_fd < 0) {
      ERROR(errno, "Failed to open file %s.\n", procname);
      return False;
   }

   lwpstatus_t status;
   ssize_t bytes = read(status_fd, &status, sizeof(status));
   if ((bytes < 0) || (bytes != sizeof(status))) {
      ERROR(errno, "Failed to read from %s.\n", procname);
      close(status_fd);
      return False;
   }

   DEBUG(3, "Registers of thread %d from process %d: ", status.pr_lwpid, pid);
   unsigned int i;
   for (i = 0; i < _NGREG; i++) {
      PDEBUG(3, "%u: %#lx, ", i, (unsigned long) status.pr_reg[i]);
   }
   PDEBUG(3, "\n");

   memcpy(regs, &status.pr_reg, sizeof(prgregset_t));
   close(status_fd);
   return True;
}

/* Modifies the register set so that a new stack frame is created
   for "invoke_gdbserver" function with an extra argument.
   The argument is written to the stack of the first thread.
 */
static Bool setup_stack_frame(pid_t pid, prgregset_t *regs)
{
   DEBUG(1, "Setting up new stack frame of process %d.\n", pid);

   /* A specific int value is passed to invoke_gdbserver(), to check
      everything goes according to the plan. */
   const int check = 0x8BADF00D; // ate bad food.

   /* A bad return address will be pushed on the stack.
      Function invoke_gdbserver() cannot return. If it ever returns,
      a NULL address pushed on the stack should ensure this is
      detected. */
   const Addr bad_return = 0;

#if defined(VGA_x86)
   Addr sp = (*regs)[UESP];
#elif defined(VGA_amd64)
   Addr sp = (*regs)[REG_RSP];
#else
   I_die_here : (sp) architecture missing in vgdb-invoker-solaris.c
#endif

   if (shared32 != NULL) {
      /* vgdb speaking with a 32bit executable. */
#if   defined(VGA_x86) || defined(VGA_amd64)
      const size_t regsize = 4;

      /* Push check argument on the stack - according to C/ia32 ABI. */
      sp = sp - regsize;
      DEBUG(1, "Pushing check argument to process %d memory.\n", pid);
      assert(regsize == sizeof(check));
      int error = write_memory(pid, sp, &check, regsize);
      if (error != 0) {
         ERROR(error, "Failed to push check argument to process %d memory.\n",
                      pid);
         detach(pid);
         return False;
      }

      sp = sp - regsize;
      DEBUG(1, "Pushing bad_return return address to process %d memory.\n",
               pid);
      /* Note that even for a 64 bits vgdb, only 4 bytes
         of NULL bad_return are written. */
      error = write_memory(pid, sp, &bad_return, regsize);
      if (error != 0) {
         ERROR(error, "Failed to push bad_return return address to process %d "
                      "memory.\n", pid);
         detach(pid);
         return False;
      }

#if   defined(VGA_x86)
      /* Set EBP, ESP, EIP to invoke gdbserver.
         vgdb is 32bits, speaking with a 32bits process. */
      (*regs)[EBP] = sp; // bp set to sp
      (*regs)[UESP] = sp;
      (*regs)[EIP] = shared32->invoke_gdbserver;
#elif defined(VGA_amd64)
      /* Set RBP, RSP, RIP to invoke gdbserver.
         vgdb is 64bits, speaking with a 32bits process. */
      (*regs)[REG_RBP] = sp; // bp set to sp
      (*regs)[REG_RSP] = sp;
      (*regs)[REG_RIP] = shared32->invoke_gdbserver;
#else
      I_die_here : not x86 or amd64 in x86/amd64 section/
#endif

#else
      I_die_here : architecture missing in vgdb-invoker-solaris.c
#endif

   } else if (shared64 != NULL) {
#if defined(VGA_x86)
      assert(0); /* 64bits process with a 32bits vgdb - no way */
#elif defined(VGA_amd64)
      /* 64bits vgdb speaking with a 64 bit process. */
      const int regsize = 8;

      /* Give check argument in rdi - according to C/amd64 ABI. */
      (*regs)[REG_RDI] = check;

      /* Push return address on stack: return to breakaddr. */
      sp = sp - regsize;
      DEBUG(1, "Pushing bad_return return address to process %d memory.\n",
               pid);
      int error = write_memory(pid, sp, &bad_return,
                               sizeof(bad_return));
      if (error != 0) {
         ERROR(error, "Failed to push bad_return return address to process %d "
                      "memory.\n", pid);
         detach(pid);
         return False;
      }

      /* set RBP, RSP, RIP to invoke gdbserver */
      (*regs)[REG_RBP] = sp; // bp set to sp
      (*regs)[REG_RSP] = sp;
      (*regs)[REG_RIP] = shared64->invoke_gdbserver;
#else
      I_die_here: architecture missing in vgdb-invoker-solaris.c
#endif
   } else {
      assert(0);
   }

   DEBUG(1, "New stack frame set up for process %d.\n", pid);
   return True;
}

/* Creates and starts an agent thread within the inferior process.
   The agent thread is created stopped and with its held signal set
   (the signal mask) having all signals except SIGKILL and SIGSTOP
   blocked. All these signals need to remain blocked while the agent
   thread is running because valgrind syscall/signal machinery expects
   that (remember: all valgrind threads are blocked in VgTs_WaitSys
   - that is the reason why we are invoking the agent, after all).
   It is necessary to resume the agent thread afterwards.
 */
static Bool invoke_agent(pid_t pid, prgregset_t *regs, id_t *agent_lwpid)
{
   assert(ctl_fd != -1);

   DEBUG(1, "Creating an agent thread within process %d.\n", pid);

   /* Create the agent thread. */
   ctl_t ctl;
   ctl.cmd = PCAGENT;
   memcpy(&ctl.arg.regs, regs, sizeof(prgregset_t));
   size_t bytes = sizeof(ctl.cmd) + sizeof(ctl.arg.regs);
   ssize_t written = write(ctl_fd, (void *) &ctl, bytes);
   if ((written < 0) || (written != bytes)) {
      ERROR(errno, "Failed to write to ctl_fd: PCAGENT.\n");
      return False;
   }

   DEBUG(1, "Obtaining agent thread lwpid for process %d.\n", pid);

   char procname[PATH_MAX];
   snprintf(procname, sizeof(procname),
            "/proc/%d/lwp/agent/lwpstatus", pid);

   int status_fd = open(procname, O_RDONLY, 0);
   if (status_fd < 0) {
      /* Operation failed but there is no way to get rid of the agent
         thread from outside. We are doomed... */
      ERROR(errno, "Failed to open file %s.\n", procname);
      return False;
   }

   lwpstatus_t status;
   ssize_t nread = read(status_fd, &status, sizeof(status));
   if ((nread < 0) || (nread != sizeof(status))) {
      ERROR(errno, "Failed to read from %s.\n", procname);
      close(status_fd);
      return False;
   }

   close(status_fd);
   *agent_lwpid = status.pr_lwpid;

   snprintf(procname, sizeof(procname),
            "/proc/%d/lwp/agent/lwpctl", pid);

   int agent_ctl_fd = open(procname, O_WRONLY, 0);
   if (agent_ctl_fd < 0) {
      /* Resuming failed but there is no way to get rid of the agent
         thread from outside. We are doomed... */
      ERROR(errno, "Failed to open file %s.\n", procname);
      return False;
   }

   DEBUG(1, "Resuming the agent thread for process %d.\n", pid);

   /* Resume the agent thread. */
   ctl.cmd = PCRUN;
   ctl.arg.flags = 0;
   bytes = sizeof(ctl.cmd) + sizeof(ctl.arg.flags);
   written = write(agent_ctl_fd, (void *) &ctl, bytes);
   if ((written < 0) || (written != bytes)) {
      /* Resuming failed but there is no way to get rid of the agent
         thread from outside. We are doomed... */
      ERROR(errno, "Failed to write to agent_ctl_fd: PCRUN 0.\n");
      close(agent_ctl_fd);
      return False;
   }

   DEBUG(1, "Agent thread lwpid %d now running within process %d.\n",
         *agent_lwpid, pid);
   close(agent_ctl_fd);
   return True;
}

/* Waits until the agent thread running inside the inferior
   process exits. */
static Bool wait_for_agent_exit(pid_t pid, id_t agent_lwpid)
{
   char procname[PATH_MAX];
   snprintf(procname, sizeof(procname), "/proc/%d/lwp/agent/lwpctl", pid);

   int agent_ctl_fd = open(procname, O_WRONLY, 0);
   if (agent_ctl_fd < 0) {
      if (errno == ENOENT) {
         DEBUG(1, "Agent control file %s no longer exists. This means "
               "agent thread %d exited meanwhile.\n",
               procname, agent_lwpid);
         return True;
      }
      ERROR(errno, "Failed to open agent control file %s.\n", procname);
      return False;
   }

   DEBUG(1, "Waiting for agent thread %d to exit.\n", agent_lwpid);

   /* Wait until the agent thread stops. This covers also the case
      when the thread exited. */
   ctl_t ctl;
   ctl.cmd = PCWSTOP;
   size_t bytes = sizeof(ctl.cmd);
   ssize_t written = write(agent_ctl_fd, (void *) &ctl, bytes);
   if ((written < 0) || (written != bytes)) {
      if (errno == ENOENT) {
         DEBUG(1, "Agent thread lwpid %d has now exited in process %d.\n",
                  agent_lwpid, pid);
      } else {
         ERROR(errno, "Failed to write to agent_ctl_fd: PCWSTOP.\n");
         close(agent_ctl_fd);
         return False;
      }
   }

   close(agent_ctl_fd);
   return True;
}

Bool invoker_invoke_gdbserver(pid_t pid)
{
   if (attach(pid) != True) {
      return False;
   }

   prgregset_t regs;
   if (get_regs(pid, &regs) != True) {
      detach(pid);
      return False;
   }

   if (setup_stack_frame(pid, &regs) != True) {
      detach(pid);
      return False;
   }

   id_t agent_lwpid;
   if (invoke_agent(pid, &regs, &agent_lwpid) != True) {
      detach(pid);
      return False;
   }

   if (wait_for_agent_exit(pid, agent_lwpid) != True) {
      detach(pid);
      return False;
   }

   detach(pid);
   return True;
}

void invoker_cleanup_restore_and_detach(void *v_pid)
{
   detach(*(int *) v_pid);
}

void invoker_restrictions_msg(void)
{
}

void invoker_valgrind_dying(void)
{
}
