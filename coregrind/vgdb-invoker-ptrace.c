/*--------------------------------------------------------------------*/
/*--- Implementation of vgdb invoker subsystem via ptrace() calls. ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Philippe Waroquiers

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

#include <alloca.h>
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ptrace.h>
#include <sys/time.h>
#include <sys/user.h>
#include <sys/wait.h>

#ifdef PTRACE_GETREGSET
// TBD: better have a configure test instead ?
#define HAVE_PTRACE_GETREGSET

// A bi-arch build using PTRACE_GET/SETREGSET needs
// some conversion code for register structures.
// So, better do not use PTRACE_GET/SETREGSET
// Rather we use PTRACE_GETREGS or PTRACE_PEEKUSER.

// The only platform on which we must use PTRACE_GETREGSET is arm64.
// The resulting vgdb cannot work in a bi-arch setup.
// -1 means we will check that PTRACE_GETREGSET works.
#  if defined(VGA_arm64)
#define USE_PTRACE_GETREGSET
#  endif
#endif

#include <sys/uio.h>
#include <elf.h>

#include <sys/procfs.h>

// glibc versions prior to 2.5 do not define PTRACE_GETSIGINFO on
// the platforms we support.
#if !((__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 5))
#   ifndef PTRACE_GETSIGINFO
#   define PTRACE_GETSIGINFO 0x4202
#   endif
#endif

// 32-bit or 64-bit wide, depending on primary architecture.
typedef Addr  CORE_ADDR;
typedef Addr  PTRACE_XFER_TYPE;
typedef void* PTRACE_ARG3_TYPE;

// if > 0, pid for which registers have to be restored.
// if == 0, means we have not yet called setregs (or have already
// restored the registers).
static int pid_of_save_regs = 0;
/* True if we have continued pid_of_save_regs after PTRACE_ATTACH. */
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

/* ptrace_(read|write)_memory are modified extracts of linux-low.c
   from gdb 6.6. Copyrighted FSF */
/* Copy LEN bytes from valgrind memory starting at MEMADDR
   to vgdb memory starting at MYADDR.  */
static
int ptrace_read_memory (pid_t inferior_pid, CORE_ADDR memaddr,
                        void *myaddr, size_t len)
{
   register int i;
   /* Round starting address down to longword boundary.  */
   register CORE_ADDR addr = memaddr & -(CORE_ADDR) sizeof (PTRACE_XFER_TYPE);
   /* Round ending address up; get number of longwords that makes.  */
   register int count
      = (((memaddr + len) - addr) + sizeof (PTRACE_XFER_TYPE) - 1)
      / sizeof (PTRACE_XFER_TYPE);
   /* Allocate buffer of that many longwords.  */
   register PTRACE_XFER_TYPE *buffer
      = (PTRACE_XFER_TYPE *) alloca (count * sizeof (PTRACE_XFER_TYPE));

   /* Read all the longwords */
   for (i = 0; i < count; i++, addr += sizeof (PTRACE_XFER_TYPE)) {
      errno = 0;
      buffer[i] = ptrace (PTRACE_PEEKTEXT, inferior_pid,
                          (PTRACE_ARG3_TYPE) addr, 0);
      if (errno)
         return errno;
   }

   /* Copy appropriate bytes out of the buffer.  */
   memcpy (myaddr,
           (char *) buffer + (memaddr & (sizeof (PTRACE_XFER_TYPE) - 1)), len);

   return 0;
}

/* Copy LEN bytes of data from vgdb memory at MYADDR
   to valgrind memory at MEMADDR.
   On failure (cannot write the valgrind memory)
   returns the value of errno.  */
__attribute__((unused)) /* not used on all platforms */
static
int ptrace_write_memory (pid_t inferior_pid, CORE_ADDR memaddr,
                         const void *myaddr, size_t len)
{
   register int i;
   /* Round starting address down to longword boundary.  */
   register CORE_ADDR addr = memaddr & -(CORE_ADDR) sizeof (PTRACE_XFER_TYPE);
   /* Round ending address up; get number of longwords that makes.  */
   register int count
      = (((memaddr + len) - addr) + sizeof (PTRACE_XFER_TYPE) - 1)
      / sizeof (PTRACE_XFER_TYPE);
   /* Allocate buffer of that many longwords.  */
   register PTRACE_XFER_TYPE *buffer
      = (PTRACE_XFER_TYPE *) alloca (count * sizeof (PTRACE_XFER_TYPE));

   if (debuglevel >= 1) {
      DEBUG (1, "Writing ");
      for (i = 0; i < len; i++)
         PDEBUG (1, "%02x", ((const unsigned char*)myaddr)[i]);
      PDEBUG(1, " to %p\n", (void *) memaddr);
   }

   /* Fill start and end extra bytes of buffer with existing memory data.  */

   buffer[0] = ptrace (PTRACE_PEEKTEXT, inferior_pid,
                       (PTRACE_ARG3_TYPE) addr, 0);

   if (count > 1) {
      buffer[count - 1]
         = ptrace (PTRACE_PEEKTEXT, inferior_pid,
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
      ptrace (PTRACE_POKETEXT, inferior_pid,
              (PTRACE_ARG3_TYPE) addr, buffer[i]);
      if (errno)
         return errno;
   }

   return 0;
}

/* subset of VG_(threads) needed for vgdb ptrace.
   This is initialized when process is attached. */
typedef struct {
   ThreadStatus status;
   Int lwpid;
}
VgdbThreadState;
static VgdbThreadState *vgdb_threads;
static int vg_n_threads;

static const
HChar* name_of_ThreadStatus ( ThreadStatus status )
{
   switch (status) {
   case VgTs_Empty:     return "VgTs_Empty";
   case VgTs_Init:      return "VgTs_Init";
   case VgTs_Runnable:  return "VgTs_Runnable";
   case VgTs_WaitSys:   return "VgTs_WaitSys";
   case VgTs_Yielding:  return "VgTs_Yielding";
   case VgTs_Zombie:    return "VgTs_Zombie";
   default:             return "VgTs_???";
  }
}

static
char *status_image (int status)
{
   static char result[256];  // large enough
   int sz = 0;
#define APPEND(...) sz += snprintf (result+sz, 256 - sz - 1, __VA_ARGS__)

   result[0] = 0;

   if (WIFEXITED(status))
      APPEND ("WIFEXITED %d ", WEXITSTATUS(status));

   if (WIFSIGNALED(status)) {
      APPEND ("WIFSIGNALED %d ", WTERMSIG(status));
      if (WCOREDUMP(status)) APPEND ("WCOREDUMP ");
   }

   if (WIFSTOPPED(status))
      APPEND ("WIFSTOPPED %d ", WSTOPSIG(status));

#ifdef WIFCONTINUED
   if (WIFCONTINUED(status))
      APPEND ("WIFCONTINUED ");
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
      p = waitpid(pid, &status, __WALL);
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
      if (signal_received == signal_expected)
         break;

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

         res = ptrace (PTRACE_GETSIGINFO, pid, NULL, newsiginfo);
         if (res != 0) {
            ERROR(errno, "PTRACE_GETSIGINFO failed: signal lost !!!!\n");
            signal_queue_sz--;
         } else
            DEBUG(1, "waitstopped PTRACE_CONT, queuing signal %d"
                  " si_signo %d si_pid %d\n",
                  signal_received, newsiginfo->si_signo, newsiginfo->si_pid);
         res = ptrace (PTRACE_CONT, pid, NULL, 0);
      } else {
         DEBUG(1, "waitstopped PTRACE_CONT with signal %d\n", signal_received);
         res = ptrace (PTRACE_CONT, pid, NULL, signal_received);
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
   long res;
   static Bool output_error = True;
   static Bool initial_attach = True;
   // For a ptrace_scope protected system, we do not want to output
   // repetitively attach error. We will output once an error
   // for the initial_attach. Once the 1st attach has succeeded, we
   // again show all errors.

   DEBUG(1, "%s PTRACE_ATTACH pid %d\n", msg, pid);
   res = ptrace (PTRACE_ATTACH, pid, NULL, NULL);
   if (res != 0) {
      if (output_error || debuglevel > 0) {
         ERROR(errno, "%s PTRACE_ATTACH pid %d %ld\n", msg, pid, res);
         if (initial_attach)
            output_error = False;
      }
      return False;
   }

   initial_attach = False;
   output_error = True;
   return waitstopped(pid, SIGSTOP, msg);
}

/* once we are attached to the pid, get the list of threads and stop
   them all.
   Returns True if all threads properly suspended, False otherwise. */
static
Bool acquire_and_suspend_threads (pid_t pid)
{
   int i;
   int rw;
   Bool pid_found = False;
   Addr vgt;
   int sz_tst;
   int off_status;
   int off_lwpid;
   int nr_live_threads = 0;

   if (shared32 != NULL) {
      vgt = shared32->threads;
      vg_n_threads = shared32->vg_n_threads;
      sz_tst = shared32->sizeof_ThreadState;
      off_status = shared32->offset_status;
      off_lwpid = shared32->offset_lwpid;
   }
   else if (shared64 != NULL) {
      vgt = shared64->threads;
      vg_n_threads = shared64->vg_n_threads;
      sz_tst = shared64->sizeof_ThreadState;
      off_status = shared64->offset_status;
      off_lwpid = shared64->offset_lwpid;
   } else {
      assert (0);
   }

   vgdb_threads = vmalloc(vg_n_threads * sizeof vgdb_threads[0]);

   /* note: the entry 0 is unused */
   DEBUG(1, "examining thread entries from tid 1 to tid %d\n", vg_n_threads-1);
   for (i = 1; i < vg_n_threads; i++) {
      vgt += sz_tst;
      rw = ptrace_read_memory(pid, vgt+off_status,
                              &(vgdb_threads[i].status),
                              sizeof(ThreadStatus));
      if (rw != 0) {
         ERROR(rw, "status ptrace_read_memory\n");
         return False;
      }

      rw = ptrace_read_memory(pid, vgt+off_lwpid,
                              &(vgdb_threads[i].lwpid),
                              sizeof(Int));
      if (rw != 0) {
         ERROR(rw, "lwpid ptrace_read_memory\n");
         return False;
      }

      if (vgdb_threads[i].status != VgTs_Empty) {
         DEBUG(1, "found tid %d status %s lwpid %d\n",
               i, name_of_ThreadStatus(vgdb_threads[i].status),
               vgdb_threads[i].lwpid);
         nr_live_threads++;
         if (vgdb_threads[i].lwpid <= 1) {
            if (vgdb_threads[i].lwpid == 0
                && vgdb_threads[i].status == VgTs_Init) {
               DEBUG(1, "not set lwpid tid %d status %s lwpid %d\n",
                     i, name_of_ThreadStatus(vgdb_threads[i].status),
                     vgdb_threads[i].lwpid);
            } else {
               ERROR(1, "unexpected lwpid tid %d status %s lwpid %d\n",
                     i, name_of_ThreadStatus(vgdb_threads[i].status),
                     vgdb_threads[i].lwpid);
            }
            /* in case we have a VtTs_Init thread with lwpid not yet set,
               we try again later. */
            return False;
         }
         if (vgdb_threads[i].lwpid == pid) {
            assert (!pid_found);
            assert (i == 1);
            pid_found = True;
         } else {
            if (!attach(vgdb_threads[i].lwpid, "attach_thread")) {
                 ERROR(0, "ERROR attach pid %d tid %d\n",
                       vgdb_threads[i].lwpid, i);
               return False;
            }
         }
      }
   }
   /* If we found no thread, it means the process is stopping, and
      we better do not force anything to happen during that. */
   if (nr_live_threads > 0)
      return True;
   else
      return False;
}

static
void detach_from_all_threads (pid_t pid)
{
   int i;
   long res;
   Bool pid_found = False;

   /* detach from all the threads  */
   for (i = 1; i < vg_n_threads; i++) {
      if (vgdb_threads[i].status != VgTs_Empty) {
         if (vgdb_threads[i].status == VgTs_Init
             && vgdb_threads[i].lwpid == 0) {
            DEBUG(1, "skipping PTRACE_DETACH pid %d tid %d status %s\n",
                  vgdb_threads[i].lwpid, i,
                  name_of_ThreadStatus (vgdb_threads[i].status));
         } else {
            if (vgdb_threads[i].lwpid == pid) {
               assert (!pid_found);
               pid_found = True;
            }
            DEBUG(1, "PTRACE_DETACH pid %d tid %d status %s\n",
                  vgdb_threads[i].lwpid, i,
                  name_of_ThreadStatus (vgdb_threads[i].status));
            res = ptrace (PTRACE_DETACH, vgdb_threads[i].lwpid, NULL, NULL);
            if (res != 0) {
               ERROR(errno, "PTRACE_DETACH pid %d tid %d status %s res %ld\n",
                     vgdb_threads[i].lwpid, i,
                     name_of_ThreadStatus (vgdb_threads[i].status),
                     res);
            }
         }
      }
   }

   free (vgdb_threads);

   if (!pid_found && pid) {
      /* No threads are live. Process is busy stopping.
         We need to detach from pid explicitly. */
      DEBUG(1, "no thread live => PTRACE_DETACH pid %d\n", pid);
      res = ptrace (PTRACE_DETACH, pid, NULL, NULL);
      if (res != 0)
         ERROR(errno, "PTRACE_DETACH pid %d res %ld\n", pid, res);
   }
}

#  if defined(VGA_arm64)
/* arm64 is extra special, old glibc defined kernel user_pt_regs, but
   newer glibc instead define user_regs_struct. */
#    ifdef HAVE_SYS_USER_REGS
static struct user_regs_struct user_save;
#    else
static struct user_pt_regs user_save;
#    endif
#  else
static struct user user_save;
#  endif
// The below indicates if ptrace_getregs (and ptrace_setregs) can be used.
// Note that some linux versions are defining PTRACE_GETREGS but using
// it gives back EIO.
// has_working_ptrace_getregs can take the following values:
//  -1 : PTRACE_GETREGS is defined
//       runtime check not yet done.
//   0 : PTRACE_GETREGS runtime check has failed.
//   1 : PTRACE_GETREGS defined and runtime check ok.
#ifdef HAVE_PTRACE_GETREGS
static int has_working_ptrace_getregs = -1;
#endif
// Similar but for PTRACE_GETREGSET
#ifdef HAVE_PTRACE_GETREGSET
static int has_working_ptrace_getregset = -1;
#endif

/* Get the registers from pid into regs.
   regs_bsz value gives the length of *regs.
   Returns True if all ok, otherwise False. */
static
Bool getregs (pid_t pid, void *regs, long regs_bsz)
{
   DEBUG(1, "getregs regs_bsz %ld\n", regs_bsz);
#  ifdef HAVE_PTRACE_GETREGSET
#  ifndef USE_PTRACE_GETREGSET
   if (has_working_ptrace_getregset)
      DEBUG(1, "PTRACE_GETREGSET defined, not used (yet?) by vgdb\n");
   has_working_ptrace_getregset = 0;
#  endif
   if (has_working_ptrace_getregset) {
      // Platforms having GETREGSET
      long res;
      elf_gregset_t elf_regs;
      struct iovec iovec;

      DEBUG(1, "getregs PTRACE_GETREGSET sizeof(elf_regs) %zu\n",
            sizeof(elf_regs));
      iovec.iov_base = regs;
      iovec.iov_len =  sizeof(elf_regs);

      res = ptrace (PTRACE_GETREGSET, pid, NT_PRSTATUS, &iovec);
      if (res == 0) {
         if (has_working_ptrace_getregset == -1) {
            // First call to PTRACE_GETREGSET successful =>
            has_working_ptrace_getregset = 1;
            DEBUG(1, "detected a working PTRACE_GETREGSET\n");
         }
         assert (has_working_ptrace_getregset == 1);
         return True;
      }
      else if (has_working_ptrace_getregset == 1) {
         // We had a working call, but now it fails.
         // This is unexpected.
         ERROR(errno, "PTRACE_GETREGSET %ld\n", res);
         return False;
      } else {
         // Check this is the first call:
         assert (has_working_ptrace_getregset == -1);
         if (errno == EIO) {
            DEBUG(1, "detected a broken PTRACE_GETREGSET with EIO\n");
            has_working_ptrace_getregset = 0;
            // Fall over to the PTRACE_GETREGS or PTRACE_PEEKUSER case.
         } else {
            ERROR(errno, "broken PTRACE_GETREGSET unexpected errno %ld\n", res);
            return False;
         }
      }
   }
#  endif

#  ifdef HAVE_PTRACE_GETREGS
   if (has_working_ptrace_getregs) {
      // Platforms having GETREGS
      long res;
      DEBUG(1, "getregs PTRACE_GETREGS\n");
      res = ptrace (PTRACE_GETREGS, pid, NULL, regs);
      if (res == 0) {
         if (has_working_ptrace_getregs == -1) {
            // First call to PTRACE_GETREGS successful =>
            has_working_ptrace_getregs = 1;
            DEBUG(1, "detected a working PTRACE_GETREGS\n");
         }
         assert (has_working_ptrace_getregs == 1);
         return True;
      }
      else if (has_working_ptrace_getregs == 1) {
         // We had a working call, but now it fails.
         // This is unexpected.
         ERROR(errno, "PTRACE_GETREGS %ld\n", res);
         return False;
      } else {
         // Check this is the first call:
         assert (has_working_ptrace_getregs == -1);
         if (errno == EIO) {
            DEBUG(1, "detected a broken PTRACE_GETREGS with EIO\n");
            has_working_ptrace_getregs = 0;
            // Fall over to the PTRACE_PEEKUSER case.
         } else {
            ERROR(errno, "broken PTRACE_GETREGS unexpected errno %ld\n", res);
            return False;
         }
      }
   }
#  endif

   // We assume  PTRACE_PEEKUSER is defined everywhere.
   {
#     ifdef PT_ENDREGS
      long peek_bsz = PT_ENDREGS;
      assert (peek_bsz <= regs_bsz);
#     else
      long peek_bsz = regs_bsz-1;
#     endif
      char *pregs = (char *) regs;
      long offset;
      errno = 0;
      DEBUG(1, "getregs PTRACE_PEEKUSER(s) peek_bsz %ld\n", peek_bsz);
      for (offset = 0; offset < peek_bsz; offset = offset + sizeof(long)) {
         *(long *)(pregs+offset) = ptrace(PTRACE_PEEKUSER, pid, offset, NULL);
         if (errno != 0) {
            ERROR(errno, "PTRACE_PEEKUSER offset %ld\n", offset);
            return False;
         }
      }
      return True;
   }

   // If neither of PTRACE_GETREGSET PTRACE_GETREGS PTRACE_PEEKUSER have
   // returned, then we are in serious trouble.
   assert (0);
}

/* Set the registers of pid to regs.
   regs_bsz value gives the length of *regs.
   Returns True if all ok, otherwise False. */
static
Bool setregs (pid_t pid, void *regs, long regs_bsz)
{
   DEBUG(1, "setregs regs_bsz %ld\n", regs_bsz);

// Note : the below is checking for GETREGSET, not SETREGSET
// as if one is defined and working, the other one should also work.
#  ifdef HAVE_PTRACE_GETREGSET
   if (has_working_ptrace_getregset) {
      // Platforms having SETREGSET
      long res;
      elf_gregset_t elf_regs;
      struct iovec iovec;

      // setregset can never be called before getregset has done a runtime check.
      assert (has_working_ptrace_getregset == 1);
      DEBUG(1, "setregs PTRACE_SETREGSET sizeof(elf_regs) %zu\n",
            sizeof(elf_regs));
      iovec.iov_base = regs;
      iovec.iov_len =  sizeof(elf_regs);
      res = ptrace (PTRACE_SETREGSET, pid, NT_PRSTATUS, &iovec);
      if (res != 0) {
         ERROR(errno, "PTRACE_SETREGSET %ld\n", res);
         return False;
      }
      return True;
   }
#  endif

// Note : the below is checking for GETREGS, not SETREGS
// as if one is defined and working, the other one should also work.
#  ifdef HAVE_PTRACE_GETREGS
   if (has_working_ptrace_getregs) {
      // Platforms having SETREGS
      long res;
      // setregs can never be called before getregs has done a runtime check.
      assert (has_working_ptrace_getregs == 1);
      DEBUG(1, "setregs PTRACE_SETREGS\n");
      res = ptrace (PTRACE_SETREGS, pid, NULL, regs);
      if (res != 0) {
         ERROR(errno, "PTRACE_SETREGS %ld\n", res);
         return False;
      }
      return True;
   }
#  endif

   {
      char *pregs = (char *) regs;
      long offset;
      long res;
#     ifdef PT_ENDREGS
      long peek_bsz = PT_ENDREGS;
      assert (peek_bsz <= regs_bsz);
#     else
      long peek_bsz = regs_bsz-1;
#     endif
      errno = 0;
      DEBUG(1, "setregs PTRACE_POKEUSER(s) %ld\n", peek_bsz);
      for (offset = 0; offset < peek_bsz; offset = offset + sizeof(long)) {
         res = ptrace(PTRACE_POKEUSER, pid, offset, *(long*)(pregs+offset));
         if (errno != 0) {
            ERROR(errno, "PTRACE_POKEUSER offset %ld res %ld\n", offset, res);
            return False;
         }
      }
      return True;
   }

   // If neither PTRACE_SETREGS not PTRACE_POKEUSER have returned,
   // then we are in serious trouble.
   assert (0);
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
         if (!stop(pid_of_save_regs, "sigstop before reset regs"))
            DEBUG(0, "Could not sigstop before reset");
      }

      DEBUG(1, "setregs restore registers pid %d\n", pid_of_save_regs);
      if (!setregs(pid_of_save_regs, &user_save.regs, sizeof(user_save.regs))) {
         ERROR(errno, "setregs restore registers pid %d after cont\n",
               pid_of_save_regs);
      }

      /* Now, we transmit all the signals we have queued. */
      if (signal_queue_sz > 0) {
         int i;
         for (i = 0; i < signal_queue_sz; i++) {
            DEBUG(1, "PTRACE_CONT to transmit queued signal %d\n",
                  signal_queue[i].si_signo);
            res = ptrace (PTRACE_CONT, pid_of_save_regs, NULL,
                          signal_queue[i].si_signo);
            if (res != 0)
               ERROR(errno, "PTRACE_CONT with signal %d\n",
                     signal_queue[i].si_signo);
            if (!stop(pid_of_save_regs, "sigstop after transmit sig"))
               DEBUG(0, "Could not sigstop after transmit sig");
         }
         free (signal_queue);
         signal_queue = NULL;
         signal_queue_sz = 0;
      }
      pid_of_save_regs = 0;
   } else {
      DEBUG(1, "PTRACE_SETREGS restore registers: no pid\n");
   }
   if (signal_queue)
      ERROR (0, "One or more signals queued were not delivered. "
             "First signal: %d\n", signal_queue[0].si_signo);
   detach_from_all_threads(pid);
}

Bool invoker_invoke_gdbserver (pid_t pid)
{
   long res;
   Bool stopped;
#  if defined(VGA_arm64)
/* arm64 is extra special, old glibc defined kernel user_pt_regs, but
   newer glibc instead define user_regs_struct. */
#    ifdef HAVE_SYS_USER_REGS
   struct user_regs_struct user_mod;
#    else
   struct user_pt_regs user_mod;
#    endif
#  else
   struct user user_mod;
#  endif
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

   if (!acquire_and_suspend_threads(pid)) {
      detach_from_all_threads(pid);
      /* if the pid does not exist anymore, we better stop */
      if (kill(pid, 0) != 0)
        XERROR (errno, "invoke_gdbserver: check for pid %d existence failed\n",
                pid);
      return False;
   }

   if (!getregs(pid, &user_mod.regs, sizeof(user_mod.regs))) {
      detach_from_all_threads(pid);
      return False;
   }
   user_save = user_mod;

#if defined(VGA_x86)
   sp = user_mod.regs.esp;
#elif defined(VGA_amd64)
   sp = user_mod.regs.rsp;
   if (shared32 != NULL) {
     /* 64bit vgdb speaking with a 32bit executable.
        To have system call restart properly, we need to sign extend rax.
        For more info:
        web search '[patch] Fix syscall restarts for amd64->i386 biarch'
        e.g. http://sourceware.org/ml/gdb-patches/2009-11/msg00592.html */
     *(long *)&user_save.regs.rax = *(int*)&user_save.regs.rax;
     DEBUG(1, "Sign extending %8.8lx to %8.8lx\n",
           user_mod.regs.rax, user_save.regs.rax);
   }
#elif defined(VGA_arm)
   sp = user_mod.regs.uregs[13];
#elif defined(VGA_arm64)
   sp = user_mod.sp;
#elif defined(VGA_ppc32)
   sp = user_mod.regs.gpr[1];
#elif defined(VGA_ppc64be) || defined(VGA_ppc64le)
   sp = user_mod.regs.gpr[1];
#elif defined(VGA_s390x)
   sp = user_mod.regs.gprs[15];
#elif defined(VGA_mips32) || defined(VGA_nanomips)
   long long *p = (long long *)user_mod.regs;
   sp = p[29];
#elif defined(VGA_mips64)
   sp = user_mod.regs[29];
#else
   I_die_here : (sp) architecture missing in vgdb-invoker-ptrace.c
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
      user_mod.regs.ebp = sp; // bp set to sp
      user_mod.regs.esp = sp;
      user_mod.regs.eip = shared32->invoke_gdbserver;
      user_mod.regs.orig_eax = -1L;
#elif defined(VGA_amd64)
      /* set ebp, esp, eip and orig_eax to invoke gdbserver */
      // compiled in 64bits, speaking with a 32bits exe
      user_mod.regs.rbp = sp; // bp set to sp
      user_mod.regs.rsp = sp;
      user_mod.regs.rip = shared32->invoke_gdbserver;
      user_mod.regs.orig_rax = -1L;
#else
      I_die_here : not x86 or amd64 in x86/amd64 section/
#endif

#elif defined(VGA_ppc32) || defined(VGA_ppc64be) || defined(VGA_ppc64le)
      user_mod.regs.nip = shared32->invoke_gdbserver;
      user_mod.regs.trap = -1L;
      /* put check arg in register 3 */
      user_mod.regs.gpr[3] = check;
      /* put NULL return address in Link Register */
      user_mod.regs.link = bad_return;

#elif defined(VGA_arm)
      /* put check arg in register 0 */
      user_mod.regs.uregs[0] = check;
      /* put NULL return address in Link Register */
      user_mod.regs.uregs[14] = bad_return;
      user_mod.regs.uregs[15] = shared32->invoke_gdbserver;

#elif defined(VGA_arm64)
      XERROR(0, "TBD arm64: vgdb a 32 bits executable with a 64 bits exe\n");

#elif defined(VGA_s390x)
      XERROR(0, "(fn32) s390x has no 32bits implementation\n");
#elif defined(VGA_mips32) || defined(VGA_nanomips)
      /* put check arg in register 4 */
      p[4] = check;
      /* put NULL return address in ra */
      p[31] = bad_return;
      p[34] = shared32->invoke_gdbserver;
      p[25] = shared32->invoke_gdbserver;
      /* make stack space for args */
      p[29] = sp - 32;

#elif defined(VGA_mips64)
      assert(0); // cannot vgdb a 32 bits executable with a 64 bits exe
#else
      I_die_here : architecture missing in vgdb-invoker-ptrace.c
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
      user_mod.regs.rdi = check;

      /* push return address on stack : return to breakaddr */
      sp &= ~0xf; // keep the stack aligned on 16 bytes ...
      sp = sp - 128; // do not touch the amd64 redzone
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
      user_mod.regs.rbp = sp; // bp set to sp
      user_mod.regs.rsp = sp;
      user_mod.regs.rip = shared64->invoke_gdbserver;
      user_mod.regs.orig_rax = -1L;

#elif defined(VGA_arm)
      assert(0); // cannot vgdb a 64 bits executable with a 32 bits exe
#elif defined(VGA_arm64)
      user_mod.regs[0] = check;
      user_mod.sp = sp;
      user_mod.pc = shared64->invoke_gdbserver;
      /* put NULL return address in Link Register */
      user_mod.regs[30] = bad_return;

#elif defined(VGA_ppc32)
      assert(0); // cannot vgdb a 64 bits executable with a 32 bits exe
#elif defined(VGA_ppc64be)
      Addr func_addr;
      Addr toc_addr;
      int rw;
      rw = ptrace_read_memory(pid, shared64->invoke_gdbserver,
                              &func_addr,
                              sizeof(Addr));
      if (rw != 0) {
         ERROR(rw, "ppc64 read func_addr\n");
         detach_from_all_threads(pid);
         return False;
      }
      rw = ptrace_read_memory(pid, shared64->invoke_gdbserver+8,
                              &toc_addr,
                              sizeof(Addr));
      if (rw != 0) {
         ERROR(rw, "ppc64 read toc_addr\n");
         detach_from_all_threads(pid);
         return False;
      }
      // We are not pushing anything on the stack, so it is not
      // very clear why the sp has to be decreased, but it seems
      // needed. The ppc64 ABI might give some lights on this ?
      user_mod.regs.gpr[1] = sp - 220;
      user_mod.regs.gpr[2] = toc_addr;
      user_mod.regs.nip = func_addr;
      user_mod.regs.trap = -1L;
      /* put check arg in register 3 */
      user_mod.regs.gpr[3] = check;
      /* put bad_return return address in Link Register */
      user_mod.regs.link = bad_return;
#elif defined(VGA_ppc64le)
      /* LE does not use the function pointer structure used in BE */
      user_mod.regs.nip = shared64->invoke_gdbserver;
      user_mod.regs.gpr[1] = sp - 512;
      user_mod.regs.gpr[12] = user_mod.regs.nip;
      user_mod.regs.trap = -1L;
      /* put check arg in register 3 */
      user_mod.regs.gpr[3] = check;
      /* put bad_return return address in Link Register */
      user_mod.regs.link = bad_return;
#elif defined(VGA_s390x)
      /* put check arg in register r2 */
      user_mod.regs.gprs[2] = check;
      /* bad_return Return address is in r14 */
      user_mod.regs.gprs[14] = bad_return;
      /* minimum stack frame */
      sp = sp - 160;
      user_mod.regs.gprs[15] = sp;
      /* set program counter */
      user_mod.regs.psw.addr = shared64->invoke_gdbserver;
#elif defined(VGA_mips32)  || defined(VGA_nanomips)
      assert(0); // cannot vgdb a 64 bits executable with a 32 bits exe
#elif defined(VGA_mips64)
      /* put check arg in register 4 */
      user_mod.regs[4] = check;
      /* put NULL return address in ra */
      user_mod.regs[31] = bad_return;
      user_mod.regs[34] = shared64->invoke_gdbserver;
      user_mod.regs[25] = shared64->invoke_gdbserver;
#else
      I_die_here: architecture missing in vgdb-invoker-ptrace.c
#endif
   }
   else {
      assert(0);
   }

   if (!setregs(pid, &user_mod.regs, sizeof(user_mod.regs))) {
      detach_from_all_threads(pid);
      return False;
   }
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
   DEBUG(1, "PTRACE_CONT to invoke\n");
   res = ptrace (PTRACE_CONT, pid, NULL, NULL);
   if (res != 0) {
      ERROR(errno, "PTRACE_CONT\n");
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
   } else {
      /* Whatever kind of problem happened. We shutdown. */
      shutting_down = True;
      return False;
   }
}

void invoker_cleanup_restore_and_detach(void *v_pid)
{
   DEBUG(1, "invoker_cleanup_restore_and_detach dying: %d\n", dying);
   if (!dying)
      restore_and_detach(*(int*)v_pid);
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
