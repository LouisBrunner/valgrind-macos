/*--------------------------------------------------------------------*/
/*--- Relay between gdb and gdbserver embedded in valgrind  vgdb.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2012 Philippe Waroquiers

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
#include "pub_core_vki.h"
#include "pub_core_libcsetjmp.h"
#include "pub_core_threadstate.h"
#include "pub_core_gdbserver.h"
#include "config.h"

#include <limits.h>
#include <unistd.h>
#include <string.h>
#include <poll.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <assert.h>
/* vgdb has two usages:
   1. relay application between gdb and the gdbserver embedded in valgrind.
   2. standalone to send monitor commands to a running valgrind-ified process

   It is made of a main program which reads arguments.  If no
   arguments are given or only --pid and --vgdb-prefix, then usage 1 is
   assumed.
   
   As relay application, vgdb reads bytes from gdb on stdin and
   writes these bytes to valgrind.  Bytes read from valgrind are
   written to gdb on stdout.  Read/Write from/to valgrind is done
   using FIFOs.  There is one thread reading from stdin, writing to
   valgrind on a FIFO.  There is one thread reading from valgrind on a
   FIFO, writing to gdb on stdout

   As a standalone utility, vgdb builds command packets to write to valgrind,
   sends it and reads the reply. The same two threads are used to write/read.
   Once all the commands are sent and their replies received, vgdb will exit.
   
*/

/* define PTRACEINVOKER to compile the ptrace related code
   which ensures a valgrind process blocked in a system call
   can be "waken up". PTRACEINVOKER implies some architecture
   specific code and/or some OS specific code. */
#if defined(VGA_arm) || defined(VGA_x86) || defined(VGA_amd64) \
    || defined(VGA_ppc32) || defined(VGA_ppc64) || defined(VGA_s390x) \
    || defined(VGP_mips32_linux)
#define PTRACEINVOKER
#else
I_die_here : (PTRACEINVOKER) architecture missing in vgdb.c
#endif

/* Some darwin specific stuff is needed as ptrace is not
   fully supported on MacOS. Till we find someone courageous
   having access to Darwin, there is no PTRACEINVOKER. */
#if defined(VGO_darwin)
#undef PTRACEINVOKER
#endif

#if defined(VGPV_arm_linux_android) || defined(VGPV_x86_linux_android)
#undef PTRACEINVOKER
#endif

#if defined(PTRACEINVOKER)
#include <sys/user.h>
#if defined(VGO_linux)
#  include <sys/prctl.h>
#  include <linux/ptrace.h>
#endif
#endif


// Outputs information for the user about ptrace_scope protection
// or ptrace not working.
static void ptrace_restrictions_msg(void);

static int debuglevel;
static struct timeval dbgtv;
/* if level <= debuglevel, print timestamp, then print provided by debug info */
#define DEBUG(level, ...) (level <= debuglevel ?                        \
                           gettimeofday(&dbgtv, NULL),                  \
                           fprintf(stderr, "%ld.%6.6ld ",               \
                                   (long int)dbgtv.tv_sec,              \
                                   (long int)dbgtv.tv_usec),            \
                           fprintf(stderr, __VA_ARGS__),fflush(stderr)  \
                           : 0)

/* same as DEBUG but does not print time stamp info */
#define PDEBUG(level, ...) (level <= debuglevel ?                       \
                            fprintf(stderr, __VA_ARGS__),fflush(stderr) \
                            : 0)

/* if errno != 0, 
   report the errno and fprintf the ... varargs on stderr. */
#define ERROR(errno, ...) ((errno == 0 ? 0 : perror("syscall failed")), \
                           fprintf(stderr, __VA_ARGS__),                \
                           fflush(stderr))
/* same as ERROR, but also exits with status 1 */
#define XERROR(errno, ...) ((errno == 0 ? 0 : perror("syscall failed")), \
                            fprintf(stderr, __VA_ARGS__),                \
                            fflush(stderr),                              \
                            exit(1))

static char *vgdb_prefix = NULL;

/* Will be set to True when any condition indicating we have to shutdown
   is encountered. */
static Bool shutting_down = False;

static VgdbShared32 *shared32;
static VgdbShared64 *shared64;
#define VS_written_by_vgdb (shared32 != NULL ?        \
                            shared32->written_by_vgdb \
                            : shared64->written_by_vgdb)
#define VS_seen_by_valgrind (shared32 != NULL ?         \
                             shared32->seen_by_valgrind \
                             : shared64->seen_by_valgrind)

#define VS_vgdb_pid (shared32 != NULL ? shared32->vgdb_pid : shared64->vgdb_pid)

/* Calls malloc (size). Exits if memory can't be allocated. */
static
void *vmalloc(size_t size)
{
   void * mem = malloc(size);
   if (mem == NULL)
      XERROR (errno, "can't allocate memory\n");
   return mem;
}

/* Calls realloc (size). Exits if memory can't be allocated. */
static
void *vrealloc(void *ptr,size_t size)
{
   void * mem = realloc(ptr, size);
   if (mem == NULL)
      XERROR (errno, "can't reallocate memory\n");
   return mem;
}

/* Return the name of a directory for temporary files. */
static
const char *vgdb_tmpdir(void)
{
   const char *tmpdir;

   tmpdir = getenv("TMPDIR");
   if (tmpdir == NULL || *tmpdir == '\0')
     tmpdir = VG_TMPDIR;
   if (tmpdir == NULL || *tmpdir == '\0')
     tmpdir = "/tmp";    /* fallback */

   return tmpdir;
}

/* Return the default path prefix for the named pipes (FIFOs) used by vgdb/gdb
   to communicate with valgrind */
static
char *vgdb_prefix_default(void)
{
   static HChar *prefix;

   if (prefix == NULL) {
      const char *tmpdir = vgdb_tmpdir();
      prefix = vmalloc(strlen(tmpdir) + strlen("/vgdb-pipe") + 1);
      strcpy(prefix, tmpdir);
      strcat(prefix, "/vgdb-pipe");
   }
   return prefix;
}

/* add nrw to the written_by_vgdb field of shared32 or shared64 */ 
static
void add_written(int nrw)
{
   if (shared32 != NULL) 
      shared32->written_by_vgdb += nrw;
   else if (shared64 != NULL) 
      shared64->written_by_vgdb += nrw;
   else
      assert(0);
}

static int shared_mem_fd = -1;
static
void map_vgdbshared (char* shared_mem)
{
   struct stat fdstat;
   void **s;
   shared_mem_fd = open(shared_mem, O_RDWR);
   /* shared_mem_fd will not be closed till vgdb exits. */

   if (shared_mem_fd == -1)
      XERROR (errno, "error opening %s shared memory file\n", shared_mem);

   if (fstat(shared_mem_fd, &fdstat) != 0)
      XERROR (errno, "fstat");

   if (fdstat.st_size == sizeof(VgdbShared64))
      s = (void*) &shared64;
   else if (fdstat.st_size == sizeof(VgdbShared32))
      s = (void*) &shared32;
   else
#if VEX_HOST_WORDSIZE == 8
      XERROR (0,
              "error size shared memory file %s.\n"
              "expecting size %d (64bits) or %d (32bits) got %ld.\n",
              shared_mem,
              (int) sizeof(VgdbShared64), (int) sizeof(VgdbShared32), 
              (long int)fdstat.st_size);
#elif VEX_HOST_WORDSIZE == 4
      XERROR (0,
              "error size shared memory file %s.\n"
              "expecting size %d (32bits) got %ld.\n",
              shared_mem,
              (int) sizeof(VgdbShared32), 
              fdstat.st_size);
#else
# error "unexpected wordsize"
#endif

#if VEX_HOST_WORDSIZE == 4
   if (shared64 != NULL)
      XERROR (0, "cannot use 32 bits vgdb with a 64bits valgrind process\n");
   /* But we can use a 64 bits vgdb with a 32 bits valgrind */
#endif

   *s = (void*) mmap (NULL, fdstat.st_size, 
                      PROT_READ|PROT_WRITE, MAP_SHARED, 
                      shared_mem_fd, 0);

   if (*s == (void *) -1)
      XERROR (errno, "error mmap shared memory file %s\n", shared_mem);

}

#if VEX_HOST_WORDSIZE == 8
typedef Addr64 CORE_ADDR;
typedef Addr64 PTRACE_XFER_TYPE;
typedef void* PTRACE_ARG3_TYPE;
#elif VEX_HOST_WORDSIZE == 4
typedef Addr32 CORE_ADDR;
typedef Addr32 PTRACE_XFER_TYPE;
typedef void* PTRACE_ARG3_TYPE;
#else
# error "unexpected wordsize"
#endif

static Bool pid_of_save_regs_continued = False;
// True if we have continued pid_of_save_regs after PTRACE_ATTACH

static Bool dying = False;
// Set to True when loss of connection indicating that the Valgrind
// process is dying.

/* To be called when connection with valgrind is lost.  In case we
have lost the connection, it means that Valgrind has closed the
connection and is busy exiting. We can't and don't have to stop it in
this case. */
static
void valgrind_dying(void)
{
   pid_of_save_regs_continued = False;
   dying = True;
}


#ifdef PTRACEINVOKER
/* ptrace_(read|write)_memory are modified extracts of linux-low.c
   from gdb 6.6. Copyrighted FSF */
/* Copy LEN bytes from inferior's memory starting at MEMADDR
   to debugger memory starting at MYADDR.  */

static
int ptrace_read_memory (pid_t inferior_pid, CORE_ADDR memaddr,
                        unsigned char *myaddr, int len)
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

/* Copy LEN bytes of data from debugger memory at MYADDR
   to inferior's memory at MEMADDR.
   On failure (cannot write the inferior)
   returns the value of errno.  */
__attribute__((unused)) /* not used on all platforms */
static
int ptrace_write_memory (pid_t inferior_pid, CORE_ADDR memaddr, 
                         const unsigned char *myaddr, int len)
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
         PDEBUG (1, "%02x", (unsigned)myaddr[i]);
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
static VgdbThreadState vgdb_threads[VG_N_THREADS];

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
   static char result[256];
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
Bool waitstopped (int pid, int signal_expected, char *msg)
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

      if (WIFEXITED(status)) {
         shutting_down = True;
         return False;
      }

      assert (WIFSTOPPED(status));
      signal_received = WSTOPSIG(status);
      if (signal_received == signal_expected)
         break;

      /* pid received a signal which is not the signal we are waiting for.
         We continue pid, transmitting this signal. */
      DEBUG(1, "waitstopped PTRACE_CONT with signal %d\n", signal_received);
      res = ptrace (PTRACE_CONT, pid, NULL, signal_received);
      if (res != 0) {
         ERROR(errno, "waitstopped PTRACE_CONT\n");
         return False;
      }
   }

   return True;
}

/* Stops the given pid, wait for the process to be stopped.
   Returns True if succesful, False otherwise.
   msg is used in tracing and error reporting. */
static
Bool stop (int pid, char *msg)
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
   Returns True if succesful, False otherwise.
   msg is used in tracing and error reporting. */
static
Bool attach (int pid, char *msg)
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
Bool acquire_and_suspend_threads(int pid)
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
      sz_tst = shared32->sizeof_ThreadState;
      off_status = shared32->offset_status;
      off_lwpid = shared32->offset_lwpid;
   }
   else if (shared64 != NULL) {
      vgt = shared64->threads;
      sz_tst = shared64->sizeof_ThreadState;
      off_status = shared64->offset_status;
      off_lwpid = shared64->offset_lwpid;
   } else {
      assert (0);
   }

   /* note: the entry 0 is unused */
   for (i = 1; i < VG_N_THREADS; i++) {
      vgt += sz_tst;
      rw = ptrace_read_memory(pid, vgt+off_status,
                              (unsigned char *)&(vgdb_threads[i].status),
                              sizeof(ThreadStatus));
      if (rw != 0) {
         ERROR(rw, "status ptrace_read_memory\n");
         return False;
      }
      
      rw = ptrace_read_memory(pid, vgt+off_lwpid,
                              (unsigned char *)&(vgdb_threads[i].lwpid),
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
void detach_from_all_threads(int pid)
{
   int i;
   long res;
   Bool pid_found = False;

   /* detach from all the threads  */
   for (i = 1; i < VG_N_THREADS; i++) {
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

   if (!pid_found && pid) {
      /* No threads are live. Process is busy stopping.
         We need to detach from pid explicitely. */
      DEBUG(1, "no thread live => PTRACE_DETACH pid %d\n", pid);
      res = ptrace (PTRACE_DETACH, pid, NULL, NULL);
      if (res != 0)
         ERROR(errno, "PTRACE_DETACH pid %d res %ld\n", pid, res);
   }
}

// if > 0, pid for which registers have to be restored.
static int pid_of_save_regs = 0;
static struct user user_save;

// The below indicates if ptrace_getregs (and ptrace_setregs) can be used.
// Note that some linux versions are defining PTRACE_GETREGS but using
// it gives back EIO.
// has_working_ptrace_getregs can take the following values:
//  -1 : PTRACE_GETREGS is defined
//       runtime check not yet done.
//   0 : PTRACE_GETREGS runtime check has failed.
//   1 : PTRACE_GETREGS defined and runtime check ok.
#ifdef PTRACE_GETREGS
static int has_working_ptrace_getregs = -1;
#endif

/* Get the registers from pid into regs.
   regs_bsz value gives the length of *regs. 
   Returns True if all ok, otherwise False. */
static
Bool getregs (int pid, void *regs, long regs_bsz)
{
   DEBUG(1, "getregs regs_bsz %ld\n", regs_bsz);
#  ifdef PTRACE_GETREGS
   if (has_working_ptrace_getregs) {
      // Platforms having GETREGS
      long res;
      DEBUG(1, "getregs PTRACE_GETREGS\n");
      res = ptrace (PTRACE_GETREGS, pid, NULL, regs);
      if (res == 0) {
         if (has_working_ptrace_getregs == -1) {
            // First call to PTRACE_GETREGS succesful =>
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

   // If neither PTRACE_GETREGS not PTRACE_PEEKUSER have returned,
   // then we are in serious trouble.
   assert (0);
}

/* Set the registers of pid to regs.
   regs_bsz value gives the length of *regs. 
   Returns True if all ok, otherwise False. */
static
Bool setregs (int pid, void *regs, long regs_bsz)
{
   DEBUG(1, "setregs regs_bsz %ld\n", regs_bsz);
// Note : the below is checking for GETREGS, not SETREGS
// as if one is defined and working, the other one should also work.
#  ifdef PTRACE_GETREGS
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
void restore_and_detach(int pid)
{
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
      pid_of_save_regs = 0;
   } else {
      DEBUG(1, "PTRACE_SETREGS restore registers: no pid\n");
   }
   detach_from_all_threads(pid);
}

/* Ensures that the gdbserver code is invoked by pid.
   If an error occurs, resets to the valgrind process
   to the state it has before being ptrace-d.
   Returns True if invoke successful, False otherwise.
*/
static
Bool invoke_gdbserver (int pid)
{
   static Bool ptrace_restrictions_msg_given = False;
   long res;
   Bool stopped;
   struct user user_mod;
   Addr sp;
   /* A specific int value is passed to invoke_gdbserver, to check
      everything goes according to the plan. */
   const int check = 0x8BADF00D; // ate bad food.

   const Addr bad_return = 0;
   // A bad return address will be pushed on the stack.
   // The function invoke_gdbserver cannot return. If ever it returns, a NULL
   // address pushed on the stack should ensure this is detected.

   /* Not yet attached. If problem, vgdb can abort,
      no cleanup needed.

      On Ubuntu>= 10.10, a /proc setting can disable ptrace.
      So, Valgrind has to SET_PTRACER this vgdb. Once this
      is done, this vgdb can ptrace the valgrind process. */

   DEBUG(1, "attach to 'main' pid %d\n", pid);
   if (!attach(pid, "attach main pid")) {
      if (!ptrace_restrictions_msg_given) {
         ptrace_restrictions_msg_given = True;
         ERROR(0, "error attach main pid %d\n", pid);
         ptrace_restrictions_msg();
      }
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
#elif defined(VGA_ppc32)
   sp = user_mod.regs.gpr[1];
#elif defined(VGA_ppc64)
   sp = user_mod.regs.gpr[1];
#elif defined(VGA_s390x)
   sp = user_mod.regs.gprs[15];
#elif defined(VGA_mips32)
   sp = user_mod.regs[29*2];
#else
   I_die_here : (sp) architecture missing in vgdb.c
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
                               (unsigned char *) &check, 
                               regsize);
      if (rw != 0) {
         ERROR(rw, "push check arg ptrace_write_memory");
         detach_from_all_threads(pid);
         return False;
      }

      sp = sp - regsize;
      DEBUG(1, "push bad_return return address ptrace_write_memory\n");
      // Note that for a 64 bits vgdb, only 4 bytes of NULL bad_return
      // are written.
      rw = ptrace_write_memory(pid, sp, 
                               (unsigned char *) &bad_return,
                               regsize);
      if (rw != 0) {
         ERROR(rw, "push bad_return return address ptrace_write_memory");
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

#elif defined(VGA_ppc32) || defined(VGA_ppc64)
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

#elif defined(VGA_s390x)
      XERROR(0, "(fn32) s390x has no 32bits implementation");
#elif defined(VGA_mips32)
      /* put check arg in register 4 */
      user_mod.regs[4*2] = check;
      user_mod.regs[4*2+1] = 0xffffffff; // sign extend $a0
      /* This sign extension is needed when vgdb 32 bits runs
         on a 64 bits OS. */
      /* put NULL return address in ra */
      user_mod.regs[31*2] = bad_return;
      user_mod.regs[31*2+1] = 0;
      user_mod.regs[34*2] = shared32->invoke_gdbserver;
      user_mod.regs[34*2+1] = 0;
      user_mod.regs[25*2] = shared32->invoke_gdbserver;
      user_mod.regs[25*2+1] = 0;
#else
      I_die_here : architecture missing in vgdb.c
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
      sp = sp - regsize;
      DEBUG(1, "push bad_return return address ptrace_write_memory\n");
      rw = ptrace_write_memory(pid, sp, 
                               (unsigned char *) &bad_return,
                               sizeof(bad_return));
      if (rw != 0) {
         ERROR(rw, "push bad_return return address ptrace_write_memory");
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
#elif defined(VGA_ppc32)
      assert(0); // cannot vgdb a 64 bits executable with a 32 bits exe
#elif defined(VGA_ppc64)
      Addr64 func_addr;
      Addr64 toc_addr;
      int rw;
      rw = ptrace_read_memory(pid, shared64->invoke_gdbserver,
                              (unsigned char *)&func_addr,
                              sizeof(Addr64));
      if (rw != 0) {
         ERROR(rw, "ppc64 read func_addr\n");
         detach_from_all_threads(pid);
         return False;
      }
      rw = ptrace_read_memory(pid, shared64->invoke_gdbserver+8,
                              (unsigned char *)&toc_addr,
                              sizeof(Addr64));
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
#elif defined(VGA_mips32)
      assert(0); // cannot vgdb a 64 bits executable with a 32 bits exe
#else
      I_die_here: architecture missing in vgdb.c
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
      /* Whatever kind of problem happened. We shutdown */
      shutting_down = True;
      return False;
   }
}
#endif

static 
void cleanup_restore_and_detach(void *v_pid)
{
   DEBUG(1, "cleanup_restore_and_detach dying: %d\n", dying);
#ifdef PTRACEINVOKER
   if (!dying)
      restore_and_detach(*(int*)v_pid);
#endif
}

/* This function loops till shutting_down becomes true.  In this loop,
   it verifies if valgrind process is reading the characters written
   by vgdb.  The verification is done every max_invoke_ms ms.  If
   valgrind is not reading characters, it will use invoke_gdbserver
   (if PTRACE_INVOKER is defined) to ensure that the gdbserver code is
   called soon by valgrind. */
static int max_invoke_ms = 100;
#define NEVER 99999999
static int cmd_time_out = NEVER;
static
void *invoke_gdbserver_in_valgrind(void *v_pid)
{
   struct timeval cmd_max_end_time;
   Bool cmd_started = False;
   struct timeval invoke_time;

   int pid = *(int *)v_pid;
   int written_by_vgdb_before_sleep;
   int seen_by_valgrind_before_sleep;
   
   int invoked_written = -1;
   unsigned int usecs;

   pthread_cleanup_push(cleanup_restore_and_detach, v_pid);

   while (!shutting_down) {
      written_by_vgdb_before_sleep = VS_written_by_vgdb;
      seen_by_valgrind_before_sleep = VS_seen_by_valgrind;
      DEBUG(3, 
            "written_by_vgdb_before_sleep %d "
            "seen_by_valgrind_before_sleep %d\n",
            written_by_vgdb_before_sleep,
            seen_by_valgrind_before_sleep);
      if (cmd_time_out != NEVER
          && !cmd_started
          && written_by_vgdb_before_sleep > seen_by_valgrind_before_sleep) {
         /* A command was started. Record the time at which it was started. */
         DEBUG(1, "IO for command started\n");
         gettimeofday(&cmd_max_end_time, NULL);
         cmd_max_end_time.tv_sec += cmd_time_out;
         cmd_started = True;
      }
      if (max_invoke_ms > 0) {
         usecs = 1000 * max_invoke_ms;
         gettimeofday(&invoke_time, NULL);
         invoke_time.tv_sec += max_invoke_ms / 1000;
         invoke_time.tv_usec += 1000 * (max_invoke_ms % 1000);
         invoke_time.tv_sec += invoke_time.tv_usec / (1000 * 1000);
         invoke_time.tv_usec = invoke_time.tv_usec % (1000 * 1000);
      } else {
         usecs = 0;
      }
      if (cmd_started) {
         // 0 usecs here means the thread just has to check gdbserver eats
         // the characters in <= cmd_time_out seconds.
         // We will just wait by 1 second max at a time.
         if (usecs == 0 || usecs > 1000 * 1000)
            usecs = 1000 * 1000;
      }
      usleep(usecs);

      /* If nothing happened during our sleep, let's try to wake up valgrind
         or check for cmd time out. */
      if (written_by_vgdb_before_sleep == VS_written_by_vgdb
          && seen_by_valgrind_before_sleep == VS_seen_by_valgrind
          && VS_written_by_vgdb > VS_seen_by_valgrind) {
         struct timeval now;
         gettimeofday(&now, NULL);
         DEBUG(2,
               "after sleep "
               "written_by_vgdb %d "
               "seen_by_valgrind %d "
               "invoked_written %d\n",
               VS_written_by_vgdb,
               VS_seen_by_valgrind,
               invoked_written);
         /* if the pid does not exist anymore, we better stop */
         if (kill(pid, 0) != 0)
           XERROR (errno, 
                   "invoke_gdbserver_in_valgrind: "
                   "check for pid %d existence failed\n", pid);
         if (cmd_started) {
            if (timercmp (&now, &cmd_max_end_time, >))
               XERROR (0, 
                       "pid %d did not handle a command in %d seconds\n",
                       pid, cmd_time_out);
         }
         if (max_invoke_ms > 0 && timercmp (&now, &invoke_time, >=)) {
            #if defined(PTRACEINVOKER)
            /* only need to wake up if the nr written has changed since
               last invoke. */
            if (invoked_written != written_by_vgdb_before_sleep) {
               if (invoke_gdbserver(pid)) {
                  /* If invoke succesful, no need to invoke again
                     for the same value of written_by_vgdb_before_sleep. */
                  invoked_written = written_by_vgdb_before_sleep;
               }
            }
            #else
            DEBUG(2, "invoke_gdbserver via ptrace not (yet) implemented\n");
            #endif
         }
      } else {
         // Something happened => restart timer check.
         if (cmd_time_out != NEVER) {
            DEBUG(2, "some IO was done => restart command\n");
            cmd_started = False;
         }
      }
   }
   pthread_cleanup_pop(0);
   return NULL;
}

static
int open_fifo (char* name, int flags, char* desc)
{
   int fd;
   DEBUG(1, "opening %s %s\n", name, desc);
   fd = open(name, flags);
   if (fd == -1)
      XERROR (errno, "error opening %s %s\n", name, desc);

   DEBUG(1, "opened %s %s fd %d\n", name, desc, fd);
   return fd;
}

/* acquire a lock on the first byte of the given fd. If not successful,
   exits with error.
   This allows to avoid having two vgdb speaking with the same Valgrind
   gdbserver as this causes serious headaches to the protocol. */
static
void acquire_lock (int fd, int valgrind_pid)
{
   struct flock fl;
   fl.l_type = F_WRLCK;
   fl.l_whence = SEEK_SET;
   fl.l_start = 0;
   fl.l_len = 1;
   if (fcntl(fd, F_SETLK, &fl) < 0) {
      if (errno == EAGAIN || errno == EACCES) {
         XERROR(errno, 
                "Cannot acquire lock.\n"
                "Probably vgdb pid %d already speaks with Valgrind pid %d\n",
                VS_vgdb_pid,
                valgrind_pid);
      } else {
         XERROR(errno, "cannot acquire lock.\n");
      }
   }

   /* Here, we have the lock. It will be released when fd will be closed. */
   /* We indicate our pid to Valgrind gdbserver */
   if (shared32 != NULL)
      shared32->vgdb_pid = getpid();
   else if (shared64 != NULL)
      shared64->vgdb_pid = getpid();
   else
      assert(0);
}

#define PBUFSIZ 16384 /* keep in sync with server.h */

/* read some characters from fd.
   Returns the nr of characters read, -1 if error.
   desc is a string used in tracing */
static
int read_buf (int fd, char* buf, char* desc)
{
   int nrread;
   DEBUG(2, "reading %s\n", desc);
   nrread = read(fd, buf, PBUFSIZ);
   if (nrread == -1) {
      ERROR (errno, "error reading %s\n", desc);
      return -1;
   }
   buf[nrread] = '\0';
   DEBUG(2, "read %s %s\n", desc, buf);
   return nrread;
}

/* write size bytes from buf to fd.
   desc is a description of the action for which the write is done.
   If notify, then add size to the shared cntr indicating to the 
   valgrind process that there is new data.
   Returns True if write is ok, False if there was a problem. */
static
Bool write_buf(int fd, char* buf, int size, char* desc, Bool notify)
{
   int nrwritten;
   int nrw;
   DEBUG(2, "writing %s len %d %s notify: %d\n", desc, size, buf, notify);
   nrwritten = 0;
   while (nrwritten < size) {
      nrw = write (fd, buf+nrwritten, size - nrwritten);
      if (nrw == -1) {
         ERROR(errno, "error write %s\n", desc);
         return False;
      }
      nrwritten = nrwritten + nrw;
      if (notify)
         add_written(nrw);
   }
   return True;
} 

typedef enum {
   FROM_GDB,
   TO_GDB,
   FROM_PID,
   TO_PID } ConnectionKind;
static const int NumConnectionKind = TO_PID+1;
static 
char *ppConnectionKind (ConnectionKind con)
{
   switch (con) {
   case FROM_GDB: return "FROM_GDB";
   case TO_GDB:   return "TO_GDB";
   case FROM_PID: return "FROM_PID";
   case TO_PID:   return "TO_PID";
   default:       return "invalid connection kind";
   }
}

static char *shared_mem;

static int from_gdb = 0; /* stdin by default, changed if --port is given. */
static char *from_gdb_to_pid; /* fifo name to write gdb command to pid */
/* Returns True in case read/write operations were done properly.
   Returns False in case of error.
   to_pid is the file descriptor to write to the process pid. */
static
Bool read_from_gdb_write_to_pid(int to_pid)
{
   char buf[PBUFSIZ+1]; // +1 for trailing \0
   int nrread;

   nrread = read_buf(from_gdb, buf, "from gdb on stdin");
   if (nrread <= 0) {
      if (nrread == 0) 
         DEBUG(1, "read 0 bytes from gdb => assume exit\n");
      else
         DEBUG(1, "error reading bytes from gdb\n");
      close (from_gdb);
      shutting_down = True;
      return False;
   }
   return write_buf(to_pid, buf, nrread, "to_pid", /* notify */ True);
}

static int to_gdb = 1; /* stdout by default, changed if --port is given. */
static char *to_gdb_from_pid; /* fifo name to read pid replies */
/* Returns True in case read/write operations were done properly.
   Returns False in case of error.
   from_pid is the file descriptor to read data from the process pid. */
static
Bool read_from_pid_write_to_gdb(int from_pid)
{
   char buf[PBUFSIZ+1]; // +1 for trailing \0
   int nrread;

   nrread = read_buf(from_pid, buf, "from pid");
   if (nrread <= 0) {
      if (nrread == 0) 
         DEBUG(1, "read 0 bytes from pid => assume exit\n");
      else
         DEBUG(1, "error reading bytes from pid\n");
      close (from_pid);
      shutting_down = True;
      return False;
   }
   return write_buf(to_gdb, buf, nrread, "to_gdb", /* notify */ False);
}

static
void wait_for_gdb_connect (int in_port)
{
   struct sockaddr_in addr;

   int listen_gdb = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
   int gdb_connect;
 
   if (-1 == listen_gdb) {
      XERROR(errno, "cannot create socket");
   }
 
    memset(&addr, 0, sizeof(addr));
 
    addr.sin_family = AF_INET;
    addr.sin_port = htons((unsigned short int)in_port);
    addr.sin_addr.s_addr = INADDR_ANY;
 
    if (-1 == bind(listen_gdb,(struct sockaddr *)&addr, sizeof(addr))) {
      XERROR(errno, "bind failed");
    }
    fprintf(stderr, "listening on port %d ...", in_port);
    fflush(stderr);
    if (-1 == listen(listen_gdb, 1)) {
      XERROR(errno, "error listen failed");
    }

    gdb_connect = accept(listen_gdb, NULL, NULL);
    if (gdb_connect < 0) {
        XERROR(errno, "accept failed");
    }
    fprintf(stderr, "connected.\n");
    fflush(stderr);
    close(listen_gdb);
    from_gdb = gdb_connect;
    to_gdb = gdb_connect;
}

/* prepares the FIFOs filenames, map the shared memory. */
static
void prepare_fifos_and_shared_mem(int pid)
{
   const HChar *user, *host;
   unsigned len;

   user = getenv("LOGNAME");
   if (user == NULL) user = getenv("USER");
   if (user == NULL) user = "???";

   host = getenv("HOST");
   if (host == NULL) host = getenv("HOSTNAME");
   if (host == NULL) host = "???";

   len = strlen(vgdb_prefix) + strlen(user) + strlen(host) + 40;
   from_gdb_to_pid = vmalloc (len);
   to_gdb_from_pid = vmalloc (len);
   shared_mem      = vmalloc (len);
   /* below 3 lines must match the equivalent in remote-utils.c */
   sprintf(from_gdb_to_pid, "%s-from-vgdb-to-%d-by-%s-on-%s",    vgdb_prefix,
           pid, user, host);
   sprintf(to_gdb_from_pid, "%s-to-vgdb-from-%d-by-%s-on-%s",    vgdb_prefix,
           pid, user, host);
   sprintf(shared_mem,      "%s-shared-mem-vgdb-%d-by-%s-on-%s", vgdb_prefix,
           pid, user, host);
   DEBUG (1, "vgdb: using %s %s %s\n", 
          from_gdb_to_pid, to_gdb_from_pid, shared_mem);

   map_vgdbshared(shared_mem);
}

/* Convert hex digit A to a number.  */

static int
fromhex (int a)
{
   if (a >= '0' && a <= '9')
      return a - '0';
   else if (a >= 'a' && a <= 'f')
      return a - 'a' + 10;
   else
      XERROR(0, "Reply contains invalid hex digit %c\n", a);
  return 0;
}

/* Returns next char from fd.  -1 if error, -2 if EOF.
   NB: must always call it with the same fd */
static int
readchar (int fd)
{
  static unsigned char buf[PBUFSIZ+1]; // +1 for trailing \0
  static int bufcnt = 0;
  static unsigned char *bufp;

  if (bufcnt-- > 0)
     return *bufp++;

  bufcnt = read_buf (fd, buf, "static buf readchar");

  if (bufcnt <= 0) {
     if (bufcnt == 0) {
        fprintf (stderr, "readchar: Got EOF\n");
        return -2;
     } else {
        ERROR (errno, "readchar\n");
        return -1;
     }
  }

  bufp = buf;
  bufcnt--;
  return *bufp++;
}

/* Read a packet from fromfd, with error checking,
   and store it in BUF.  
   Returns length of packet, or -1 if error or -2 if EOF.
   Writes ack on ackfd */

static int
getpkt (char *buf, int fromfd, int ackfd)
{
  char *bp;
  unsigned char csum, c1, c2;
  int c;
  
  while (1) {
     csum = 0;

     while (1) {
        c = readchar (fromfd);
        if (c == '$')
           break;
        DEBUG(2, "[getpkt: discarding char '%c']\n", c);
        if (c < 0)
           return c;
     }

     bp = buf;
     while (1) {
        c = readchar (fromfd);
        if (c < 0)
           return c;
        if (c == '#')
           break;
        if (c == '*') {
           int repeat;
           int r;
           int prev;
           prev = *(bp-1);
           csum += c;
           repeat = readchar (fromfd);
           csum += repeat;
           for (r = 0; r < repeat - 29; r ++)
              *bp++ = prev;
        } else {
           *bp++ = c;
           csum += c;
        }
     }
     *bp = 0;

     c1 = fromhex (readchar (fromfd));
     c2 = fromhex (readchar (fromfd));

     if (csum == (c1 << 4) + c2)
	break;

     fprintf (stderr, "Bad checksum, sentsum=0x%x, csum=0x%x, buf=%s\n",
              (c1 << 4) + c2, csum, buf);
     if (write (ackfd, "-", 1) != 1)
        ERROR(0, "error when writing - (nack)\n");
     else
        add_written(1);
  }

  DEBUG(2, "getpkt (\"%s\");  [sending ack] \n", buf);
  if (write (ackfd, "+", 1) != 1)
     ERROR(0, "error when writing + (ack)\n");
  else
     add_written(1);
  return bp - buf;
}

static int sigint = 0;
static int sigterm = 0;
static int sigpipe = 0;
static int sighup = 0;
static int sigusr1 = 0;
static int sigalrm = 0;
static int sigusr1_fd = -1;
static pthread_t invoke_gdbserver_in_valgrind_thread;

static
void received_signal (int signum)
{
   if (signum == SIGINT)
      sigint++;
   else if (signum == SIGUSR1) {
      sigusr1++;
      if (sigusr1_fd >= 0) {
         char control_c = '\003';
         write_buf(sigusr1_fd, &control_c, 1,
                   "write \\003 on SIGUSR1", /* notify */ True);
      }
   }
   else if (signum == SIGTERM) {
      shutting_down = True;
      sigterm++;
   } else if (signum == SIGHUP) {
      shutting_down = True;
      sighup++;
   } else if (signum == SIGPIPE) {
      sigpipe++;
   } else if (signum == SIGALRM) {
      sigalrm++;
#if defined(VGPV_arm_linux_android) || defined(VGPV_x86_linux_android)
      /* Android has no pthread_cancel. As it also does not have
         PTRACE_INVOKER, there is no need for cleanup action.
         So, we just do nothing. */
      DEBUG(1, "sigalrm received, no action on android\n");
#else
      /* Note: we cannot directly invoke restore_and_detach : this must
         be done by the thread that has attached. 
         We have in this thread pushed a cleanup handler that will
         cleanup what is needed. */
      DEBUG(1, "pthread_cancel invoke_gdbserver_in_valgrind_thread\n");
      pthread_cancel(invoke_gdbserver_in_valgrind_thread);
#endif
   } else {
      ERROR(0, "unexpected signal %d\n", signum);
   }
}

/* install the signal handlers allowing e.g. vgdb to cleanup in
   case of termination. */
static
void install_handlers(void)
{
   struct sigaction action, oldaction;

   action.sa_handler = received_signal;
   sigemptyset (&action.sa_mask);
   action.sa_flags = 0;
   
   /* SIGINT: when user types C-c in gdb, this sends
      a SIGINT to vgdb + causes a character to be sent to remote gdbserver.
      The later is enough to wakeup the valgrind process. */
   if (sigaction (SIGINT, &action, &oldaction) != 0)
      XERROR (errno, "vgdb error sigaction SIGINT\n");
   /* We might do something more intelligent than just
      reporting this SIGINT E.g. behave similarly to the gdb: two
      control-C without feedback from the debugged process would
      mean to stop debugging it. */

   /* SIGUSR1: this is used to facilitate automatic testing.  When
      vgdb receives this signal, it will simulate the user typing C-c. */
   if (sigaction (SIGUSR1, &action, &oldaction) != 0)
      XERROR (errno, "vgdb error sigaction SIGUSR1\n");
   

   /* SIGTERM: can receive this signal (e.g. from gdb) to terminate vgdb
      when detaching or similar. A clean shutdown will be done as both
      the read and write side will detect an end of file. */
   if (sigaction (SIGTERM, &action, &oldaction) != 0)
      XERROR (errno, "vgdb error sigaction SIGTERM\n");
   
   /* SIGPIPE: can receive this signal when gdb detaches or kill the
      process debugged: gdb will close its pipes to vgdb. vgdb
      must resist to this signal to allow a clean shutdown. */
   if (sigaction (SIGPIPE, &action, &oldaction) != 0)
      XERROR (errno, "vgdb error sigaction SIGPIPE\n");
   
   /* SIGALRM: in case invoke thread is blocked, alarm is used
      to cleanup.  */
   if (sigaction (SIGALRM, &action, &oldaction) != 0)
      XERROR (errno, "vgdb error sigaction SIGALRM\n");
}

/* close the FIFOs provided connections, terminate the invoker thread.  */
static
void close_connection(int to_pid, int from_pid)
{
   DEBUG(1, "nr received signals: sigint %d sigterm %d sighup %d sigpipe %d\n",
         sigint, sigterm, sighup, sigpipe);
   /* Note that we do not forward sigterm to the valgrind process:
      a sigterm signal is (probably) received from gdb if the user wants to
      kill the debugged process. The kill instruction has been given to
      the valgrind process, which should execute a clean exit. */

   /* We first close the connection to pid. The pid will then
      terminates its gdbserver work. We keep the from pid
      fifo opened till the invoker thread is finished.
      This allows the gdbserver to finish sending its last reply. */
   if (close(to_pid) != 0)
      ERROR(errno, "close to_pid\n");

   /* if there is a task that was busy trying to wake up valgrind
      process, we wait for it to be terminated otherwise threads
      in the valgrind process can stay stopped if vgdb main
      exits before the invoke thread had time to detach from
      all valgrind threads. */
   if (max_invoke_ms > 0 || cmd_time_out != NEVER) {
      int join;

      /* It is surprisingly complex to properly shutdown or exit the
         valgrind process in which gdbserver has been invoked through
         ptrace.  In the normal case (gdb detaches from the process,
         or process is continued), the valgrind process will reach the
         breakpoint place.  Using ptrace, vgdb will ensure the
         previous activity of the process is resumed (e.g. restart a
         blocking system call).  The special case is when gdb asks the
         valgrind process to exit (using either the "kill" command or
         "monitor exit").  In such a case, the valgrind process will
         call exit.  But a ptraced process will be blocked in exit,
         waiting for the ptracing process to detach or die. vgdb
         cannot detach unconditionally as otherwise, in the normal
         case, the valgrind process would stop abnormally with SIGSTOP
         (as vgdb would not be there to catch it). vgdb can also not
         die unconditionally otherwise again, similar problem.  So, we
         assume that most of the time, we arrive here in the normal
         case, and so, the breakpoint has been encountered by the
         valgrind process, so the invoker thread will exit and the
         join will succeed.  For the "kill" case, we cause an alarm
         signal to be sent after a few seconds. This means that in the
         normal case, the gdbserver code in valgrind process must have
         returned the control in less than the alarm nr of seconds,
         otherwise, valgrind will stop abnormally with SIGSTOP. */
      (void) alarm (3);

      DEBUG(1, "joining with invoke_gdbserver_in_valgrind_thread\n");
      join = pthread_join(invoke_gdbserver_in_valgrind_thread, NULL);
      if (join != 0)
         XERROR 
            (join, 
             "vgdb error pthread_join invoke_gdbserver_in_valgrind_thread\n");
   }
   if (close(from_pid) != 0)
      ERROR(errno, "close from_pid\n");
}

/* Relay data between gdb and Valgrind gdbserver, till EOF or an
   error is encountered. */
static
void gdb_relay (int pid)
{
   int from_pid = -1; /* fd to read from pid */
   int to_pid = -1; /* fd to write to pid */

   int shutdown_loop = 0;
   fprintf (stderr, "relaying data between gdb and process %d\n", pid);
   fflush (stderr);

   if (max_invoke_ms > 0)
      pthread_create(&invoke_gdbserver_in_valgrind_thread, NULL, 
                     invoke_gdbserver_in_valgrind, (void *) &pid);
   to_pid = open_fifo(from_gdb_to_pid, O_WRONLY, "write to pid");
   acquire_lock (shared_mem_fd, pid);
   
   from_pid = open_fifo (to_gdb_from_pid, O_RDONLY|O_NONBLOCK, 
                         "read mode from pid");

   sigusr1_fd = to_pid; /* allow simulating user typing control-c */

   while (1) {
      ConnectionKind ck;
      int ret;
      struct pollfd pollfds[NumConnectionKind];
      
      /* watch data written by gdb, watch POLLERR on both gdb fd */
      pollfds[FROM_GDB].fd = from_gdb;
      pollfds[FROM_GDB].events = POLLIN;
      pollfds[FROM_GDB].revents = 0;
      pollfds[TO_GDB].fd = to_gdb;
      pollfds[TO_GDB].events = 0;
      pollfds[TO_GDB].revents = 0;
      
      /* watch data written by pid, watch POLLERR on both pid fd */
      pollfds[FROM_PID].fd = from_pid;
      pollfds[FROM_PID].events = POLLIN;
      pollfds[FROM_PID].revents = 0;
      pollfds[TO_PID].fd = to_pid;
      pollfds[TO_PID].events = 0;
      pollfds[TO_PID].revents = 0;
      
      ret = poll(pollfds, 
                 NumConnectionKind, 
                 (shutting_down ? 
                  1 /* one second */ 
                  : -1 /* infinite */));
      DEBUG(2, "poll ret %d errno %d\n", ret, errno);

      /* check for unexpected error */
      if (ret <= 0 && errno != EINTR) {
         ERROR (errno, "unexpected poll ret %d\n", ret);
         shutting_down = True;
         break;
      }
      
      /* check for data to read */
      for (ck = 0; ck < NumConnectionKind; ck ++) {
         if (pollfds[ck].revents & POLLIN) {
            switch (ck) {
            case FROM_GDB: 
               if (!read_from_gdb_write_to_pid(to_pid))
                  shutting_down = True;
               break;
            case FROM_PID:
               if (!read_from_pid_write_to_gdb(from_pid))
                  shutting_down = True;
               break;
            default: XERROR(0, "unexpected POLLIN on %s\n",
                               ppConnectionKind(ck));
            }
         }
      }

      /* check for an fd being in error condition */
      for (ck = 0; ck < NumConnectionKind; ck ++) {
         if (pollfds[ck].revents & POLLERR) {
            DEBUG(1, "connection %s fd %d POLLERR error condition\n",
                     ppConnectionKind(ck), pollfds[ck].fd);
            valgrind_dying();
            shutting_down = True;
         }
         if (pollfds[ck].revents & POLLHUP) {
            DEBUG(1, "connection %s fd %d POLLHUP error condition\n",
                  ppConnectionKind(ck), pollfds[ck].fd);
            valgrind_dying();
            shutting_down = True;
         }
         if (pollfds[ck].revents & POLLNVAL) {
            DEBUG(1, "connection %s fd %d POLLNVAL error condition\n",
                  ppConnectionKind(ck), pollfds[ck].fd);
            valgrind_dying();
            shutting_down = True;
         }
      }
      
      if (shutting_down) {
         /* we let some time to the final packets to be transferred */
         shutdown_loop++;
         if (shutdown_loop > 3)
            break;
      }
   }
   close_connection(to_pid, from_pid);
}

static int packet_len_for_command(char *cmd)
{
   /* cmd will be send as a packet $qRcmd,xxxx....................xx#cc      */
   return                          7+     2*strlen(cmd)             +3  + 1;
}

/* hyper-minimal protocol implementation that
   sends the provided commands (using qRcmd packets)
   and read and display their replies. */
static
void standalone_send_commands(int pid, 
                              int last_command,
                              char *commands[] )
{
   int from_pid = -1; /* fd to read from pid */
   int to_pid = -1; /* fd to write to pid */

   int i;
   int hi;
   unsigned char hex[3];
   unsigned char cksum;
   unsigned char *hexcommand;
   unsigned char buf[PBUFSIZ+1]; // +1 for trailing \0
   int buflen;
   int nc;


   if (max_invoke_ms > 0 || cmd_time_out != NEVER)
      pthread_create(&invoke_gdbserver_in_valgrind_thread, NULL, 
                     invoke_gdbserver_in_valgrind, (void *) &pid);

   to_pid = open_fifo(from_gdb_to_pid, O_WRONLY, "write to pid");
   acquire_lock (shared_mem_fd, pid);

   /* first send a C-c \003 to pid, so that it wakes up the process
      After that, we can open the fifo from the pid in read mode
      We then start to wait for packets (normally first a resume reply)
      At that point, we send our command and expect replies */
   buf[0] = '\003';
   write_buf(to_pid, buf, 1, "write \\003 to wake up", /* notify */ True);
   from_pid = open_fifo(to_gdb_from_pid, O_RDONLY, 
                        "read cmd result from pid");
   
   for (nc = 0; nc <= last_command; nc++) {
      fprintf (stderr, "sending command %s to pid %d\n", commands[nc], pid);
      fflush (stderr);
      
      /* prepare hexcommand $qRcmd,xxxx....................xx#cc      */
      hexcommand = vmalloc (packet_len_for_command(commands[nc]));
      hexcommand[0] = 0;
      strcat (hexcommand, "$qRcmd,");
      for (i = 0; i < strlen(commands[nc]); i++) {
         sprintf(hex, "%02x", commands[nc][i]);
         strcat (hexcommand, hex);
      }
      /* checksum (but without the $) */
      cksum = 0;
      for (hi = 1; hi < strlen(hexcommand); hi++)
         cksum+=hexcommand[hi];
      strcat(hexcommand, "#");
      sprintf(hex, "%02x", cksum);
      strcat(hexcommand, hex);
      write_buf(to_pid, hexcommand, strlen(hexcommand), 
                "writing hex command to pid", /* notify */ True);

      /* we exit of the below loop explicitely when the command has
         been handled or because a signal handler will set
         shutting_down. */
      while (!shutting_down) {
         buflen = getpkt(buf, from_pid, to_pid);
         if (buflen < 0) {
            ERROR (0, "error reading packet\n");
            if (buflen == -2)
               valgrind_dying();
            break;
         }
         if (strlen(buf) == 0) {
            DEBUG(0, "empty packet rcvd (packet qRcmd not recognised?)\n");
            break;
         }
         if (strcmp(buf, "OK") == 0) {
            DEBUG(1, "OK packet rcvd\n");
            break;
         }
         if (buf[0] == 'E') {
            DEBUG(0, 
                  "E NN error packet rcvd: %s (unknown monitor command?)\n",
                  buf);
            break;
         }
         if (buf[0] == 'W') {
            DEBUG(0, "W stopped packet rcvd: %s\n", buf);
            break;
         }
         if (buf[0] == 'T') {
            DEBUG(1, "T resume reply packet received: %s\n", buf);
            continue;
         }
         
         /* must be here an O packet with hex encoded string reply 
            => decode and print it */
         if (buf[0] != 'O') {
            DEBUG(0, "expecting O packet, received: %s\n", buf);
            continue;
         }
         {
            char buf_print[buflen/2 + 1];
            for (i = 1; i < buflen; i = i + 2)
               buf_print[i/2] = (fromhex(*(buf+i)) << 4) 
                     + fromhex(*(buf+i+1));
            buf_print[buflen/2] = 0;
            printf("%s", buf_print);
            fflush(stdout);
         }
      }
      free (hexcommand);
   }
   shutting_down = True;

   close_connection(to_pid, from_pid);
}

/* report to user the existence of a vgdb-able valgrind process 
   with given pid */
static
void report_pid (int pid, Bool on_stdout)
{
   char cmdline_file[100];
   char cmdline[1000];
   int fd;
   int i, sz;

   sprintf(cmdline_file, "/proc/%d/cmdline", pid);
   fd = open (cmdline_file, O_RDONLY);
   if (fd == -1) {
      DEBUG(1, "error opening cmdline file %s %s\n", 
            cmdline_file, strerror(errno));
      sprintf(cmdline, "(could not obtain process command line)");
   } else {
      sz = read(fd, cmdline, 1000);
      for (i = 0; i < sz; i++)
         if (cmdline[i] == 0)
            cmdline[i] = ' ';
      cmdline[sz] = 0;
      close (fd);
   }  
   fprintf((on_stdout ? stdout : stderr), "use --pid=%d for %s\n", pid, cmdline);
   fflush((on_stdout ? stdout : stderr));
}

/* Possibly produces additional usage information documenting the
   ptrace restrictions. */
static
void ptrace_restrictions_msg(void)
{
#  ifdef PR_SET_PTRACER
   char *ptrace_scope_setting_file = "/proc/sys/kernel/yama/ptrace_scope";
   int fd = -1;
   char ptrace_scope = 'X';
   fd = open (ptrace_scope_setting_file, O_RDONLY, 0);
   if (fd >= 0 && (read (fd, &ptrace_scope, 1) == 1) && (ptrace_scope != '0')) {
      fprintf (stderr,
               "Note: your kernel restricts ptrace invoker using %s\n"
               "vgdb will only be able to attach to a Valgrind process\n"
               "blocked in a system call *after* an initial successful attach\n",
               ptrace_scope_setting_file);
   } else if (ptrace_scope == 'X') {
      DEBUG (1, 
             "PR_SET_PTRACER defined"
             " but could not determine ptrace scope from %s\n",
             ptrace_scope_setting_file);
   }
   if (fd >= 0)
      close (fd);
#  endif

#  ifndef PTRACEINVOKER
   fprintf(stderr, 
           "Note: ptrace invoker not implemented\n"
           "For more info: read user manual section"
           " 'Limitations of the Valgrind gdbserver'\n");
#  endif
}

static
void usage(void)
{
   fprintf(stderr,
"Usage: vgdb [OPTION]... [[-c] COMMAND]...\n"
"vgdb (valgrind gdb) has two usages\n"
"  1. standalone to send monitor commands to a Valgrind gdbserver.\n"
"     The OPTION(s) must be followed by the command to send\n"
"     To send more than one command, separate the commands with -c\n"
"  2. relay application between gdb and a Valgrind gdbserver.\n"
"     Only OPTION(s) can be given.\n"
"\n"
" OPTIONS are [--pid=<number>] [--vgdb-prefix=<prefix>]\n"
"             [--wait=<number>] [--max-invoke-ms=<number>]\n"
"             [--port=<portnr>\n"
"             [--cmd-time-out=<number>] [-l] [-D] [-d]\n"
"             \n"
"  --pid arg must be given if multiple Valgrind gdbservers are found.\n"
"  --vgdb-prefix arg must be given to both Valgrind and vgdb utility\n"
"      if you want to change the default prefix for the FIFOs communication\n"
"      between the Valgrind gdbserver and vgdb.\n"
"  --wait (default 0) tells vgdb to check during the specified number\n"
"      of seconds if a Valgrind gdbserver can be found.\n"
"  --max-invoke-ms (default 100) gives the nr of milli-seconds after which vgdb\n"
"      will force the invocation of the Valgrind gdbserver (if the Valgrind\n"
"         process is blocked in a system call).\n"
"  --port instructs vgdb to listen for gdb on the specified port nr.\n"
"  --cmd-time-out (default 99999999) tells vgdb to exit if the found Valgrind\n"
"     gdbserver has not processed a command after number seconds\n"
"  -l  arg tells to show the list of running Valgrind gdbserver and then exit.\n"
"  -D  arg tells to show shared mem status and then exit.\n"
"  -d  arg tells to show debug info. Multiple -d args for more debug info\n"
"\n"
"  -h --help shows this message\n"
"  To get help from the Valgrind gdbserver, use vgdb help\n"
"\n"
           );
   ptrace_restrictions_msg();  
}

/* If show_list, outputs on stdout the list of Valgrind processes with gdbserver activated.
                 and then exits.

   else if arg_pid == -1, waits maximum check_trials seconds to discover
   a valgrind pid appearing.

   Otherwise verify arg_pid is valid and corresponds to a Valgrind process
   with gdbserver activated.

   Returns the pid to work with
   or exits in case of error (e.g. no pid found corresponding to arg_pid */

static
int search_arg_pid(int arg_pid, int check_trials, Bool show_list)
{
   int i;
   int pid = -1;

   if (arg_pid == 0 || arg_pid < -1) {
      fprintf (stderr, "vgdb error: invalid pid %d given\n", arg_pid);
      exit (1);
   } else {
      /* search for a matching named fifo. 
         If we have been given a pid, we will check that the matching FIFO is
         there (or wait the nr of check_trials for this to appear).
         If no pid has been given, then if we find only one FIFO,
         we will use this to build the pid to use.
         If we find multiple processes with valid FIFO, we report them and will
         exit with an error. */
      DIR *vgdb_dir;
      char *vgdb_dir_name = vmalloc (strlen (vgdb_prefix) + 3);
      struct dirent *f;
      int is;
      int nr_valid_pid = 0;
      const char *suffix = "-from-vgdb-to-"; /* followed by pid */
      char *vgdb_format = vmalloc (strlen(vgdb_prefix) + strlen(suffix) + 1);
      
      strcpy (vgdb_format, vgdb_prefix);
      strcat (vgdb_format, suffix);
      
      strcpy (vgdb_dir_name, vgdb_prefix);
      
      for (is = strlen(vgdb_prefix) - 1; is >= 0; is--)
         if (vgdb_dir_name[is] == '/') {
            vgdb_dir_name[is+1] = '\0';
            break;
         }
      if (strlen(vgdb_dir_name) == 0)
         strcpy (vgdb_dir_name, "./");

      DEBUG(1, "searching pid in directory %s format %s\n", 
            vgdb_dir_name, vgdb_format);

      /* try to find FIFOs with valid pid.
         On exit of the loop, pid is set to:
         the last pid found if show_list (or -1 if no process was listed)
         -1 if no FIFOs matching a running process is found
         -2 if multiple FIFOs of running processes are found
         otherwise it is set to the (only) pid found that can be debugged
      */
      for (i = 0; i < check_trials; i++) {
         DEBUG(1, "check_trial %d \n", i);
         if (i > 0)
           /* wait one second before checking again */
           sleep(1);

         vgdb_dir = opendir (vgdb_dir_name);
         if (vgdb_dir == NULL)
            XERROR (errno, 
                    "vgdb error: opening directory %s searching vgdb fifo\n", 
                    vgdb_dir_name);
      
         errno = 0; /* avoid complain if vgdb_dir is empty */
         while ((f = readdir (vgdb_dir))) {
            struct stat st;
            char pathname[strlen(vgdb_dir_name) + strlen(f->d_name) + 1];
            char *wrongpid;
            int newpid;

            strcpy (pathname, vgdb_dir_name);
            strcat (pathname, f->d_name);
            DEBUG(3, "checking pathname is FIFO %s\n", pathname);
            if (stat (pathname, &st) != 0) {
               if (debuglevel >= 3)
                  ERROR (errno, "vgdb error: stat %s searching vgdb fifo\n", 
                         pathname);
            } else if (S_ISFIFO (st.st_mode)) {
               DEBUG(3, "trying FIFO %s\n", pathname);
               if (strncmp (pathname, vgdb_format, 
                            strlen (vgdb_format)) == 0) {
                  newpid = strtol(pathname + strlen (vgdb_format), 
                                  &wrongpid, 10);
                  if (*wrongpid == '-' && newpid > 0 
                      && kill (newpid, 0) == 0) {
                     nr_valid_pid++;
                     if (show_list) {
                        report_pid (newpid, /*on_stdout*/ True);
                        pid = newpid;
                     } else if (arg_pid != -1) {
                        if (arg_pid == newpid) {
                           pid = newpid;
                        }
                     } else if (nr_valid_pid > 1) {
                        if (nr_valid_pid == 2) {
                           fprintf 
                              (stderr, 
                               "no --pid= arg given"
                               " and multiple valgrind pids found:\n");
                           report_pid (pid, /*on_stdout*/ False);
                        }
                        pid = -2;
                        report_pid (newpid, /*on_stdout*/ False);
                     } else {
                        pid = newpid;
                     }
                  }
               }
            }
            errno = 0; /* avoid complain if at the end of vgdb_dir */
         }
         if (f == NULL && errno != 0)
            XERROR (errno, "vgdb error: reading directory %s for vgdb fifo\n", 
                    vgdb_dir_name);

         closedir (vgdb_dir);
         if (pid != -1)
            break;
      }

      free (vgdb_dir_name);
      free (vgdb_format);
   }
   
   if (show_list) {
      exit (1);
   } else if (pid == -1) {
      if (arg_pid == -1)
         fprintf (stderr, "vgdb error: no FIFO found and no pid given\n");
      else
         fprintf (stderr, "vgdb error: no FIFO found matching pid %d\n", 
                  arg_pid);
      exit (1);
   }
   else if (pid == -2) {
      /* no arg_pid given, multiple FIFOs found */
      exit (1);
   }
   else {
      return pid;
   }
}

/* return true if the numeric value of an option of the 
   form --xxxxxxxxx=<number> could properly be extracted
   from arg. If True is returned, *value contains the
   extracted value.*/
static
Bool numeric_val(char* arg, int *value)
{
   const char *eq_pos = strchr(arg, '=');
   char *wrong;
   long long int long_value;

   if (eq_pos == NULL)
      return False;

   long_value = strtoll(eq_pos+1, &wrong, 10);
   if (long_value < 0 || long_value > INT_MAX)
      return False;
   if (*wrong)
      return False;

   *value = (int) long_value;
   return True;
}

/* true if arg matches the provided option */
static
Bool is_opt(char* arg, char *option)
{
   int option_len = strlen(option);
   if (option[option_len-1] == '=')
      return (0 == strncmp(option, arg, option_len));
   else
      return (0 == strcmp(option, arg));
}

/* Parse command lines options. If error(s), exits.
   Otherwise returns the options in *p_... args.
   commands must be big enough for the commands extracted from argv.
   On return, *p_last_command gives the position in commands where
   the last command has been allocated (using vmalloc). */
static
void parse_options(int argc, char** argv,
                   Bool *p_show_shared_mem,
                   Bool *p_show_list,
                   int *p_arg_pid,
                   int *p_check_trials,
                   int *p_port,
                   int *p_last_command,
                   char *commands[])
{
   Bool show_shared_mem = False;
   Bool show_list = False;
   int arg_pid = -1;
   int check_trials = 1;
   int last_command = -1;
   int int_port = 0;

   int i;
   int arg_errors = 0;

   for (i = 1; i < argc; i++) {
      if (is_opt(argv[i], "--help") || is_opt(argv[i], "-h")) {
         usage();
         exit(0);
      } else if (is_opt(argv[i], "-d")) {
         debuglevel++;
      } else if (is_opt(argv[i], "-D")) {
         show_shared_mem = True;
      } else if (is_opt(argv[i], "-l")) {
         show_list = True;
      } else if (is_opt(argv[i], "--pid=")) {
         int newpid;
         if (!numeric_val(argv[i], &newpid)) {
            fprintf (stderr, "invalid --pid argument %s\n", argv[i]);
            arg_errors++;
         } else if (arg_pid != -1) {
            fprintf (stderr, "multiple --pid arguments given\n");
            arg_errors++;
         } else {
            arg_pid = newpid;
         }
      } else if (is_opt(argv[i], "--wait=")) {
         if (!numeric_val(argv[i], &check_trials)) {
            fprintf (stderr, "invalid --wait argument %s\n", argv[i]);
            arg_errors++;
         }
      } else if (is_opt(argv[i], "--max-invoke-ms=")) {
         if (!numeric_val(argv[i], &max_invoke_ms)) {
            fprintf (stderr, "invalid --max-invoke-ms argument %s\n", argv[i]);
            arg_errors++;
         }
      } else if (is_opt(argv[i], "--cmd-time-out=")) {
         if (!numeric_val(argv[i], &cmd_time_out)) {
            fprintf (stderr, "invalid --cmd-time-out argument %s\n", argv[i]);
            arg_errors++;
         }
      } else if (is_opt(argv[i], "--port=")) {
         if (!numeric_val(argv[i], &int_port)) {
            fprintf (stderr, "invalid --port argument %s\n", argv[i]);
            arg_errors++;
         }
      } else if (is_opt(argv[i], "--vgdb-prefix=")) {
         vgdb_prefix = argv[i] + 14;
      } else if (is_opt(argv[i], "-c")) {
         last_command++;
         commands[last_command] = vmalloc (1);
         commands[last_command][0] = '\0';
      } else if (0 == strncmp(argv[i], "-", 1)) {
         fprintf (stderr, "unknown or invalid argument %s\n", argv[i]);
         arg_errors++;
      } else {
         int len;
         if (last_command == -1) {
            /* only one command, no -c command indicator */
            last_command++;
            commands[last_command] = vmalloc (1);
            commands[last_command][0] = '\0';
         }
         len = strlen(commands[last_command]);
         commands[last_command] = vrealloc (commands[last_command], 
                                            len + 1 + strlen(argv[i]) + 1);
         if (len > 0)
            strcat (commands[last_command], " ");
         strcat (commands[last_command], argv[i]);
         if (packet_len_for_command(commands[last_command]) > PBUFSIZ) {
            fprintf (stderr, "command %s too long\n", commands[last_command]);
            arg_errors++;
         }
            
      }
   }

   if (vgdb_prefix == NULL)
      vgdb_prefix = vgdb_prefix_default();

   if (isatty(0) 
       && !show_shared_mem 
       && !show_list
       && int_port == 0
       && last_command == -1) {
      arg_errors++;
      fprintf (stderr, 
               "Using vgdb standalone implies to give -D or -l or a COMMAND\n");
   }

   if (show_shared_mem && show_list) {
      arg_errors++;
      fprintf (stderr,
               "Can't use both -D and -l options\n");
   }

   if (max_invoke_ms > 0 
       && cmd_time_out != NEVER
       && (cmd_time_out * 1000) <= max_invoke_ms) {
      arg_errors++;
      fprintf (stderr,
               "--max-invoke-ms must be < --cmd-time-out * 1000\n");
   }

   if (show_list && arg_pid != -1) {
      arg_errors++;
      fprintf (stderr,
               "Can't use both --pid and -l options\n");
   }

   if (int_port > 0 && last_command != -1) {
      arg_errors++;
      fprintf (stderr,
               "Can't use --port to send commands\n");
   }

   if (arg_errors > 0) {
      fprintf (stderr, "args error. Try `vgdb --help` for more information\n");
      exit(1);
   }

   *p_show_shared_mem = show_shared_mem;
   *p_show_list = show_list;
   *p_arg_pid = arg_pid;
   *p_check_trials = check_trials;
   *p_port = int_port;
   *p_last_command = last_command;
}

int main(int argc, char** argv)
{
   int i;
   int pid;

   Bool show_shared_mem;
   Bool show_list;
   int arg_pid;
   int check_trials;
   int in_port;
   int last_command;
   char *commands[argc]; // we will never have more commands than args.

   parse_options(argc, argv,
                 &show_shared_mem,
                 &show_list,
                 &arg_pid,
                 &check_trials,
                 &in_port,
                 &last_command,
                 commands);
  
   /* when we are working as a relay for gdb, handle some signals by
      only reporting them (according to debug level). Also handle these
      when ptrace will be used: vgdb must clean up the ptrace effect before
      dying. */
   if (max_invoke_ms > 0 || last_command == -1)
      install_handlers();

   pid = search_arg_pid (arg_pid, check_trials, show_list);

   prepare_fifos_and_shared_mem(pid);

   if (in_port > 0)
      wait_for_gdb_connect(in_port);

   if (show_shared_mem) {
      fprintf(stderr, 
              "vgdb %d "
              "written_by_vgdb %d "
              "seen_by_valgrind %d\n"
              "vgdb pid %d\n",
              VS_vgdb_pid,
              VS_written_by_vgdb,
              VS_seen_by_valgrind,
              VS_vgdb_pid);
      exit (0);
   }

   if (last_command >= 0) {
      standalone_send_commands(pid, last_command, commands);
   } else {
      gdb_relay(pid);
   }
      

   free (from_gdb_to_pid);
   free (to_gdb_from_pid);
   free (shared_mem);

   for (i = 0; i <= last_command; i++)
      free (commands[i]);
   return 0;
}
