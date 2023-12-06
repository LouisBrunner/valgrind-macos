/*--------------------------------------------------------------------*/
/*--- Relay between gdb and gdbserver embedded in valgrind  vgdb.c ---*/
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

/* For accept4.  */
#define _GNU_SOURCE
#include "vgdb.h"

#include "config.h"

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>

#include "m_gdbserver/remote-utils-shared.h"

/* vgdb has three usages:
   1. relay application between gdb and the gdbserver embedded in valgrind.
   2. standalone to send monitor commands to a running valgrind-ified process
   3. multi mode where vgdb uses the GDB extended remote protocol.

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

   When --multi is given vgdb communicates with GDB through the extended remote
   protocol and will launch valgrind whenever GDB sends the vRun packet, after
   which it will function in the first mode, relaying packets between GDB and
   the gdbserver embedded in valgrind till that valgrind quits. vgdb will stay
   connected to GDB.
*/

int debuglevel;
Bool timestamp = False;
char timestamp_out[20];
static char *vgdb_prefix = NULL;
static char *valgrind_path = NULL;
static char **vargs;
static int cvargs = 0;

char *timestamp_str (Bool produce)
{
   static char out[50];
   char *ptr;
   struct timeval dbgtv;
   struct tm *ts_tm;

   if (produce) {
      gettimeofday(&dbgtv, NULL);
      ts_tm = localtime(&dbgtv.tv_sec);
      ptr = out + strftime(out, sizeof(out), "%H:%M:%S", ts_tm);
      sprintf(ptr, ".%6.6ld ", (long)dbgtv.tv_usec);
   } else {
      out[0] = 0;
   }
   return out;
}

/* Will be set to True when any condition indicating we have to shutdown
   is encountered. */
Bool shutting_down = False;

VgdbShared32 *shared32;
VgdbShared64 *shared64;
#define VS_written_by_vgdb (shared32 != NULL ?        \
                            shared32->written_by_vgdb \
                            : shared64->written_by_vgdb)
#define VS_seen_by_valgrind (shared32 != NULL ?         \
                             shared32->seen_by_valgrind \
                             : shared64->seen_by_valgrind)

#define VS_vgdb_pid (shared32 != NULL ? shared32->vgdb_pid : shared64->vgdb_pid)

void *vmalloc(size_t size)
{
   void * mem = malloc(size);
   if (mem == NULL)
      XERROR(errno, "can't allocate memory\n");
   return mem;
}

void *vrealloc(void *ptr,size_t size)
{
   void * mem = realloc(ptr, size);
   if (mem == NULL)
      XERROR(errno, "can't reallocate memory\n");
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
void map_vgdbshared(char* shared_mem, int check_trials)
{
   struct stat fdstat;
   void **s;
   int tries = 50;
   int err;

   /* valgrind might still be starting up, give it 5 seconds by
    * default, or check_trails seconds if it is set by --wait
    * to more than a second.  */
   if (check_trials > 1) {
     DEBUG(1, "check_trials %d\n", check_trials);
     tries = check_trials * 10;
   }
   do {
      shared_mem_fd = open(shared_mem, O_RDWR | O_CLOEXEC);
      err = errno;
      if (shared_mem_fd == -1 && err == ENOENT && tries > 0)
         usleep (100000); /* wait 0.1 seconds */
   } while (shared_mem_fd == -1 && err == ENOENT && tries-- > 0);

   /* shared_mem_fd will not be closed till vgdb exits. */

   if (shared_mem_fd == -1)
      XERROR(errno, "error opening %s shared memory file\n", shared_mem);

   if (fstat(shared_mem_fd, &fdstat) != 0)
      XERROR(errno, "fstat\n");

   if (fdstat.st_size == sizeof(VgdbShared64))
      s = (void*) &shared64;
   else if (fdstat.st_size == sizeof(VgdbShared32))
      s = (void*) &shared32;
   else
#if VEX_HOST_WORDSIZE == 8
      XERROR(0,
             "error size shared memory file %s.\n"
             "expecting size %d (64bits) or %d (32bits) got %ld.\n",
             shared_mem,
             (int) sizeof(VgdbShared64), (int) sizeof(VgdbShared32),
             (long int)fdstat.st_size);
#elif VEX_HOST_WORDSIZE == 4
      XERROR(0,
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
      XERROR(0, "cannot use 32 bits vgdb with a 64bits valgrind process\n");
   /* But we can use a 64 bits vgdb with a 32 bits valgrind */
#endif

   *s = (void*) mmap(NULL, fdstat.st_size,
                     PROT_READ|PROT_WRITE, MAP_SHARED,
                     shared_mem_fd, 0);

   if (*s == (void *) -1)
      XERROR(errno, "error mmap shared memory file %s\n", shared_mem);

}

/* This function loops till shutting_down becomes true.  In this loop,
   it verifies if valgrind process is reading the characters written
   by vgdb.  The verification is done every max_invoke_ms ms.  If
   valgrind is not reading characters, it will use invoker_invoke_gdbserver
   to ensure that the gdbserver code is called soon by valgrind. */
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

   pthread_cleanup_push(invoker_cleanup_restore_and_detach, v_pid);

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
           XERROR(errno,
                  "invoke_gdbserver_in_valgrind: "
                  "check for pid %d existence failed\n", pid);
         if (cmd_started) {
            if (timercmp(&now, &cmd_max_end_time, >))
               XERROR(0,
                      "pid %d did not handle a command in %d seconds\n",
                      pid, cmd_time_out);
         }
         if (max_invoke_ms > 0 && timercmp (&now, &invoke_time, >=)) {
            /* only need to wake up if the nr written has changed since
               last invoke. */
            if (invoked_written != written_by_vgdb_before_sleep) {
               if (invoker_invoke_gdbserver(pid)) {
                  /* If invoke successful, no need to invoke again
                     for the same value of written_by_vgdb_before_sleep. */
                  invoked_written = written_by_vgdb_before_sleep;
               }
            }
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
int open_fifo(const char* name, int flags, const char* desc)
{
   int fd;
   DEBUG(1, "opening %s %s\n", name, desc);
   fd = open(name, flags | O_CLOEXEC);
   if (fd == -1)
      XERROR(errno, "error opening %s %s\n", name, desc);

   DEBUG(1, "opened %s %s fd %d\n", name, desc, fd);
   return fd;
}

/* acquire a lock on the first byte of the given fd. If not successful,
   exits with error.
   This allows to avoid having two vgdb speaking with the same Valgrind
   gdbserver as this causes serious headaches to the protocol. */
static
void acquire_lock(int fd, int valgrind_pid)
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
int read_buf(int fd, char* buf, const char* desc)
{
   int nrread;
   DEBUG(2, "reading %s\n", desc);
   /* The file descriptor is on non-blocking mode and read_buf should only
      be called when poll gave us an POLLIN event signaling the file
      descriptor is ready for reading from. Still sometimes we do get an
      occasional EAGAIN. Just do as told in that case and try to read
      again.  */
   do {
      nrread = read(fd, buf, PBUFSIZ);
   } while (nrread == -1 && errno == EAGAIN);
   if (nrread == -1) {
      ERROR(errno, "error reading %s\n", desc);
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
Bool write_buf(int fd, const char* buf, int size, const char* desc, Bool notify)
{
   int nrwritten;
   int nrw;
   DEBUG(2, "writing %s len %d %.*s notify: %d\n", desc, size,
         size, buf, notify);
   nrwritten = 0;
   while (nrwritten < size) {
      nrw = write(fd, buf+nrwritten, size - nrwritten);
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
const char *ppConnectionKind(ConnectionKind con)
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

static int to_gdb = 1; /* stdout by default, changed if --port is given. */
static char *to_gdb_from_pid; /* fifo name to read pid replies */

/* Returns True in case read/write operations were done properly.
   Returns False in case of error.
   to_pid is the file descriptor to write to the process pid. */
static
Bool read_from_gdb_write_to_pid(int to_pid)
{
   char buf[PBUFSIZ+1]; // +1 for trailing \0
   int nrread;
   Bool ret;

   nrread = read_buf(from_gdb, buf, "from gdb on stdin");
   if (nrread <= 0) {
      if (nrread == 0)
         DEBUG(1, "read 0 bytes from gdb => assume exit\n");
      else
         DEBUG(1, "error reading bytes from gdb\n");
      close(from_gdb);
      shutting_down = True;
      return False;
   }
   ret = write_buf(to_pid, buf, nrread, "to_pid", /* notify */ True);
   if (!ret) {
      /* Let gdb know the packet couldn't be delivered.  */
      write_buf(to_gdb, "$E01#a6", 8, "error back to gdb", False);
   }
   return ret;
}

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
      close(from_pid);
      shutting_down = True;
      return False;
   }
   return write_buf(to_gdb, buf, nrread, "to_gdb", /* notify */ False);
}

static
void wait_for_gdb_connect(int in_port)
{
   struct sockaddr_in addr;

#ifdef SOCK_CLOEXEC
   int listen_gdb = socket(PF_INET, SOCK_STREAM | SOCK_CLOEXEC, IPPROTO_TCP);
#else
   int listen_gdb = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
#endif

   int gdb_connect;

   if (-1 == listen_gdb) {
      XERROR(errno, "cannot create socket\n");
   }

   /* allow address reuse to avoid "address already in use" errors */

   int one = 1;
   if (setsockopt(listen_gdb, SOL_SOCKET, SO_REUSEADDR,
                  &one, sizeof(one)) < 0) {
      XERROR(errno, "cannot enable address reuse\n");
   }

    memset(&addr, 0, sizeof(addr));

    addr.sin_family = AF_INET;
    addr.sin_port = htons((unsigned short int)in_port);
    addr.sin_addr.s_addr = INADDR_ANY;

    if (-1 == bind(listen_gdb, (struct sockaddr *)&addr, sizeof(addr))) {
      XERROR(errno, "bind failed\n");
    }
    TSFPRINTF(stderr, "listening on port %d ...", in_port);
    if (-1 == listen(listen_gdb, 1)) {
      XERROR(errno, "error listen failed\n");
    }

#ifdef SOCK_CLOEXEC
    gdb_connect = accept4(listen_gdb, NULL, NULL, SOCK_CLOEXEC);
#else
    gdb_connect = accept(listen_gdb, NULL, NULL);
#endif

    if (gdb_connect < 0) {
        XERROR(errno, "accept failed\n");
    }
    fprintf(stderr, "connected.\n");
    fflush(stderr);
    close(listen_gdb);
    from_gdb = gdb_connect;
    to_gdb = gdb_connect;
}

/* prepares the FIFOs filenames, map the shared memory. */
static
void prepare_fifos_and_shared_mem(int pid, int check_trials)
{
   const HChar *user, *host;
   unsigned len;

   user = getenv("LOGNAME");
   if (user == NULL) user = getenv("USER");
   if (user == NULL) user = "???";
   if (strchr(user, '/')) user = "???";

   host = getenv("HOST");
   if (host == NULL) host = getenv("HOSTNAME");
   if (host == NULL) host = "???";
   if (strchr(host, '/')) host = "???";

   len = strlen(vgdb_prefix) + strlen(user) + strlen(host) + 40;
   from_gdb_to_pid = vmalloc(len);
   to_gdb_from_pid = vmalloc(len);
   shared_mem      = vmalloc(len);
   /* below 3 lines must match the equivalent in remote-utils.c */
   sprintf(from_gdb_to_pid, "%s-from-vgdb-to-%d-by-%s-on-%s",    vgdb_prefix,
           pid, user, host);
   sprintf(to_gdb_from_pid, "%s-to-vgdb-from-%d-by-%s-on-%s",    vgdb_prefix,
           pid, user, host);
   sprintf(shared_mem,      "%s-shared-mem-vgdb-%d-by-%s-on-%s", vgdb_prefix,
           pid, user, host);
   DEBUG(1, "vgdb: using %s %s %s\n",
         from_gdb_to_pid, to_gdb_from_pid, shared_mem);

   map_vgdbshared(shared_mem, check_trials);
}

static void
cleanup_fifos_and_shared_mem(void)
{
   free(from_gdb_to_pid);
   free(to_gdb_from_pid);
   free(shared_mem);
   close(shared_mem_fd);
}

/* Convert hex digit A to a number.  */

static int
fromhex(int a)
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
readchar(int fd)
{
  static char buf[PBUFSIZ+1]; // +1 for trailing \0
  static int bufcnt = 0;
  static unsigned char *bufp;
  // unsigned bufp to e.g. avoid having 255 converted to int -1

  if (bufcnt-- > 0)
     return *bufp++;

  bufcnt = read_buf(fd, buf, "static buf readchar");

  if (bufcnt <= 0) {
     if (bufcnt == 0) {
        TSFPRINTF(stderr, "readchar: Got EOF\n");
        return -2;
     } else {
        ERROR(errno, "readchar\n");
        return -1;
     }
  }

  bufp = (unsigned char *)buf;
  bufcnt--;
  return *bufp++;
}

/* Read a packet from fromfd, with error checking,
   and store it in BUF.
   If checksum incorrect, writes a - on ackfd.
   Returns length of packet, or -1 if error or -2 if EOF. */
static int
getpkt(char *buf, int fromfd, int ackfd)
{
  char *bp;
  unsigned char csum, c1, c2;
  int c;

  while (1) {
     csum = 0;

     while (1) {
        c = readchar(fromfd);
        if (c == '$')
           break;
        DEBUG(2, "[getpkt: discarding char '%c']\n", c);
        if (c < 0)
           return c;
     }

     bp = buf;
     while (1) {
        c = readchar(fromfd);
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
           repeat = readchar(fromfd);
           csum += repeat;
           for (r = 0; r < repeat - 29; r ++)
              *bp++ = prev;
        } else {
           *bp++ = c;
           csum += c;
        }
     }
     *bp = 0;

     c1 = fromhex(readchar (fromfd));
     c2 = fromhex(readchar (fromfd));

     if (csum == (c1 << 4) + c2)
        break;

     TSFPRINTF(stderr, "Bad checksum, sentsum=0x%x, csum=0x%x, buf=%s\n",
               (c1 << 4) + c2, csum, buf);
     if (write(ackfd, "-", 1) != 1)
        ERROR(errno, "error when writing - (nack)\n");
     else
        add_written(1);
  }

  DEBUG(2, "getpkt (\"%s\");  [no ack] \n", buf);
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
void received_signal(int signum)
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
#if defined(VGPV_arm_linux_android) \
    || defined(VGPV_x86_linux_android) \
    || defined(VGPV_mips32_linux_android) \
    || defined(VGPV_arm64_linux_android)
      /* Android has no pthread_cancel. As it also does not have
         an invoker implementation, there is no need for cleanup action.
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
   sigemptyset(&action.sa_mask);
   action.sa_flags = 0;

   /* SIGINT: when user types C-c in gdb, this sends
      a SIGINT to vgdb + causes a character to be sent to remote gdbserver.
      The later is enough to wakeup the valgrind process. */
   if (sigaction(SIGINT, &action, &oldaction) != 0)
      XERROR(errno, "vgdb error sigaction SIGINT\n");
   /* We might do something more intelligent than just
      reporting this SIGINT E.g. behave similarly to the gdb: two
      control-C without feedback from the debugged process would
      mean to stop debugging it. */

   /* SIGUSR1: this is used to facilitate automatic testing.  When
      vgdb receives this signal, it will simulate the user typing C-c. */
   if (sigaction(SIGUSR1, &action, &oldaction) != 0)
      XERROR(errno, "vgdb error sigaction SIGUSR1\n");


   /* SIGTERM: can receive this signal (e.g. from gdb) to terminate vgdb
      when detaching or similar. A clean shutdown will be done as both
      the read and write side will detect an end of file. */
   if (sigaction(SIGTERM, &action, &oldaction) != 0)
      XERROR(errno, "vgdb error sigaction SIGTERM\n");

   /* SIGPIPE: can receive this signal when gdb detaches or kill the
      process debugged: gdb will close its pipes to vgdb. vgdb
      must resist to this signal to allow a clean shutdown. */
   if (sigaction(SIGPIPE, &action, &oldaction) != 0)
      XERROR(errno, "vgdb error sigaction SIGPIPE\n");

   /* SIGALRM: in case invoke thread is blocked, alarm is used
      to cleanup.  */
   if (sigaction(SIGALRM, &action, &oldaction) != 0)
      XERROR(errno, "vgdb error sigaction SIGALRM\n");

   /* unmask all signals, in case the process that launched vgdb
      masked some. */
   if (sigprocmask(SIG_SETMASK, &action.sa_mask, NULL) != 0)
      XERROR(errno, "vgdb error sigprocmask\n");
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
      (void) alarm(3);

      DEBUG(1, "joining with invoke_gdbserver_in_valgrind_thread\n");
      join = pthread_join(invoke_gdbserver_in_valgrind_thread, NULL);
      if (join != 0)
         XERROR
            (join,
             "vgdb error pthread_join invoke_gdbserver_in_valgrind_thread\n");
   }
#if !defined(VGO_freebsd)
   if (close(from_pid) != 0)
      ERROR(errno, "close from_pid\n");
#endif
}

/* Returns an allocated hex-decoded string from the buf. Stops decoding
   at end of buf (zero) or when seeing the delim char.  */
static
char *decode_hexstring (const char *buf, size_t prefixlen, size_t len)
{
   int buflen;
   char *buf_print;

   if (len)
       buflen = len;
   else
       buflen = strlen(buf) - prefixlen;

   buf_print = vmalloc (buflen/2 + 1);

   for (int i = 0; i < buflen; i = i + 2) {
      buf_print[i/2] = ((fromhex(buf[i+prefixlen]) << 4)
                        + fromhex(buf[i+prefixlen+1]));
   }
   buf_print[buflen/2] = '\0';
   DEBUG(1, "decode_hexstring: %s\n", buf_print);
   return buf_print;
}

static Bool
write_to_gdb (const char *m, int cnt)
{
   int written = 0;
   while (written < cnt) {
      int res = write (to_gdb, m + written, cnt - written);
      if (res < 0) {
         perror ("write_to_gdb");
         return False;
      }
      written += res;
   }

   return True;
}

static Bool
write_checksum (const char *str)
{
  unsigned char csum = 0;
  int i = 0;
  while (str[i] != 0)
    csum += str[i++];

  char p[2];
  p[0] = tohex ((csum >> 4) & 0x0f);
  p[1] = tohex (csum & 0x0f);
  return write_to_gdb (p, 2);
}

static Bool
write_reply(const char *reply)
{
   write_to_gdb ("$", 1);
   write_to_gdb (reply, strlen (reply));
   write_to_gdb ("#", 1);
   return write_checksum (reply);
}

/* Creates a packet from a string message, caller needs to free.  */
static char *
create_packet(const char *msg)
{
   unsigned char csum = 0;
   int i = 1;
   char *p = vmalloc (strlen (msg) + 5); /* $ + msg + # + hexhex + 0 */
   strcpy (&p[1], msg);
   p[0] = '$';
   while (p[i] != 0)
      csum += p[i++];
   p[i++] = '#';
   p[i++] = tohex ((csum >> 4) & 0x0f);
   p[i++] = tohex (csum & 0x0f);
   p[i] = '\0';
   return p;
}

static int read_one_char (char *c)
{
   int i;
   do
       i = read (from_gdb, c, 1);
   while (i == -1 && errno == EINTR);

   return i;
}

static Bool
send_packet(const char *reply, int noackmode)
{
   int ret;
   char c;

send_packet_start:
   if (!write_reply(reply))
     return False;
   if (!noackmode) {
     // Look for '+' or '-'.
     // We must wait for "+" if !noackmode.
      do {
         ret = read_one_char(&c);
         if (ret <= 0)
            return False;
         // And if in !noackmode if we get "-" we should resent the packet.
         if (c == '-')
            goto send_packet_start;
      } while (c != '+');
     DEBUG(1, "sent packet to gdb got: %c\n",c);
   }
   return True;
}

// Reads one packet from_gdb starting with $ into buf.
// Skipping any other characters.
// Returns the size of the packet, 0 for end of input,
// or -1 if no packet could be read.
static int receive_packet(char *buf, int noackmode)
{
   int bufcnt = 0;
   int ret;
   char c;
   char c1 = '\0';
   char c2 = '\0';
   unsigned char csum = 0;

   // Look for first '$' (start of packet) or error.
receive_packet_start:
   do {
     ret = read_one_char(&c);
     if (ret <= 0)
       return ret;
   } while (c != '$');

   // Found start of packet ('$')
   while (bufcnt < (PBUFSIZ+1)) {
      ret = read_one_char(&c);
      if (ret <= 0)
         return ret;
      if (c == '#') {
         if ((ret = read_one_char(&c1)) <= 0
             || (ret = read_one_char(&c2)) <= 0) {
            return ret;
         }
         c1 = fromhex(c1);
         c2 = fromhex(c2);
         break;
      }
      buf[bufcnt] = c;
      csum += buf[bufcnt];
      bufcnt++;
   }

   // Packet complete, add terminator.
   buf[bufcnt] ='\0';

   if (!(csum == (c1 << 4) + c2)) {
      TSFPRINTF(stderr, "Bad checksum, sentsum=0x%x, csum=0x%x, buf=%s\n",
                (c1 << 4) + c2, csum, buf);
      if (!noackmode)
         if (!write_to_gdb ("-", 1))
            return -1;
      /* Try again, gdb should resend the packet.  */
      bufcnt = 0;
      csum = 0;
      goto receive_packet_start;
   }

   if (!noackmode)
     if (!write_to_gdb ("+", 1))
       return -1;
   return bufcnt;
}

// Returns a pointer to the char after the next delim char.
static const char *next_delim_string (const char *buf, char delim)
{
  while (*buf) {
      if (*buf++ == delim)
           break;
   }
   return buf;
}

/* buf starts with the packet name followed by the delimiter, for example
 * vRun;2f62696e2f6c73, ";" is the delimiter here, or
 * qXfer:features:read:target.xml:0,1000, where the delimiter is ":".
 * The packet name is thrown away and the hex string is decoded and
 * is placed in decoded_string (the caller owns this and is responsible
 * for freeing it). */
static int split_hexdecode(const char *buf, const char *string,
                           const char *delim, char **decoded_string)
{
   const char *next_str = next_delim_string(buf, *delim);
   if (next_str) {
     *decoded_string = decode_hexstring (next_str, 0, 0);
     DEBUG(1, "split_hexdecode decoded %s\n", *decoded_string);
     return 1;
   } else {
     TSFPRINTF(stderr, "%s decoding error: finding the hex string in %s failed!\n", string, buf);
     return 0;
   }
}

static size_t count_delims(char delim, char *buf)
{
   size_t count = 0;
   char *ptr = buf;

   while (*ptr)
       count += *ptr++ == delim;
   return count;
}

// Determine the length of the arguments.
// This depends on the len array being initialized to -1 for each element.
// We first skip the command (e.g. vRun;arg0;arg1)
static void count_len(char delim, char *buf, size_t *len)
{
   int i = 0;
   char *ptr = buf;

   // Skip the command
   while (*ptr && *ptr != delim)
      ptr++;

   // Delimiter counts towards the first arg0
   if (*ptr == delim) {
      ptr++;
      len[i]++;
   }

   // For each arg0... count chars (delim counts towards next arg)
   while (*ptr) {
      i += *ptr++ == delim;
      len[i]++;
   }
}

/* Declare here, will be used early, implementation follows later. */
static void gdb_relay(int pid, int send_noack_mode, char *q_buf);

/* Returns zero on success (and the pid of the valgrind process),
   or the errno from the child on failure.  */
static
int fork_and_exec_valgrind (int argc, char **argv, const char *working_dir,
                            int in_port, pid_t *pid)
{
   int err = 0;
   // We will use a pipe to track what the child does,
   // so we can report failure.
   int pipefd[2];
#ifdef HAVE_PIPE2
   if (pipe2 (pipefd, O_CLOEXEC) == -1) {
      err = errno;
      perror ("pipe2 failed");
      return err;
   }
#else
   if (pipe (pipefd) == -1) {
      err = errno;
      perror ("pipe failed");
      return err;
   } else {
      if (fcntl (pipefd[0], F_SETFD, FD_CLOEXEC) == -1
          || fcntl (pipefd[1], F_SETFD, FD_CLOEXEC) == -1) {
         err = errno;
         perror ("fcntl failed");
         close (pipefd[0]);
         close (pipefd[1]);
         return err;
      }
   }
#endif

   pid_t p = fork ();
   if (p < 0) {
      err = errno;
      perror ("fork failed");
      return err;
   } else if (p > 0) {
      // I am the parent (vgdb), p is the pid of the child (valgrind)
      // We only read from the child to see if everything is OK.
      // If the pipe closes, we get zero back, which is good.
      // An error reading the pipe is bad (and really shouldn't happen).
      // Otherwise the child sent us an errno code about what went wrong.
      close (pipefd[1]);

      while (err == 0) {
         int r = read (pipefd[0], &err, sizeof (int));
         if (r == 0) // end of file, good pipe closed after execve
            break;
         if (r == -1) {
            if (errno == EINTR)
               continue;
            else {
               err = errno;
               perror ("pipe read");
            }
         }
      }

      close (pipefd[0]);
      if (err != 0)
         return err;
      else {
         *pid = p;
         return 0;
      }
   } else {
      // p == 0, I am the child (will start valgrind)
      // We write success to the pipe, no need to read from it.
      close (pipefd[0]);

      if (working_dir != NULL && working_dir[0] != '\0') {
         if (chdir (working_dir) != 0) {
            err = errno;
            perror("chdir");
            // We try to write the result to the parent, but always exit.
            int written = 0;
            while (written < sizeof (int)) {
               int nrw = write (pipefd[1], &err, sizeof (int) - written);
               if (nrw == -1)
                  break;
               written += nrw;
            }
            _exit (-1);
         }
      }

      /* When in stdio mode (talking to gdb through stdin/stdout, not
         through a socket), redirect stdout to stderr and close stdin
         for the inferior. That way at least some output can be seen,
         but there will be no input.  */
      if (in_port <= 0) {
         /* close stdin */
         close (0);
         /* open /dev/null as new stdin */
         (void)open ("/dev/null", O_RDONLY);
         /* redirect stdout as stderr */
         dup2 (2, 1);
      }

      /* Try to launch valgrind. Add --vgdb-error=0 to stop immediately so we
         can attach and --launched-with-multi to let valgrind know it doesn't
         need to show a banner how to connect to gdb, we will do that
         automagically. And add --vgdb-shadow-registers=yes to make shadow
         registers available by default. Add any other valgrind arguments the
         user gave with --vargs. Then the rest of the arguments to valgrind are
         the program to exec plus its arguments. */
      const int extra_vargs = 3;
      /* vargv[0] == "valgrind",
         vargv[1..extra_vargs] == static valgrind arguments vgdb needs,
         vargv[extra_vargs+1..extra_vargs+1+cvargs] == user valgrind arguments,
         vargv[extra_vargs+1+cvargs..extra_vargs+1+cvargs+args] == prog + args,
         vargs[arguments - 1] = NULL */
      int arguments = 1 + extra_vargs + cvargs + argc + 1;
      // We combine const and non-const char[]. This is mildly annoying
      // since we then need a char *const * for execvp. So we strdup the
      // const char*. Not pretty :{
      char **vargv = vmalloc (arguments * sizeof (char *));
      vargv[0] = strdup ("valgrind");
      vargv[1] = strdup ("--vgdb-error=0");
      vargv[2] = strdup ("--launched-with-multi=yes");
      vargv[3] = strdup ("--vgdb-shadow-registers=yes");
      // Add --vargs
      for (int i = 0; i < cvargs; i++) {
         vargv[i + extra_vargs + 1] = vargs[i];
      }
      // Add command and args
      for (int i = 0; i < argc; i++) {
         vargv[i + extra_vargs + 1 + cvargs] = argv[i];
      }
      vargv[arguments - 1] = NULL;

      if (!valgrind_path) {
         // TODO use execvpe (or something else if not on GNU/Linux
         /* We want to make a copy of the environ on start. When we
            get a QEnvironmentReset we copy that back. If we get an
            EvironSet/Add/Remove we update the copy. */
         execvp ("valgrind", vargv);
      }
      else {
         vargv[0] = valgrind_path;
         execvp (vargv[0], vargv);
      }

      // We really shouldn't get here...
      err = errno;
      /* Note we are after fork and exec failed, we cannot really call
         perror or printf in this situation since they aren't async-safe.  */
      // perror ("execvp valgrind");
      // printf ("execve returned??? confusing: %d\n", res);
      // We try to write the result to the parent, but always exit.
      int written = 0;
      while (written < sizeof (int)) {
         int nrw = write (pipefd[1], &err, sizeof (int) - written);
         if (nrw == -1)
            break;
         written += nrw;
      }
      _exit (-1);
   }

   abort (); // Impossible
}

/* Do multi stuff.  */
static
void do_multi_mode(int check_trials, int in_port)
{
   char *buf = vmalloc(PBUFSIZ+1);
   char *q_buf = vmalloc(PBUFSIZ+1); //save the qSupported packet sent by gdb
                                     //to send it to the valgrind gdbserver later
   q_buf[0] = '\0';
   int noackmode = 0, pkt_size = 0, bad_unknown_packets = 0;
   char *string = NULL;
   char *working_dir = NULL;
   DEBUG(1, "doing multi stuff...\n");
   while (1){
      /* We get zero if the pipe was closed (EOF), or -1 on error reading from
         the pipe to gdb. */
       pkt_size = receive_packet(buf, noackmode);
       if (pkt_size <= 0) {
          DEBUG(1, "receive_packet: %d\n", pkt_size);
          break;
       }
       
       DEBUG(1, "packet received: '%s'\n", buf);

#define QSUPPORTED "qSupported:"
#define STARTNOACKMODE "QStartNoAckMode"
#define QRCMD "qRcmd" // This is the monitor command in gdb
#define VRUN "vRun"
#define XFER "qXfer"
#define QATTACHED "qAttached"
#define QENVIRONMENTHEXENCODED "QEnvironmentHexEncoded"
#define QENVIRONMENTRESET "QEnvironmentReset"
#define QENVIRONMENTUNSET "QEnvironmentUnset"
#define QSETWORKINGDIR "QSetWorkingDir"
#define QTSTATUS "qTStatus"
       
       if (strncmp(QSUPPORTED, buf, strlen(QSUPPORTED)) == 0) {
          DEBUG(1, "CASE %s\n", QSUPPORTED);
          // And here is our reply.
          // XXX error handling? We don't check the arguments.
          char *reply;
          strcpy(q_buf, buf);
          // Keep this in sync with coregrind/m_gdbserver/server.c
          if (asprintf (&reply,
                        "PacketSize=%x;"
                        "QStartNoAckMode+;"
                        "QPassSignals+;"
                        "QCatchSyscalls+;"
                        /* Just report support always. */
                        "qXfer:auxv:read+;"
                        /* We'll force --vgdb-shadow-registers=yes */
                        "qXfer:features:read+;"
                        "qXfer:exec-file:read+;"
                        "qXfer:siginfo:read+;"
                        /* Extra vgdb support before valgrind starts up. */
                        "QEnvironmentHexEncoded+;"
                        "QEnvironmentReset+;"
                        "QEnvironmentUnset+;"
                        "QSetWorkingDir+", (UInt)PBUFSIZ - 1) != -1) {
             send_packet(reply, noackmode);
             free (reply);
          } else {
             XERROR(errno, "asprintf failed\n");
          }
       }
       else if (strncmp(STARTNOACKMODE, buf, strlen(STARTNOACKMODE)) == 0) {
          // We have to ack this one
          send_packet("OK", 0);
          noackmode = 1;
       }
       else if (buf[0] == '!') {
          send_packet("OK", noackmode);
       }
       else if (buf[0] == '?') {
          send_packet("W00", noackmode);
       }
       else if (strncmp("H", buf, strlen("H")) == 0) {
          // Set thread packet, but we are not running yet.
          send_packet("E01", noackmode);
       }
       else if (strncmp("vMustReplyEmpty", buf, strlen("vMustReplyEmpty")) == 0) {
          send_packet ("", noackmode);
       }
       else if (strncmp(QRCMD, buf, strlen(QRCMD)) == 0) {
           static const char *no_running_str =
              "No running target, monitor commands not available yet.\n";
           int str_count = strlen (no_running_str);
           char hex[2 * str_count + 1];
           hexify(hex, no_running_str, str_count);
           send_packet(hex, noackmode);

          char *decoded_string = decode_hexstring (buf, strlen (QRCMD) + 1, 0);
          DEBUG(1, "qRcmd decoded: %s\n", decoded_string);
          free (decoded_string);
       }
       else if (strncmp(VRUN, buf, strlen(VRUN)) == 0) {
          // vRun;filename[;argument]*
          // vRun, filename and arguments are split on ';',
          // no ';' at the end.
          // If there are no arguments count is one (just the filename).
          // Otherwise it is the number of arguments plus one (the filename).
          // The filename must be there and starts after the first ';'.
          // TODO: Handle vRun;[;argument]*
          // https://www.sourceware.org/gdb/onlinedocs/gdb/Packets.html#Packets
          // If filename is an empty string, the stub may use a default program
          // (e.g. the last program run).
          size_t count = count_delims(';', buf);
          size_t *len = vmalloc(count * sizeof(count));
          const char *delim = ";";
          const char *next_str = next_delim_string(buf, *delim);
          char **decoded_string = vmalloc(count * sizeof (char *));

          // Count the lenghts of each substring, init to -1 to compensate for
          // each substring starting with a delim char.
          for (size_t i = 0; i < count; i++)
             len[i] = -1;
          count_len(';', buf, len);
          if (next_str) {
             DEBUG(1, "vRun: next_str %s\n", next_str);
             for (size_t i = 0; i < count; i++) {
                /* Handle the case when the arguments
                 * was specified to gdb's run command
                 * but no remote exec-file was set,
                 * so the first vRun argument is missing.
                 * For example vRun;;6c.  */
                if (*next_str == *delim) {
                   next_str++;
                   /* empty string that can be freed. */
                   decoded_string[i] = strdup("");
                }
                else {
                   decoded_string[i] = decode_hexstring (next_str, 0, len[i]);
                   if (i < count - 1)
                      next_str = next_delim_string(next_str, *delim);
                }
                DEBUG(1, "vRun decoded: %s, next_str %s, len[%zu] %zu\n",
                      decoded_string[i], next_str, i, len[i]);
             }

             /* If we didn't get any arguments or the filename is an empty
                string, valgrind won't know which program to run.  */
             DEBUG (1, "count: %zu, len[0]: %zu\n", count, len[0]);
             if (! count || len[0] == 0) {
                free(len);
                for (size_t i = 0; i < count; i++)
                   free (decoded_string[i]);
                free (decoded_string);
                send_packet ("E01", noackmode);
                continue;
             }

             /* We have collected the decoded strings so we can use them to
                launch valgrind with the correct arguments... We then use the
                valgrind pid to start relaying packets.  */
             pid_t valgrind_pid = -1;
             int res = fork_and_exec_valgrind ((int)count,
                                               decoded_string,
                                               working_dir,
                                               in_port,
                                               &valgrind_pid);

             if (res == 0) {
                // Lets report we Stopped with SIGTRAP (05).
                send_packet ("S05", noackmode);
                prepare_fifos_and_shared_mem(valgrind_pid, check_trials);
                DEBUG(1, "from_gdb_to_pid %s, to_gdb_from_pid %s\n",
                      from_gdb_to_pid, to_gdb_from_pid);
                // gdb_relay is an endless loop till valgrind quits.
                shutting_down = False;

                gdb_relay (valgrind_pid, 1, q_buf);
                cleanup_fifos_and_shared_mem();
                DEBUG(1, "valgrind relay done\n");
                int status;
                pid_t p = waitpid (valgrind_pid, &status, 0);
                DEBUG(2, "waitpid: %d\n", (int) p);
                if (p == -1)
                   DEBUG(1, "waitpid error %s\n", strerror (errno));
                else {
                   if (WIFEXITED(status))
                      DEBUG(1, "valgrind exited with %d\n",
                            WEXITSTATUS(status));
                   else if (WIFSIGNALED(status))
                      DEBUG(1, "valgrind kill by signal %d\n",
                            WTERMSIG(status));
                   else
                      DEBUG(1, "valgrind unexpectedly stopped or continued");
                }
             } else {
                send_packet ("E01", noackmode);
                DEBUG(1, "OOPS! couldn't launch valgrind %s\n",
                      strerror (res));
             }

             free(len);
             for (int i = 0; i < count; i++)
		free (decoded_string[i]);
             free (decoded_string);
	  } else {
             free(len);
             send_packet ("E01", noackmode);
             DEBUG(1, "vRun decoding error: no next_string!\n");
             continue;
	  }
       } else if (strncmp(QATTACHED, buf, strlen(QATTACHED)) == 0) {
          send_packet ("1", noackmode);
          DEBUG(1, "qAttached sent: '1'\n");
          const char *next_str = next_delim_string(buf, ':');
          if (next_str) {
             char *decoded_string = decode_hexstring (next_str, 0, 0);
             DEBUG(1, "qAttached decoded: %s, next_str %s\n", decoded_string, next_str);
             free (decoded_string);
          } else {
             DEBUG(1, "qAttached decoding error: strdup of %s failed!\n", buf);
             continue;
          }
       } /* Reset the state of environment variables in the remote target
            before starting the inferior. In this context, reset means
            unsetting all environment variables that were previously set
            by the user (i.e., were not initially present in the environment). */
       else if (strncmp(QENVIRONMENTRESET, buf,
                        strlen(QENVIRONMENTRESET)) == 0) {
          send_packet ("OK", noackmode);
          // TODO clear all environment strings. We're not using
          // environment strings now. But we should.
       } else if (strncmp(QENVIRONMENTHEXENCODED, buf,
                          strlen(QENVIRONMENTHEXENCODED)) == 0) {
          send_packet ("OK", noackmode);
          if (!split_hexdecode(buf, QENVIRONMENTHEXENCODED, ":", &string))
             break;
          // TODO Collect all environment strings and add them to environ
          // before launching valgrind.
          free (string);
          string = NULL;
       } else if (strncmp(QENVIRONMENTUNSET, buf,
                          strlen(QENVIRONMENTUNSET)) == 0) {
          send_packet ("OK", noackmode);
          if (!split_hexdecode(buf, QENVIRONMENTUNSET, ":", &string))
             break;
          // TODO Remove this environment string from the collection.
          free (string);
          string = NULL;
       } else if (strncmp(QSETWORKINGDIR, buf,
                          strlen(QSETWORKINGDIR)) == 0) {
          // Silly, but we can only reply OK, even if the working directory is
          // bad. Errors will be reported when we try to execute the actual
          // process.
          send_packet ("OK", noackmode);
          // Free any previously set working_dir
          free (working_dir);
          working_dir = NULL;
          if (!split_hexdecode(buf, QSETWORKINGDIR, ":", &working_dir)) {
             continue; // We cannot report the error to gdb...
          }
          DEBUG(1, "set working dir to: %s\n", working_dir);
       } else if (strncmp(XFER, buf, strlen(XFER)) == 0) {
          char *buf_dup = strdup(buf);
          DEBUG(1, "strdup: buf_dup %s\n", buf_dup);
          if (buf_dup) {
             const char *delim = ":";
             size_t count = count_delims(delim[0], buf);
             if (count < 4) {
                strsep(&buf_dup, delim);
                strsep(&buf_dup, delim);
                strsep(&buf_dup, delim);
                char *decoded_string = decode_hexstring (buf_dup, 0, 0);
                DEBUG(1, "qXfer decoded: %s, buf_dup %s\n", decoded_string, buf_dup);
                free (decoded_string);
             }
             free (buf_dup);
          } else {
             DEBUG(1, "qXfer decoding error: strdup of %s failed!\n", buf);
             free (buf_dup);
             continue;
          }
          // Whether we could decode it or not, we cannot handle it now.  We
          // need valgrind gdbserver to properly reply. So error out here.
          send_packet ("E00", noackmode);
       } else if (strncmp(QTSTATUS, buf, strlen(QTSTATUS)) == 0) {
          // We don't support trace experiments
          DEBUG(1, "Got QTSTATUS\n");
          send_packet ("", noackmode);
       } else if (strcmp("qfThreadInfo", buf) == 0) {
          DEBUG(1, "Got qfThreadInfo\n");
          /* There are no threads yet, reply 'l' end of list. */
          send_packet ("l", noackmode);
       } else if (buf[0] != '\0') {
          // We didn't understand.
          DEBUG(1, "Unknown packet received: '%s'\n", buf);
          bad_unknown_packets++;
          if (bad_unknown_packets > 10) {
             DEBUG(1, "Too many bad/unknown packets received\n");
             break;
          }
          send_packet ("", noackmode);
       }
   }
   DEBUG(1, "done doing multi stuff...\n");
   free(working_dir);
   free(buf);
   free(q_buf);

   shutting_down = True;
   close (to_gdb);
   close (from_gdb);
}

/* Relay data between gdb and Valgrind gdbserver, till EOF or an error is
   encountered. q_buf is the qSupported packet received from gdb.  */
static
void gdb_relay(int pid, int send_noack_mode, char *q_buf)
{
   int from_pid = -1; /* fd to read from pid */
   int to_pid = -1; /* fd to write to pid */

   int shutdown_loop = 0;
   TSFPRINTF(stderr, "relaying data between gdb and process %d\n", pid);

   if (max_invoke_ms > 0)
      pthread_create(&invoke_gdbserver_in_valgrind_thread, NULL,
                     invoke_gdbserver_in_valgrind, (void *) &pid);
   to_pid = open_fifo(from_gdb_to_pid, O_WRONLY, "write to pid");
   acquire_lock(shared_mem_fd, pid);

   from_pid = open_fifo(to_gdb_from_pid, O_RDONLY|O_NONBLOCK,
                        "read mode from pid");

   sigusr1_fd = to_pid; /* allow simulating user typing control-c */

   Bool waiting_for_noack_mode = False;
   Bool waiting_for_qsupported = False;
   if(send_noack_mode) {
       DEBUG(1, "gdb_relay: to_pid %d, from_pid: %d\n", to_pid, from_pid);
       write_buf(to_pid, "$QStartNoAckMode#b0", 19,
                 "write start no ack mode",
                 /* notify */ True);
       waiting_for_noack_mode = True;
   }

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
         ERROR(errno, "unexpected poll ret %d\n", ret);
         shutting_down = True;
         break;
      }

      /* check for data to read */
      for (ck = 0; ck < NumConnectionKind; ck ++) {
         if (pollfds[ck].revents & POLLIN) {
            switch (ck) {
            case FROM_GDB:
               if (waiting_for_noack_mode || waiting_for_qsupported)
                  break; /* Don't add any messages while vgdb is talking. */
               if (!read_from_gdb_write_to_pid(to_pid))
                  shutting_down = True;
               break;
            case FROM_PID:
               // First handle any messages from vgdb
               if (waiting_for_noack_mode) {
                  char buf[PBUFSIZ+1]; // +1 for trailing \0
                  size_t buflen;
                  buflen = getpkt(buf, from_pid, to_pid);
                  if (buflen != 2 || strcmp(buf, "OK") != 0) {
                     if (buflen != 2)
                        ERROR(0, "no ack mode: unexpected buflen %zu, buf %s\n",
                              buflen, buf);
                     else
                        ERROR(0, "no ack mode: unexpected packet %s\n", buf);
                  }
                  waiting_for_noack_mode = False;

                  /* Propagate qSupported to valgrind, we already replied.  */
                  if (q_buf != NULL && q_buf[0] != '\0') {
                     char *pkt = create_packet (q_buf);
                     write_buf(to_pid, pkt, strlen(pkt),
                               "write qSupported", /* notify */ True);
                     free(pkt);
                     waiting_for_qsupported = True;
                  }
               } else if (waiting_for_qsupported) {
                  char buf[PBUFSIZ+1]; // +1 for trailing \0
                  size_t buflen;
                  buflen = getpkt(buf, from_pid, to_pid);
                  /* Should we sanity check the result?  */
                  if (buflen > 0) {
                     waiting_for_qsupported = False;
                  } else {
                     ERROR(0, "Unexpected getpkt for qSupported reply: %zu\n",
                           buflen);
                  }
               } else if (!read_from_pid_write_to_gdb(from_pid))
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
            invoker_valgrind_dying();
            shutting_down = True;
         }
         if (pollfds[ck].revents & POLLHUP) {
            DEBUG(1, "connection %s fd %d POLLHUP error condition\n",
                  ppConnectionKind(ck), pollfds[ck].fd);
            invoker_valgrind_dying();
            shutting_down = True;
         }
         if (pollfds[ck].revents & POLLNVAL) {
            DEBUG(1, "connection %s fd %d POLLNVAL error condition\n",
                  ppConnectionKind(ck), pollfds[ck].fd);
            invoker_valgrind_dying();
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
   char hex[3];
   unsigned char cksum;
   char *hexcommand;
   char buf[PBUFSIZ+1]; // +1 for trailing \0
   int buflen;
   int nc;


   if (max_invoke_ms > 0 || cmd_time_out != NEVER)
      pthread_create(&invoke_gdbserver_in_valgrind_thread, NULL,
                     invoke_gdbserver_in_valgrind, (void *) &pid);

   to_pid = open_fifo(from_gdb_to_pid, O_WRONLY, "write to pid");
   acquire_lock(shared_mem_fd, pid);

   /* first send a C-c \003 to pid, so that it wakes up the process
      After that, we can open the fifo from the pid in read mode
      We then start to wait for packets (normally first a resume reply)
      At that point, we send our command and expect replies */
   buf[0] = '\003';
   i = 0;
   while (!write_buf(to_pid, buf, 1,
                     "write \\003 to wake up", /* notify */ True)) {
      /* If write fails, retries up to 10 times every 0.5 seconds
         This aims at solving the race condition described in
         remote-utils.c remote_finish function. */
      usleep(500*1000);
      i++;
      if (i >= 10)
         XERROR(errno, "failed to send wake up char after 10 trials\n");
   }
   from_pid = open_fifo(to_gdb_from_pid, O_RDONLY,
                        "read cmd result from pid");

   /* Enable no ack mode. */
   write_buf(to_pid, "$QStartNoAckMode#b0", 19, "write start no ack mode",
             /* notify */ True);
   buflen = getpkt(buf, from_pid, to_pid);
   if (buflen != 2 || strcmp(buf, "OK") != 0) {
      if (buflen != 2)
         ERROR(0, "no ack mode: unexpected buflen %d\n", buflen);
      else
         ERROR(0, "no ack mode: unexpected packet %s\n", buf);
   }

   for (nc = 0; nc <= last_command; nc++) {
      TSFPRINTF(stderr, "sending command %s to pid %d\n", commands[nc], pid);

      /* prepare hexcommand $qRcmd,xxxx....................xx#cc      */
      hexcommand = vmalloc(packet_len_for_command(commands[nc]));
      hexcommand[0] = 0;
      strcat(hexcommand, "$qRcmd,");
      for (i = 0; i < strlen(commands[nc]); i++) {
         sprintf(hex, "%02x", (unsigned char) commands[nc][i]);
         // Need to use unsigned char, to avoid sign extension.
         strcat(hexcommand, hex);
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

      /* we exit of the below loop explicitly when the command has
         been handled or because a signal handler will set
         shutting_down. */
      while (!shutting_down) {
         buflen = getpkt(buf, from_pid, to_pid);
         if (buflen < 0) {
            ERROR(0, "error reading packet\n");
            if (buflen == -2)
               invoker_valgrind_dying();
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
      free(hexcommand);
   }
   shutting_down = True;

   close_connection(to_pid, from_pid);
}

/* report to user the existence of a vgdb-able valgrind process
   with given pid.
   Note: this function does not use XERROR if an error is encountered
   while producing the command line for pid, as this is not critical
   and at least on MacOS, reading cmdline is not available. */
static
void report_pid(int pid, Bool on_stdout)
{
   char cmdline_file[50];   // large enough
   int fd, i;
   FILE *out = on_stdout ? stdout : stderr;

   TSFPRINTF(out, "use --pid=%d for ", pid);

   sprintf(cmdline_file, "/proc/%d/cmdline", pid);
   fd = open(cmdline_file, O_RDONLY | O_CLOEXEC);
   if (fd == -1) {
      DEBUG(1, "error opening cmdline file %s %s\n",
            cmdline_file, strerror(errno));
      fprintf(out, "(could not open process command line)\n");
   } else {
      char cmdline[100];
      ssize_t sz;
      while ((sz = read(fd, cmdline, sizeof cmdline - 1)) > 0) {
         for (i = 0; i < sz; i++)
            if (cmdline[i] == 0)
               cmdline[i] = ' ';
         cmdline[sz] = 0;
         fprintf(out, "%s", cmdline);
      }
      if (sz == -1) {
         DEBUG(1, "error reading cmdline file %s %s\n",
               cmdline_file, strerror(errno));
         fprintf(out, "(error reading process command line)");
      }
      fprintf(out, "\n");
      close(fd);
   }
   fflush(out);
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
"             [--cmd-time-out=<number>] [-l] [-T] [-D] [-d]\n"
"             [--multi] [--valgrind=<valgrind-exe>] [--vargs ...]\n"
"             \n"
"  --pid arg must be given if multiple Valgrind gdbservers are found.\n"
"  --vgdb-prefix arg must be given to both Valgrind and vgdb utility\n"
"      if you want to change the prefix (default %s) for the FIFOs communication\n"
"      between the Valgrind gdbserver and vgdb.\n"
"  --wait (default 0) tells vgdb to check during the specified number\n"
"      of seconds if a Valgrind gdbserver can be found.\n"
"  --max-invoke-ms (default 100) gives the nr of milli-seconds after which vgdb\n"
"      will force the invocation of the Valgrind gdbserver (if the Valgrind\n"
"         process is blocked in a system call).\n"
"  --port instructs vgdb to listen for gdb on the specified port nr.\n"
"  --cmd-time-out (default 99999999) tells vgdb to exit if the found Valgrind\n"
"     gdbserver has not processed a command after number seconds\n"
"  --multi start in extended-remote mode, wait for gdb to tell us what to run\n"
"   --valgrind, pass the path to valgrind to use. If not specified, the system valgrind will be launched.\n"
"   --vargs everything that follows is an argument for valgrind.\n"
"  -l  arg tells to show the list of running Valgrind gdbserver and then exit.\n"
"  -T  arg tells to add timestamps to vgdb information messages.\n"
"  -D  arg tells to show shared mem status and then exit.\n"
"  -d  arg tells to show debug info. Multiple -d args for more debug info\n"
"\n"
"  -h --help shows this message\n"
#ifdef VG_GDBSCRIPTS_DIR
"  The GDB python code defining GDB front end valgrind commands is:\n       %s\n"
#endif
"  To get help from the Valgrind gdbserver, use vgdb help\n"
"\n", vgdb_prefix_default()
#ifdef VG_GDBSCRIPTS_DIR
    , VG_GDBSCRIPTS_DIR "/valgrind-monitor.py"
#endif
   );
   invoker_restrictions_msg();
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
      TSFPRINTF(stderr, "vgdb error: invalid pid %d given\n", arg_pid);
      exit(1);
   } else {
      /* search for a matching named fifo.
         If we have been given a pid, we will check that the matching FIFO is
         there (or wait the nr of check_trials for this to appear).
         If no pid has been given, then if we find only one FIFO,
         we will use this to build the pid to use.
         If we find multiple processes with valid FIFO, we report them and will
         exit with an error. */
      DIR *vgdb_dir;
      char *vgdb_dir_name = vmalloc(strlen (vgdb_prefix) + 3);
      struct dirent *f;
      int is;
      int nr_valid_pid = 0;
      const char *suffix = "-from-vgdb-to-"; /* followed by pid */
      char *vgdb_format = vmalloc(strlen(vgdb_prefix) + strlen(suffix) + 1);

      strcpy(vgdb_format, vgdb_prefix);
      strcat(vgdb_format, suffix);

      if (strchr(vgdb_prefix, '/') != NULL) {
         strcpy(vgdb_dir_name, vgdb_prefix);
         for (is = strlen(vgdb_prefix) - 1; is >= 0; is--)
            if (vgdb_dir_name[is] == '/') {
               vgdb_dir_name[is+1] = '\0';
               break;
            }
      } else {
         strcpy(vgdb_dir_name, "");
      }

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

         vgdb_dir = opendir(strlen(vgdb_dir_name) ? vgdb_dir_name : "./");
         if (vgdb_dir == NULL)
            XERROR(errno,
                   "vgdb error: opening directory %s searching vgdb fifo\n",
                   vgdb_dir_name);

         errno = 0; /* avoid complain if vgdb_dir is empty */
         while ((f = readdir(vgdb_dir))) {
            struct stat st;
            char pathname[strlen(vgdb_dir_name) + strlen(f->d_name) + 1];
            char *wrongpid;
            int newpid;

            strcpy(pathname, vgdb_dir_name);
            strcat(pathname, f->d_name);
            DEBUG(3, "checking pathname is FIFO %s\n", pathname);
            if (stat(pathname, &st) != 0) {
               if (debuglevel >= 3)
                  ERROR(errno, "vgdb error: stat %s searching vgdb fifo\n",
                        pathname);
            } else if (S_ISFIFO(st.st_mode)) {
               DEBUG(3, "trying FIFO %s\n", pathname);
               if (strncmp(pathname, vgdb_format,
                           strlen(vgdb_format)) == 0) {
                  newpid = strtol(pathname + strlen(vgdb_format),
                                  &wrongpid, 10);
                  if (*wrongpid == '-' && newpid > 0
                      && kill(newpid, 0) == 0) {
                     nr_valid_pid++;
                     if (show_list) {
                        report_pid(newpid, /*on_stdout*/ True);
                        pid = newpid;
                     } else if (arg_pid != -1) {
                        if (arg_pid == newpid) {
                           pid = newpid;
                        }
                     } else if (nr_valid_pid > 1) {
                        if (nr_valid_pid == 2) {
                           TSFPRINTF
                              (stderr,
                               "no --pid= arg given"
                               " and multiple valgrind pids found:\n");
                           report_pid(pid, /*on_stdout*/ False);
                        }
                        pid = -2;
                        report_pid(newpid, /*on_stdout*/ False);
                     } else {
                        pid = newpid;
                     }
                  }
               }
            }
            errno = 0; /* avoid complain if at the end of vgdb_dir */
         }
         if (f == NULL && errno != 0)
            XERROR(errno, "vgdb error: reading directory %s for vgdb fifo\n",
                   vgdb_dir_name);

         closedir(vgdb_dir);
         if (pid != -1)
            break;
      }

      free(vgdb_dir_name);
      free(vgdb_format);
   }

   if (show_list) {
      exit(1);
   } else if (pid == -1) {
      if (arg_pid == -1)
         TSFPRINTF(stderr, "vgdb error: no FIFO found and no pid given\n");
      else
         TSFPRINTF(stderr, "vgdb error: no FIFO found matching pid %d\n",
                   arg_pid);
      exit(1);
   }
   else if (pid == -2) {
      /* no arg_pid given, multiple FIFOs found */
      exit(1);
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
Bool is_opt(char* arg, const char *option)
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
                   Bool *p_multi_mode,
                   int *p_arg_pid,
                   int *p_check_trials,
                   int *p_port,
                   int *p_last_command,
                   char *commands[])
{
   Bool show_shared_mem = False;
   Bool show_list = False;
   Bool multi_mode = False;
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
      } else if (is_opt(argv[i], "--multi")) {
         multi_mode = True;
      } else if (is_opt(argv[i], "-T")) {
         timestamp = True;
      } else if (is_opt(argv[i], "--pid=")) {
         int newpid;
         if (!numeric_val(argv[i], &newpid)) {
            TSFPRINTF(stderr, "invalid --pid argument %s\n", argv[i]);
            arg_errors++;
         } else if (arg_pid != -1) {
            TSFPRINTF(stderr, "multiple --pid arguments given\n");
            arg_errors++;
         } else {
            arg_pid = newpid;
         }
      } else if (is_opt(argv[i], "--wait=")) {
         if (!numeric_val(argv[i], &check_trials)) {
            TSFPRINTF(stderr, "invalid --wait argument %s\n", argv[i]);
            arg_errors++;
         }
      } else if (is_opt(argv[i], "--max-invoke-ms=")) {
         if (!numeric_val(argv[i], &max_invoke_ms)) {
            TSFPRINTF(stderr, "invalid --max-invoke-ms argument %s\n", argv[i]);
            arg_errors++;
         }
      } else if (is_opt(argv[i], "--cmd-time-out=")) {
         if (!numeric_val(argv[i], &cmd_time_out)) {
            TSFPRINTF(stderr, "invalid --cmd-time-out argument %s\n", argv[i]);
            arg_errors++;
         }
      } else if (is_opt(argv[i], "--port=")) {
         if (!numeric_val(argv[i], &int_port)) {
            TSFPRINTF(stderr, "invalid --port argument %s\n", argv[i]);
            arg_errors++;
         }
      } else if (is_opt(argv[i], "--vgdb-prefix=")) {
         if (vgdb_prefix) {
            // was specified more than once on the command line
            // ignore earlier uses
            free(vgdb_prefix);
         }
         vgdb_prefix = strdup (argv[i] + 14);
      } else if (is_opt(argv[i], "--valgrind=")) {
          char *path = argv[i] + 11;
         /* Compute the absolute path.  */
          valgrind_path = realpath(path, NULL);
          if (!valgrind_path) {
              TSFPRINTF(stderr, "%s is not a correct path. %s, exiting.\n",
			path, strerror (errno));
              exit(1);
          }
          DEBUG(2, "valgrind's real path: %s\n", valgrind_path);
      } else if (is_opt(argv[i], "--vargs")) {
         // Everything that follows now is an argument for valgrind
         // No other options (or commands) can follow
         // argc - i is the number of left over arguments
         // allocate enough space, put all args in it.
         cvargs = argc - i - 1;
         vargs = vmalloc (cvargs * sizeof(*vargs));
         i++;
         for (int j = 0; i < argc; i++) {
            vargs[j] = argv[i];
            j++;
         }
      } else if (is_opt(argv[i], "-c")) {
         last_command++;
         commands[last_command] = vmalloc(1);
         commands[last_command][0] = '\0';
      } else if (0 == strncmp(argv[i], "-", 1)) {
         TSFPRINTF(stderr, "unknown or invalid argument %s\n", argv[i]);
         arg_errors++;
      } else {
         int len;
         if (last_command == -1) {
            /* only one command, no -c command indicator */
            last_command++;
            commands[last_command] = vmalloc(1);
            commands[last_command][0] = '\0';
         }
         len = strlen(commands[last_command]);
         commands[last_command] = vrealloc(commands[last_command],
                                           len + 1 + strlen(argv[i]) + 1);
         if (len > 0)
            strcat(commands[last_command], " ");
         strcat(commands[last_command], argv[i]);
         if (packet_len_for_command(commands[last_command]) > PBUFSIZ) {
            TSFPRINTF(stderr, "command %s too long\n", commands[last_command]);
            arg_errors++;
         }

      }
   }

   if (vgdb_prefix == NULL)
      vgdb_prefix = vgdb_prefix_default();

   if (multi_mode
       && (show_shared_mem
	   || show_list
	   || last_command != -1)) {
      arg_errors++;
      TSFPRINTF(stderr,
		"Cannot use -D, -l or COMMANDs when using --multi mode\n");
   }

   if (isatty(0)
       && !show_shared_mem
       && !show_list
       && int_port == 0
       && last_command == -1) {
      arg_errors++;
      TSFPRINTF(stderr,
                "Using vgdb standalone implies to give -D or -l or a COMMAND\n");
   }

   if (show_shared_mem && show_list) {
      arg_errors++;
      TSFPRINTF(stderr,
                "Can't use both -D and -l options\n");
   }

   if (max_invoke_ms > 0
       && cmd_time_out != NEVER
       && (cmd_time_out * 1000) <= max_invoke_ms) {
      arg_errors++;
      TSFPRINTF(stderr,
                "--max-invoke-ms must be < --cmd-time-out * 1000\n");
   }

   if (show_list && arg_pid != -1) {
      arg_errors++;
      TSFPRINTF(stderr,
                "Can't use both --pid and -l options\n");
   }

   if (int_port > 0 && last_command != -1) {
      arg_errors++;
      TSFPRINTF(stderr,
                "Can't use --port to send commands\n");
   }

   if (arg_errors > 0) {
      TSFPRINTF(stderr, "args error. Try `vgdb --help` for more information\n");
      exit(1);
   }

   *p_show_shared_mem = show_shared_mem;
   *p_show_list = show_list;
   *p_multi_mode = multi_mode;
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
   Bool multi_mode;
   int arg_pid;
   int check_trials;
   int in_port;
   int last_command;
   char *commands[argc]; // we will never have more commands than args.

   parse_options(argc, argv,
                 &show_shared_mem,
                 &show_list,
		 &multi_mode,
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

   if (!multi_mode) {
      pid = search_arg_pid(arg_pid, check_trials, show_list);

      /* We pass 1 for check_trails here, because search_arg_pid already waited.  */
      prepare_fifos_and_shared_mem(pid, 1);
   } else {
      pid = 0;
   }

   if (in_port > 0)
      wait_for_gdb_connect(in_port);

   if (show_shared_mem) {
      TSFPRINTF(stderr,
                "vgdb %d "
                "written_by_vgdb %d "
                "seen_by_valgrind %d\n",
                VS_vgdb_pid,
                VS_written_by_vgdb,
                VS_seen_by_valgrind);
      TSFPRINTF(stderr, "vgdb pid %d\n", VS_vgdb_pid);
      exit(0);
   }

   if (multi_mode) {
      /* check_trails is the --wait argument in seconds, defaulting to 1
       * if not given.  */
      do_multi_mode (check_trials, in_port);
   } else if (last_command >= 0) {
      standalone_send_commands(pid, last_command, commands);
   } else {
      gdb_relay(pid, 0, NULL);
   }

   free(vgdb_prefix);
   free(valgrind_path);
   if (!multi_mode)
      cleanup_fifos_and_shared_mem();

   for (i = 0; i <= last_command; i++)
      free(commands[i]);
   return 0;
}
