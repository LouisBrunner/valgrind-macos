/* -*- mode: C; c-basic-offset: 3; -*- */

#include <assert.h>
#include <elf.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/user.h>
#include <sys/wait.h>

static int
err_out(const char *msg)
{
   perror(msg);
   return 1;
}

static int
non_empty(const char *buf, size_t len)
{
   size_t i;
   int c = 0;
   volatile const char *p = buf;

   for (i = 0; i != len; i++)
      c |= p[i];
   return c;
}

static int
do_child(void)
{
   if (ptrace(PTRACE_TRACEME, 0, NULL, NULL) == -1)
      return err_out("ptrace traceme");
   raise(SIGUSR1);
   return 0;
}

int
main(void)
{
   char buf[1024];
   struct iovec iov;
   pid_t cpid, pid;
   int status;

   cpid = fork();
   if (cpid == -1)
      return err_out("fork");
   if (cpid == 0)
      return do_child();

   pid = wait(&status);
   if (pid == -1)
      return err_out("wait");

   /* Intentionally provide an uninitialized buffer to ptrace. */
   iov.iov_len = sizeof(buf);
   iov.iov_base = buf;
   if (ptrace(0x4204, cpid, NT_PRSTATUS, &iov) == -1)
      return err_out("ptrace getregset");

   assert(iov.iov_base == buf);
   assert(iov.iov_len > 0 && iov.iov_len < sizeof(buf));

   /* We're assuming here that NT_PRSTATUS never contains
      all-zeros. */
   assert(non_empty(buf, iov.iov_len));
   puts("OK");
   return 0;
}
