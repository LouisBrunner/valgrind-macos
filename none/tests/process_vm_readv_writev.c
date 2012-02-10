#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/uio.h>

static int status = EXIT_SUCCESS;

#ifdef HAVE_PROCESS_VM_READV

static void test_process_vm_readv()
{
   char lbuf[] = "123456";
   char rbuf[] = "ABCDEF";

   struct iovec lvec[2];
   struct iovec rvec[2];

   lvec[0].iov_base = lbuf + 1;
   lvec[0].iov_len = 1;
   lvec[1].iov_base = lbuf + 3;
   lvec[1].iov_len = 2;

   rvec[0].iov_base = rbuf + 1;
   rvec[0].iov_len = 2;
   rvec[1].iov_base = rbuf + 4;
   rvec[1].iov_len = 1;

   if (process_vm_readv(getpid(),
                        lvec, 2,
                        rvec, 2,
                        0 ) < 0 ) {
      perror("process_vm_readv");
      status = EXIT_FAILURE;
   }

   if (strcmp(lbuf, "1B3CE6") != 0) {
      fprintf(stderr, "Expected: \"1B3CE6\"; Got: \"%s\"\n", lbuf);
      status = EXIT_FAILURE;
   }
}

#endif /* defined( HAVE_PROCESS_VM_READV ) */

#ifdef HAVE_PROCESS_VM_WRITEV

static void test_process_vm_writev()
{
   char lbuf[] = "123456";
   char rbuf[] = "ABCDEF";

   struct iovec lvec[2];
   struct iovec rvec[2];

   lvec[0].iov_base = lbuf + 1;
   lvec[0].iov_len = 1;
   lvec[1].iov_base = lbuf + 3;
   lvec[1].iov_len = 2;

   rvec[0].iov_base = rbuf + 1;
   rvec[0].iov_len = 2;
   rvec[1].iov_base = rbuf + 4;
   rvec[1].iov_len = 1;

   if (process_vm_writev(getpid(),
                         lvec, 2,
                         rvec, 2,
                         0 ) < 0 ) {
      perror("process_vm_writev");
      status = EXIT_FAILURE;
   }

   if (strcmp(rbuf, "A24D5F") != 0) {
      fprintf(stderr, "Expected: \"A24D5F\"; Got: \"%s\"\n", rbuf);
      status = EXIT_FAILURE;
   }
}

#endif /* defined( HAVE_PROCESS_VM_WRITEV ) */

int main(int argc, char *argv[])
{
#ifdef HAVE_PROCESS_VM_READV
   test_process_vm_readv();
#endif
#ifdef HAVE_PROCESS_VM_WRITEV
   test_process_vm_writev();
#endif
   return status;
}
