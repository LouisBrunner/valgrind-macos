#include "../../memcheck.h"
#include "scalar.h"
#include <unistd.h>
#include <sched.h>
#include <signal.h>
#include <sys/shm.h>

// See memcheck/tests/x86-linux/scalar.c for an explanation of what this test
// is doing.

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];
   long  res;

   VALGRIND_MAKE_MEM_NOACCESS(0, 0x1000);

   // The nocancel syscalls all use the same wrappers as the corresponding
   // non-nocancel syscall.  This means that if we try to test both in the
   // same file, the nocancel ones won't result in errors being generated
   // because errors are too similar.  So we test them in this separate file.

   // __NR_read_nocancel 396
   // __NR_write_nocancel 397
   // __NR_open_nocancel 398
   // __NR_close_nocancel 399
   // __NR_wait4_nocancel 400
   // __NR_recvmsg_nocancel 401
   // __NR_sendmsg_nocancel 402
   // __NR_recvfrom_nocancel 403
   // __NR_accept_nocancel 404
   // __NR_msync_nocancel 405
   // __NR_fcntl_nocancel 406
   // __NR_select_nocancel 407
   // __NR_fsync_nocancel 408
   // __NR_connect_nocancel 409
   // __NR_sigsuspend_nocancel 410

   GO(__NR_sigsuspend_nocancel, 410, "ignore");
   // (I don't know how to test this...)

   // __NR_readv_nocancel 411
   // __NR_writev_nocancel 412
   // __NR_sendto_nocancel 413
   // __NR_pread_nocancel 414
   // __NR_pwrite_nocancel 415
   // __NR_waitid_nocancel 416
   // __NR_poll_nocancel 417
   // __NR_msgsnd_nocancel 418
   // __NR_msgrcv_nocancel 419

   // The error doesn't appear because it's a dup of the one from sem_wait.
   GO(__NR_sem_wait_nocancel, 420, "1s 0m");
   SY(__NR_sem_wait_nocancel, x0); FAIL;

   // __NR_aio_suspend_nocancel 421
   // __NR___sigwait_nocancel 422
   // __NR___semwait_signal_nocancel 423

   return 0;
}

