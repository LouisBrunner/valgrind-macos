#define _GNU_SOURCE
#include <sys/syscall.h>
#include <linux/wait.h>
#include <sched.h>
#include "test.h"

char stack[4096];

void saynum(int num)
{
   char *nums="012345679";
   say(nums+num,1);
}

void test_thread(void *arg)
{
   mysleep(0,50000);
   SAY("thread_start");
   mysleep(0,50000);
   SAY("thread_end");
   EXIT(1);
}

void test_child(void)
{
   mysleep(0,50000);
   SAY("child_start");
   mysleep(0,50000);
   SAY("child_end");
   EXIT(1);
}


void forkish(void)
{
   int pid;
   int status;

   asm volatile( "lghi 2, 0\n"
                 "lgr  3, 0\n"
                 "svc  120\n"      // CLONE
                 "lr   %0, 2\n"
                 :"=d" (pid)
                 :: "2", "3", "cc");
   if (!pid)
      test_child();
   SAY("MAIN_proc");
   asm volatile( "lr 2, %2\n"
                 "lgr 3,%1\n"
                 "lgr 4,%3\n"
                 "lghi 5,0\n"
                 "svc 114\n"       // WAIT4
                 : "=m" (status)
                 : "d" (&status), "d" (pid), "d" (__WALL)
                 : "2","3","4","5", "cc");
   say("wait returned:",14);
   saynum(status >> 8);
   say("\n",1);
   SAY("MAIN_proc_end");
}

void threadish(void)
{
   register long ret   asm("2") = (unsigned long) stack+4096-160;
   register long flags asm("3") = CLONE_THREAD | CLONE_SIGHAND |
                                  CLONE_PARENT | CLONE_VM |
                                  CLONE_FILES  | CLONE_FS |
                                  CLONE_PTRACE;

   asm volatile( "svc 120\n"
                 :"+d"(ret)
                 : "d"(flags)
                 : "cc", "memory");
   if (!ret)
      test_thread(0);
   SAY("MAIN_thread");
   mysleep(0,10000);
   SAY("MAIN_thread_end");
}

int main(void)
{
   forkish();
   threadish();
   EXIT(1);
   return 0;      // shuts up the compiler
}
