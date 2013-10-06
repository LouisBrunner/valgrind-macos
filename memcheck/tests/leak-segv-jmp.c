#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "../memcheck.h"
#include "leak.h"
#include <sys/mman.h>
#include <sys/syscall.h>

typedef unsigned long            UWord;
typedef unsigned long long int   ULong;
// Below code is copied from m_syscall.c
// Refer to this file for syscall convention.
#if defined(VGP_x86_linux)
extern UWord do_syscall_WRK (UWord syscall_no, 
                             UWord a1, UWord a2, UWord a3,
                             UWord a4, UWord a5, UWord a6
                             );
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"	push	%esi\n"
"	push	%edi\n"
"	push	%ebx\n"
"	push	%ebp\n"
"	movl	16+ 4(%esp),%eax\n"
"	movl	16+ 8(%esp),%ebx\n"
"	movl	16+12(%esp),%ecx\n"
"	movl	16+16(%esp),%edx\n"
"	movl	16+20(%esp),%esi\n"
"	movl	16+24(%esp),%edi\n"
"	movl	16+28(%esp),%ebp\n"
"	int	$0x80\n"
"	popl	%ebp\n"
"	popl	%ebx\n"
"	popl	%edi\n"
"	popl	%esi\n"
"	ret\n"
".previous\n"
);
#elif defined(VGP_amd64_linux)
extern UWord do_syscall_WRK (
          UWord syscall_no, 
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6
       );
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"	movq	%rdi, %rax\n"
"	movq	%rsi, %rdi\n"
"	movq	%rdx, %rsi\n"
"	movq	%rcx, %rdx\n"
"	movq	%r8,  %r10\n"
"	movq	%r9,  %r8\n"
"	movq    8(%rsp), %r9\n"	 /* last arg from stack */
"	syscall\n"
"	ret\n"
".previous\n"
);

#elif defined(VGP_ppc32_linux)
extern ULong do_syscall_WRK (
          UWord syscall_no, 
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6
       );
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"        mr      0,3\n"
"        mr      3,4\n"
"        mr      4,5\n"
"        mr      5,6\n"
"        mr      6,7\n"
"        mr      7,8\n"
"        mr      8,9\n"
"        sc\n"                  /* syscall: sets %cr0.so on error         */
"        mfcr    4\n"           /* %cr -> low word of return var          */
"        rlwinm  4,4,4,31,31\n" /* rotate flag bit so to lsb, and mask it */
"        blr\n"                 /* and return                             */
".previous\n"
);

#elif defined(VGP_arm_linux)
extern UWord do_syscall_WRK (
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6,
          UWord syscall_no
       );
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"         push    {r4, r5, r7}\n"
"         ldr     r4, [sp, #12]\n"
"         ldr     r5, [sp, #16]\n"
"         ldr     r7, [sp, #20]\n"
"         svc     0x0\n"
"         pop     {r4, r5, r7}\n"
"         bx      lr\n"
".previous\n"
);
#elif defined(VGP_s390x_linux)
UWord do_syscall_WRK (
   UWord syscall_no,
   UWord arg1, UWord arg2, UWord arg3,
   UWord arg4, UWord arg5, UWord arg6
   )
{
   register UWord __arg1 asm("2") = arg1;
   register UWord __arg2 asm("3") = arg2;
   register UWord __arg3 asm("4") = arg3;
   register UWord __arg4 asm("5") = arg4;
   register UWord __arg5 asm("6") = arg5;
   register UWord __arg6 asm("7") = arg6;
   register ULong __svcres asm("2");

   __asm__ __volatile__ (
                 "lgr %%r1,%1\n\t"
                 "svc 0\n\t"
		: "=d" (__svcres)
		: "a" (syscall_no),
		  "0" (__arg1),
		  "d" (__arg2),
		  "d" (__arg3),
		  "d" (__arg4),
		  "d" (__arg5),
		  "d" (__arg6)
		: "1", "cc", "memory");

   return (UWord) (__svcres);
}

#elif defined(VGP_mips64_linux)
extern UWord do_syscall_WRK (
          UWord syscall_no,
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6
       )
{
   UWord out;
   __asm__ __volatile__ (
                 "move $v0, %1\n\t"
                 "move $a0, %2\n\t"
                 "move $a1, %3\n\t"
                 "move $a2, %4\n\t"
                 "move $a3, %5\n\t"
                 "move $8,  %6\n\t"  /* We use numbers because some compilers */
                 "move $9,  %7\n\t"  /* don't recognize $a4 and $a5 */
                 "syscall\n"
                 "move %0, $v0\n\t"
                 : /*out*/ "=r" (out)
                 : "r"(syscall_no), "r"(a1), "r"(a2), "r"(a3),
                   "r"(a4), "r"(a5), "r"(a6)
                 : "v0", "v1", "a0", "a1", "a2", "a3", "$8", "$9");
   return out;
}

#else
// Ensure the file compiles even if the syscall nr is not defined.
#ifndef __NR_mprotect
#define __NR_mprotect 0
#endif
UWord do_syscall_WRK (UWord syscall_no, 
                      UWord a1, UWord a2, UWord a3,
                      UWord a4, UWord a5, UWord a6
                      )
{
   // not implemented. vgtest prereq should avoid this to be called.
   return -1;
}
#endif



char **b10;
int mprotect_result = 0;
static void non_simd_mprotect (long tid, void* addr, long len)
{
   mprotect_result = do_syscall_WRK(__NR_mprotect,
                                    (UWord) addr, len, PROT_NONE,
                                    0, 0, 0);
}

void f(void)
{
   long pagesize;
#define RNDPAGEDOWN(a) ((long)a & ~(pagesize-1))
   int i;
   const int nr_ptr = (10000 * 4)/sizeof(char*);

   b10 = calloc (nr_ptr * sizeof(char*), 1);
   for (i = 0; i < nr_ptr; i++)
      b10[i] = (char*)b10;
   b10[4000] = malloc (1000);
   
   fprintf(stderr, "expecting no leaks\n");
   fflush(stderr);
   VALGRIND_DO_LEAK_CHECK;

   // make b10[4000] undefined. This should create a leak.
   (void) VALGRIND_MAKE_MEM_UNDEFINED (&b10[4000], sizeof(char*));
   fprintf(stderr, "expecting a leak\n");
   fflush(stderr);
   VALGRIND_DO_LEAK_CHECK;

   // make  b10[4000] defined again.
   (void) VALGRIND_MAKE_MEM_DEFINED (&b10[4000], sizeof(char*));

   // now make some bricolage to have some pages around b10[4000]
   // unreadable. The leak check should recover from that
   // thanks to a SEGV handler and a setjmp/longjmp.
   // This setjmp/longjmp is useful if there is a desync between
   // the aspacemgr and the real pages mapping.
   // To have such a discrepancy, we resort on a non SIMD call
   // to mprotect the pages : as this syscall will not be seen
   // by Valgrind core, the aspacemgr will not get a chance
   // to stay synchronised.
   pagesize = sysconf(_SC_PAGE_SIZE);
   if (pagesize == -1)
      perror ("sysconf failed");
   
   if (RUNNING_ON_VALGRIND)
     (void) VALGRIND_NON_SIMD_CALL2(non_simd_mprotect, RNDPAGEDOWN(&b10[4000]), 2 * pagesize);
   else
      mprotect_result = mprotect((void*) RNDPAGEDOWN(&b10[4000]), 2 * pagesize, PROT_NONE);
   fprintf(stderr, "mprotect result %d\n", mprotect_result);

   fprintf(stderr, "expecting a leak again\n");
   fflush(stderr);
   VALGRIND_DO_LEAK_CHECK;

   if (RUNNING_ON_VALGRIND)
     (void) VALGRIND_NON_SIMD_CALL2(non_simd_mprotect,
                                    RNDPAGEDOWN(&b10[0]),
                                    RNDPAGEDOWN(&(b10[nr_ptr-1]))
                                    - RNDPAGEDOWN(&(b10[0])));
   else
      mprotect_result = mprotect((void*) RNDPAGEDOWN(&b10[0]),
                                 RNDPAGEDOWN(&(b10[nr_ptr-1]))
                                 - RNDPAGEDOWN(&(b10[0])),
                                 PROT_NONE);
   fprintf(stderr, "full mprotect result %d\n", mprotect_result);

   fprintf(stderr, "expecting a leak again after full mprotect\n");
   fflush(stderr);
   VALGRIND_DO_LEAK_CHECK;

   fprintf(stderr, "finished\n");
}

int main(void)
{
   DECLARE_LEAK_COUNTERS;

   GET_INITIAL_LEAK_COUNTS;

   f();   // see leak-cases.c


   GET_FINAL_LEAK_COUNTS;

   PRINT_LEAK_COUNTS(stderr);

   return 0;
}
