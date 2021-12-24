#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "../memcheck.h"
#include "leak.h"
#include <sys/mman.h>
#include <sys/syscall.h>

typedef unsigned int             UInt;
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

#elif defined(VGP_arm64_linux)
extern UWord do_syscall_WRK (
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6,
          UWord syscall_no
       );
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"        mov x8, x6\n"
"        mov x6, 0\n"
"        mov x7, 0\n"
"        svc 0\n"
"        ret\n"
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

#elif defined(VGP_x86_solaris)
extern ULong
do_syscall_WRK(UWord a1, UWord a2, UWord a3,
               UWord a4, UWord a5, UWord a6,
               UWord a7, UWord a8,
               UWord syscall_no,
               UInt *errflag);
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"        movl    40(%esp), %ecx\n"      /* assume syscall success */
"        movl    $0, (%ecx)\n"
"        movl    36(%esp), %eax\n"
"        int     $0x91\n"
"        jnc     1f\n"                  /* jump if success */
"        movl    40(%esp), %ecx\n"      /* syscall failed - set *errflag */
"        movl    $1, (%ecx)\n"
"1:      ret\n"
".previous\n"
);

#elif defined(VGP_amd64_solaris)
extern ULong
do_syscall_WRK(UWord a1, UWord a2, UWord a3,
               UWord a4, UWord a5, UWord a6,
               UWord a7, UWord a8,
               UWord syscall_no,
               UInt *errflag);
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"       movq    %rcx, %r10\n"           /* pass rcx in r10 instead */
"       movq    32(%rsp), %rcx\n"       /* assume syscall success */
"       movl    $0, (%rcx)\n"
"       movq    24(%rsp), %rax\n"
"       syscall\n"
"       jnc     1f\n"                   /* jump if success */
"       movq    32(%rsp), %rcx\n"       /* syscall failed - set *errflag */
"       movl    $1, (%rcx)\n"
"1:     ret\n"
".previous\n"
);

#elif defined(VGP_x86_freebsd)

#define __NR_mprotect 74

/* Incoming args (syscall number + up to 8 args) are on the stack.
   FreeBSD has a syscall called 'syscall' that takes all args (including
   the syscall number) off the stack.  Since we're called, the return
   address is on the stack as expected, so we can just call syscall(2)
   and it Just Works.  Error is when carry is set.
*/
extern ULong do_syscall_WRK (
          UWord syscall_no,
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6,
          UWord a7, UWord a8, UInt *flags
       );
asm(
".text\n"
"do_syscall_WRK:\n"
"      movl    $0,%eax\n"      /* syscall number = "syscall" (0) to avoid stack frobbing
*/
"      int     $0x80\n"
"      jb      1f\n"
"      ret\n"
"1:    movl    40(%esp),%ecx\n"        /* store carry in *flags */
"      movl    $1,(%ecx)\n"
"      ret\n"
".previous\n"
);

#elif defined(VGP_amd64_freebsd)

#define __NR_mprotect 74

extern UWord do_syscall_WRK (
          UWord syscall_no,    /* %rdi */
          UWord a1,            /* %rsi */
          UWord a2,            /* %rdx */
          UWord a3,            /* %rcx */
          UWord a4,            /* %r8 */
          UWord a5,            /* %r9 */
          UWord a6,            /* 8(%rsp) */
          UWord a7,            /* 16(%rsp) */
          UWord a8,            /* 24(%rsp) */
          UInt *flags,         /* 32(%rsp) */
          UWord *rv2           /* 40(%rsp) */
       );
asm(
".text\n"
"do_syscall_WRK:\n"
        /* Convert function calling convention --> syscall calling
           convention */
"      pushq   %rbp\n"
"      movq    %rsp, %rbp\n"
"      movq    %rdi, %rax\n"    /* syscall_no */
"      movq    %rsi, %rdi\n"    /* a1 */
"      movq    %rdx, %rsi\n"    /* a2 */
"      movq    %rcx, %rdx\n"    /* a3 */
"      movq    %r8,  %r10\n"    /* a4 */
"      movq    %r9,  %r8\n"     /* a5 */
"      movq    16(%rbp), %r9\n"  /* a6 last arg from stack, account for %rbp */
"      movq    24(%rbp), %r11\n" /* a7 from stack */
"      pushq  %r11\n"
"      movq    32(%rbp), %r11\n" /* a8 from stack */
"      pushq  %r11\n"
"      subq    $8,%rsp\n"      /* fake return addr */
"      syscall\n"
"      jb      1f\n"
"      movq    48(%rbp),%rsi\n"
"      movq    %rdx, (%rsi)\n"
"      movq    %rbp, %rsp\n"
"      popq    %rbp\n"
"      ret\n"
"1:\n"
"      movq    40(%rbp), %rsi\n"
"      movl    $1,(%rsi)\n"
"      movq    %rbp, %rsp\n"
"      popq    %rbp\n"
"      ret\n"
".previous\n"
);

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
char *interior_ptrs[3];
int mprotect_result = 0;
static void non_simd_mprotect (long tid, void* addr, long len)
{
#if defined(VGP_x86_solaris) || defined(VGP_amd64_solaris)
   UInt err = 0;
   mprotect_result = do_syscall_WRK((UWord) addr, len, PROT_NONE,
                                    0, 0, 0, 0, 0, SYS_mprotect, 
                                    &err);
   if (err)
      mprotect_result = -1;
#elif defined(VGP_arm64_linux)
   mprotect_result = do_syscall_WRK((UWord) addr, len, PROT_NONE,
                                    0, 0, 0,
                                    __NR_mprotect);
#elif defined(VGP_x86_freebsd)

   UInt flags = 0U;
   mprotect_result = do_syscall_WRK(__NR_mprotect,
                                    (UWord) addr, len, PROT_NONE,
                                    0, 0, 0, 0, 0, &flags);
#elif defined(VGP_amd64_freebsd)

   UInt flags = 0U;
   UWord rv2 = 0U;
   mprotect_result = do_syscall_WRK(__NR_mprotect,
                                    (UWord) addr, len, PROT_NONE,
                                    0, 0, 0, 0, 0, &flags, &rv2);

#else
   mprotect_result = do_syscall_WRK(__NR_mprotect,
                                    (UWord) addr, len, PROT_NONE,
                                    0, 0, 0);
#endif
}

// can this work without global variable for return value?
static void my_mprotect_none(void* addr, long len)
{
   if (RUNNING_ON_VALGRIND)
     (void) VALGRIND_NON_SIMD_CALL2(non_simd_mprotect,
                                    addr,
                                    len);
   else
      mprotect_result = mprotect(addr,
                                 len,
                                 PROT_NONE);
}

void f(void)
{
   long pagesize;
#define RNDPAGEDOWN(a) ((long)a & ~(pagesize-1))
   int i;
   const int nr_ptr = (10000 * 20)/sizeof(char*);

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
   
   my_mprotect_none((void*) RNDPAGEDOWN(&b10[4000]), 2 * pagesize);
   fprintf(stderr, "mprotect result %d\n", mprotect_result);

   fprintf(stderr, "expecting a leak again\n");
   fflush(stderr);
   VALGRIND_DO_LEAK_CHECK;

   my_mprotect_none((void*) RNDPAGEDOWN(&b10[0]),
                                 RNDPAGEDOWN(&(b10[nr_ptr-1]))
                                 - RNDPAGEDOWN(&(b10[0])));
   fprintf(stderr, "full mprotect result %d\n", mprotect_result);

   fprintf(stderr, "expecting a leak again after full mprotect\n");
   fflush(stderr);
   VALGRIND_DO_LEAK_CHECK;

   // allocate memory but keep only interior pointers to trigger various
   // heuristics
   // Allocate some memory:
   interior_ptrs[0] = calloc (nr_ptr * sizeof(char*), 1);

   // Inner pointer after 3 sizeT: triggers the stdstring heuristic:
   interior_ptrs[2] = interior_ptrs[0] + 3 * sizeof(size_t);

   // Inner pointer after 1 ULong: triggers the length64 heuristic:
   interior_ptrs[1] = interior_ptrs[0] + sizeof(unsigned long);

   // Inner pointer after a size: triggers the newarray heuristics.
   interior_ptrs[0] += sizeof(size_t);

   my_mprotect_none( (void*) RNDPAGEDOWN((interior_ptrs[0] - sizeof(size_t))),
                     RNDPAGEDOWN(nr_ptr * sizeof(char*)));
   fprintf(stderr, "mprotect result %d\n", mprotect_result);

   fprintf(stderr, "expecting heuristic not to crash after full mprotect\n");
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
