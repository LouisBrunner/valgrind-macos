/* Tests that the process can exit even if daemon thread is still running.
   This test does *not* use any libc; it interfaces only with kernel. */

#include <sys/lwp.h>
#include <sys/mman.h>
#include <sys/regset.h>
#include <sys/syscall.h>
#include <ucontext.h>

#if defined(__amd64) || defined(__i386)
#include <sys/segments.h>
#endif

extern void bzero(void *ptr, size_t n);

#if defined(VGP_x86_solaris)
asm("\n"
".text\n"
".globl bzero\n"
"bzero:\n"
"   push %edi\n"
"   movl $0, %eax\n"
"   movl 12(%esp), %ecx\n"
"   movl 8(%esp), %edi\n"
"   rep  stosb\n"
"   pop  %edi\n"
"   ret\n"
);
#elif defined(VGP_amd64_solaris)
asm("\n"
".text\n"
".globl bzero\n"
"bzero:\n"
"   push %rdi\n"
"   movq %rsi, %rcx\n"
"   movq $0, %rax\n"
"   rep  stosb\n"
"   pop  %rdi\n"
"   ret\n"
);
#else
#  error "Unknown platform"
#endif

static void sleep(unsigned int sec) {
   timespec_t ts;
   ts.tv_sec = (time_t)sec;
   ts.tv_nsec = 0;

#if defined(VGP_x86_solaris)
   __asm__ __volatile__ (
      "pushl $0\n"
      "pushl %[TS]\n"
      "pushl $0xdeadbeef\n"
      "movl  %[SYSNO], %%eax\n"
      "int   $0x91\n"
      "addl  $12, %%esp\n"
      :
      : [TS] "g" (&ts), [SYSNO] "n" (SYS_nanosleep)
      : "eax", "edx", "cc", "memory");
#elif defined(VGP_amd64_solaris)
   __asm__ __volatile__ (
      "movq %[SYSNO], %%rax\n"
      "movq %[TS], %%rdi\n"
      "movq $0, %%rsi\n"
      "syscall\n"
      :
      : [TS] "g" (&ts), [SYSNO] "n" (SYS_nanosleep)
      : "rax", "rdx", "rdi", "rsi", "cc", "memory");
#else
#  error "Unknown platform"
#endif
}

static void lwp_exit(void) {
#if defined(VGP_x86_solaris)
   __asm__ __volatile__ (
      "movl %[SYSNO], %%eax\n"
      "int  $0x91\n"
      :
      : [SYSNO] "n" (SYS_lwp_exit)
      : "eax", "edx", "cc", "memory");
#elif defined(VGP_amd64_solaris)
   __asm__ __volatile__ (
      "movq %[SYSNO], %%rax\n"
      "syscall\n"
      :
      : [SYSNO] "n" (SYS_lwp_exit)
      : "rax", "rdx", "cc", "memory");
#else
#  error "Unknown platform"
#endif
}

#define STACK_FLAGS (MAP_PRIVATE | MAP_NORESERVE | MAP_ANON)
#define STACK_PROT  (PROT_READ | PROT_WRITE)
static void *allocate_stack(size_t stacksize) {
   void *address = NULL;

#if defined(VGP_x86_solaris)
   __asm__ __volatile__ (
      "pushl $0\n"
      "pushl $-1\n"
      "pushl %[FLAGS]\n"
      "pushl %[PROT]\n"
      "pushl %[SIZE]\n"
      "pushl $0\n"
      "pushl $0xdeadbeef\n"
      "movl  %[SYSNO], %%eax\n"
      "int   $0x91\n"
      "addl  $28, %%esp\n"
      "movl %%eax, %[ADDRESS]\n"
      : [ADDRESS] "=r" (address)
      : [FLAGS] "n" (STACK_FLAGS), [PROT] "n" (STACK_PROT),
        [SIZE] "g" (stacksize), [SYSNO] "n" (SYS_mmap)
      : "eax", "edx", "cc", "memory");
#elif defined(VGP_amd64_solaris)
   __asm__ __volatile__ (
      "movq %[SYSNO], %%rax\n"
      "movq $0, %%rdi\n"
      "movq %[SIZE], %%rsi\n"
      "movq %[PROT], %%rdx\n"
      "movq %[FLAGS], %%r10\n"
      "movq $-1, %%r8\n"
      "movq $0, %%r9\n"
      "syscall\n"
      "movq %%rax, %[ADDRESS]\n"
      : [ADDRESS] "=r" (address)
      : [FLAGS] "n" (STACK_FLAGS), [PROT] "n" (STACK_PROT),
        [SIZE] "g" (stacksize), [SYSNO] "n" (SYS_mmap)
      : "rax", "rdx", "rdi", "rsi", "r10", "r8", "r9", "cc", "memory");
#else
#  error "Unknown platform"
#endif

   return address;
}
#undef STACK_FLAGS
#undef STACK_PROT

static void thread_func(void) {
   sleep(10000);
}

#define LWP_FLAGS (LWP_SUSPENDED | LWP_DETACHED | LWP_DAEMON)
static id_t lwp_create(void *stack) {
   id_t tid;

   ucontext_t ucontext;
   bzero(&ucontext, sizeof(ucontext));
   ucontext.uc_flags = UC_CPU;

#if defined(VGP_x86_solaris)
   __asm__ __volatile__ (
      "mov %%ss, %[STACK_SEG]\n"
      : [STACK_SEG] "=r" (ucontext.uc_mcontext.gregs[SS])
      :
      :);
   ucontext.uc_mcontext.gregs[EIP] = (greg_t) thread_func;
   ucontext.uc_mcontext.gregs[UESP] = (greg_t) stack;
   ucontext.uc_mcontext.gregs[EBP] = (greg_t) stack;
#elif defined(VGP_amd64_solaris)
   ucontext.uc_mcontext.gregs[REG_SS] = UDS_SEL;
   ucontext.uc_mcontext.gregs[REG_RIP] = (greg_t) thread_func;
   ucontext.uc_mcontext.gregs[REG_RSP] = (greg_t) stack;
   ucontext.uc_mcontext.gregs[REG_RBP] = (greg_t) stack;
#else
#  error "Unknown platform"
#endif

#if defined(VGP_x86_solaris)
   __asm__ __volatile__ (
      "pushl $0\n"
      "pushl %[FLAGS]\n"
      "pushl %[UCONTEXT]\n"
      "pushl $0xdeadbeef\n"
      "movl  %[SYSNO], %%eax\n"
      "int   $0x91\n"
      "addl  $16, %%esp\n"
      "movl %%eax, %[TID]\n"
      : [TID] "=r" (tid)
      : [FLAGS] "n" (LWP_FLAGS), [UCONTEXT] "g" (&ucontext),
        [SYSNO] "n" (SYS_lwp_create)
      : "eax", "edx", "cc", "memory");
#elif defined(VGP_amd64_solaris)
   __asm__ __volatile__ (
      "movq %[SYSNO], %%rax\n"
      "movq %[UCONTEXT], %%rdi\n"
      "movq %[FLAGS], %%rsi\n"
      "movq $0, %%rdx\n"
      "syscall\n"
      "movl %%eax, %[TID]\n"
      : [TID] "=r" (tid)
      : [FLAGS] "n" (LWP_FLAGS), [UCONTEXT] "g" (&ucontext),
        [SYSNO] "n" (SYS_lwp_create)
      : "rax", "rdx", "rdi", "rsi", "cc", "memory");
#else
#  error "Unknown platform"
#endif

   return tid;
}

static void lwp_continue(id_t tid) {
#if defined(VGP_x86_solaris)
   __asm__ __volatile__ (
      "pushl %[TID]\n"
      "pushl $0xdeadbeef\n"
      "movl  %[SYSNO], %%eax\n"
      "int   $0x91\n"
      "addl  $8, %%esp\n"
      :
      : [TID] "m" (tid), [SYSNO] "n" (SYS_lwp_continue)
      : "eax", "edx", "cc", "memory");
#elif defined(VGP_amd64_solaris)
   __asm__ __volatile__ (
      "movq %[SYSNO], %%rax\n"
      "xor %%rdi, %%rdi\n"
      "movl %[TID], %%edi\n"
      "syscall\n"
      :
      : [TID] "r" (tid), [SYSNO] "n" (SYS_lwp_continue)
      : "rax", "rdx", "rdi", "cc", "memory");
#else
#  error "Unknown platform"
#endif
}

#define STACK_SIZE 16384
void _start(void) {
   void *stack = allocate_stack(STACK_SIZE);
   id_t tid = lwp_create((char *) stack + STACK_SIZE);
   lwp_continue(tid);
   sleep(5);
   lwp_exit();
   return; /* not reached */
}

