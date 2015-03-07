#include <sys/mman.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>
#include <unistd.h>
#include <sys/syscall.h>
#include "../../config.h"

#define VG_STRINGIFZ(__str)  #__str
#define VG_STRINGIFY(__str)  VG_STRINGIFZ(__str)

extern void _exit_with_stack_teardown(void*, size_t);

/* Below code is modified version of android bionic
   pthread_exit: when a detached thread exits: it munmaps
   its stack and then exits. We cannot do that in C,
   as we cannot touch the stack after the munmap
   and before the exit. */

#if defined(VGP_x86_linux)
asm("\n"
    ".text\n"
    "\t.globl _exit_with_stack_teardown\n"
    "\t.type  _exit_with_stack_teardown,@function\n"
    "_exit_with_stack_teardown:\n"
    // We can trash registers because this function never returns.
    "\tmov 4(%esp), %ebx\n"             // stackBase
    "\tmov 8(%esp), %ecx\n"             // stackSize
    "\tmov $"VG_STRINGIFY(__NR_munmap)", %eax\n"
    "\tint $0x80\n"
    // If munmap failed, we ignore the failure and exit anyway.

    "\tmov $0, %ebx\n"                  // status
    "\tmovl $"VG_STRINGIFY(__NR_exit)", %eax\n"
    "\tint $0x80\n");
    // The exit syscall does not return.

#elif defined(VGP_amd64_linux)
asm("\n"
    ".text\n"
    "\t.globl _exit_with_stack_teardown\n"
    "\t.type  _exit_with_stack_teardown,@function\n"
    "_exit_with_stack_teardown:\n"
    "\tmov $"VG_STRINGIFY(__NR_munmap)", %eax\n"
    "\tsyscall\n"
    // If munmap failed, we ignore the failure and exit anyway.
    "\tmov $0, %rdi\n"
    "\tmov $"VG_STRINGIFY(__NR_exit)", %eax\n"
    "\tsyscall\n");
  // The exit syscall does not return.

#elif defined(VGP_arm_linux)
asm("\n"
    ".text\n"
    "\t.globl _exit_with_stack_teardown\n"
    "\t.type  _exit_with_stack_teardown,%function\n"
    "_exit_with_stack_teardown:\n"
    "\tldr r7, ="VG_STRINGIFY(__NR_munmap)"\n"
    "\tswi #0\n"
    // If munmap failed, we ignore the failure and exit anyway.

    "\tmov r0, #0\n"
    "\tldr r7, ="VG_STRINGIFY(__NR_exit)"\n"
    "\tswi #0\n");
  // The exit syscall does not return.

#else
void _exit_with_stack_teardown(void*stack, size_t sz)
{
   // asm code not done for this platform.
   // Do nothing, just return. The thread will exit spontaneously
}

#endif
static void *stack;
static size_t sz = 64 * 1024;

/* This one detaches, does its own thing. */
void* child_fn ( void* arg )
{
  int r;
  r= pthread_detach( pthread_self() ); assert(!r);
  _exit_with_stack_teardown(stack, sz);
  return NULL;
}

/* Parent creates 1 child, that will detach, and exit after destroying
   its own stack. */
int main ( void )
{
   int r;
   pthread_attr_t attr;
   pthread_t child;

   r = pthread_attr_init(&attr); assert(!r);
# if !defined(VGO_darwin)
   stack = mmap(NULL, sz, PROT_READ|PROT_WRITE,  MAP_PRIVATE | MAP_ANONYMOUS,
                -1, 0);
# else
   stack = mmap(NULL, sz, PROT_READ|PROT_WRITE,  MAP_PRIVATE | MAP_ANON,
                -1, 0);
# endif
   assert(stack != (void *)-1);
   r = pthread_attr_setstack(&attr, stack, sz);
   r = pthread_create(&child, &attr, child_fn, NULL); assert(!r);
   sleep(1);

   return 0;
}
