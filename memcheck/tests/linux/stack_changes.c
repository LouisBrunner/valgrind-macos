#include <stdio.h>
#include <stdlib.h>
#include <ucontext.h>
#include "tests/sys_mman.h"

#include "valgrind.h"

#define STACK_SIZE (10 * 4096)

// This test is checking the libc context calls (setcontext, etc.) and
// checks that Valgrind notices their stack changes properly.

typedef  struct ucontext  mycontext;

mycontext ctx1, ctx2, oldc;
int count;

void hello(mycontext *newc)
{
    printf("hello, world: %d\n", count);
    if (count++ == 2)
        newc = &oldc;
    setcontext(newc);
}

int init_context(mycontext *uc)
{
    void *stack;
    int ret;

    if (getcontext(uc) == -1) {
        //perror("getcontext");
        printf("getcontext() doesn't seem to work\n");
        exit(1);
    }

    stack = (void *)mmap(0, STACK_SIZE, PROT_READ|PROT_WRITE|PROT_EXEC,
                                        MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);

    if (stack == (void*)-1) {
        perror("mmap");
        exit(1);
    }

    ret = VALGRIND_STACK_REGISTER(stack, stack + STACK_SIZE);

    uc->uc_link = NULL;
    uc->uc_stack.ss_sp = stack;
    uc->uc_stack.ss_size = STACK_SIZE;
    uc->uc_stack.ss_flags = 0;

    return ret;
}

int main(int argc, char **argv)
{
    int c1 = init_context(&ctx1);
    int c2 = init_context(&ctx2);

    makecontext(&ctx1, (void (*)()) hello, 1, &ctx2);
    makecontext(&ctx2, (void (*)()) hello, 1, &ctx1);

    swapcontext(&oldc, &ctx1);

    (void)VALGRIND_STACK_DEREGISTER(c1);
    //free(ctx1.uc_stack.ss_sp);
    (void)VALGRIND_STACK_DEREGISTER(c2);
    //free(ctx2.uc_stack.ss_sp);

    return 0;
}
