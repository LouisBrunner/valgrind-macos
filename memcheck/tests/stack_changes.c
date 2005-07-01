#include <stdio.h>
#include <stdlib.h>
#include <ucontext.h>

#include "valgrind.h"

#define STACK_SIZE 4096

struct ucontext ctx1, ctx2, oldc;
int count;

void hello(struct ucontext *newc)
{
    printf("hello, world: %d\n", count);
    if (count++ == 2)
        newc = &oldc;
    setcontext(newc);
}

int init_context(struct ucontext *uc)
{
    void *stack;
    int ret;

    if (getcontext(uc) == -1) {
        perror("getcontext");
        exit(1);
    }

    if ((stack = malloc(STACK_SIZE)) == NULL) {
        perror("malloc");
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

    makecontext(&ctx1, (void (*)()) hello, 2, &ctx2);
    makecontext(&ctx2, (void (*)()) hello, 2, &ctx1);

    swapcontext(&oldc, &ctx1);

    VALGRIND_STACK_DEREGISTER(c1);
    free(ctx1.uc_stack.ss_sp);
    VALGRIND_STACK_DEREGISTER(c2);
    free(ctx2.uc_stack.ss_sp);

    return 0;
}
