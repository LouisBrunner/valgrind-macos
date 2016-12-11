#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>
// Based on a test by Steven Stewart-Gallus, see 342040
int fork_routine(void *arg)
{
        write(1, "fork_routine\n", 13);
	_Exit(EXIT_SUCCESS);
}

int main(void)
{
	long page_size = sysconf(_SC_PAGE_SIZE);
	assert(page_size != -1);

	/* We need an extra page for signals */
	long stack_size = sysconf(_SC_THREAD_STACK_MIN) + page_size;
	assert(stack_size != -1);

	size_t stack_and_guard_size = page_size + stack_size + page_size;
	void *child_stack = mmap(
	    NULL, stack_and_guard_size, PROT_READ | PROT_WRITE,
	    MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN, -1, 0);
	if (NULL == child_stack) {
		perror("mmap");
		return EXIT_FAILURE;
	}

	/* Guard pages are shared between the stacks */
	if (-1 == mprotect((char *)child_stack, page_size, PROT_NONE)) {
		perror("mprotect");
		return EXIT_FAILURE;
	}

	if (-1 == mprotect((char *)child_stack + page_size + stack_size,
	                   page_size, PROT_NONE)) {
		perror("mprotect");
		return EXIT_FAILURE;
	}

	void *stack_start = (char *)child_stack + page_size + stack_size;
        if (0)
           printf("stack_start %p page_size %d stack_size %d\n",
                  stack_start, (int)page_size, (int)stack_size);
        write(1, "parent before clone\n", 20);
	pid_t child =
	    clone(fork_routine, stack_start,
	          SIGCHLD | CLONE_VFORK | CLONE_VM, NULL);
        write(1, "parent after clone\n", 19);
	if (-1 == child) {
		perror("clone");
		return EXIT_FAILURE;
	}

	for (;;) {
		int xx;
		switch (waitpid(child, &xx, 0)) {
		case -1:
			switch (errno) {
			case EINTR:
				continue;
			default:
				perror("waitpid");
				return EXIT_FAILURE;
			}

		default:
			return EXIT_SUCCESS;
		}
	}
}
