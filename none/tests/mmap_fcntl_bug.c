
/* Test program to demonstrate valgrind breaking fcntl locks during
 * mmap.  Feed it a r/w file, such as its own source code. */

/* See bug 280965. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <err.h>

int main(int argc, char *argv[])
{
	struct flock fl;
	const char *file = /* argv[1]; */
			   "mmap_fcntl_bug.c";
	int fd, status;
        off_t initial;

	if (!file)
		errx(1, "Usage: %s <normal-file>", argv[0]);

	fd = open(file, O_RDWR);
	if (fd < 0)
		err(1, "Opening %s", file);

        // reproduce bug 297991: mmap interferes with fd position
        initial = lseek(fd, 123, SEEK_SET);
        if (123 != initial)
                err(1, "initial off_t differs from 123 (TEST FAILED)");
        if (lseek(fd, 0, SEEK_CUR) != 123)
                err(1, "zero offset from initial differs from 123 (TEST FAILED)");

	fl.l_type = F_WRLCK;
	fl.l_whence = SEEK_SET;
	fl.l_start = 0;
	fl.l_len = 1;

	/* I'm assuming no one else tries to lock this! */
	if (fcntl(fd, F_SETLK, &fl) != 0)
		err(1, "Locking %s", file);

	/* If under valgrind, mmap re-opens and closes file, screwing us */
	if (mmap(NULL, getpagesize(), PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0) == MAP_FAILED)
		err(1, "mmap of %s", file);
        if (lseek(fd, 0, SEEK_CUR) != 123)
                errx(1, "zero offset from initial after mmap differs from 123 (TEST FAILED)");

	switch (fork()) {
	case 0:
		/* Child.  Lock should fail. */
		if (fcntl(fd, F_SETLK, &fl) == 0)
			exit(1);
		exit(0);
	case -1:
		err(1, "Fork failed");
	}

	if (wait(&status) == -1)
		 err(1, "Child vanished?");

	if (!WIFEXITED(status))
		errx(1, "Child died with signal %i", WTERMSIG(status));

	switch (WEXITSTATUS(status)) {
	case 1:
		errx(1, "Child got lock, we must have dropped it (TEST FAILED)");
	case 0:
		fprintf(stderr, "Child exited with zero (TEST PASSED).\n");
		return 0;
	default:
		errx(1, "Child weird exit status %i", WEXITSTATUS(status));
	}
}
