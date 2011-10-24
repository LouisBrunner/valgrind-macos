
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

	if (!file)
		errx(1, "Usage: %s <normal-file>", argv[0]);

	fd = open(file, O_RDWR);
	if (fd < 0)
		err(1, "Opening %s", file);

	fl.l_type = F_WRLCK;
	fl.l_whence = SEEK_SET;
	fl.l_start = 0;
	fl.l_len = 1;

	/* I'm assuming noone else tries to lock this! */
	if (fcntl(fd, F_SETLK, &fl) != 0)
		err(1, "Locking %s", file);

	/* If under valgrind, mmap re-opens and closes file, screwing us */
	if (mmap(NULL, getpagesize(), PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0) == MAP_FAILED)
		err(1, "mmap of %s", file);

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
