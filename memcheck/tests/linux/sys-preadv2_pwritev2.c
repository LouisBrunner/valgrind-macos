#define _GNU_SOURCE
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/uio.h>
#include <string.h>
#include "../../memcheck.h"

#include <errno.h>

int main(int argc, char **argv)
{
    char str0[] = "hello ";
    char str1[] = "world\n";
    struct iovec iov[2];
    int fd;

    fd = open("prwv2_source", O_CREAT | O_RDWR, 0644);
    if (fd == -1) {
        perror("prwv2_source");
        exit(EXIT_FAILURE);
    }

    iov[0].iov_base = str0;
    iov[0].iov_len = strlen(str0);
    iov[1].iov_base = str1;
    iov[1].iov_len = strlen(str1);

    /* Check pwritev2 and preadv2 called with the correct arguments works. */
    if (pwritev2(fd, iov, 2, 0, 0) == -1) {
        perror("pwritev2");
        exit(EXIT_FAILURE);
    }

    if (preadv2(fd, iov, 2, 0, 0) == -1) {
        perror("preadv2");
        printf("errno: %d\n", errno);
        exit(EXIT_FAILURE);
    }

    /* Check valgrind will produce expected warnings for the
       various wrong arguments. */
    do {
        /* always allocate 16 bytes to not to have different .exps for different reg sizes */
        char *mem = malloc(16);
        void *t = (void *) &mem[0];
        void *z = (void *) -1;
	int c = *((int *) &mem[4]);
        int flag = *((int *) &mem[8]);
        pwritev2(fd, NULL, 2, 0, 0);
        pwritev2(fd, z, 2, 0, 0);
        pwritev2(fd, t, 2, 0, 0);
        pwritev2(fd, iov, -1, 0, 0);
        pwritev2(fd, iov, c, 0, 0);
        pwritev2(fd, iov, 2, -5, 0);
        pwritev2(-1, iov, 2, -5, 0);
        pwritev2(fd, iov, 2, -5, flag);

        preadv2(fd, NULL, 2, 0, 0);
        preadv2(fd, z, 2, 0, 0);
        preadv2(fd, t, 2, 0, 0);
        preadv2(fd, iov, -1, 0, 0);
        preadv2(fd, iov, c, 0, 0);
        preadv2(fd, iov, 2, -5, 0);
        preadv2(-1, iov, 2, -5, 0);

        iov[1].iov_base = (void *) -1;
        pwritev2(fd, iov, 2, 0, 0);
        preadv2(fd, iov, 2, 0, 0);
        free(mem);
    } while (0);

    close(fd);
    unlink("prwv2_source");
    exit(EXIT_SUCCESS);
}
