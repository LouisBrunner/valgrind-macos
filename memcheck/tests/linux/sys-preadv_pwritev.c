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

    fd = open("prwv_source", O_CREAT | O_RDWR, 0644);
    if (fd == -1) {
        perror("prwv2_source");
        exit(EXIT_FAILURE);
    }

    iov[0].iov_base = str0;
    iov[0].iov_len = strlen(str0);
    iov[1].iov_base = str1;
    iov[1].iov_len = strlen(str1);

    /* Check pwritev and preadv called with the correct arguments works. */
    if (pwritev(fd, iov, 2, 0) == -1) {
        perror("pwritev");
        exit(EXIT_FAILURE);
    }

    if (preadv(fd, iov, 2, 0) == -1) {
        perror("preadv");
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
        pwritev(fd, NULL, 2, 0);
        pwritev(fd, z, 2, 0);
        pwritev(fd, t, 2, 0);
        pwritev(fd, iov, -1, 0);
        pwritev(fd, iov, c, 0);
        pwritev(fd, iov, 2, -5);
        pwritev(-1, iov, 2, -5);

        preadv(fd, NULL, 2, 0);
        preadv(fd, z, 2, 0);
        preadv(fd, t, 2, 0);
        preadv(fd, iov, -1, 0);
        preadv(fd, iov, c, 0);
        preadv(fd, iov, 2, -5);
        preadv(-1, iov, 2, -5);

        iov[1].iov_base = (void *) -1;
        pwritev(fd, iov, 2, 0);
        preadv(fd, iov, 2, 0);
        free(mem);
    } while (0);

    close(fd);
    unlink("prwv_source");
    exit(EXIT_SUCCESS);
}
