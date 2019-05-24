#define _GNU_SOURCE
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include "../../memcheck.h"

int main(int argc, char **argv)
{
    int fd_in, fd_out;
    struct stat stat;
    loff_t len, ret;

    fd_in = open("copy_file_range_source", O_CREAT | O_RDWR, 0644);
    if (fd_in == -1) {
        perror("open copy_file_range_source");
        exit(EXIT_FAILURE);
    }

    if (write(fd_in, "foo bar\n", 8) != 8) {
        perror("writing to the copy_file_range_source");
        exit(EXIT_FAILURE);
    }
    lseek(fd_in, 0, SEEK_SET);

    if (fstat(fd_in, &stat) == -1) {
        perror("fstat");
        exit(EXIT_FAILURE);
    }

    len = stat.st_size;

    fd_out = open("copy_file_range_dest", O_CREAT | O_WRONLY | O_TRUNC, 0644);
    if (fd_out == -1) {
        perror("open copy_file_range_dest");
        exit(EXIT_FAILURE);
    }

    /* Check copy_file_range called with the correct arguments works. */
    do {
        ret = copy_file_range(fd_in, NULL, fd_out, NULL, len, 0);
        if (ret == -1) {
            perror("copy_file_range");
            exit(EXIT_FAILURE);
        }

        len -= ret;
    } while (len > 0);

    /* Check valgrind will produce expected warnings for the
       various wrong arguments. */
    do {
        void *t = 0; VALGRIND_MAKE_MEM_UNDEFINED (&t, sizeof (void *));
        void *z = (void *) -1;

        ret = copy_file_range(fd_in, t, fd_out, NULL, len, 0);
        ret = copy_file_range(fd_in, NULL, fd_out, z, len, 0);
        ret = copy_file_range(- 1, NULL, - 1, NULL, len, 0);
    } while (0);

    close(fd_in);
    close(fd_out);
    unlink("copy_file_range_source");
    unlink("copy_file_range_dest");
    exit(EXIT_SUCCESS);
}
