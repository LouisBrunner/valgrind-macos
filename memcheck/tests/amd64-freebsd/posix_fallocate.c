#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

int main(void)
{
    int fd = open("foo", O_RDWR|O_CREAT, 0666);
    if (fd < 0) return 1;
    posix_fallocate(fd, 0, 400000);

    int badfd = 42;
    int x;
    posix_fallocate(badfd, x, 20);
    posix_fallocate(badfd, 0, x);
    x = posix_fallocate(badfd, 0, 20);
    if (x != EBADF)
        fprintf(stderr, "Unexpected return value: %d\n", x);
}

