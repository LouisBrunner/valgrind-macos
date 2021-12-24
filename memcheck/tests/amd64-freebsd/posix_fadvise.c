#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

int main(void)
{
    int fd = open("foo", O_RDWR|O_CREAT, 0666);
    if (fd < 0) return 1;
    posix_fadvise( fd, 0, 4096, POSIX_FADV_WILLNEED );
    posix_fadvise( fd, 0, 0, POSIX_FADV_NOREUSE );

    int badfd = 42;
    posix_fadvise( badfd, 0, 4096, POSIX_FADV_WILLNEED );

    int x;
    posix_fadvise(x,      1, 2, POSIX_FADV_NORMAL);
    posix_fadvise(badfd, x, 2, POSIX_FADV_NORMAL);
    posix_fadvise(badfd, 1, x, POSIX_FADV_NORMAL);
    posix_fadvise(badfd, 1, 2, x);

    x = posix_fadvise(badfd
    , 1, 2, POSIX_FADV_NORMAL);

    if (x != EBADF)
        fprintf(stderr, "Unexpected return value: %d\n", x);
}

