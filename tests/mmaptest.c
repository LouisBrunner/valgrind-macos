#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>

int main()
{
    int fd;

    mkdir("dir", 0666);
    fd = open("dir", O_RDONLY);
    mmap(NULL, 4711, PROT_READ, MAP_PRIVATE, fd, 0);
    return 0;
}
