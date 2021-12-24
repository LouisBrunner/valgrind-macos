#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include "../../memcheck.h"


int main()
{
    const char* filename = strdup("chflags.tst");
    int fd = open(filename, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
 
    fchflags(fd, UF_APPEND);
 
    // a couple of errors while the file is open
    int badfd = fd;
    unsigned long badflags = UF_NODUMP;
    VALGRIND_MAKE_MEM_UNDEFINED(&badfd, sizeof(int));
    VALGRIND_MAKE_MEM_UNDEFINED(&badflags, sizeof(unsigned long));
 
    fchflags(badfd, UF_REPARSE);
    fchflags(fd, badflags);
    close(fd);
 
    chflags(filename, UF_SYSTEM);
    lchflags(filename, UF_SYSTEM);
    chflagsat(AT_FDCWD, filename, UF_SYSTEM, 0);
 
    chflags(filename, badflags);
    lchflags(filename, badflags);
    chflagsat(AT_FDCWD, filename, badflags, 0);
 
    int badatflag;
    chflagsat(AT_FDCWD, filename, UF_SYSTEM, badatflag);
 
    free((void*)filename);
 
    chflags(filename, UF_SYSTEM);
    lchflags(filename, UF_SYSTEM);
    chflagsat(AT_FDCWD, filename, UF_SYSTEM, 0);
}

