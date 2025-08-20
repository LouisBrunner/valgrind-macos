#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/param.h>
#include <iostream>
#include <cstring>

int main()
{
    char path[MAXPATHLEN];
    char linkedPath[MAXPATHLEN];
    char currentDirectory[MAXPATHLEN];
    char parentDirectory[MAXPATHLEN];

    getcwd(currentDirectory, MAXPATHLEN);

    ssize_t res = readlinkat(AT_FDCWD, "readlinkat_link.cpp", linkedPath, MAXPATHLEN);
    if (res == -1)
    {
        std::cerr << "Error: readlinkat test 1 failed\n";
    }
    if (!strcmp(linkedPath, "readlink.cpp"))
    {
        std::cerr << "Error: readlinkat test 1 unexpected resolved path - " << linkedPath << '\n';
    }

    int dotdot;
    if ((dotdot = open("..", O_DIRECTORY | O_RDONLY)) == -1)
    {
        throw std::runtime_error("failed to open .");
    }
    else
    {
        res = readlinkat(dotdot, "freebsd/readlinkat_link.cpp", linkedPath, MAXPATHLEN);
         if (res == -1)
        {
            std::cerr << "Error: readlinkat test 2 failed\n";
        }
        if (!strcmp(linkedPath, "readlink.cpp"))
        {
            std::cerr << "Error: readlinkat test 2 unexpected resolved path - " << linkedPath << '\n';
        }
    }
    close(dotdot);

    chdir("..");
    res = readlinkat(AT_FDCWD, "freebsd/readlinkat_link.cpp", linkedPath, MAXPATHLEN);
    if (res == -1)
    {
        std::cerr << "Error: readlinkat test 3 failed\n";
    }
    if (!strcmp(linkedPath, "readlink.cpp"))
    {
        std::cerr << "Error: readlinkat test 3 unexpected resolved path - " << linkedPath << '\n';
    }
}
