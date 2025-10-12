#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/param.h>
#include <iostream>
#include <cstring>

int main()
{
    char linkedPath[MAXPATHLEN];
    char currentDirectory[MAXPATHLEN];

    getcwd(currentDirectory, MAXPATHLEN);
    std::string absolutePath{currentDirectory};
    absolutePath += "/readlinkat_link.cpp";

    ssize_t res = readlinkat(AT_FDCWD, "readlinkat_link.cpp", linkedPath, MAXPATHLEN);
    if (res == -1)
    {
        std::cerr << "Error: readlinkat test 1 failed\n";
    }
    else
    {
        if (!strcmp(linkedPath, "readlink.cpp"))
        {
            std::cerr << "Error: readlinkat test 1 unexpected resolved path - " << linkedPath << '\n';
        }
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
        else
        {
            if (!strcmp(linkedPath, "readlink.cpp"))
            {
                std::cerr << "Error: readlinkat test 2 unexpected resolved path - " << linkedPath << '\n';
            }
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

    int uninit;
    res = readlinkat(uninit, absolutePath.c_str(), linkedPath, MAXPATHLEN);
    if (res == -1)
    {
        std::cerr << "Error: readlinkat test 4 failed\n";
    }
    if (!strcmp(linkedPath, "readlink.cpp"))
    {
        std::cerr << "Error: readlinkat test 4 unexpected resolved path - " << linkedPath << '\n';
    }

}
