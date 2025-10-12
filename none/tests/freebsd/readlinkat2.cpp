#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/param.h>
#include <iostream>
#include <cstring>
#include <cstdlib>

int main(int argc, char** argv)
{
    char linkedPath[MAXPATHLEN];
    char selfAbsolutePath[MAXPATHLEN];
    auto pid{getpid()};
    std::string pidString{std::to_string(pid)};
    std::string procPidFile{std::string("/proc/") + pidString + "/file"};
    realpath(argv[0], selfAbsolutePath);
    std::string selfAbsolutePathString(selfAbsolutePath);

    ssize_t res = readlinkat(AT_FDCWD, "/proc/curproc/file", linkedPath, MAXPATHLEN);
    if (res == -1)
    {
        std::cerr << "Error: readlinkat test 1 failed\n";
    }
    else
    {
        if (selfAbsolutePathString != linkedPath)
        {
            std::cerr << "Error: readlinkat test 1 unexpected resolved path - " << linkedPath << '\n';
        }
    }

    res = readlinkat(AT_FDCWD, procPidFile.c_str(), linkedPath, MAXPATHLEN);
    if (res == -1)
    {
        std::cerr << "Error: readlinkat test 2 failed\n";
    }
    else
    {
        if (selfAbsolutePathString != linkedPath)
        {
            std::cerr << "Error: readlinkat test 2 unexpected resolved path - " << linkedPath << '\n';
        }
    }

    // @todo PJF do some tests with cwd as /proc /proc/PID and /proc/curproc
    // and a rlative path to 'file'
    // not yet implemented in Valgrind
    chdir("/proc");

    // @todo PJF do some tests as above but with fd as /proc /proc/PID and /proc/curproc
    int slash;
    if ((slash = open("/", O_DIRECTORY | O_RDONLY)) == -1)
    {
        throw std::runtime_error("failed to open /");
    }
    close(slash);
}
