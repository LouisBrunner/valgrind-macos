// For Bug 505673
// Valgrind crashes with an internal error and SIGBUS when the guest tries to open its own file with O_WRONLY|O_CREAT|O_TRUNC
#include <fcntl.h>
#include <cerrno>
#include <stdexcept>
#include <vector>
#include <unistd.h>
#include <sys/syscall.h>
#include <unistd.h>

int main(int argc, char** argv)
{
    std::vector<int> flags{O_WRONLY|O_CREAT|O_TRUNC, O_WRONLY, O_RDWR};

    // On FreeBSD libc open uses syscall openat (at least on 14.2)
    for (auto f : flags)
    {
        int res = open(argv[0], f, 0666);
        if (-1 != res)
        {
            throw std::runtime_error("open should have failed");
        }
        else
        {
            if (errno != ETXTBSY)
            {
                throw std::runtime_error("errno should be ETXTBSY");
            }
        }
    }

    // repeat the above, but with syscall SYS_open
    for (auto f : flags)
    {
        int res = syscall(SYS_open, argv[0], f, 0666);
        if (-1 != res)
        {
            throw std::runtime_error("open should have failed");
        }
        else
        {
            if (errno != ETXTBSY)
            {
                throw std::runtime_error("errno should be ETXTBSY");
            }
        }
    }

    int dotdot;
    if ((dotdot = open("..", O_DIRECTORY | O_RDONLY)) == -1)
    {
        throw std::runtime_error("failed to open ..");
    }
    else
    {
        for (auto f : flags)
        {
            int res = openat(dotdot, "freebsd/open_client", f, 0666);
            if (-1 != res)
            {
                throw std::runtime_error("open should have failed");
            }
            else
            {
                if (errno != ETXTBSY)
                {
                    throw std::runtime_error("errno should be ETXTBSY");
                }
            }
        }
    }
    close(dotdot);

    chdir("..");

    // check that relative paths work
    for (auto f : flags)
    {
        int res = open("freebsd/open_client", f, 0666);
        if (-1 != res)
        {
            throw std::runtime_error("open should have failed");
        }
        else
        {
            if (errno != ETXTBSY)
            {
                throw std::runtime_error("errno should be ETXTBSY");
            }
        }
    }

    for (auto f : flags)
    {
        int res = syscall(SYS_open, "freebsd/open_client", f, 0666);
        if (-1 != res)
        {
            throw std::runtime_error("open should have failed");
        }
        else
        {
            if (errno != ETXTBSY)
            {
                throw std::runtime_error("errno should be ETXTBSY");
            }
        }
    }
}
