// For Bug 505673
// Valgrind crashes with an internal error and SIGBUS when the guest tries to open its own file with O_WRONLY|O_CREAT|O_TRUNC
#include <fcntl.h>
#include <cerrno>
#include <stdexcept>
#include <string>
#include <vector>
#include <unistd.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <linux/openat2.h>

int main(int argc, char** argv)
{
    std::vector<__u64> flags{O_WRONLY|O_CREAT|O_TRUNC, O_WRONLY, O_RDWR};
    auto pid = getpid();
    auto ppe = std::string("/proc/") + std::to_string(pid) + "/exe";
    std::vector<std::string> names{argv[0], "/proc/self/exe", ppe};
    int dotdot;

    for (const auto& n : names)
    {
        for (auto f : flags)
        {
            int res = open(n.c_str(), f, 0666);
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

#if defined(SYS_open)
        for (auto f : flags)
        {
            int res = syscall(SYS_open, n.c_str(), f, 0666);

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
#endif
    }

    if ((dotdot = open("..", O_DIRECTORY | O_RDONLY)) == -1)
    {
        throw std::runtime_error("failed to open ,.");
    }
    else
    {
        for (auto f : flags)
        {
            int res = openat(dotdot, "linux/open_client", f, 0666);
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

#if defined(SYS_openat2)
    for (const auto& n : names)
    {
        for (auto f : flags)
        {
            struct open_how oh = { .flags=f, .mode=((f&static_cast<__u64>(O_CREAT))?0666UL:0UL), .resolve=0 };
            int res = syscall(SYS_openat2, AT_FDCWD, n.c_str(), &oh, sizeof(oh));
            if (-1 != res)
            {
                throw std::runtime_error("openat2 should have failed");
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

    for (auto f : flags)
    {
        struct open_how oh = { .flags=f, .mode=((f&static_cast<__u64>(O_CREAT))?0666UL:0UL), .resolve=0 };
        int res = syscall(SYS_openat2, dotdot, "linux/open_client", &oh, sizeof(oh));
        if (-1 != res)
        {
            throw std::runtime_error("openat2 should have failed");
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
        struct open_how oh = { .flags=f, .mode=((f&static_cast<__u64>(O_CREAT))?0666UL:0UL), .resolve=RESOLVE_IN_ROOT };
        int res = syscall(SYS_openat2, dotdot, "/linux/open_client", &oh, sizeof(oh));
        if (-1 != res)
        {
            throw std::runtime_error("openat2 should have failed");
        }
        else
        {
            if (errno != ETXTBSY)
            {
                throw std::runtime_error("errno should be ETXTBSY");
            }
        }
    }
#endif

    close(dotdot);

    chdir("..");

    // check that relative paths work
    for (auto f : flags)
    {
        int res = open("linux/open_client", f, 0666);
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

#if defined(SYS_open)
    for (auto f : flags)
    {
        int res = syscall(SYS_open, "linux/open_client", f, 0666);
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
#endif

#if defined(SYS_openat2)
    for (auto f : flags)
    {
        struct open_how oh = { .flags=f, .mode=((f&static_cast<__u64>(O_CREAT))?0666UL:0UL), .resolve=0 };
        int res = syscall(SYS_openat2, AT_FDCWD, "linux/open_client", &oh, sizeof(oh));
        if (-1 != res)
        {
            throw std::runtime_error("openat2 should have failed");
        }
        else
        {
            if (errno != ETXTBSY)
            {
                throw std::runtime_error("errno should be ETXTBSY");
            }
        }
    }
#endif
}
