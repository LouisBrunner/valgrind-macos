// For Bug 5056910
// openat2 with RESOLVE_NO_MAGICLINKS succeeds on /proc/self/exe
#include <fcntl.h>
#include <cerrno>
#include <stdexcept>
#include <string>
#include <unistd.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <linux/openat2.h>
#include <sstream>

int main(int argc, char** argv)
{
    auto pid = getpid();
    auto ppe = std::string("/proc/") + std::to_string(pid) + "/exe";
#if defined(SYS_openat2)
   struct open_how oh = { .flags=O_RDONLY, .mode=0UL, .resolve=RESOLVE_NO_MAGICLINKS };
   int res = syscall(SYS_openat2, AT_FDCWD, "/proc/self/exe", &oh, sizeof(oh));
   if (-1 != res)
   {
      throw std::runtime_error("openat2 should have failed");
   }
   else
   {
      if (errno != ELOOP)
      {
         std::stringstream ss;
         ss << "errno should be ELOOP (value is " << errno << ')';
         throw std::runtime_error(ss.str());
      }
   }

   res = syscall(SYS_openat2, AT_FDCWD, ppe.c_str(), &oh, sizeof(oh));
   if (-1 != res)
   {
       throw std::runtime_error("openat2 should have failed");
   }
   else
   {
       if (errno != ELOOP)
       {
           std::stringstream ss;
           ss << "errno should be ELOOP (value is " << errno << ')';
           throw std::runtime_error(ss.str());
       }
   }
#endif
}
