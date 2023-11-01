// roughly based on the code for Firefox class BinaryPath
// https://searchfox.org/mozilla-central/source/xpcom/build/BinaryPath.h#185

#include <iostream>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <limits.h>
#include <string>
#include <memory>
#include <cstring>

using std::cerr;
using std::cout;
using std::string;

int main(int argc, char **argv)
{
   int mib[] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1};
   size_t len;

   if (sysctl(mib, 4, NULL, &len, NULL, 0) != 0) {
      cout << "sysctl failed to get path length: " << std::strerror(errno) << '\n';
      return -1;
   }

   std::unique_ptr<char[]> aResult(new char[len]);

   if (sysctl(mib, 4, aResult.get(), &len, NULL, 0) != 0)  {
      cout << "sysctl failed to get path: " << strerror(errno) << '\n';
      return -1;
   }

   if (string(aResult.get()) == argv[1]) {
      cout << "OK\n";
   } else {
      cout << "Not OK aResult " << aResult.get() << " argv[1] " << argv[1] << '\n';
   }

   if (sysctl(mib, 4, NULL, NULL, NULL, 0) != -1) {
      cout << "OK syscall failed\n";
      return -1;
   } else {
      cout << "sysctl succeeded when it should have failed\n";
   }
}

