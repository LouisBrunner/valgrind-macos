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

   if (argc < 2)
   {
      std::cout << "ERROR: missing argument, expected \"" << argv[0] << " {absolute path of " << argv[0] << "}\"\n";
      return -1;
   }

   if (sysctl(mib, 4, nullptr, &len, nullptr, 0) != 0) {
      cout << "ERROR: sysctl failed to get path length: " << std::strerror(errno) << '\n';
      return -1;
   }

   // not for regtest, path length may vary
   // std::cout << "read length " << len << '\n';

   std::unique_ptr<char[]> aResult(new char[len]);

   if (sysctl(mib, 4, aResult.get(), &len, nullptr, 0) != 0)  {
      cout << "ERROR: sysctl failed to get path: " << strerror(errno) << '\n';
      return -1;
   }

   if (string(aResult.get()) == argv[1]) {
      cout << "OK: got expected pathname\n";
   } else {
      cout << "ERROR: aResult " << aResult.get() << " argv[1] " << argv[1] << '\n';
   }

   if (sysctl(mib, 4, aResult.get(), nullptr, nullptr, 0) != 0)  {
      cout << "OK: sysctl failed with nullptr length: " << strerror(errno) << '\n';
   } else {
      cout << "ERROR: nullptr length sysctl succeeded when it should have failed\n";
   }

   size_t bad_len = len - 3U;
   if (sysctl(mib, 4, aResult.get(), &bad_len, nullptr, 0) != 0)  {
      cout << "OK: sysctl failed to get path with bad_len: " << strerror(errno) << '\n';
   } else {
      cout << "ERROR: bad_len sysctl succeeded when it should have failed\n";
      return -1;
   }

   if (sysctl(mib, 4, nullptr, &len, nullptr, 0) != -1) {
      cout << "OK: sysctl failed with nullptr name: " << strerror(errno) << '\n';
      return -1;
   } else {
      cout << "ERROR: nullptr name sysctl succeeded when it should have failed\n";
   }
}
