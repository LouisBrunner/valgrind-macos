// roughly based on the code for Firefox class BinaryPath
// https://searchfox.org/mozilla-central/source/xpcom/build/BinaryPath.h#185

#include <iostream>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <limits.h>
#include <string>

using std::cerr;
using std::cout;
using std::string;

int main(int argc, char **argv)
{
   char aResult[PATH_MAX];
   int mib[4];
   mib[0] = CTL_KERN;
   mib[1] = KERN_PROC;
   mib[2] = KERN_PROC_PATHNAME;
   mib[3] = -1;

   size_t len = PATH_MAX;
   if (sysctl(mib, 4, aResult, &len, nullptr, 0) < 0) {
      cerr << "sysctl failed\n";
      return -1;
   }
   if (string(aResult) == argv[1]) {
      cout << "OK\n";
   } else {
      cerr << "Not OK aResult " << aResult << " argv[1] " << argv[1] << '\n';
   }
}
