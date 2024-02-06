#include <iostream>
//#include <fstream>
#include <string>
#include <limits.h>
#include <unistd.h>
#include <fcntl.h>
#include <cassert>

int main()
{
   char resolvedPath[PATH_MAX];
   auto count = readlink("/proc/curproc/file", resolvedPath, PATH_MAX);
   resolvedPath[count] = '\0';
   //std::cout << "resolvedPath: " << resolvedPath << '\n';
   char resolvedPath2[PATH_MAX];
   auto count2 = readlinkat(AT_FDCWD, "/proc/curproc/file", resolvedPath2, PATH_MAX);
   resolvedPath2[count2] = '\0';
   //std::cout << "resolvedPath2: " << resolvedPath2 << '\n';
   std::string rp(resolvedPath);
   assert(rp == resolvedPath2);
   
   auto n = rp.rfind("proc_pid_file");
   
   std::string filename(rp.substr(n));
   //std::cout << "filename: " << filename << '\n';
   assert(filename == "proc_pid_file");
}
