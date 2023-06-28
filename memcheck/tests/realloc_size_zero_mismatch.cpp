#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cerrno>

using std::realloc;
using std::cout;
using std::perror;

int main(void)
{
   char* p = new char[1024];
   p[0] = '\0';
   errno = 0;
   // mismatch
   p = static_cast<char *>(realloc(p, 0));
   if (p) {
      cout << "p not nullptr after realloc 0\n";
   } else {
      cout << "p is nullptr after realloc 0\n";
   }
   if (errno) {
      perror("realloc(something, 0):");
   }
   // mismatch again
   delete [] p;

   errno = 0;
   volatile void *ptr = NULL;
   volatile size_t size = 0U;
   char *p2 = static_cast<char *>(realloc(const_cast<void*>(ptr), size));
   if (p2) {
      cout << "p2 not nullptr after realloc 0\n";
   } else {
      cout << "p2 is nullptr after realloc 0\n";
   }
   if (errno) {
      perror("realloc(NULL, 0):");
   }
   // mismatch
   delete [] p2;
}
