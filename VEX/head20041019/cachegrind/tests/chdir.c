#include <unistd.h>

// Before the bug was fixed, if a program changed working directory, things
// would break and the cachegrind.out.<pid> file wouldn't get written.
int main(void)
{
   chdir("..");

   return 0;
}
