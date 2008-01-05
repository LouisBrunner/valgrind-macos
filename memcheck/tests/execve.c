#include <unistd.h>

int main(void)
{
   char* null_filename = NULL;
   char* bad[2]  = { (char*)1, NULL };
   char* good[1] = {           NULL };

   execve(null_filename, bad, bad);
   execve("/bin/true", good, good);

   return 0;
}
