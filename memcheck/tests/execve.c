#include <unistd.h>

int main(void)
{
   char* bad[2]  = { (char*)1, NULL };
   char* good[1] = {           NULL };

   execve(NULL, bad, bad);
   execve("/bin/true", good, good);

   return 0;
}
