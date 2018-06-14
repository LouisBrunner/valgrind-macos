#include <stdio.h>
#include <unistd.h>
#include <linux/unistd.h>

int main(int argc, char **argv)
{
   syscall(__NR_membarrier, 0/*query*/);
   fprintf(stderr, "Done.\n");
   return 0;
}
