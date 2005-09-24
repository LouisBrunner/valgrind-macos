// This is a regtest for bug #113230, in which 64-bit platforms mistakenly
// behaved as if pipe() took an array of 64-bit ints, when it really takes
// an array of 32-bit ints.

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
   int *filedes = malloc(2 * sizeof(int));

   pipe(filedes);

   return 0;
}
