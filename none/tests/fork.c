
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

int main(void)
{
  pid_t pid;

  pid = fork ();

  printf("my pid is %s\n", pid==0 ? "zero" : "non-zero");

  return 0;
}
