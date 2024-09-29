#include <unistd.h>
#include <sys/wait.h>

int main()
{
  pid_t pid = fork ();
  if (pid == 0)
    execlp("true", "true", NULL);

  // Wait till true succeeds
  wait (NULL);
  return 0;
}
