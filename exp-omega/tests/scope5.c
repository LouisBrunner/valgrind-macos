#include <stdlib.h>

static void func1(void)
{
  char *pointer = 0;

  pointer = malloc(64); /* Line 7 */

  return;
}                      /* Leak report here Line 10 */

int main(int argc, char *argv[])
{
  int i = 0;

  for(i = 0; i < 5; i++)
  {
    func1();
  }

  return 0;
}
