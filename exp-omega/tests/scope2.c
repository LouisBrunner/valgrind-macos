#include <stdlib.h>

static void func1(void)
{
  char *pointer = 0;

  pointer = malloc(64); /* Line 7 */

  return;
}                      /* Leak report Line 10 */

int main(int argc, char *argv[])
{
  func1();

  return 0;
}
