#include <stdlib.h>

static void func1(char *pointer)
{
  __attribute__((unused)) int dummy = 0;
  return;
}              /* Line 7 x86-64*/

int main(int argc, char *argv[])
{
  func1((char *)malloc(64)); /* Line 11 */

  return 0;
} /* Line 14 x86 due to timing of stack invalidation */
