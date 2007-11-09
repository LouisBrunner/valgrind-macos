#include <stdlib.h>

int main(int argc, char *argv[])
{
  char *pointer = 0;

  pointer = malloc(64); /* Line 7 */

  do
  {
    char *pointer2 = 0;
    
    pointer2 = malloc(32); /* Line 13 */

  } while (0);
    

  return 0;
}                      /* Leak report Line 19 */
