#include <stdlib.h>

int main(int argc, char **argv)
{
  asm ("int $129");
  
  exit(0);
}
