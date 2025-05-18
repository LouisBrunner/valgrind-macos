#include <unistd.h>

int main (int argc, char **argv)
{
   close (0);
   close (1);
   close (1);
   close (0);
   return 0;
}
