#include <sys/ioctl.h>

int main()
{
   ioctl(9, TIOCSIG, 9);
   return 0;
}
