#include <sys/ioctl.h>

int main()
{
#ifdef TIOCSIG
   ioctl(9, TIOCSIG, 9);
#endif
   return 0;
}
