/* Tests for TIOCSBRK per https://bugs.kde.org/show_bug.cgi?id=208217
 */

#include <sys/ioctl.h>

int main(int argc, const char *argv[])
{
#ifdef TIOCSBRK
   ioctl(1, TIOCSBRK, 0);
   ioctl(1, TIOCCBRK, 0);
#endif

   return 0;
}
