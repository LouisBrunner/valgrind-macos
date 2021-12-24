/* Tests Valgrind moans about unknown ioctl.
   10 unique moans should be produced.
 */

#include <unistd.h>
#include <sys/ioctl.h>

/* An ioctl request for a strange ioctl device driver.
   The choice of values here needs to match the logic in
   ML_(PRE_unknown_ioctl) and take into account that _IOC_NONE
   is not == 0 everywhere. */
# if defined(VGO_linux)
#define IOCTL_REQUEST_BASE (0x12345670 | _IOC(_IOC_NONE,0,0,0))
# elif defined(VGO_freebsd)
#define IOCTL_REQUEST_BASE (0x12345670 | _IO(0,0))
# else
#define IOCTL_REQUEST_BASE  0x12345670
# endif

int main(int argc, const char *argv[])
{
   ioctl(-1, IOCTL_REQUEST_BASE + 0x0);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x1);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x0);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x2);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x3);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x4);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x1);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x5);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x5);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x1);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x6);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x7);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x8);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x1);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x9);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x1);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x0);
   ioctl(-1, IOCTL_REQUEST_BASE + 0xa);
   ioctl(-1, IOCTL_REQUEST_BASE + 0xb);
   ioctl(-1, IOCTL_REQUEST_BASE + 0xc);
   ioctl(-1, IOCTL_REQUEST_BASE + 0xd);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x1);
   ioctl(-1, IOCTL_REQUEST_BASE + 0xe);
   ioctl(-1, IOCTL_REQUEST_BASE + 0x0);
   ioctl(-1, IOCTL_REQUEST_BASE + 0xf);

   return 0;
}
