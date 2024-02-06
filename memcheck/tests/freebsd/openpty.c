/* Build with cc -lutil valgrind-ptsname.c */

#include <sys/types.h>
#include <sys/ioctl.h>

#include <unistd.h>
#include <termios.h>

#include <libutil.h>

/*
 * ==18203== Syscall param ioctl(generic) points to uninitialised byte(s)
 * ==18203==    at 0x49AAE7A: ioctl (in /lib/libc.so.7)
 * ==18203==    by 0x490C116: fdevname_r (in /lib/libc.so.7)
 * ==18203==    by 0x49E1567: ptsname (in /lib/libc.so.7)
 * ==18203==    by 0x486B5E2: openpty (in /lib/libutil.so.9)
 * ==18203==    by 0x2016E6: main (in /tmp/a.out)
 * ==18203==  Address 0x1ffc000a74 is on thread 1's stack
 * ==18203==  in frame #1, created by fdevname_r (???:)
 */

#if 0
/* Relevant bit from lib/libc/gen/fdevname.c */
char *
fdevname_r(int fd, char *buf, int len)
{
   struct fiodgname_arg fgn;

   /*
    * buf here points to a `static` buffer in ptsname.c, these are the only
    * two members of fiodgname_arg.
    */
   fgn.buf = buf;
   fgn.len = len;

   if (_ioctl(fd, FIODGNAME, &fgn) == -1)
      return (NULL);
   return (buf);
}
#endif

int
main()
{
   int master, slave;

   (void)openpty(&master, &slave, NULL, NULL, NULL);

   return (0);
}

