
/* A program which sets a readable fd to have a timeout, and therefore
   needs --weird-hacks=ioctl-VTIME in order to run without
   blocking. */

#include <stdio.h>
#include <sys/ioctl.h>
#include <termio.h>

int main ( void )
{
   int c;
   int res;
         struct termio tty, oldtty;

          /**
           ** Save the old tty settings, and get rid of echo
           ** for the new tty settings
           **/
          ioctl(0, TCGETA, &oldtty);
          tty = oldtty;
          tty.c_lflag    &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
          tty.c_cc[VMIN]  = 0;
          tty.c_cc[VTIME] = 5;
          res = ioctl(0, TCSETA, &tty);
	  printf("first ioctl returned %d\n", res);

          /**
           ** Now do whatever stuff you want non-echoed
           **/
	  while (1) {
	    c = getchar();
	    printf("got %d\n", c);
	  }

          /**
           ** Now reset the old settings
           **/
          res = ioctl(0, TCSETA, &oldtty);
	  printf("second ioctl returned %d\n", res);

return 0;
}
