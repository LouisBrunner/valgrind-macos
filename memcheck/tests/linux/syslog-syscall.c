/** Test program for the syslog() system call.
 *  From the syslog(2) man page:
 *    If you need the libc function syslog()  (which  talks  to  syslogd(8)),
 *    then look at syslog(3).  The system call of this name is about control-
 *    ling the kernel printk()  buffer,  and  the  glibc  version  is  called
 *    klogctl().
 */

#include "../../config.h"
#include <stdio.h>
#if defined(HAVE_SYS_KLOG_H)
#include <sys/klog.h>
#endif

int main(int argc, char** argv)
{
  int number_of_unread_characters;
#if defined HAVE_KLOGCTL
  number_of_unread_characters = klogctl(9, 0, 0);
#endif
  fprintf(stderr, "Done.\n");
  return 0 * number_of_unread_characters;
}
