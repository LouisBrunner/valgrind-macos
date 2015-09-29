/* Tests for ioctl wrappers.
   More complicated ones than just trivial ones in scalar_ioctl. */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>
#include <net/if.h>
#include <sys/socket.h>
#include <sys/sockio.h>

/* sockio */
__attribute__((noinline))
static int test_SIOCGIFCONF(void)
{
   int fd = socket(AF_INET, SOCK_DGRAM, 0);
   if (fd < 0)
      perror("socket");

   int n_ifs;
   if (ioctl(fd, SIOCGIFNUM, &n_ifs) < 0)
      perror("ioctl(SIOCGIFNUM)");

   struct ifconf ifc;
   ifc.ifc_len = (n_ifs + 1) * sizeof(struct ifreq);
   ifc.ifc_buf = malloc((n_ifs + 1) * sizeof(struct ifreq));
   if (ifc.ifc_buf == NULL)
      perror("malloc");

   if (ioctl(fd, SIOCGIFCONF, &ifc) < 0)
      perror("ioctl(SIOCGIFCONF)");

   /* Check definedness of ifc attributes ... */
   int x = 0;
   if (ifc.ifc_len != 0) x = -1; else x = -2;
   if (ifc.ifc_req != NULL) x = -3; else x = -4;
   if (strcmp(ifc.ifc_req[0].ifr_name, "") != 0) x = -5; else x = -6;
   /* ... and now one which is not defined. */
   if (strcmp(ifc.ifc_req[n_ifs].ifr_name, "") != 0) x = -7; else x = -8;

   free(ifc.ifc_buf);
   close(fd);
   return x;
}

__attribute__((noinline))
static int test_SIOCGLIFCONF(void)
{
   int fd = socket(AF_INET, SOCK_DGRAM, 0);
   if (fd < 0)
      perror("socket");

   struct lifnum lifn;
   lifn.lifn_family = AF_INET;
   lifn.lifn_flags = 0;
   if (ioctl(fd, SIOCGLIFNUM, &lifn) < 0)
      perror("ioctl(SIOCGLIFNUM)");

   struct lifconf lifc;
   lifc.lifc_family = AF_INET;
   lifc.lifc_flags = 0;
   lifc.lifc_len = (lifn.lifn_count + 1) * sizeof(struct lifreq);
   lifc.lifc_buf = malloc((lifn.lifn_count + 1) * sizeof(struct lifreq));
   if (lifc.lifc_buf == NULL)
      perror("malloc");

   if (ioctl(fd, SIOCGLIFCONF, &lifc) < 0)
      perror("ioctl(SIOCGLIFCONF)");

   /* Check definedness of lifc attributes ... */
   int x = 0;
   if (lifc.lifc_len != 0) x = -1; else x = -2;
   if (lifc.lifc_req != NULL) x = -3; else x = -4;
   if (strcmp(lifc.lifc_req[0].lifr_name, "") != 0) x = -5; else x = -6;
   /* ... and now one which is not defined. */
   if (strcmp(lifc.lifc_req[lifn.lifn_count].lifr_name, "") != 0)
      x = -7; else x = -8;

   free(lifc.lifc_buf);
   close(fd);
   return x;
}

int main(void)
{
   /* sockio */
   test_SIOCGIFCONF();
   test_SIOCGLIFCONF();

   return 0;
}
