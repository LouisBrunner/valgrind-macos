#include <port.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <errno.h>
int main(void) {
   int p = port_create();
   port_event_t e;
   uint_t n = 1;
   struct timespec t = {0, 100000000};
   port_getn(p, &e, 1, &n, &t);   /* timeout, ETIME expected */
   printf("%s\n", strerror(errno)); /* triggers strerror_l→dgettext_l→calloc */
}
