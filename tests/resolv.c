
#include <resolv.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
   printf("PRE _res.nscount = %d\n", _res.nscount);
   fflush(stdout);
   res_init();
   printf("POST _res.nscount = %d\n", _res.nscount);
   fflush(stdout);
   return 0;
}
