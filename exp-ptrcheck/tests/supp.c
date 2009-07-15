#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>

int main(void)
{
   int   i = 11; int fd = open("/dev/null", O_WRONLY);
   char* buf = malloc(sizeof(char) * 6);
   char  c = buf[-1];                  // LoadStoreErr
   char* x = buf + (long)buf;          // ArithErr
   char* y = (char*)((long)buf * i);   // AsmErr
   write(fd, buf+3, 5);                // SysParamErr
   close(fd);
   return x-y+c;
}
