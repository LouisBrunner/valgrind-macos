#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <sys/fcntl.h>

int main(void)
{
   // good
   char buf[PATH_MAX];
   char* self_path = "../../tests/freebsd/realpath.c";
   realpath(self_path, buf);
   
   // bad
   int * bad_int1 = malloc(sizeof(char));
   int * bad_int2 = malloc(sizeof(char));
   size_t * bad_sz = malloc(sizeof(char));
   *bad_int1 = AT_FDCWD;
   *bad_int2 = 0;
   *bad_sz = PATH_MAX;
   syscall(SYS___realpathat, *bad_int1, self_path, buf, *bad_sz,  *bad_int2);
   free(bad_int1);
   free(bad_int2);
   
   // ugly
   char * bad_buf = malloc(100);
   char* bad_buf2 = strdup(self_path);
   free(bad_buf);
   free(bad_buf2);
   // fingers crossed
   realpath(bad_buf2, bad_buf);
   
   
}
