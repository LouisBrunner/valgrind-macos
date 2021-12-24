#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

int main()
{
    const char* filename = strdup("revoke.tst");
    (void)open(filename, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);

    // add a gratuitous syscall
    (void)getdtablesize();
 
    // OK 
    revoke(filename);

    // with an invalid param
    free((void*)filename);
    revoke(filename);
}
