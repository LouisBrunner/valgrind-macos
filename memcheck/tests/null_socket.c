#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>


// It's legit to have NULL buffers;  before the bug was fixed Valgrind
// reported spurious errors for the buffers.
int main(void)
{
    if (-1 != accept(99, NULL, 0))
       printf("accept succeeded?\n");
    
    if (-1 != recvfrom(0, NULL, 0, 0, NULL, 0))
       printf("recvfrom succeeded?\n");
    
    if (-1 != getsockopt(0, 0, 0, NULL, 0))
       printf("getsockopt succeeded?\n");

    return 0;
}
