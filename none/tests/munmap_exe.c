#include <unistd.h>
#include "tests/sys_mman.h"
#include <stdio.h>
#include <stdlib.h>

/* Point of this is that the fd of an PROT_EXEC segment is -1, so Valgrind
   shouldn't add it to its list of exe segs, and thus it won't be discarded
   upon the munmap() (so no "discard" message). */

int main()
{
    void* m;
    
    m = mmap(NULL, 100, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);

    if (m == (void*)-1) {
       fprintf(stderr, "error mmapping\n");
       exit(1);
    }
    
    munmap(m, 100);

    return 0;
}
