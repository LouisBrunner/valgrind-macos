#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

int main(int argc, char **argv)
{
    void *buf =
        mmap(NULL, 1 << 20, PROT_WRITE | PROT_READ, MAP_PRIVATE | MAP_NOSYNC | MAP_ALIGNED(20) | MAP_ANON, -1, 0);
    if (buf != MAP_FAILED)
    {
        assert(((uintptr_t)buf & 0xFFFFF) == 0);
        if (((uintptr_t)buf & 0xFFFFF) != 0)
        {
            puts("Failure");
            return EXIT_FAILURE;
        }
    }
    puts("Success");
    return EXIT_SUCCESS;
}
