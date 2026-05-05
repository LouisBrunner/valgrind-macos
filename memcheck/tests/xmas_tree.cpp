#include <new>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "../memcheck.h"

struct Ae
{
   virtual ~Ae()
   {
   }
};
struct Be
{
   virtual ~Be()
   {
   }
};
struct Ce : public Ae, public Be
{
   virtual ~Ce()
   {
   }
};

void* reachable;
Be *interior;

int suppress_me()
{
   int qqq;
   if (qqq)
       return 2;
    return 1;
}

int main()
{
    std::align_val_t misalign(static_cast<std::align_val_t>(63U));
    std::align_val_t align(static_cast<std::align_val_t>(64U));
    std::align_val_t align2(static_cast<std::align_val_t>(32U));
    std::size_t size(32);
    std::size_t badsize(42);
    std::nothrow_t tag;
    int count{0};

    char *mem = static_cast<char*>(operator new[](size, tag));
    if (mem[31])
        ++count;
    if (mem[32])
        ++count;
    operator delete(mem, misalign, tag);

    mem = static_cast<char*>(operator new(size, align, tag));
    operator delete(mem, align2, tag);

    mem = static_cast<char*>(malloc(20));
    mem = static_cast<char*>(realloc(mem, 0));
    delete mem;

    mem = static_cast<char*>(operator new[](size));
    memcpy(mem+10, mem+5, 10);
    operator delete[](mem, badsize);

    mem = static_cast<char*>(malloc(-1));

    int fd{42};
    int bad;
    fd += bad;
    fd -= bad;
    char* buf{nullptr};
    ++buf;
    write(fd, buf, fd);

    int zzz;
    VALGRIND_CHECK_MEM_IS_DEFINED(&zzz, 4);

    reachable = malloc(10);
    mem = static_cast<char*>(malloc(20));

    char* indirect = static_cast<char*>(malloc(30));
    memcpy(&mem[8], &indirect, sizeof(indirect));
    mem = nullptr;

    count += suppress_me();

    interior = new Ce;  // interior ptr.

    return count;
}
