#include <cstdlib>
#include <new>
#include <iostream>
#include <cassert>
#include <cstdio>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "valgrind.h"

int main() {
    std::align_val_t align(static_cast<std::align_val_t>(64U));
    size_t size(32);
    size_t badsize(33);
    void *mem = nullptr;

    mem = operator new(size);
    operator delete(mem, badsize);
    mem = nullptr;

    mem = operator new[](size);
    operator delete[](mem, badsize);

    mem = operator new(size, align);
    operator delete(mem, badsize, align);
    mem = nullptr;

    mem = operator new[](size, align);
    operator delete[](mem, badsize, align);
}
