#include <cstdlib>
#include <new>
#include <iostream>
#include "../memcheck.h"

int main() {
    std::align_val_t align(static_cast<std::align_val_t>(64U));
    std::align_val_t uninitalign(static_cast<std::align_val_t>(64U));
    size_t uninitsize(16);
    size_t size(16);
    std::nothrow_t tag;
    void *mem;
    VALGRIND_MAKE_MEM_UNDEFINED(&uninitsize, sizeof(uninitsize));
    VALGRIND_MAKE_MEM_UNDEFINED(&uninitalign, sizeof(uninitalign));
    
    mem = operator new(uninitsize, uninitalign);
    operator delete(mem, uninitalign);
    
    mem = operator new[](uninitsize, uninitalign);
    operator delete[](mem, uninitalign);
    
    // doesn't matter that tag is uninit
    // don't want to see an error
    mem = operator new(uninitsize, uninitalign, tag);
    operator delete(mem, uninitalign, tag);

    mem = operator new[](uninitsize, uninitalign, tag);
    operator delete[](mem, uninitalign, tag);
    
    mem = operator new(size);
    operator delete(mem, uninitsize);
    
    mem = operator new[](size);
    operator delete[](mem, uninitsize);
    
    mem = operator new(size, align);
    operator delete(mem, uninitsize, uninitalign);
    
    mem = operator new[](size, align);
    operator delete[](mem, uninitsize, uninitalign);
}

