#include <new>
#include <cassert>
#include <cstdlib>
#include "valgrind.h"

int main() {
    std::align_val_t misalign(static_cast<std::align_val_t>(63U));
    std::align_val_t zeroalign(static_cast<std::align_val_t>(0U));
    std::align_val_t onealign(static_cast<std::align_val_t>(1U));
    std::align_val_t align(static_cast<std::align_val_t>(64U));
    std::size_t size(32);
    std::nothrow_t tag;
    void *mem = nullptr;
    
    // libc++ will allocate something for size zero
    // but libstdc++ doesn't
    mem = operator new(size, zeroalign, tag);
    if (RUNNING_ON_VALGRIND) {
       assert(!mem);
    }
    operator delete(mem, zeroalign, tag);
    mem = nullptr;
    
    mem = operator new(size, onealign, tag);
    assert(mem);
    operator delete(mem, onealign, tag);
    mem = nullptr;
    
    mem = operator new(size, align);
    operator delete(mem, misalign);
    mem = nullptr;
    
    mem = operator new[](size, align);
    operator delete[](mem, misalign);
    mem = nullptr;
    
    // doesn't matter that tag is uninit
    // don't want to see an error
    mem = operator new(size, misalign, tag);
    operator delete(mem, misalign, tag);
    assert(!mem);

    mem = operator new[](size, misalign, tag);
    operator delete[](mem, misalign, tag);
    assert(!mem);
    
    mem = operator new(size, align);
    operator delete(mem, size, misalign);
    mem = nullptr;
    
    mem = operator new[](size, align);
    operator delete[](mem, size, misalign);
    
    // initially this test had two throwing
    // versions called from fork()s
    // but that doesn't mix well with xml
    // so they have split out int versions 2 and 3
}
