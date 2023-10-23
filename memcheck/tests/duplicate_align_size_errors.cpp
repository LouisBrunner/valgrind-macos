#include <new>
#include <cassert>
#include <cstdlib>
#include "tests/malloc.h"
#include "../../config.h"

int main()
{
    std::align_val_t misalign(static_cast<std::align_val_t>(63U));
    std::align_val_t zeroalign(static_cast<std::align_val_t>(0U));
    std::align_val_t onealign(static_cast<std::align_val_t>(1U));
    std::align_val_t align(static_cast<std::align_val_t>(64U));
    std::align_val_t alignx2(static_cast<std::align_val_t>(128U));
    std::size_t size(32);
    std::nothrow_t tag;
    void *mem = nullptr;

    for (int i = 0 ; i < 2 ; ++i)
    {
       // Err_BadAlign alignment only
       mem = operator new(size, zeroalign, tag);
       operator delete(mem, zeroalign, tag);
       mem = nullptr;

#if defined(HAVE_ALIGNED_ALLOC)
       // Err_BadAlign size and alignment
       mem  = aligned_alloc(64U, 100U);
       if (mem)
       {
          free(mem);
          mem = nullptr;
       }

       // Err.BadSize
       mem  = aligned_alloc(64U, 0U);
       if (mem)
       {
          free(mem);
          mem = nullptr;
       }
#endif

       // Err_SizeMismatch
       mem = operator new(size, align);
       operator delete(mem, size+1, align);
       mem = nullptr;

        // Err_AlignMismatch
       mem = operator new[](size, align);
       operator delete[](mem, size, alignx2);
       mem = nullptr;
    }
}
