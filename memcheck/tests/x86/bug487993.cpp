// the original issue ocurred with Eigen3
// this is roughly the same using a simple struct
// 
#include <cassert>
#include <memory>
//#include <iostream>

struct Test
{
    alignas(16) double array[16];
};

int main()
{
#if defined(__sun__)
    const auto alignment_mask{0x7UL};
#else
    const auto alignment_mask{0xfUL};
#endif
    std::unique_ptr<Test> test = std::make_unique<Test>();
    //std::cerr << "test " << test.get() << ' ' << (reinterpret_cast<size_t>(test.get()) & 0xfUL) << '\n';
    assert((reinterpret_cast<size_t>(test.get()) & alignment_mask) == 0);
}

    
