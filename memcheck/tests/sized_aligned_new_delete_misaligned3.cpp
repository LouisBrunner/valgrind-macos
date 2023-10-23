
#include <new>
#include <cassert>

int main() {
    std::align_val_t misalign(static_cast<std::align_val_t>(63U));
    std::size_t size(32);
    void *mem = operator new[](size, misalign);
    // should throw
    assert(false);
}
