// operator new(unsigned)
// operator new[](unsigned)
// operator new(unsigned, std::nothrow_t const&)
// operator new[](unsigned, std::nothrow_t const&)

#include <stdlib.h>

#include <new>

using std::nothrow_t;

// A big structure.  Its details don't matter.
typedef struct {
           int array[1000];
        } s;

__attribute__((noinline)) void* operator new (std::size_t n)
{
    return malloc(n);
}

__attribute__((noinline)) void* operator new (std::size_t n, std::nothrow_t const &)
{
    return malloc(n);
}

__attribute__((noinline)) void* operator new[] (std::size_t n)
{
    return malloc(n);
}

__attribute__((noinline)) void* operator new[] (std::size_t n, std::nothrow_t const &)
{
    return malloc(n);
}

__attribute__((noinline)) void* operator new (std::size_t size, std::align_val_t al)
{
    return aligned_alloc(static_cast<std::size_t>(al), size);
}

__attribute__((noinline)) void* operator new[] (std::size_t size, std::align_val_t al)
{
    return aligned_alloc(static_cast<std::size_t>(al), size);
}

__attribute__((noinline)) void* operator new(std::size_t size, std::align_val_t al, const std::nothrow_t&) noexcept
{
    return aligned_alloc(static_cast<std::size_t>(al), size);
}

__attribute__((noinline)) void* operator new[](std::size_t size, std::align_val_t al, const std::nothrow_t&) noexcept
{
    return aligned_alloc(static_cast<std::size_t>(al), size);
}

__attribute__((noinline)) void operator delete (void* p)
{
    free(p);
}

__attribute__((noinline)) void operator delete[] (void* p)
{
    free(p);
}

__attribute__((noinline))  void operator delete (void* ptr, std::align_val_t al ) noexcept
{
    free(ptr);
}

__attribute__((noinline))  void operator delete[] (void* ptr, std::align_val_t al ) noexcept
{
    free(ptr);
}

int main(void)
{
    s*        p1 = new                s;
    s*        p2 = new (std::nothrow) s;
    char*     c1 = new                char[2000];
    char*     c2 = new (std::nothrow) char[2000];
    s*        pa1 = static_cast<s*>(operator new(sizeof(*pa1), static_cast<std::align_val_t>(256U)));
    s*        pa2 = static_cast<s*>(operator new[](sizeof(*pa2)*10U, static_cast<std::align_val_t>(256U)));
    std::nothrow_t tag;
    s*        pa3 = static_cast<s*>(operator new(sizeof(*pa3), static_cast<std::align_val_t>(256U), tag));
    s*        pa4 = static_cast<s*>(operator new[](sizeof(*pa4)*10U, static_cast<std::align_val_t>(256U), tag));
    delete p1;
    delete p2;
    delete [] c1;
    delete [] c2;
    operator delete(pa1, static_cast<std::align_val_t>(256U));
    operator delete(pa2, static_cast<std::align_val_t>(256U));
    operator delete(pa3, static_cast<std::align_val_t>(256U));
    operator delete(pa4, static_cast<std::align_val_t>(256U));
    return 0;
}


