#include <new>

int main()
{
   std::nothrow_t tag;
   void *mem = operator new(1024U, static_cast<std::align_val_t>(256U));
   operator delete(mem);
   mem = operator new[](1024U, static_cast<std::align_val_t>(256U));
   operator delete[](mem);
   mem = operator new(1024U, static_cast<std::align_val_t>(256U), tag);
   operator delete(mem);
   mem = operator new[](1024U, static_cast<std::align_val_t>(256U), tag);
   operator delete[](mem);
}
