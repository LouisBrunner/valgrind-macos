// Simple smoke test to see that the demangler is actually working
#include <cstdlib>

namespace abc {
template <typename T1, typename T2> 
class def {
  public:
    T1 xyzzy(T1 *p, T2 *)
    {
      free(p);
      return 10;
    }
  };
};

template <typename T> 
class magic {
public:
  T xyzzy(T *p)
  {
    return (new abc::def<int,typeof(*this)>)->xyzzy(p, 0);
  }
};

int main()
{
   magic<int> *c = new magic<int>;

   c->xyzzy(new int);
   return 0;
}
