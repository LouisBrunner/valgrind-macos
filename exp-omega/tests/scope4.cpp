#include <stdlib.h>

class Class1
{
public:
  Class1(char *cpointer = NULL) : p(cpointer){};
  ~Class1()
  {
    if(p)
    {
      free(p);
    }
  };

private:
  char *p;
};

Class1 function1(void)
{
  Class1 c((char *)malloc(64));

  return c;
}

void function2(void)
{
  Class1 c = function1();

  return;
}

int main(int argc, char* argv[])
{
  function2();

  return 0;
}
