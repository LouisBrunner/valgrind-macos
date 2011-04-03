#include <stdlib.h>

static void test()
  {
    void* leak __attribute__((unused));
    int i;
    for (i = 0; i < 1000; i++)
       leak = (void*)malloc( 1 );
  }
  int main()
  {
    test();
    return 0;
  }
