static void test()
  {
    void* leak;
    int i;
    for (i = 0; i < 1000; i++)
       leak = (void*)malloc( 100 );
  }
  int main()
  {
    test();
    return 0;
  }
