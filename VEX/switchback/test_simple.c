

static void bar ( void*(*service)(int,int) )
{
   __asm__ __volatile__ ("addi         17, 14, 5");
}

void entry ( void*(*service)(int,int) )
{
  bar(service);
  service(0,0);
}
