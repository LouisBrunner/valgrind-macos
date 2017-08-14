

static void bar ( void*(*service)(int,int) )
{
  int i;
  for (i = 0; i < 100000; i++) ;
  service(1, 'h');
  service(1, 'e');
  service(1, 'l');
  service(1, 'l');
  service(1, 'o');
  service(1, '\n');
}

void entry ( void*(*service)(int,int) )
{
  bar(service);
  service(0,0);
}

