

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

asm(
"nop\n"
"nop\n"
"nop\n"
"nop\n"
"nop\n"
"nop\n"
"nop\n"
"nop\n"
".align 8\n"
".byte 0x11\n"
".byte 0x22\n"
".byte 0x33\n"
".byte 0x44\n"
".byte 0x55\n"
".byte 0x66\n"
".byte 0x77\n"
".byte 0x88\n"
);
void entry ( void*(*service)(int,int) )
{
  bar(service);
  service(0,0);
}

