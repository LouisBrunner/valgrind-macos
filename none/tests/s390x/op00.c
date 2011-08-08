/* Test for invalid instruction 00. */
int main(int argc, char *argv[])
{
  asm volatile (".hword 0\n");
  return 0;
}

