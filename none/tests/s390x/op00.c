/* Test for invalid instruction 00. */
int main(int argc, char *argv[])
{
  asm volatile (".insn e,0x0000");
  return 0;
}
