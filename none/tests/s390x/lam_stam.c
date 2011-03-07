/* LAM, STAM, load,store access multiple */
#include <stdio.h>
#include <unistd.h>

char output[44];
char input[44] = "0123456789\n"
                 "0123456789\n"
                 "0123456789\n"
                 "0123456789\n";

int main()
{
  asm volatile( "larl  1,input\n\t"
                "larl  2,output\n\t"
                "lam   3,13,0(1)\n\t"
                "stam  3,13,0(2)\n\t":::"1", "2");

  write(1, output, sizeof output);
  return 0;
}
