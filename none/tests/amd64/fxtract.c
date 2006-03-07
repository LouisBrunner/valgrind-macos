
#include <stdio.h>
#include <math.h>

double arg, res1, res2;

extern void do_fxtract ( void );
asm("\n"
".global do_fxtract\n"
"do_fxtract:\n"
"\tfinit\n"
"\tfldl arg\n"
"\tfxtract\n"
"\tfstpl res1\n"
"\tfstpl res2\n"
"\tret"
);

void try ( double x )
{
  arg = x * 1.414213562373049;
  res1 = res2 = 0.0;
  do_fxtract();
  printf("%17.10e  -> %14.10f %14.10f\n", arg, res1, res2);
}

int main ( void )
{
  int i;

  /* positives */

  for (i = 0; i < 40; i++)
     try( 1.27 + (double)(i*10 - 200) );

  try(+0.0);
  try(1.0 / 0.0);
  try(sqrt(-1.0));

  try(5.1e-308);
  try(4.1e-308);
  try(3.1e-308);
  try(2.1e-308);
  try(1.1e-308);
  try(0.9e-308);
  try(0.7e-308);
  try(0.6e-308);
  try(0.5e-308);
  try(0.4e-308);
  try(0.3e-308);
  try(0.1e-308);

  try(1.3e-320);
  try(1.3e-321);
  try(1.3e-322);
  try(1.3e-323);
  try(0.9e-323);
  try(0.7e-323);
  try(0.5e-323);
  try(0.3e-323);
  try(0.2e-323);
  try(1.3e-324);

  /* negatives */

  printf("\n");
  for (i = 0; i < 40; i++)
     try( - (1.27 + (double)(i*10 - 200)) );

  try(-0.0);
  try(-(1.0 / 0.0));
  try(-sqrt(-1.0));

  try(-5.1e-308);
  try(-4.1e-308);
  try(-3.1e-308);
  try(-2.1e-308);
  try(-1.1e-308);
  try(-0.9e-308);
  try(-0.7e-308);
  try(-0.6e-308);
  try(-0.5e-308);
  try(-0.4e-308);
  try(-0.3e-308);
  try(-0.1e-308);

  try(-1.3e-320);
  try(-1.3e-321);
  try(-1.3e-322);
  try(-1.3e-323);
  try(-0.9e-323);
  try(-0.7e-323);
  try(-0.5e-323);
  try(-0.3e-323);
  try(-0.2e-323);
  try(-1.3e-324);

  return 0;
}
