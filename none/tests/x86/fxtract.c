#include "tests/asm.h"
#include <stdio.h>
#include <math.h>
#include <stdbool.h>

double arg, res1, res2;

extern void do_fxtract ( void );
asm("\n"
".text\n"
VG_SYM(do_fxtract) ":\n"
"\tfinit\n"
"\tfldl " VG_SYM(arg) "\n"
"\tfxtract\n"
"\tfstpl " VG_SYM(res1) "\n"
"\tfstpl " VG_SYM(res2) "\n"
"\tret\n"
".previous\n"
);

static inline void print_double (double d, bool first)
{
   int width = first ? 17 : 14;
   if (isnan(d)) {
      if (signbit(d)) {
         printf("%*s", width, "-nan");
      } else {
         printf("%*s", width, "nan");
      }
   } else if (isinf(d)) {
      if (signbit(d)) {
         printf("%*s", width, "-inf");
      } else {
         printf("%*s", width, "inf");
      }
   } else {
      if (first) {
         printf ("%*.10e", width, d);
      } else {
         printf ("%*.10f", width, d);
      }
   }
}

void try ( double x )
{
  arg = x * 1.414213562373049;
  res1 = res2 = 0.0;
  do_fxtract();
  print_double(arg, true);
  printf("  -> ");
  print_double(res1, false);
  printf(" ");
  print_double(res2, false);
  printf("\n");
}

int main ( void )
{
  int i;

  /* positives */

  for (i = 0; i < 40; i++)
     try( 1.27 + (double)(i*10 - 200) );

  try(+0.0);
  try(__builtin_inf());
  try(__builtin_nan(""));

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
  try(-__builtin_inf());
  try(-__builtin_nan(""));

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
