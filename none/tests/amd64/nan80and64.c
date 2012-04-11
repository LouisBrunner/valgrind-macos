
/* Test conversions between 64- and 80- bit quiet NaNs.  Uses
   "canonical forms" for qNaNs.  It also tests sNaNs but it's not
   clear what the canonical form of them should be, so the results are
   pretty much irrelevant.  Failure to do this right is the cause
   of https://bugzilla.mozilla.org/show_bug.cgi?id=738117
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef  unsigned char  UChar;


void do_64_to_80 ( UChar* dst, UChar* src )
{
   __asm__ __volatile__(
      "fldl (%0); fstpt (%1)"
      : : "r"(src), "r"(dst) : "memory"
   );
}

void do_80_to_64 ( UChar* dst, UChar* src )
{
   __asm__ __volatile__(
      "fldt (%0); fstpl (%1)"
      : : "r"(src), "r"(dst) : "memory"
   );
}

void print80 ( char* s, UChar* v )
{
   int i;
   printf("%s", s);
   for (i = 9; i >= 0; i--)
      printf("%02x", (unsigned int)v[i]);
   printf("\n");
}

void print64 ( char* s, UChar* v )
{
   int i;
   printf("%s", s);
   for (i = 7; i >= 0; i--) {
      printf("%02x", (unsigned int)v[i]);
   }
   printf("\n");
}

#if 0
void gen_qnan_64 ( UChar* dst )
{

}
#endif

#define SWAPC(_xx,_yy) { UChar tmp = _xx; _xx = _yy; _yy = tmp; }

static void rev64 ( UChar* f64 )
{
   SWAPC( f64[0], f64[7] );
   SWAPC( f64[1], f64[6] );
   SWAPC( f64[2], f64[5] );
   SWAPC( f64[3], f64[4] );
}

static void rev80 ( UChar* f80 )
{
   SWAPC( f80[0], f80[9] );
   SWAPC( f80[1], f80[8] );
   SWAPC( f80[2], f80[7] );
   SWAPC( f80[3], f80[6] );
   SWAPC( f80[4], f80[5] );
}

#undef SWAPC

int main ( void )
{
  UChar ref_qnan64[8]
        = { 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

  UChar ref_snan64[8] 
        = { 0x7f, 0xf4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

  UChar ref_qnan80[10] 
        = { 0x7f, 0xff, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

  UChar ref_snan80[10]
        = { 0x7f, 0xff, 0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

  rev64( ref_qnan64 );
  rev64( ref_snan64 );
  rev80( ref_qnan80 );
  rev80( ref_snan80 );

  UChar* res = malloc(10);
#define ZAP memset(res, 0x55, 10)


  int pass;
  for (pass = 1; pass <= 2; pass++) {

  ZAP; do_64_to_80( res, ref_qnan64 );
  print64( "src = qnan64: ", ref_qnan64 );
  print80( "dst = qnan80: ", res );
  printf("\n");

  ZAP; do_64_to_80( res, ref_snan64 );
  print64( "src = snan64: ", ref_snan64 );
  print80( "dst = snan80: ", res );
  printf("\n");

  ZAP; do_80_to_64( res, ref_qnan80 );
  print80( "src = qnan80: ", ref_qnan80 );
  print64( "dst = qnan64: ", res );
  printf("\n");

  ZAP; do_80_to_64( res, ref_snan80 );
  print80( "src = snan80: ", ref_snan80 );
  print64( "dst = snan64: ", res );
  printf("\n");

  /* now make all the reference inputs negative and do it again */

  ref_qnan64[7] ^= 0x80;
  ref_snan64[7] ^= 0x80;

  ref_qnan80[9] ^= 0x80;
  ref_snan80[9] ^= 0x80;

  }

#undef ZAP

  free(res);
  return 0;
}
