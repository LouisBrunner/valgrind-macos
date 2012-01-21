#include <stdio.h>
#include <inttypes.h>
#include "../../drd/drd.h"

volatile float   f;
volatile double  d;
volatile int8_t  i8;
volatile int16_t i16;
volatile int32_t i32;
volatile int64_t i64;

int main(int argc, char** argv)
{
  DRD_TRACE_VAR(f);
  DRD_TRACE_VAR(d);
  DRD_TRACE_VAR(i8);
  DRD_TRACE_VAR(i16);
  DRD_TRACE_VAR(i32);
  DRD_TRACE_VAR(i64);

  fprintf(stderr, "float\n");
  f = 1;
  f += 2;
  fprintf(stderr, "double\n");
  d = 3;
  d += 4;
  fprintf(stderr, "uint8_t\n");
  i8 = 5;
  i8 += 6;
  fprintf(stderr, "uint16_t\n");
  i16 = 7;
  i16++;
  fprintf(stderr, "uint32_t\n");
  i32 = 8;
  __sync_add_and_fetch(&i32, 1);
  fprintf(stderr, "uint64_t\n");
  i64 = 9;
  __sync_add_and_fetch(&i64, 0x12345678ULL);

  DRD_STOP_TRACING_VAR(f);
  DRD_STOP_TRACING_VAR(d);
  DRD_STOP_TRACING_VAR(i8);
  DRD_STOP_TRACING_VAR(i16);
  DRD_STOP_TRACING_VAR(i32);
  DRD_STOP_TRACING_VAR(i64);

  fprintf(stderr, "Done.\n");
  return 0;
}
