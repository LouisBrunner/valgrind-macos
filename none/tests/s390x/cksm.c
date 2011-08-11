#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "test.h"

uint32_t data[64];

/* The result of a checksum operation */
typedef struct {
  uint64_t addr;
  uint64_t len;
  uint32_t sum;
  char     cc;
} cksm_t;


/* Compute the checksum via the cksm insn */
static __attribute__((noinline)) cksm_t
cksm_by_insn(const uint32_t *buff, uint64_t len, uint32_t sum)
{
  const uint32_t *init_addr = buff;
  uint64_t init_length = len;
  uint64_t addr;
  char cc;
  cksm_t result;
  register uint64_t reg2 asm("2") = (uint64_t) buff;
  register uint64_t reg3 asm("3") = len;

  asm volatile( "       lhi     4,42\n\t"
                "       xr      4,4\n\t"        /* set cc to != 0 */
                "0:	cksm	%0,%1\n\t"	/* do checksum on longs */
		"	jo	0b\n\t"
		: "+d" (sum), "+d" (reg2), "+d" (reg3) : : "cc", "memory");

  cc   = get_cc();
  len  = reg3;
  addr = reg2;

  /* Check the results */
  if(addr != (uint64_t)init_addr + init_length)
    printf("FAIL: address not updated properly\n");

  if(len != 0)
    printf("FAIL: length not zero\n");

  if (cc != 0)
    printf("FAIL: condition code not zero\n");

  result.addr = addr;
  result.len  = len;
  result.cc   = cc;
  result.sum  = sum;

  return result;
}


/* Compute the checksum via hand-crafted algorithm */
static __attribute__((noinline)) cksm_t
cksm_by_hand(const uint32_t *buff, uint64_t len, uint32_t sum)
{
  cksm_t result;
  unsigned int n;
  uint64_t v64;
  uint32_t final;

  for (n=0; n < len/4; n++) {
    /* Add 4 bytes to the sum. Do this in 64-bit arithmetic so it's
       easy to see whether there was a carry-out. */
    v64 = sum;
    v64 += buff[n];
    /* If there was a carry-out, add 1 to the sum. */
    if (v64 >> 32)
      sum = sum + buff[n] + 1;
    else
      sum = sum + buff[n];
  }

  if (len != 0) {
    switch (len % 4) {
    case 0:
      final = 0;  // suppress gcc warning 
      /* done */
      break;

    case 1:
      final = buff[n] & 0xFF000000;
      break;

    case 2:
      final = buff[n] & 0xFFFF0000;
      break;

    case 3:
      final = buff[n] & 0xFFFFFF00;
      break;
    }

    if (len % 4) {
      v64 = sum;
      v64 += final;
      /* If there was a carry-out, add 1 to the sum. */
      if (v64 >> 32)
        sum = sum + final + 1;
      else
        sum = sum + final;
    }    
  }    

  result.addr = (uint64_t)buff + len;
  result.len  = 0;
  result.cc   = 0;
  result.sum  = sum;

  return result;
}

/* The results computed by-insn and by-hand must compare equal and
   the sum must be identical to EXPECTED_SUM. */
int
compare_results(cksm_t by_hand, cksm_t by_insn, uint32_t expected_sum)
{
  int rc = 0;

  if (by_hand.sum != by_insn.sum) {
    ++rc;
    printf("FAIL: sum:   by-hand %"PRIx32"  by-insn %"PRIx32"\n",
           by_hand.sum, by_insn.sum);
  }

  if (by_hand.addr != by_insn.addr) {
    ++rc;
    printf("FAIL: addr:  by-hand %"PRIx64"  by-insn %"PRIx64"\n",
           by_hand.addr, by_insn.addr);
  }

  if (by_hand.len != by_insn.len) {
    ++rc;
    printf("FAIL: len:   by-hand %"PRIx64"  by-insn %"PRIx64"\n",
           by_hand.len, by_insn.len);
  }

  if (by_hand.cc != by_insn.cc) {
    ++rc;
    printf("FAIL: cc:    by-hand %d  by-insn %d\n",
           by_hand.cc, by_insn.cc);
  }

  if (by_insn.sum != expected_sum) {
    ++rc;
    printf("FAIL: sum:   by-insn %"PRIx32"  expected %"PRIx32"\n",
           by_insn.sum, expected_sum);
  }

  if (by_hand.sum != expected_sum) {
    ++rc;
    printf("FAIL: sum:   by-hand %"PRIx32"  expected %"PRIx32"\n",
           by_hand.sum, expected_sum);
  }

  return rc;
}

/* Run a testcase. Compute the checksum by-hand and by-insn and compare
   the results */
void
run_test(const char *name, const uint32_t *buff, uint64_t len, uint32_t sum,
         uint32_t expected_sum)
{
  cksm_t by_hand, by_insn;

  by_hand = cksm_by_hand(buff, len, sum);
  by_insn = cksm_by_insn(buff, len, sum);
  if (compare_results(by_hand, by_insn, expected_sum) != 0) {
    printf("%s failed\n", name);
  }
}

int main ()
{
  uint32_t sum, expected_sum;
  uint64_t len;

  /* ---------------- test 1 ------------------------------ */
  /* Add one word to an initial sum; no carry */
  sum = 2;
  data[0] = 1;
  len = 4;
  expected_sum = 3;
  run_test("test1", data, len, sum, expected_sum);

  /* ---------------- test 2 ------------------------------ */
  /* Add one word to an initial sum; with carry */
  sum = 1;
  data[0] = 0xffffffff;
  len = 4;
  expected_sum = 1;
  run_test("test2", data, len, sum, expected_sum);

  /* ---------------- test 3 ------------------------------ */
  /* Add 15 words to an initial sum; no carry */
  sum      = 0x1;
  data[0]  = 0x4;
  data[1]  = 0x10;
  data[2]  = 0x40;
  data[3]  = 0x100;
  data[4]  = 0x400;
  data[5]  = 0x1000;
  data[6]  = 0x4000;
  data[7]  = 0x10000;
  data[8]  = 0x40000;
  data[9]  = 0x100000;
  data[10] = 0x400000;
  data[11] = 0x1000000;
  data[12] = 0x4000000;
  data[13] = 0x10000000;
  data[14] = 0x40000000;
  len = 60;
  expected_sum = 0x55555555;
  run_test("test3", data, len, sum, expected_sum);

  /* ---------------- test 4 ------------------------------ */
  /* Add some words such that every addition generates a carry.
     The data is such that the least significant byte is zero,
     and the carrys from intermediate additions will accumulate
     in the least significant byte. */
  sum      = 0xff000000;
  data[0]  = 0x80000000;   /* 7f0000001 */
  data[1]  = 0x85000000;   /* 040000002 */
  data[2]  = 0xff000000;   /* 030000003 */
  data[3]  = 0xff000000;   /* 020000004 */
  data[4]  = 0xff000000;   /* 010000005 */
  data[5]  = 0xff000000;   /* 000000006 */
  len = 24;
  expected_sum = 0x00000006;
  run_test("test4", data, len, sum, expected_sum);

  /* ---------------- test 5 ------------------------------ */
  /* No words are added. Pass a NULL pointer so an attempt to
     load would raise a SIGSEGV. */
  len = 0;
  sum = 42;
  expected_sum = sum;
  run_test("test5", NULL, len, sum, expected_sum);

  /* ---------------- test 6 ------------------------------ */
  /* Add 1 byte; no carry */
  sum = 0x02000000;
  len = 1;
  data[0] = 0x7fffffff;
  expected_sum = 0x81000000;
  run_test("test6", data, len, sum, expected_sum);

  /* ---------------- test 7 ------------------------------ */
  /* Add 1 byte; carry */
  sum = 0x02000000;
  len = 1;
  data[0] = 0xffffffff;
  expected_sum = 0x01000001;
  run_test("test7", data, len, sum, expected_sum);

  /* ---------------- test 8 ------------------------------ */
  /* Add 2 bytes; no carry */
  sum = 0x00020000;
  len = 2;
  data[0] = 0x7fffffff;
  expected_sum = 0x80010000;
  run_test("test8", data, len, sum, expected_sum);

  /* ---------------- test 9 ------------------------------ */
  /* Add 2 bytes; carry */
  sum = 0x00020000;
  len = 2;
  data[0] = 0xffffffff;
  expected_sum = 0x00010001;
  run_test("test9", data, len, sum, expected_sum);

  /* ---------------- test 10 ------------------------------ */
  /* Add 3 bytes; no carry */
  sum = 0x00000200;
  len = 3;
  data[0] = 0x7fffffff;
  expected_sum = 0x80000100;
  run_test("test10", data, len, sum, expected_sum);

  /* ---------------- test 11 ------------------------------ */
  /* Add 3 bytes; carry */
  sum = 0x00000200;
  len = 3;
  data[0] = 0xffffffff;
  expected_sum = 0x00000101;
  run_test("test11", data, len, sum, expected_sum);

  return 0;
}
