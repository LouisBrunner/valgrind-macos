/*******************************************************************************
 * Derived from the test case for the "dcbzl" instruction support by
 * Dave Goodell * <goodell@mcs.anl.gov>
 * (see: Bug 135264 - dcbzl instruction missing)
 * and: coregrind/m_machine.c/find_ppc_dcbz_sz()
 ******************************************************************************/

/* ensure we have posix_memalign */
#define _POSIX_C_SOURCE 200112L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static int query_block_size(void)
{
#define MAX_DCBZL_SZB (128) /* largest known effect of dcbzl */
  char *test_block = NULL;
  register char *rb asm ("r14");
  int block_size, test_block_size = 4 * MAX_DCBZL_SZB, err;
  char *p;

  err = posix_memalign ((void **)&test_block, MAX_DCBZL_SZB, test_block_size);
  if (err) {
    fprintf(stderr, "posix_memalign() failed (err = %d [%s])\n", err, strerror(err));
    return err;
  }

  rb = test_block;

  memset(rb, 0xff, test_block_size);
  asm volatile ("dcbzl 0, %[RB]" : : [RB] "r" (rb));
  for (block_size = 0, p = rb; (p - rb) < test_block_size; p++)
    if (!*p)
      block_size++;
  assert(block_size == 16 || block_size == 32 || block_size == 64 || block_size == 128);

  free(test_block);
  return block_size;
}

/* Test dcbzl at addr in buffer given dcbzl_block_size */
static void test_dcbzl_at(char *addr, char *buffer, int block_size)
{
  int i;

  /* Note: Assumption is that the length of buffer is three times the block_size. */
  memset(buffer, 0xff, 3 * block_size);
  asm volatile ("dcbzl %[RA], %[RB]" : : [RA] "r" (0), [RB] "r" (addr));
  for (i = 0; i < block_size; i++) {
    assert(buffer[i] == 0xff);
    assert(buffer[block_size + i] == 0x00);
    assert(buffer[2 * block_size + i] == 0xff);
  }
}

/* Test for insn: dcbzl */  
static int test_dcbzl(void)
{
  int err;
  char *buffer = NULL;
  int buffer_size;
  int block_size;
  
  block_size = query_block_size();
  assert(block_size == 16 || block_size == 32 || block_size == 64 || block_size == 128);
  buffer_size = 3 * block_size;
  err = posix_memalign((void **) &buffer, block_size, buffer_size);
  if (err) {
    fprintf(stderr, "posix_memalign() failed (err = %d [%s])\n", err, strerror(err));
    return err;
  }
  
  /* check at aligned address within the test block */
  test_dcbzl_at(&buffer[block_size], buffer, block_size);
  fprintf(stdout, "Passed dcbzl test at aligned address within the test block.\n");

  /* check at un-aligned (1 modulo block_size) address within the test block */
  test_dcbzl_at(&buffer[block_size+1], buffer, block_size);
  fprintf(stdout, "Passed dcbzl test at un-aligned (1 modulo block_size) address within the test block.\n");

  /* check at un-aligned ((block_size - 1) modulo block_size) address within the test block */
  test_dcbzl_at(&buffer[2 * block_size - 1], buffer, block_size);
  fprintf(stdout, "Passed dcbzl test at un-aligned ((block_size - 1) modulo block_size) address within the test block.\n");
  
  free(buffer);
  return 0;
}

int main(int argc, char **argv)
{
  int status;
  status = test_dcbzl ();
  return status;
}
