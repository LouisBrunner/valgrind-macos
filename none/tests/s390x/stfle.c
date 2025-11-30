#include <stdio.h>
#include <assert.h>

/* Return the number of double words needed to store all facility bits */
static unsigned get_num_facility_dw(void)
{
   unsigned long long dummy[1];

   register unsigned long long r0 asm("0") = 0;
   asm volatile(".insn s,0xb2b00000,%0\n" /* stfle */
                : "=Q" (dummy), "+d"(r0)
                :
                : "cc", "memory");
   return r0 + 1;
}

unsigned long long stfle(unsigned long dw, unsigned bit_to_test)
{
  unsigned long long hoststfle[dw];
  register unsigned long long __nr asm("0") = dw - 1;
  int cc;

  asm volatile(" .insn s,0xb2b00000,%0 \n" /* stfle */
               "ipm %2\n"
               "srl %2,28\n"
               : "=Q" (*hoststfle), "+d" (__nr), "=d" (cc) : : "cc", "memory");

  printf("the value of cc is %d and #double words is %llu\n", cc, __nr + 1);

  for (unsigned i = 0; i < dw; ++i) {
     if (bit_to_test < (i + 1) * 64) {
        bit_to_test -= i * 64;
        return (hoststfle[i] & (1ULL << (63 - bit_to_test)));
     }
  }
  assert(0);
}

int main(void)
{
  int dw = get_num_facility_dw();

  /* Test #1: Make sure STFLE returns sensible values. z/Arch facilities
              must be present. */
  if ((stfle(dw, 1)) && stfle(dw, 2))
    printf("The z/Architecture architectural mode is installed and active\n");
  else
    printf("The z/Architecture architectural mode is not installed\n");

  /* Test #2: Make sure the STFLE is supported. */
  if (stfle(dw, 7))
    printf("STFLE facility is installed\n");
  else
    printf("STFLE facility is not installed\n");

  /* Test #2.1: Test facility 77 which is installed for z196 and later */
  if (stfle(dw, 77))
    printf("Facility 77 is installed\n");
  else
    printf("Facility 77 is not installed\n");

  /* Test #3: Tell STFLE to only write 1 DW of facility bits. Expected condition
              code should be 3 because this test is run on those machines only
              that need 3 do double words to store facility bits. */
  dw = 1;
  if ((stfle(dw, 1)) && stfle(dw, 2))
    printf("The z/Architecture architectural mode is installed and active\n");
  else
    printf("The z/Architecture architectural mode is not installed\n");

  /* Test #4: Constrained transactional-execution */
  if (stfle(dw, 50)) {
     printf("Constrained transactional-execution is supported\n");
  } else {
     printf("No constrained transactional-execution facility available\n");
  }
  return 0;
}
