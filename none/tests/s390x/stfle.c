#include <stdio.h>

/* Number of double words needed to store all facility bits. */
#define S390_NUM_FACILITY_DW 2


unsigned long long stfle(unsigned long dw, unsigned bit_to_test)
{
  unsigned long long hoststfle[S390_NUM_FACILITY_DW], match;
  register unsigned long long __nr asm("0") = dw - 1;
  int cc;

  asm volatile(" .insn s,0xb2b00000,%0 \n" /* stfle */
               "ipm %2\n"
               "srl %2,28\n"
               : "=m" (*hoststfle), "+d" (__nr), "=d" (cc) : : "cc", "memory");

  printf("the value of cc is %d and #double words is %llu\n", cc, __nr + 1);
  if (bit_to_test < 64)
    match = (hoststfle[0] & (1ULL << (63 - bit_to_test)));
  else if (bit_to_test < 128)
    match = (hoststfle[1] & (1ULL << (63 - bit_to_test)));
  else
    printf("code needs to be updated\n");

  return match;
}

int main()
{
  int dw = S390_NUM_FACILITY_DW;

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

  /* Test #3: Tell STFLE to only write 1 DW of facility bits. Expected condition
              code should be 3 because this test is run on those machines only
              that need 2 do double words to store facility bits. */
  dw = 1;
  if ((stfle(dw, 1)) && stfle(dw, 2))
    printf("The z/Architecture architectural mode is installed and active\n");
  else
    printf("The z/Architecture architectural mode is not installed\n");

  return 0;
}
