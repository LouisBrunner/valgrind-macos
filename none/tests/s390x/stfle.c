#include <stdio.h>

/* Number of double words needed to store all facility bits. */
#define S390_NUM_FACILITY_DW 4


unsigned long long stfle(unsigned long dw, unsigned bit_to_test)
{
  unsigned long long hoststfle[S390_NUM_FACILITY_DW];
  register unsigned long long __nr asm("0") = dw - 1;
  int cc;

  asm volatile(" .insn s,0xb2b00000,%0 \n" /* stfle */
               "ipm %2\n"
               "srl %2,28\n"
               : "=Q" (*hoststfle), "+d" (__nr), "=d" (cc) : : "cc", "memory");

  printf("the value of cc is %d and #double words is %llu\n", cc, __nr + 1);
  if (bit_to_test < 64)
    return (hoststfle[0] & (1ULL << (63 - bit_to_test)));
  else if (bit_to_test < 128)
    return (hoststfle[1] & (1ULL << (63 - bit_to_test)));
  else if (bit_to_test < 192)
    return (hoststfle[2] & (1ULL << (63 - bit_to_test)));

  printf("code needs to be updated\n");
  return 0;
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
              that need 3 do double words to store facility bits. */
  dw = 1;
  if ((stfle(dw, 1)) && stfle(dw, 2))
    printf("The z/Architecture architectural mode is installed and active\n");
  else
    printf("The z/Architecture architectural mode is not installed\n");

  /* Test #4: Message security assist */
  if (stfle(dw, 17)) {
     printf("MSA facility is present\n");
  } else {
     printf("No MSA facility available\n");
  }
  return 0;
}
