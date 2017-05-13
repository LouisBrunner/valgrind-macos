
#include <stdio.h>
#include <string.h>

typedef  unsigned long long int  ULong;
typedef  unsigned int  UInt;

#define CC_SHIFT_O   11
#define CC_SHIFT_S   7
#define CC_SHIFT_Z   6
#define CC_SHIFT_A   4
#define CC_SHIFT_C   0
#define CC_SHIFT_P   2

#define CC_MASK_O    (1ULL << CC_SHIFT_O)
#define CC_MASK_S    (1ULL << CC_SHIFT_S)
#define CC_MASK_Z    (1ULL << CC_SHIFT_Z)
#define CC_MASK_A    (1ULL << CC_SHIFT_A)
#define CC_MASK_C    (1ULL << CC_SHIFT_C)
#define CC_MASK_P    (1ULL << CC_SHIFT_P)

#define CC_MASK_OSZACP \
   (CC_MASK_O | CC_MASK_S | CC_MASK_Z | CC_MASK_A | CC_MASK_C | CC_MASK_P)


void showFlags(/*OUT*/char* str, int nStr, ULong flags)
{
   // Ignore everything except OSZACP, because V differs from real h/w in
   // flags other than OSZACP, and we don't want that to confuse the
   // results here
   memset(str, 0, nStr);
   sprintf(str, "%c%c%c%c%c%c",
           (flags & CC_MASK_O) ? 'o' : '-',
           (flags & CC_MASK_S) ? 's' : '-',
           (flags & CC_MASK_Z) ? 'z' : '-',
           (flags & CC_MASK_A) ? 'a' : '-',
           (flags & CC_MASK_C) ? 'c' : '-',
           (flags & CC_MASK_P) ? 'p' : '-');
}

__attribute__((noinline))
void do_test ( ULong val, UInt ix )
{
  ULong o, s, z, a, c, p, flags_before;
  for (o = 0; o < 2; o++) {
  for (s = 0; s < 2; s++) {
  for (z = 0; z < 2; z++) {
  for (a = 0; a < 2; a++) {
  for (c = 0; c < 2; c++) {
  for (p = 0; p < 2; p++) {
    flags_before = (o ? CC_MASK_O : 0)
                 | (s ? CC_MASK_S : 0)
                 | (z ? CC_MASK_Z : 0)
                 | (a ? CC_MASK_A : 0)
                 | (c ? CC_MASK_C : 0)
                 | (p ? CC_MASK_P : 0);
    ULong block[4] = { flags_before, val, ix, 0 };
    __asm__ __volatile__(
      "movq  0(%0),  %%r15"    "\n\t" // flags_before
      "pushq %%r15"            "\n\t" 
      "popfq"                  "\n\t" 
      "movq  8(%0),  %%r14"    "\n\t" // val
      "movq  16(%0), %%r13"    "\n\t" // ix
      "bt    %%r13,  %%r14"    "\n\t" 
      "pushfq"                 "\n\t" 
      "popq %%r15"             "\n\t"
      "movq %%r15,   24(%0)"   "\n" // block[3]
      : : "r"(&block[0]) : "cc","memory","r13","r14","r15"
    );
    ULong flags_after = block[3];
    flags_after &= CC_MASK_OSZACP;
    char flags_after_str[100];
    char flags_before_str[100];
    showFlags(flags_before_str, 100, flags_before);
    showFlags(flags_after_str, 100, flags_after);
    printf("flags 0x%03llx(%s)  val 0x%llx  ix %d  ->  flags 0x%03llx(%s)\n",
           flags_before, flags_before_str, val, ix,
           flags_after, flags_after_str);
  }}}}}}
}

int main ( void )
{
   do_test(0x8000, 14); // should always return C == 0
   printf("\n");
   do_test(0x8000, 15); // should always return C == 1
   return 0;
}
