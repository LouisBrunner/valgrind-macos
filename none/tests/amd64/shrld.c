
#include <stdio.h>

typedef unsigned long long int ULong;

ULong data;
ULong xtra;
ULong amt;
ULong flags_in;
ULong result;
ULong flags_out;

#define AMD64G_CC_SHIFT_O   11
#define AMD64G_CC_SHIFT_S   7
#define AMD64G_CC_SHIFT_Z   6
#define AMD64G_CC_SHIFT_A   4
#define AMD64G_CC_SHIFT_C   0
#define AMD64G_CC_SHIFT_P   2

#define AMD64G_CC_MASK_O    (1 << AMD64G_CC_SHIFT_O)
#define AMD64G_CC_MASK_S    (1 << AMD64G_CC_SHIFT_S)
#define AMD64G_CC_MASK_Z    (1 << AMD64G_CC_SHIFT_Z)
#define AMD64G_CC_MASK_A    (1 << AMD64G_CC_SHIFT_A)
#define AMD64G_CC_MASK_C    (1 << AMD64G_CC_SHIFT_C)
#define AMD64G_CC_MASK_P    (1 << AMD64G_CC_SHIFT_P)

#define MASK_OSZACP  \
   (AMD64G_CC_MASK_O | AMD64G_CC_MASK_S | AMD64G_CC_MASK_Z \
    | AMD64G_CC_MASK_A | AMD64G_CC_MASK_C | AMD64G_CC_MASK_P)

extern void shld64 ( void );
asm("\n"
".text\n"
"shld64:\n"
"\tpushq %rsi\n"
"\tpushq %r11\n"
"\tpushq %rcx\n"
"\tmovq data, %rsi\n"
"\tmovq xtra, %r11\n"
"\tmovq amt, %rcx\n"
"\tpushq flags_in\n"
"\tpopfq\n"
"\tshldq %cl, %r11, %rsi\n"
"\tmovq %rsi, result\n"
"\tpushfq\n"
"\tpopq flags_out\n"
"\tpopq %rcx\n"
"\tpopq %r11\n"
"\tpopq %rsi\n"
"\tret\n"
".previous\n"
);

extern void shld32 ( void );
asm("\n"
".text\n"
"shld32:\n"
"\tpushq %rsi\n"
"\tpushq %r11\n"
"\tpushq %rcx\n"
"\tmovq data, %rsi\n"
"\tmovq xtra, %r11\n"
"\tmovq amt, %rcx\n"
"\tpushq flags_in\n"
"\tpopfq\n"
"\tshldl %cl, %r11d, %esi\n"
"\tmovq %rsi, result\n"
"\tpushfq\n"
"\tpopq flags_out\n"
"\tpopq %rcx\n"
"\tpopq %r11\n"
"\tpopq %rsi\n"
"\tret\n"
".previous\n"
);

extern void shld16 ( void );
asm("\n"
".text\n"
"shld16:\n"
"\tpushq %rsi\n"
"\tpushq %r11\n"
"\tpushq %rcx\n"
"\tmovq data, %rsi\n"
"\tmovq xtra, %r11\n"
"\tmovq amt, %rcx\n"
"\tpushq flags_in\n"
"\tpopfq\n"
"\tshldw %cl, %r11w, %si\n"
"\tmovq %rsi, result\n"
"\tpushfq\n"
"\tpopq flags_out\n"
"\tpopq %rcx\n"
"\tpopq %r11\n"
"\tpopq %rsi\n"
"\tret\n"
".previous\n"
);


extern void shrd64 ( void );
asm("\n"
".text\n"
"shrd64:\n"
"\tpushq %rsi\n"
"\tpushq %r11\n"
"\tpushq %rcx\n"
"\tmovq data, %rsi\n"
"\tmovq xtra, %r11\n"
"\tmovq amt, %rcx\n"
"\tpushq flags_in\n"
"\tpopfq\n"
"\tshrdq %cl, %r11, %rsi\n"
"\tmovq %rsi, result\n"
"\tpushfq\n"
"\tpopq flags_out\n"
"\tpopq %rcx\n"
"\tpopq %r11\n"
"\tpopq %rsi\n"
"\tret\n"
".previous\n"
);

extern void shrd32 ( void );
asm("\n"
".text\n"
"shrd32:\n"
"\tpushq %rsi\n"
"\tpushq %r11\n"
"\tpushq %rcx\n"
"\tmovq data, %rsi\n"
"\tmovq xtra, %r11\n"
"\tmovq amt, %rcx\n"
"\tpushq flags_in\n"
"\tpopfq\n"
"\tshrdl %cl, %r11d, %esi\n"
"\tmovq %rsi, result\n"
"\tpushfq\n"
"\tpopq flags_out\n"
"\tpopq %rcx\n"
"\tpopq %r11\n"
"\tpopq %rsi\n"
"\tret\n"
".previous\n"
);

extern void shrd16 ( void );
asm("\n"
".text\n"
"shrd16:\n"
"\tpushq %rsi\n"
"\tpushq %r11\n"
"\tpushq %rcx\n"
"\tmovq data, %rsi\n"
"\tmovq xtra, %r11\n"
"\tmovq amt, %rcx\n"
"\tpushq flags_in\n"
"\tpopfq\n"
"\tshrdw %cl, %r11w, %si\n"
"\tmovq %rsi, result\n"
"\tpushfq\n"
"\tpopq flags_out\n"
"\tpopq %rcx\n"
"\tpopq %r11\n"
"\tpopq %rsi\n"
"\tret\n"
".previous\n"
);


int main ( void )
{
  int i;
  ULong mask;

  printf("\nleft 64\n");
  for (i = 0; i < 260; i++) {
    mask = MASK_OSZACP;
    if (i > 1) mask &= ~AMD64G_CC_MASK_O;
    if (i > 0) mask &= ~AMD64G_CC_MASK_A;
    data = 0x1122334455667788ULL;
    xtra = 0x3141592727182818ULL;
    flags_in = 0ULL;
    amt = (ULong)i;
    shld64();
    printf("%3d 0x%016llx 0x%llx\n", i, result, flags_out & mask);
  }

  printf("\nleft 32\n");
  for (i = 0; i < 260; i++) {
    mask = MASK_OSZACP;
    if (i > 1) mask &= ~AMD64G_CC_MASK_O;
    if (i > 0) mask &= ~AMD64G_CC_MASK_A;
    data = 0x1122334455667788ULL;
    xtra = 0x3141592727182818ULL;
    flags_in = 0ULL;
    amt = (ULong)i;
    shld32();
    printf("%3d 0x%016llx 0x%llx\n", i, result, flags_out & mask);
  }
  printf("\n");

  printf("\nleft 16\n");
  for (i = 0; i < 260; i++) {
    mask = MASK_OSZACP;
    if (i > 1) mask &= ~AMD64G_CC_MASK_O;
    if (i > 0) mask &= ~AMD64G_CC_MASK_A;
    data = 0x1122334455667788ULL;
    xtra = 0x987654321987abcdULL;
    flags_in = 0ULL;
    amt = (ULong)i;
    shld16();
    printf("%3d 0x%016llx 0x%llx\n", i, result, flags_out & mask);
  }
  printf("\n");

  printf("\nright 64\n");
  for (i = 0; i < 260; i++) {
    mask = MASK_OSZACP;
    if (i > 1) mask &= ~AMD64G_CC_MASK_O;
    if (i > 0) mask &= ~AMD64G_CC_MASK_A;
    data = 0x1122334455667788ULL;
    xtra = 0x3141592727182818ULL;
    flags_in = 0ULL;
    amt = (ULong)i;
    shrd64();
    printf("%3d 0x%016llx 0x%llx\n", i, result, flags_out & mask);
  }

  printf("\nright 32\n");
  for (i = 0; i < 260; i++) {
    mask = MASK_OSZACP;
    if (i > 1) mask &= ~AMD64G_CC_MASK_O;
    if (i > 0) mask &= ~AMD64G_CC_MASK_A;
    data = 0x1122334455667788ULL;
    xtra = 0x3141592727182818ULL;
    flags_in = 0ULL;
    amt = (ULong)i;
    shrd32();
    printf("%3d 0x%016llx 0x%llx\n", i, result, flags_out & mask);
  }
  printf("\n");

  printf("\nright 16\n");
  for (i = 0; i < 260; i++) {
    mask = MASK_OSZACP;
    if (i > 1) mask &= ~AMD64G_CC_MASK_O;
    if (i > 0) mask &= ~AMD64G_CC_MASK_A;
    data = 0x1122334455667788ULL;
    xtra = 0x987654321987abcdULL;
    flags_in = 0ULL;
    amt = (ULong)i;
    shrd16();
    printf("%3d 0x%016llx 0x%llx\n", i, result, flags_out & mask);
  }
  printf("\n");

  return 0;
}
