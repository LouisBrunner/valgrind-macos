#include <stdio.h>

#define branch(mask,i2,_v1)                            \
  ({                                                   \
        unsigned char taken;                           \
        unsigned long v1 = _v1;                        \
        asm volatile(   "       tmll %[v]," #i2 "\n\t" \
                 	"	brc " #mask " ,1f\n\t" \
                        "       mvi %[taken],0\n\t"    \
			"	j   0f\n\t"            \
			"1:	mvi %[taken],1\n\t"    \
			"0:	bcr 0,0 /* nop */\n\t" \
             : [taken] "=Q" (taken)                    \
             : [v] "d"(v1)                             \
             : "cc");                                  \
        taken;                                         \
   })

void
tmll_mask_0(void)
{
  int wrong, ok;
  unsigned long v;

  printf("Test #1  mask == 0, value == ~0  --> cc == 0\n");

  v = ~0ULL;
  wrong = ok = 0;

  if (branch(0,  0, v)) ++wrong; else ++ok;
  if (branch(1,  0, v)) ++wrong; else ++ok;
  if (branch(2,  0, v)) ++wrong; else ++ok;
  if (branch(3,  0, v)) ++wrong; else ++ok;
  if (branch(4,  0, v)) ++wrong; else ++ok;
  if (branch(5,  0, v)) ++wrong; else ++ok;
  if (branch(6,  0, v)) ++wrong; else ++ok;
  if (branch(7,  0, v)) ++wrong; else ++ok;
  if (branch(8,  0, v)) ++ok; else ++wrong;
  if (branch(9,  0, v)) ++ok; else ++wrong;
  if (branch(10, 0, v)) ++ok; else ++wrong;
  if (branch(11, 0, v)) ++ok; else ++wrong;
  if (branch(12, 0, v)) ++ok; else ++wrong;
  if (branch(13, 0, v)) ++ok; else ++wrong;
  if (branch(14, 0, v)) ++ok; else ++wrong;
  if (branch(15, 0, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tmll_value_0(void)
{
  int wrong, ok;
  unsigned long v;

  printf("Test #2  mask == 0xFFF, value == 0  --> cc == 0\n");

  v = 0;
  wrong = ok = 0;

  if (branch(0,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(1,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(2,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(3,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(4,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(5,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(6,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(7,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(8,  0xFFFF, v)) ++ok; else ++wrong;
  if (branch(9,  0xFFFF, v)) ++ok; else ++wrong;
  if (branch(10, 0xFFFF, v)) ++ok; else ++wrong;
  if (branch(11, 0xFFFF, v)) ++ok; else ++wrong;
  if (branch(12, 0xFFFF, v)) ++ok; else ++wrong;
  if (branch(13, 0xFFFF, v)) ++ok; else ++wrong;
  if (branch(14, 0xFFFF, v)) ++ok; else ++wrong;
  if (branch(15, 0xFFFF, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tmll_all_selected_bits_set_1(void)
{
  int wrong, ok;
  unsigned long v;

  printf("Test #3  mask == 0xFFFF, value == 0xFFFF  --> cc == 3\n");

  v = 0xFFFF;
  wrong = ok = 0;

  if (branch(0,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(1,  0xFFFF, v)) ++ok; else ++wrong;
  if (branch(2,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(3,  0xFFFF, v)) ++ok; else ++wrong;
  if (branch(4,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(5,  0xFFFF, v)) ++ok; else ++wrong;
  if (branch(6,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(7,  0xFFFF, v)) ++ok; else ++wrong;
  if (branch(8,  0xFFFF, v)) ++wrong; else ++ok;
  if (branch(9,  0xFFFF, v)) ++ok; else ++wrong;
  if (branch(10, 0xFFFF, v)) ++wrong; else ++ok;
  if (branch(11, 0xFFFF, v)) ++ok; else ++wrong;
  if (branch(12, 0xFFFF, v)) ++wrong; else ++ok;
  if (branch(13, 0xFFFF, v)) ++ok; else ++wrong;
  if (branch(14, 0xFFFF, v)) ++wrong; else ++ok;
  if (branch(15, 0xFFFF, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tmll_all_selected_bits_set_2(void)
{
  int wrong, ok;
  unsigned long v;

  printf("Test #4  mask == 0x8000, value == 0x8000  --> cc == 3\n");

  v = 0x8000;
  wrong = ok = 0;

  if (branch(0,  0x8000, v)) ++wrong; else ++ok;
  if (branch(1,  0x8000, v)) ++ok; else ++wrong;
  if (branch(2,  0x8000, v)) ++wrong; else ++ok;
  if (branch(3,  0x8000, v)) ++ok; else ++wrong;
  if (branch(4,  0x8000, v)) ++wrong; else ++ok;
  if (branch(5,  0x8000, v)) ++ok; else ++wrong;
  if (branch(6,  0x8000, v)) ++wrong; else ++ok;
  if (branch(7,  0x8000, v)) ++ok; else ++wrong;
  if (branch(8,  0x8000, v)) ++wrong; else ++ok;
  if (branch(9,  0x8000, v)) ++ok; else ++wrong;
  if (branch(10, 0x8000, v)) ++wrong; else ++ok;
  if (branch(11, 0x8000, v)) ++ok; else ++wrong;
  if (branch(12, 0x8000, v)) ++wrong; else ++ok;
  if (branch(13, 0x8000, v)) ++ok; else ++wrong;
  if (branch(14, 0x8000, v)) ++wrong; else ++ok;
  if (branch(15, 0x8000, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tmll_some_selected_bits_set_msb_set(void)
{
  int wrong, ok;
  unsigned long v;

  printf("Test #5  mask == 0xF000, value == 0x9000  --> cc == 2\n");

  v = 0x9000;
  wrong = ok = 0;

  if (branch(0,  0xF000, v)) ++wrong; else ++ok;
  if (branch(1,  0xF000, v)) ++wrong; else ++ok;
  if (branch(2,  0xF000, v)) ++ok; else ++wrong;
  if (branch(3,  0xF000, v)) ++ok; else ++wrong;
  if (branch(4,  0xF000, v)) ++wrong; else ++ok;
  if (branch(5,  0xF000, v)) ++wrong; else ++ok;
  if (branch(6,  0xF000, v)) ++ok; else ++wrong;
  if (branch(7,  0xF000, v)) ++ok; else ++wrong;
  if (branch(8,  0xF000, v)) ++wrong; else ++ok;
  if (branch(9,  0xF000, v)) ++wrong; else ++ok;
  if (branch(10, 0xF000, v)) ++ok; else ++wrong;
  if (branch(11, 0xF000, v)) ++ok; else ++wrong;
  if (branch(12, 0xF000, v)) ++wrong; else ++ok;
  if (branch(13, 0xF000, v)) ++wrong; else ++ok;
  if (branch(14, 0xF000, v)) ++ok; else ++wrong;
  if (branch(15, 0xF000, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tmll_some_selected_bits_set_msb_not_set(void)
{
  int wrong, ok;
  unsigned long v;

  printf("Test #6  mask == 0xF000, value == 0x3000  --> cc == 1\n");

  v = 0x3000;
  wrong = ok = 0;

  if (branch(0,  0xF000, v)) ++wrong; else ++ok;
  if (branch(1,  0xF000, v)) ++wrong; else ++ok;
  if (branch(2,  0xF000, v)) ++wrong; else ++ok;
  if (branch(3,  0xF000, v)) ++wrong; else ++ok;
  if (branch(4,  0xF000, v)) ++ok; else ++wrong;
  if (branch(5,  0xF000, v)) ++ok; else ++wrong;
  if (branch(6,  0xF000, v)) ++ok; else ++wrong;
  if (branch(7,  0xF000, v)) ++ok; else ++wrong;
  if (branch(8,  0xF000, v)) ++wrong; else ++ok;
  if (branch(9,  0xF000, v)) ++wrong; else ++ok;
  if (branch(10, 0xF000, v)) ++wrong; else ++ok;
  if (branch(11, 0xF000, v)) ++wrong; else ++ok;
  if (branch(12, 0xF000, v)) ++ok; else ++wrong;
  if (branch(13, 0xF000, v)) ++ok; else ++wrong;
  if (branch(14, 0xF000, v)) ++ok; else ++wrong;
  if (branch(15, 0xF000, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}


int main()
{
  tmll_mask_0();
  tmll_value_0();
  tmll_all_selected_bits_set_1();
  tmll_all_selected_bits_set_2();
  tmll_some_selected_bits_set_msb_set();
  tmll_some_selected_bits_set_msb_not_set();

  return 0;
}
