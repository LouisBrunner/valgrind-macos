#include <stdio.h>

#define branch(mask,i2,_v1)                            \
  ({                                                   \
        unsigned char taken;                           \
        unsigned char v1 = _v1;                        \
        asm volatile(   "       tm %[v]," #i2 "\n\t" \
                 	"	brc " #mask " ,1f\n\t" \
                        "       mvi %[taken],0\n\t"    \
			"	j   0f\n\t"            \
			"1:	mvi %[taken],1\n\t"    \
			"0:	bcr 0,0 /* nop */\n\t" \
             : [taken] "=Q" (taken)                    \
             : [v] "Q"(v1)                             \
             : "cc");                                  \
        taken;                                         \
   })

void
tm_mask_0(void)
{
  int wrong, ok;
  unsigned char v;

  printf("Test #1  mask == 0, value == ~0  --> cc == 0\n");

  v = ~0;
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
tm_value_0(void)
{
  int wrong, ok;
  unsigned char v;

  printf("Test #2  mask == 0xFF, value == 0  --> cc == 0\n");

  v = 0;
  wrong = ok = 0;

  if (branch(0,  0xFF, v)) ++wrong; else ++ok;
  if (branch(1,  0xFF, v)) ++wrong; else ++ok;
  if (branch(2,  0xFF, v)) ++wrong; else ++ok;
  if (branch(3,  0xFF, v)) ++wrong; else ++ok;
  if (branch(4,  0xFF, v)) ++wrong; else ++ok;
  if (branch(5,  0xFF, v)) ++wrong; else ++ok;
  if (branch(6,  0xFF, v)) ++wrong; else ++ok;
  if (branch(7,  0xFF, v)) ++wrong; else ++ok;
  if (branch(8,  0xFF, v)) ++ok; else ++wrong;
  if (branch(9,  0xFF, v)) ++ok; else ++wrong;
  if (branch(10, 0xFF, v)) ++ok; else ++wrong;
  if (branch(11, 0xFF, v)) ++ok; else ++wrong;
  if (branch(12, 0xFF, v)) ++ok; else ++wrong;
  if (branch(13, 0xFF, v)) ++ok; else ++wrong;
  if (branch(14, 0xFF, v)) ++ok; else ++wrong;
  if (branch(15, 0xFF, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tm_all_selected_bits_set_1(void)
{
  int wrong, ok;
  unsigned char v;

  printf("Test #3  mask == 0xFF, value == 0xFF  --> cc == 3\n");

  v = 0xFF;
  wrong = ok = 0;

  if (branch(0,  0xFF, v)) ++wrong; else ++ok;
  if (branch(1,  0xFF, v)) ++ok; else ++wrong;
  if (branch(2,  0xFF, v)) ++wrong; else ++ok;
  if (branch(3,  0xFF, v)) ++ok; else ++wrong;
  if (branch(4,  0xFF, v)) ++wrong; else ++ok;
  if (branch(5,  0xFF, v)) ++ok; else ++wrong;
  if (branch(6,  0xFF, v)) ++wrong; else ++ok;
  if (branch(7,  0xFF, v)) ++ok; else ++wrong;
  if (branch(8,  0xFF, v)) ++wrong; else ++ok;
  if (branch(9,  0xFF, v)) ++ok; else ++wrong;
  if (branch(10, 0xFF, v)) ++wrong; else ++ok;
  if (branch(11, 0xFF, v)) ++ok; else ++wrong;
  if (branch(12, 0xFF, v)) ++wrong; else ++ok;
  if (branch(13, 0xFF, v)) ++ok; else ++wrong;
  if (branch(14, 0xFF, v)) ++wrong; else ++ok;
  if (branch(15, 0xFF, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tm_all_selected_bits_set_2(void)
{
  int wrong, ok;
  unsigned char v;

  printf("Test #4  mask == 0x80, value == 0x80  --> cc == 3\n");

  v = 0x80;
  wrong = ok = 0;

  if (branch(0,  0x80, v)) ++wrong; else ++ok;
  if (branch(1,  0x80, v)) ++ok; else ++wrong;
  if (branch(2,  0x80, v)) ++wrong; else ++ok;
  if (branch(3,  0x80, v)) ++ok; else ++wrong;
  if (branch(4,  0x80, v)) ++wrong; else ++ok;
  if (branch(5,  0x80, v)) ++ok; else ++wrong;
  if (branch(6,  0x80, v)) ++wrong; else ++ok;
  if (branch(7,  0x80, v)) ++ok; else ++wrong;
  if (branch(8,  0x80, v)) ++wrong; else ++ok;
  if (branch(9,  0x80, v)) ++ok; else ++wrong;
  if (branch(10, 0x80, v)) ++wrong; else ++ok;
  if (branch(11, 0x80, v)) ++ok; else ++wrong;
  if (branch(12, 0x80, v)) ++wrong; else ++ok;
  if (branch(13, 0x80, v)) ++ok; else ++wrong;
  if (branch(14, 0x80, v)) ++wrong; else ++ok;
  if (branch(15, 0x80, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tm_some_selected_bits_set_msb_set(void)
{
  int wrong, ok;
  unsigned char v;

  printf("Test #5  mask == 0xF0, value == 0x90  --> cc == 1\n");

  v = 0x90;
  wrong = ok = 0;

  if (branch(0,  0xF0, v)) ++wrong; else ++ok;
  if (branch(1,  0xF0, v)) ++wrong; else ++ok;
  if (branch(2,  0xF0, v)) ++wrong; else ++ok;
  if (branch(3,  0xF0, v)) ++wrong; else ++ok;
  if (branch(4,  0xF0, v)) ++ok; else ++wrong;
  if (branch(5,  0xF0, v)) ++ok; else ++wrong;
  if (branch(6,  0xF0, v)) ++ok; else ++wrong;
  if (branch(7,  0xF0, v)) ++ok; else ++wrong;
  if (branch(8,  0xF0, v)) ++wrong; else ++ok;
  if (branch(9,  0xF0, v)) ++wrong; else ++ok;
  if (branch(10, 0xF0, v)) ++wrong; else ++ok;
  if (branch(11, 0xF0, v)) ++wrong; else ++ok;
  if (branch(12, 0xF0, v)) ++ok; else ++wrong;
  if (branch(13, 0xF0, v)) ++ok; else ++wrong;
  if (branch(14, 0xF0, v)) ++ok; else ++wrong;
  if (branch(15, 0xF0, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
tm_some_selected_bits_set_msb_not_set(void)
{
  int wrong, ok;
  unsigned char v;

  printf("Test #6  mask == 0xF0, value == 0x30  --> cc == 1\n");

  v = 0x30;
  wrong = ok = 0;

  if (branch(0,  0xF0, v)) ++wrong; else ++ok;
  if (branch(1,  0xF0, v)) ++wrong; else ++ok;
  if (branch(2,  0xF0, v)) ++wrong; else ++ok;
  if (branch(3,  0xF0, v)) ++wrong; else ++ok;
  if (branch(4,  0xF0, v)) ++ok; else ++wrong;
  if (branch(5,  0xF0, v)) ++ok; else ++wrong;
  if (branch(6,  0xF0, v)) ++ok; else ++wrong;
  if (branch(7,  0xF0, v)) ++ok; else ++wrong;
  if (branch(8,  0xF0, v)) ++wrong; else ++ok;
  if (branch(9,  0xF0, v)) ++wrong; else ++ok;
  if (branch(10, 0xF0, v)) ++wrong; else ++ok;
  if (branch(11, 0xF0, v)) ++wrong; else ++ok;
  if (branch(12, 0xF0, v)) ++ok; else ++wrong;
  if (branch(13, 0xF0, v)) ++ok; else ++wrong;
  if (branch(14, 0xF0, v)) ++ok; else ++wrong;
  if (branch(15, 0xF0, v)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

int main()
{
  tm_mask_0();
  tm_value_0();
  tm_all_selected_bits_set_1();
  tm_all_selected_bits_set_2();
  tm_some_selected_bits_set_msb_set();
  tm_some_selected_bits_set_msb_not_set();

  return 0;
}
