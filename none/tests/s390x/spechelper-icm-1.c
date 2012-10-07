#include <stdio.h>

#define branch(mask,icmm,_v1)                          \
  ({                                                   \
        unsigned char taken;                           \
        unsigned b1 = _v1;                             \
        asm volatile(	"	larl 1, 2f\n\t"        \
			"	l 0, 0(1)\n\t"         \
			"       icm 0," #icmm",%[b1]\n\t"     \
                 	"	brc " #mask " ,1f\n\t" \
                        "       mvi %[taken],0\n\t"    \
			"	j   0f\n\t"            \
			"1:	mvi %[taken],1\n\t"    \
			"	j   0f\n\t"            \
			"2:	.long 0\n\t"           \
			"0:	bcr 0,0 /* nop */\n\t" \
             : [taken] "=Q" (taken)                    \
             : [b1] "Q"(b1)                            \
             : "cc", "0","1");                         \
        taken;                                         \
   })

void
icm_mask_0(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #1  mask == 0, value == 0xFFFFFFFF  --> cc == 0\n");

  v1 = 0xFFFFFFFF;
  wrong = ok = 0;

  if (branch(0,  0, v1)) ++wrong; else ++ok;
  if (branch(1,  0, v1)) ++wrong; else ++ok;
  if (branch(2,  0, v1)) ++wrong; else ++ok;
  if (branch(3,  0, v1)) ++wrong; else ++ok;
  if (branch(4,  0, v1)) ++wrong; else ++ok;
  if (branch(5,  0, v1)) ++wrong; else ++ok;
  if (branch(6,  0, v1)) ++wrong; else ++ok;
  if (branch(7,  0, v1)) ++wrong; else ++ok;
  if (branch(8,  0, v1)) ++ok; else ++wrong;
  if (branch(9,  0, v1)) ++ok; else ++wrong;
  if (branch(10, 0, v1)) ++ok; else ++wrong;
  if (branch(11, 0, v1)) ++ok; else ++wrong;
  if (branch(12, 0, v1)) ++ok; else ++wrong;
  if (branch(13, 0, v1)) ++ok; else ++wrong;
  if (branch(14, 0, v1)) ++ok; else ++wrong;
  if (branch(15, 0, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
icm_value_0(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #2  mask == 0xF, value == 0  --> cc == 0\n");

  v1 = 0;
  wrong = ok = 0;

  if (branch(0,  15, v1)) ++wrong; else ++ok;
  if (branch(1,  15, v1)) ++wrong; else ++ok;
  if (branch(2,  15, v1)) ++wrong; else ++ok;
  if (branch(3,  15, v1)) ++wrong; else ++ok;
  if (branch(4,  15, v1)) ++wrong; else ++ok;
  if (branch(5,  15, v1)) ++wrong; else ++ok;
  if (branch(6,  15, v1)) ++wrong; else ++ok;
  if (branch(7,  15, v1)) ++wrong; else ++ok;
  if (branch(8,  15, v1)) ++ok; else ++wrong;
  if (branch(9,  15, v1)) ++ok; else ++wrong;
  if (branch(10, 15, v1)) ++ok; else ++wrong;
  if (branch(11, 15, v1)) ++ok; else ++wrong;
  if (branch(12, 15, v1)) ++ok; else ++wrong;
  if (branch(13, 15, v1)) ++ok; else ++wrong;
  if (branch(14, 15, v1)) ++ok; else ++wrong;
  if (branch(15, 15, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

/* loads one byte */
void
icm_one_byte_msb_set(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #3  load one byte, msb set  --> cc == 1\n");

  v1 = 0x80000000;
  wrong = ok = 0;

  if (branch(0,  8, v1)) ++wrong; else ++ok;
  if (branch(1,  8, v1)) ++wrong; else ++ok;
  if (branch(2,  8, v1)) ++wrong; else ++ok;
  if (branch(3,  8, v1)) ++wrong; else ++ok;
  if (branch(4,  8, v1)) ++ok; else ++wrong;
  if (branch(5,  8, v1)) ++ok; else ++wrong;
  if (branch(6,  8, v1)) ++ok; else ++wrong;
  if (branch(7,  8, v1)) ++ok; else ++wrong;
  if (branch(8,  8, v1)) ++wrong; else ++ok;
  if (branch(9,  8, v1)) ++wrong; else ++ok;
  if (branch(10, 8, v1)) ++wrong; else ++ok;
  if (branch(11, 8, v1)) ++wrong; else ++ok;
  if (branch(12, 8, v1)) ++ok; else ++wrong;
  if (branch(13, 8, v1)) ++ok; else ++wrong;
  if (branch(14, 8, v1)) ++ok; else ++wrong;
  if (branch(15, 8, v1)) ++ok; else ++wrong;

  if (branch(0,  4, v1)) ++wrong; else ++ok;
  if (branch(1,  4, v1)) ++wrong; else ++ok;
  if (branch(2,  4, v1)) ++wrong; else ++ok;
  if (branch(3,  4, v1)) ++wrong; else ++ok;
  if (branch(4,  4, v1)) ++ok; else ++wrong;
  if (branch(5,  4, v1)) ++ok; else ++wrong;
  if (branch(6,  4, v1)) ++ok; else ++wrong;
  if (branch(7,  4, v1)) ++ok; else ++wrong;
  if (branch(8,  4, v1)) ++wrong; else ++ok;
  if (branch(9,  4, v1)) ++wrong; else ++ok;
  if (branch(10, 4, v1)) ++wrong; else ++ok;
  if (branch(11, 4, v1)) ++wrong; else ++ok;
  if (branch(12, 4, v1)) ++ok; else ++wrong;
  if (branch(13, 4, v1)) ++ok; else ++wrong;
  if (branch(14, 4, v1)) ++ok; else ++wrong;
  if (branch(15, 4, v1)) ++ok; else ++wrong;

  if (branch(0,  2, v1)) ++wrong; else ++ok;
  if (branch(1,  2, v1)) ++wrong; else ++ok;
  if (branch(2,  2, v1)) ++wrong; else ++ok;
  if (branch(3,  2, v1)) ++wrong; else ++ok;
  if (branch(4,  2, v1)) ++ok; else ++wrong;
  if (branch(5,  2, v1)) ++ok; else ++wrong;
  if (branch(6,  2, v1)) ++ok; else ++wrong;
  if (branch(7,  2, v1)) ++ok; else ++wrong;
  if (branch(8,  2, v1)) ++wrong; else ++ok;
  if (branch(9,  2, v1)) ++wrong; else ++ok;
  if (branch(10, 2, v1)) ++wrong; else ++ok;
  if (branch(11, 2, v1)) ++wrong; else ++ok;
  if (branch(12, 2, v1)) ++ok; else ++wrong;
  if (branch(13, 2, v1)) ++ok; else ++wrong;
  if (branch(14, 2, v1)) ++ok; else ++wrong;
  if (branch(15, 2, v1)) ++ok; else ++wrong;

  if (branch(0,  1, v1)) ++wrong; else ++ok;
  if (branch(1,  1, v1)) ++wrong; else ++ok;
  if (branch(2,  1, v1)) ++wrong; else ++ok;
  if (branch(3,  1, v1)) ++wrong; else ++ok;
  if (branch(4,  1, v1)) ++ok; else ++wrong;
  if (branch(5,  1, v1)) ++ok; else ++wrong;
  if (branch(6,  1, v1)) ++ok; else ++wrong;
  if (branch(7,  1, v1)) ++ok; else ++wrong;
  if (branch(8,  1, v1)) ++wrong; else ++ok;
  if (branch(9,  1, v1)) ++wrong; else ++ok;
  if (branch(10, 1, v1)) ++wrong; else ++ok;
  if (branch(11, 1, v1)) ++wrong; else ++ok;
  if (branch(12, 1, v1)) ++ok; else ++wrong;
  if (branch(13, 1, v1)) ++ok; else ++wrong;
  if (branch(14, 1, v1)) ++ok; else ++wrong;
  if (branch(15, 1, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 64)
    printf("FAILED\n");
  else
    printf("OK\n");
}

/* loads two bytes */
void
icm_two_bytes_msb_set(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #4  load two bytes, msb set  --> cc == 1\n");

  v1 = 0x80000000;
  wrong = ok = 0;

  if (branch(0,  12, v1)) ++wrong; else ++ok;
  if (branch(1,  12, v1)) ++wrong; else ++ok;
  if (branch(2,  12, v1)) ++wrong; else ++ok;
  if (branch(3,  12, v1)) ++wrong; else ++ok;
  if (branch(4,  12, v1)) ++ok; else ++wrong;
  if (branch(5,  12, v1)) ++ok; else ++wrong;
  if (branch(6,  12, v1)) ++ok; else ++wrong;
  if (branch(7,  12, v1)) ++ok; else ++wrong;
  if (branch(8,  12, v1)) ++wrong; else ++ok;
  if (branch(9,  12, v1)) ++wrong; else ++ok;
  if (branch(10, 12, v1)) ++wrong; else ++ok;
  if (branch(11, 12, v1)) ++wrong; else ++ok;
  if (branch(12, 12, v1)) ++ok; else ++wrong;
  if (branch(13, 12, v1)) ++ok; else ++wrong;
  if (branch(14, 12, v1)) ++ok; else ++wrong;
  if (branch(15, 12, v1)) ++ok; else ++wrong;

  if (branch(0,  10, v1)) ++wrong; else ++ok;
  if (branch(1,  10, v1)) ++wrong; else ++ok;
  if (branch(2,  10, v1)) ++wrong; else ++ok;
  if (branch(3,  10, v1)) ++wrong; else ++ok;
  if (branch(4,  10, v1)) ++ok; else ++wrong;
  if (branch(5,  10, v1)) ++ok; else ++wrong;
  if (branch(6,  10, v1)) ++ok; else ++wrong;
  if (branch(7,  10, v1)) ++ok; else ++wrong;
  if (branch(8,  10, v1)) ++wrong; else ++ok;
  if (branch(9,  10, v1)) ++wrong; else ++ok;
  if (branch(10, 10, v1)) ++wrong; else ++ok;
  if (branch(11, 10, v1)) ++wrong; else ++ok;
  if (branch(12, 10, v1)) ++ok; else ++wrong;
  if (branch(13, 10, v1)) ++ok; else ++wrong;
  if (branch(14, 10, v1)) ++ok; else ++wrong;
  if (branch(15, 10, v1)) ++ok; else ++wrong;

  if (branch(0,   9, v1)) ++wrong; else ++ok;
  if (branch(1,   9, v1)) ++wrong; else ++ok;
  if (branch(2,   9, v1)) ++wrong; else ++ok;
  if (branch(3,   9, v1)) ++wrong; else ++ok;
  if (branch(4,   9, v1)) ++ok; else ++wrong;
  if (branch(5,   9, v1)) ++ok; else ++wrong;
  if (branch(6,   9, v1)) ++ok; else ++wrong;
  if (branch(7,   9, v1)) ++ok; else ++wrong;
  if (branch(8,   9, v1)) ++wrong; else ++ok;
  if (branch(9,   9, v1)) ++wrong; else ++ok;
  if (branch(10,  9, v1)) ++wrong; else ++ok;
  if (branch(11,  9, v1)) ++wrong; else ++ok;
  if (branch(12,  9, v1)) ++ok; else ++wrong;
  if (branch(13,  9, v1)) ++ok; else ++wrong;
  if (branch(14,  9, v1)) ++ok; else ++wrong;
  if (branch(15,  9, v1)) ++ok; else ++wrong;

  if (branch(0,   6, v1)) ++wrong; else ++ok;
  if (branch(1,   6, v1)) ++wrong; else ++ok;
  if (branch(2,   6, v1)) ++wrong; else ++ok;
  if (branch(3,   6, v1)) ++wrong; else ++ok;
  if (branch(4,   6, v1)) ++ok; else ++wrong;
  if (branch(5,   6, v1)) ++ok; else ++wrong;
  if (branch(6,   6, v1)) ++ok; else ++wrong;
  if (branch(7,   6, v1)) ++ok; else ++wrong;
  if (branch(8,   6, v1)) ++wrong; else ++ok;
  if (branch(9,   6, v1)) ++wrong; else ++ok;
  if (branch(10,  6, v1)) ++wrong; else ++ok;
  if (branch(11,  6, v1)) ++wrong; else ++ok;
  if (branch(12,  6, v1)) ++ok; else ++wrong;
  if (branch(13,  6, v1)) ++ok; else ++wrong;
  if (branch(14,  6, v1)) ++ok; else ++wrong;
  if (branch(15,  6, v1)) ++ok; else ++wrong;

  if (branch(0,   5, v1)) ++wrong; else ++ok;
  if (branch(1,   5, v1)) ++wrong; else ++ok;
  if (branch(2,   5, v1)) ++wrong; else ++ok;
  if (branch(3,   5, v1)) ++wrong; else ++ok;
  if (branch(4,   5, v1)) ++ok; else ++wrong;
  if (branch(5,   5, v1)) ++ok; else ++wrong;
  if (branch(6,   5, v1)) ++ok; else ++wrong;
  if (branch(7,   5, v1)) ++ok; else ++wrong;
  if (branch(8,   5, v1)) ++wrong; else ++ok;
  if (branch(9,   5, v1)) ++wrong; else ++ok;
  if (branch(10,  5, v1)) ++wrong; else ++ok;
  if (branch(11,  5, v1)) ++wrong; else ++ok;
  if (branch(12,  5, v1)) ++ok; else ++wrong;
  if (branch(13,  5, v1)) ++ok; else ++wrong;
  if (branch(14,  5, v1)) ++ok; else ++wrong;
  if (branch(15,  5, v1)) ++ok; else ++wrong;

  if (branch(0,   3, v1)) ++wrong; else ++ok;
  if (branch(1,   3, v1)) ++wrong; else ++ok;
  if (branch(2,   3, v1)) ++wrong; else ++ok;
  if (branch(3,   3, v1)) ++wrong; else ++ok;
  if (branch(4,   3, v1)) ++ok; else ++wrong;
  if (branch(5,   3, v1)) ++ok; else ++wrong;
  if (branch(6,   3, v1)) ++ok; else ++wrong;
  if (branch(7,   3, v1)) ++ok; else ++wrong;
  if (branch(8,   3, v1)) ++wrong; else ++ok;
  if (branch(9,   3, v1)) ++wrong; else ++ok;
  if (branch(10,  3, v1)) ++wrong; else ++ok;
  if (branch(11,  3, v1)) ++wrong; else ++ok;
  if (branch(12,  3, v1)) ++ok; else ++wrong;
  if (branch(13,  3, v1)) ++ok; else ++wrong;
  if (branch(14,  3, v1)) ++ok; else ++wrong;
  if (branch(15,  3, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 96)
    printf("FAILED\n");
  else
    printf("OK\n");
}

/* loads three bytes */
void
icm_three_bytes_msb_set(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #5  load three bytes, msb set  --> cc == 1\n");

  v1 = 0x80000000;
  wrong = ok = 0;

  if (branch(0,  14, v1)) ++wrong; else ++ok;
  if (branch(1,  14, v1)) ++wrong; else ++ok;
  if (branch(2,  14, v1)) ++wrong; else ++ok;
  if (branch(3,  14, v1)) ++wrong; else ++ok;
  if (branch(4,  14, v1)) ++ok; else ++wrong;
  if (branch(5,  14, v1)) ++ok; else ++wrong;
  if (branch(6,  14, v1)) ++ok; else ++wrong;
  if (branch(7,  14, v1)) ++ok; else ++wrong;
  if (branch(8,  14, v1)) ++wrong; else ++ok;
  if (branch(9,  14, v1)) ++wrong; else ++ok;
  if (branch(10, 14, v1)) ++wrong; else ++ok;
  if (branch(11, 14, v1)) ++wrong; else ++ok;
  if (branch(12, 14, v1)) ++ok; else ++wrong;
  if (branch(13, 14, v1)) ++ok; else ++wrong;
  if (branch(14, 14, v1)) ++ok; else ++wrong;
  if (branch(15, 14, v1)) ++ok; else ++wrong;

  if (branch(0,  13, v1)) ++wrong; else ++ok;
  if (branch(1,  13, v1)) ++wrong; else ++ok;
  if (branch(2,  13, v1)) ++wrong; else ++ok;
  if (branch(3,  13, v1)) ++wrong; else ++ok;
  if (branch(4,  13, v1)) ++ok; else ++wrong;
  if (branch(5,  13, v1)) ++ok; else ++wrong;
  if (branch(6,  13, v1)) ++ok; else ++wrong;
  if (branch(7,  13, v1)) ++ok; else ++wrong;
  if (branch(8,  13, v1)) ++wrong; else ++ok;
  if (branch(9,  13, v1)) ++wrong; else ++ok;
  if (branch(10, 13, v1)) ++wrong; else ++ok;
  if (branch(11, 13, v1)) ++wrong; else ++ok;
  if (branch(12, 13, v1)) ++ok; else ++wrong;
  if (branch(13, 13, v1)) ++ok; else ++wrong;
  if (branch(14, 13, v1)) ++ok; else ++wrong;
  if (branch(15, 13, v1)) ++ok; else ++wrong;

  if (branch(0,  11, v1)) ++wrong; else ++ok;
  if (branch(1,  11, v1)) ++wrong; else ++ok;
  if (branch(2,  11, v1)) ++wrong; else ++ok;
  if (branch(3,  11, v1)) ++wrong; else ++ok;
  if (branch(4,  11, v1)) ++ok; else ++wrong;
  if (branch(5,  11, v1)) ++ok; else ++wrong;
  if (branch(6,  11, v1)) ++ok; else ++wrong;
  if (branch(7,  11, v1)) ++ok; else ++wrong;
  if (branch(8,  11, v1)) ++wrong; else ++ok;
  if (branch(9,  11, v1)) ++wrong; else ++ok;
  if (branch(10, 11, v1)) ++wrong; else ++ok;
  if (branch(11, 11, v1)) ++wrong; else ++ok;
  if (branch(12, 11, v1)) ++ok; else ++wrong;
  if (branch(13, 11, v1)) ++ok; else ++wrong;
  if (branch(14, 11, v1)) ++ok; else ++wrong;
  if (branch(15, 11, v1)) ++ok; else ++wrong;

  if (branch(0,   7, v1)) ++wrong; else ++ok;
  if (branch(1,   7, v1)) ++wrong; else ++ok;
  if (branch(2,   7, v1)) ++wrong; else ++ok;
  if (branch(3,   7, v1)) ++wrong; else ++ok;
  if (branch(4,   7, v1)) ++ok; else ++wrong;
  if (branch(5,   7, v1)) ++ok; else ++wrong;
  if (branch(6,   7, v1)) ++ok; else ++wrong;
  if (branch(7,   7, v1)) ++ok; else ++wrong;
  if (branch(8,   7, v1)) ++wrong; else ++ok;
  if (branch(9,   7, v1)) ++wrong; else ++ok;
  if (branch(10,  7, v1)) ++wrong; else ++ok;
  if (branch(11,  7, v1)) ++wrong; else ++ok;
  if (branch(12,  7, v1)) ++ok; else ++wrong;
  if (branch(13,  7, v1)) ++ok; else ++wrong;
  if (branch(14,  7, v1)) ++ok; else ++wrong;
  if (branch(15,  7, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 64)
    printf("FAILED\n");
  else
    printf("OK\n");
}

/* loads four bytes */
void
icm_four_bytes_msb_set(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #6  load four bytes, msb set  --> cc == 1\n");

  v1 = 0x80000000;
  wrong = ok = 0;

  if (branch(0,  15, v1)) ++wrong; else ++ok;
  if (branch(1,  15, v1)) ++wrong; else ++ok;
  if (branch(2,  15, v1)) ++wrong; else ++ok;
  if (branch(3,  15, v1)) ++wrong; else ++ok;
  if (branch(4,  15, v1)) ++ok; else ++wrong;
  if (branch(5,  15, v1)) ++ok; else ++wrong;
  if (branch(6,  15, v1)) ++ok; else ++wrong;
  if (branch(7,  15, v1)) ++ok; else ++wrong;
  if (branch(8,  15, v1)) ++wrong; else ++ok;
  if (branch(9,  15, v1)) ++wrong; else ++ok;
  if (branch(10, 15, v1)) ++wrong; else ++ok;
  if (branch(11, 15, v1)) ++wrong; else ++ok;
  if (branch(12, 15, v1)) ++ok; else ++wrong;
  if (branch(13, 15, v1)) ++ok; else ++wrong;
  if (branch(14, 15, v1)) ++ok; else ++wrong;
  if (branch(15, 15, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

/* loads one byte */
void
icm_one_byte_msb_not_set(void)
{
  int wrong, ok, v1;

  printf("Test #7  load one byte, msb not set  --> cc == 2\n");

  v1 = 0x7FFFFFFF;
  wrong = ok = 0;

  if (branch(0,  8, v1)) ++wrong; else ++ok;
  if (branch(1,  8, v1)) ++wrong; else ++ok;
  if (branch(2,  8, v1)) ++ok; else ++wrong;
  if (branch(3,  8, v1)) ++ok; else ++wrong;
  if (branch(4,  8, v1)) ++wrong; else ++ok;
  if (branch(5,  8, v1)) ++wrong; else ++ok;
  if (branch(6,  8, v1)) ++ok; else ++wrong;
  if (branch(7,  8, v1)) ++ok; else ++wrong;
  if (branch(8,  8, v1)) ++wrong; else ++ok;
  if (branch(9,  8, v1)) ++wrong; else ++ok;
  if (branch(10, 8, v1)) ++ok; else ++wrong;
  if (branch(11, 8, v1)) ++ok; else ++wrong;
  if (branch(12, 8, v1)) ++wrong; else ++ok;
  if (branch(13, 8, v1)) ++wrong; else ++ok;
  if (branch(14, 8, v1)) ++ok; else ++wrong;
  if (branch(15, 8, v1)) ++ok; else ++wrong;

  if (branch(0,  4, v1)) ++wrong; else ++ok;
  if (branch(1,  4, v1)) ++wrong; else ++ok;
  if (branch(2,  4, v1)) ++ok; else ++wrong;
  if (branch(3,  4, v1)) ++ok; else ++wrong;
  if (branch(4,  4, v1)) ++wrong; else ++ok;
  if (branch(5,  4, v1)) ++wrong; else ++ok;
  if (branch(6,  4, v1)) ++ok; else ++wrong;
  if (branch(7,  4, v1)) ++ok; else ++wrong;
  if (branch(8,  4, v1)) ++wrong; else ++ok;
  if (branch(9,  4, v1)) ++wrong; else ++ok;
  if (branch(10, 4, v1)) ++ok; else ++wrong;
  if (branch(11, 4, v1)) ++ok; else ++wrong;
  if (branch(12, 4, v1)) ++wrong; else ++ok;
  if (branch(13, 4, v1)) ++wrong; else ++ok;
  if (branch(14, 4, v1)) ++ok; else ++wrong;
  if (branch(15, 4, v1)) ++ok; else ++wrong;

  if (branch(0,  2, v1)) ++wrong; else ++ok;
  if (branch(1,  2, v1)) ++wrong; else ++ok;
  if (branch(2,  2, v1)) ++ok; else ++wrong;
  if (branch(3,  2, v1)) ++ok; else ++wrong;
  if (branch(4,  2, v1)) ++wrong; else ++ok;
  if (branch(5,  2, v1)) ++wrong; else ++ok;
  if (branch(6,  2, v1)) ++ok; else ++wrong;
  if (branch(7,  2, v1)) ++ok; else ++wrong;
  if (branch(8,  2, v1)) ++wrong; else ++ok;
  if (branch(9,  2, v1)) ++wrong; else ++ok;
  if (branch(10, 2, v1)) ++ok; else ++wrong;
  if (branch(11, 2, v1)) ++ok; else ++wrong;
  if (branch(12, 2, v1)) ++wrong; else ++ok;
  if (branch(13, 2, v1)) ++wrong; else ++ok;
  if (branch(14, 2, v1)) ++ok; else ++wrong;
  if (branch(15, 2, v1)) ++ok; else ++wrong;

  if (branch(0,  1, v1)) ++wrong; else ++ok;
  if (branch(1,  1, v1)) ++wrong; else ++ok;
  if (branch(2,  1, v1)) ++ok; else ++wrong;
  if (branch(3,  1, v1)) ++ok; else ++wrong;
  if (branch(4,  1, v1)) ++wrong; else ++ok;
  if (branch(5,  1, v1)) ++wrong; else ++ok;
  if (branch(6,  1, v1)) ++ok; else ++wrong;
  if (branch(7,  1, v1)) ++ok; else ++wrong;
  if (branch(8,  1, v1)) ++wrong; else ++ok;
  if (branch(9,  1, v1)) ++wrong; else ++ok;
  if (branch(10, 1, v1)) ++ok; else ++wrong;
  if (branch(11, 1, v1)) ++ok; else ++wrong;
  if (branch(12, 1, v1)) ++wrong; else ++ok;
  if (branch(13, 1, v1)) ++wrong; else ++ok;
  if (branch(14, 1, v1)) ++ok; else ++wrong;
  if (branch(15, 1, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 64)
    printf("FAILED\n");
  else
    printf("OK\n");
}

/* loads two bytes */
void
icm_two_bytes_msb_not_set(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #8  load two bytes, msb not set  --> cc == 2\n");

  v1 = 0x7FFFFFFF;
  wrong = ok = 0;

  if (branch(0,  12, v1)) ++wrong; else ++ok;
  if (branch(1,  12, v1)) ++wrong; else ++ok;
  if (branch(2,  12, v1)) ++ok; else ++wrong;
  if (branch(3,  12, v1)) ++ok; else ++wrong;
  if (branch(4,  12, v1)) ++wrong; else ++ok;
  if (branch(5,  12, v1)) ++wrong; else ++ok;
  if (branch(6,  12, v1)) ++ok; else ++wrong;
  if (branch(7,  12, v1)) ++ok; else ++wrong;
  if (branch(8,  12, v1)) ++wrong; else ++ok;
  if (branch(9,  12, v1)) ++wrong; else ++ok;
  if (branch(10, 12, v1)) ++ok; else ++wrong;
  if (branch(11, 12, v1)) ++ok; else ++wrong;
  if (branch(12, 12, v1)) ++wrong; else ++ok;
  if (branch(13, 12, v1)) ++wrong; else ++ok;
  if (branch(14, 12, v1)) ++ok; else ++wrong;
  if (branch(15, 12, v1)) ++ok; else ++wrong;

  if (branch(0,  10, v1)) ++wrong; else ++ok;
  if (branch(1,  10, v1)) ++wrong; else ++ok;
  if (branch(2,  10, v1)) ++ok; else ++wrong;
  if (branch(3,  10, v1)) ++ok; else ++wrong;
  if (branch(4,  10, v1)) ++wrong; else ++ok;
  if (branch(5,  10, v1)) ++wrong; else ++ok;
  if (branch(6,  10, v1)) ++ok; else ++wrong;
  if (branch(7,  10, v1)) ++ok; else ++wrong;
  if (branch(8,  10, v1)) ++wrong; else ++ok;
  if (branch(9,  10, v1)) ++wrong; else ++ok;
  if (branch(10, 10, v1)) ++ok; else ++wrong;
  if (branch(11, 10, v1)) ++ok; else ++wrong;
  if (branch(12, 10, v1)) ++wrong; else ++ok;
  if (branch(13, 10, v1)) ++wrong; else ++ok;
  if (branch(14, 10, v1)) ++ok; else ++wrong;
  if (branch(15, 10, v1)) ++ok; else ++wrong;

  if (branch(0,   9, v1)) ++wrong; else ++ok;
  if (branch(1,   9, v1)) ++wrong; else ++ok;
  if (branch(2,   9, v1)) ++ok; else ++wrong;
  if (branch(3,   9, v1)) ++ok; else ++wrong;
  if (branch(4,   9, v1)) ++wrong; else ++ok;
  if (branch(5,   9, v1)) ++wrong; else ++ok;
  if (branch(6,   9, v1)) ++ok; else ++wrong;
  if (branch(7,   9, v1)) ++ok; else ++wrong;
  if (branch(8,   9, v1)) ++wrong; else ++ok;
  if (branch(9,   9, v1)) ++wrong; else ++ok;
  if (branch(10,  9, v1)) ++ok; else ++wrong;
  if (branch(11,  9, v1)) ++ok; else ++wrong;
  if (branch(12,  9, v1)) ++wrong; else ++ok;
  if (branch(13,  9, v1)) ++wrong; else ++ok;
  if (branch(14,  9, v1)) ++ok; else ++wrong;
  if (branch(15,  9, v1)) ++ok; else ++wrong;

  if (branch(0,   6, v1)) ++wrong; else ++ok;
  if (branch(1,   6, v1)) ++wrong; else ++ok;
  if (branch(2,   6, v1)) ++ok; else ++wrong;
  if (branch(3,   6, v1)) ++ok; else ++wrong;
  if (branch(4,   6, v1)) ++wrong; else ++ok;
  if (branch(5,   6, v1)) ++wrong; else ++ok;
  if (branch(6,   6, v1)) ++ok; else ++wrong;
  if (branch(7,   6, v1)) ++ok; else ++wrong;
  if (branch(8,   6, v1)) ++wrong; else ++ok;
  if (branch(9,   6, v1)) ++wrong; else ++ok;
  if (branch(10,  6, v1)) ++ok; else ++wrong;
  if (branch(11,  6, v1)) ++ok; else ++wrong;
  if (branch(12,  6, v1)) ++wrong; else ++ok;
  if (branch(13,  6, v1)) ++wrong; else ++ok;
  if (branch(14,  6, v1)) ++ok; else ++wrong;
  if (branch(15,  6, v1)) ++ok; else ++wrong;

  if (branch(0,   5, v1)) ++wrong; else ++ok;
  if (branch(1,   5, v1)) ++wrong; else ++ok;
  if (branch(2,   5, v1)) ++ok; else ++wrong;
  if (branch(3,   5, v1)) ++ok; else ++wrong;
  if (branch(4,   5, v1)) ++wrong; else ++ok;
  if (branch(5,   5, v1)) ++wrong; else ++ok;
  if (branch(6,   5, v1)) ++ok; else ++wrong;
  if (branch(7,   5, v1)) ++ok; else ++wrong;
  if (branch(8,   5, v1)) ++wrong; else ++ok;
  if (branch(9,   5, v1)) ++wrong; else ++ok;
  if (branch(10,  5, v1)) ++ok; else ++wrong;
  if (branch(11,  5, v1)) ++ok; else ++wrong;
  if (branch(12,  5, v1)) ++wrong; else ++ok;
  if (branch(13,  5, v1)) ++wrong; else ++ok;
  if (branch(14,  5, v1)) ++ok; else ++wrong;
  if (branch(15,  5, v1)) ++ok; else ++wrong;

  if (branch(0,   3, v1)) ++wrong; else ++ok;
  if (branch(1,   3, v1)) ++wrong; else ++ok;
  if (branch(2,   3, v1)) ++ok; else ++wrong;
  if (branch(3,   3, v1)) ++ok; else ++wrong;
  if (branch(4,   3, v1)) ++wrong; else ++ok;
  if (branch(5,   3, v1)) ++wrong; else ++ok;
  if (branch(6,   3, v1)) ++ok; else ++wrong;
  if (branch(7,   3, v1)) ++ok; else ++wrong;
  if (branch(8,   3, v1)) ++wrong; else ++ok;
  if (branch(9,   3, v1)) ++wrong; else ++ok;
  if (branch(10,  3, v1)) ++ok; else ++wrong;
  if (branch(11,  3, v1)) ++ok; else ++wrong;
  if (branch(12,  3, v1)) ++wrong; else ++ok;
  if (branch(13,  3, v1)) ++wrong; else ++ok;
  if (branch(14,  3, v1)) ++ok; else ++wrong;
  if (branch(15,  3, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 96)
    printf("FAILED\n");
  else
    printf("OK\n");
}


/* loads three bytes */
void
icm_three_bytes_msb_not_set(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #9  load three bytes, msb not set  --> cc == 2\n");

  v1 = 0x7FFFFFFF;
  wrong = ok = 0;

  if (branch(0,  14, v1)) ++wrong; else ++ok;
  if (branch(1,  14, v1)) ++wrong; else ++ok;
  if (branch(2,  14, v1)) ++ok; else ++wrong;
  if (branch(3,  14, v1)) ++ok; else ++wrong;
  if (branch(4,  14, v1)) ++wrong; else ++ok;
  if (branch(5,  14, v1)) ++wrong; else ++ok;
  if (branch(6,  14, v1)) ++ok; else ++wrong;
  if (branch(7,  14, v1)) ++ok; else ++wrong;
  if (branch(8,  14, v1)) ++wrong; else ++ok;
  if (branch(9,  14, v1)) ++wrong; else ++ok;
  if (branch(10, 14, v1)) ++ok; else ++wrong;
  if (branch(11, 14, v1)) ++ok; else ++wrong;
  if (branch(12, 14, v1)) ++wrong; else ++ok;
  if (branch(13, 14, v1)) ++wrong; else ++ok;
  if (branch(14, 14, v1)) ++ok; else ++wrong;
  if (branch(15, 14, v1)) ++ok; else ++wrong;

  if (branch(0,  13, v1)) ++wrong; else ++ok;
  if (branch(1,  13, v1)) ++wrong; else ++ok;
  if (branch(2,  13, v1)) ++ok; else ++wrong;
  if (branch(3,  13, v1)) ++ok; else ++wrong;
  if (branch(4,  13, v1)) ++wrong; else ++ok;
  if (branch(5,  13, v1)) ++wrong; else ++ok;
  if (branch(6,  13, v1)) ++ok; else ++wrong;
  if (branch(7,  13, v1)) ++ok; else ++wrong;
  if (branch(8,  13, v1)) ++wrong; else ++ok;
  if (branch(9,  13, v1)) ++wrong; else ++ok;
  if (branch(10, 13, v1)) ++ok; else ++wrong;
  if (branch(11, 13, v1)) ++ok; else ++wrong;
  if (branch(12, 13, v1)) ++wrong; else ++ok;
  if (branch(13, 13, v1)) ++wrong; else ++ok;
  if (branch(14, 13, v1)) ++ok; else ++wrong;
  if (branch(15, 13, v1)) ++ok; else ++wrong;

  if (branch(0,  11, v1)) ++wrong; else ++ok;
  if (branch(1,  11, v1)) ++wrong; else ++ok;
  if (branch(2,  11, v1)) ++ok; else ++wrong;
  if (branch(3,  11, v1)) ++ok; else ++wrong;
  if (branch(4,  11, v1)) ++wrong; else ++ok;
  if (branch(5,  11, v1)) ++wrong; else ++ok;
  if (branch(6,  11, v1)) ++ok; else ++wrong;
  if (branch(7,  11, v1)) ++ok; else ++wrong;
  if (branch(8,  11, v1)) ++wrong; else ++ok;
  if (branch(9,  11, v1)) ++wrong; else ++ok;
  if (branch(10, 11, v1)) ++ok; else ++wrong;
  if (branch(11, 11, v1)) ++ok; else ++wrong;
  if (branch(12, 11, v1)) ++wrong; else ++ok;
  if (branch(13, 11, v1)) ++wrong; else ++ok;
  if (branch(14, 11, v1)) ++ok; else ++wrong;
  if (branch(15, 11, v1)) ++ok; else ++wrong;

  if (branch(0,   7, v1)) ++wrong; else ++ok;
  if (branch(1,   7, v1)) ++wrong; else ++ok;
  if (branch(2,   7, v1)) ++ok; else ++wrong;
  if (branch(3,   7, v1)) ++ok; else ++wrong;
  if (branch(4,   7, v1)) ++wrong; else ++ok;
  if (branch(5,   7, v1)) ++wrong; else ++ok;
  if (branch(6,   7, v1)) ++ok; else ++wrong;
  if (branch(7,   7, v1)) ++ok; else ++wrong;
  if (branch(8,   7, v1)) ++wrong; else ++ok;
  if (branch(9,   7, v1)) ++wrong; else ++ok;
  if (branch(10,  7, v1)) ++ok; else ++wrong;
  if (branch(11,  7, v1)) ++ok; else ++wrong;
  if (branch(12,  7, v1)) ++wrong; else ++ok;
  if (branch(13,  7, v1)) ++wrong; else ++ok;
  if (branch(14,  7, v1)) ++ok; else ++wrong;
  if (branch(15,  7, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 64)
    printf("FAILED\n");
  else
    printf("OK\n");
}

/* loads four bytes */
void
icm_four_bytes_msb_not_set(void)
{
  int wrong, ok;
  unsigned v1;

  printf("Test #10  load four bytes, msb not set  --> cc == 2\n");

  v1 = 0x7FFFFFFF;
  wrong = ok = 0;

  if (branch(0,  15, v1)) ++wrong; else ++ok;
  if (branch(1,  15, v1)) ++wrong; else ++ok;
  if (branch(2,  15, v1)) ++ok; else ++wrong;
  if (branch(3,  15, v1)) ++ok; else ++wrong;
  if (branch(4,  15, v1)) ++wrong; else ++ok;
  if (branch(5,  15, v1)) ++wrong; else ++ok;
  if (branch(6,  15, v1)) ++ok; else ++wrong;
  if (branch(7,  15, v1)) ++ok; else ++wrong;
  if (branch(8,  15, v1)) ++wrong; else ++ok;
  if (branch(9,  15, v1)) ++wrong; else ++ok;
  if (branch(10, 15, v1)) ++ok; else ++wrong;
  if (branch(11, 15, v1)) ++ok; else ++wrong;
  if (branch(12, 15, v1)) ++wrong; else ++ok;
  if (branch(13, 15, v1)) ++wrong; else ++ok;
  if (branch(14, 15, v1)) ++ok; else ++wrong;
  if (branch(15, 15, v1)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

int main()
{
  icm_mask_0();
  icm_value_0();

  icm_one_byte_msb_set();
  icm_two_bytes_msb_set();
  icm_three_bytes_msb_set();
  icm_four_bytes_msb_set();

  icm_one_byte_msb_not_set();
  icm_two_bytes_msb_not_set();
  icm_three_bytes_msb_not_set();
  icm_four_bytes_msb_not_set();

  return 0;
}
