#include <stdio.h>

#define branch(mask,_v1,_v2)                           \
  ({                                                   \
        unsigned char taken;                           \
        unsigned b1 = _v1;                             \
        unsigned b2 = _v2;                             \
        asm volatile(   "       alr %[b1],%[b2]\n\t"   \
                 	"	brc " #mask " ,1f\n\t" \
                        "       mvi %[taken],0\n\t"    \
			"	j   0f\n\t"            \
			"1:	mvi %[taken],1\n\t"    \
			"0:	bcr 0,0 /* nop */\n\t" \
             : [taken] "=Q" (taken), [b1] "+d"(b1)     \
             : [b2] "d"(b2)                            \
             : "cc");                                  \
        taken;                                         \
   })


void
alr_cc_0(void)
{
  unsigned v1, v2;
  int wrong, ok;

  printf("Test #1  cc == 0\n");

  v1 = v2 = 0;
  wrong = ok = 0;
  if (branch(0,  v1, v2)) ++wrong; else ++ok;
  if (branch(1,  v1, v2)) ++wrong; else ++ok;
  if (branch(2,  v1, v2)) ++wrong; else ++ok;
  if (branch(3,  v1, v2)) ++wrong; else ++ok;
  if (branch(4,  v1, v2)) ++wrong; else ++ok;
  if (branch(5,  v1, v2)) ++wrong; else ++ok;
  if (branch(6,  v1, v2)) ++wrong; else ++ok;
  if (branch(7,  v1, v2)) ++wrong; else ++ok;
  if (branch(8,  v1, v2)) ++ok; else ++wrong;
  if (branch(9,  v1, v2)) ++ok; else ++wrong;
  if (branch(10, v1, v2)) ++ok; else ++wrong;
  if (branch(11, v1, v2)) ++ok; else ++wrong;
  if (branch(12, v1, v2)) ++ok; else ++wrong;
  if (branch(13, v1, v2)) ++ok; else ++wrong;
  if (branch(14, v1, v2)) ++ok; else ++wrong;
  if (branch(15, v1, v2)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
alr_cc_1(void)
{
  unsigned v1, v2;
  int wrong, ok;

  printf("Test #2  cc == 1\n");

  v1 = v2 = 42;
  wrong = ok = 0;

  if (branch(0,  v1, v2)) ++wrong; else ++ok;
  if (branch(1,  v1, v2)) ++wrong; else ++ok;
  if (branch(2,  v1, v2)) ++wrong; else ++ok;
  if (branch(3,  v1, v2)) ++wrong; else ++ok;
  if (branch(4,  v1, v2)) ++ok; else ++wrong;
  if (branch(5,  v1, v2)) ++ok; else ++wrong;
  if (branch(6,  v1, v2)) ++ok; else ++wrong;
  if (branch(7,  v1, v2)) ++ok; else ++wrong;
  if (branch(8,  v1, v2)) ++wrong; else ++ok;
  if (branch(9,  v1, v2)) ++wrong; else ++ok;
  if (branch(10, v1, v2)) ++wrong; else ++ok;
  if (branch(11, v1, v2)) ++wrong; else ++ok;
  if (branch(12, v1, v2)) ++ok; else ++wrong;
  if (branch(13, v1, v2)) ++ok; else ++wrong;
  if (branch(14, v1, v2)) ++ok; else ++wrong;
  if (branch(15, v1, v2)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
alr_cc_2(void)
{
  unsigned v1, v2;
  int wrong, ok;

  printf("Test #3  cc == 2\n");

  v1 = 0xFFFFFFFF;
  v2 = 1;

  wrong = ok = 0;

  if (branch(0,  v1, v2)) ++wrong; else ++ok;
  if (branch(1,  v1, v2)) ++wrong; else ++ok;
  if (branch(2,  v1, v2)) ++ok; else ++wrong;
  if (branch(3,  v1, v2)) ++ok; else ++wrong;
  if (branch(4,  v1, v2)) ++wrong; else ++ok;
  if (branch(5,  v1, v2)) ++wrong; else ++ok;
  if (branch(6,  v1, v2)) ++ok; else ++wrong;
  if (branch(7,  v1, v2)) ++ok; else ++wrong;
  if (branch(8,  v1, v2)) ++wrong; else ++ok;
  if (branch(9,  v1, v2)) ++wrong; else ++ok;
  if (branch(10, v1, v2)) ++ok; else ++wrong;
  if (branch(11, v1, v2)) ++ok; else ++wrong;
  if (branch(12, v1, v2)) ++wrong; else ++ok;
  if (branch(13, v1, v2)) ++wrong; else ++ok;
  if (branch(14, v1, v2)) ++ok; else ++wrong;
  if (branch(15, v1, v2)) ++ok; else ++wrong;

  if (wrong != 0 /* || ok != 16 */)
    printf("FAILED\n");
  else
    printf("OK\n");
}

void
alr_cc_3(void)
{
  unsigned v1, v2;
  int wrong, ok;

  printf("Test #4  cc == 3\n");

  v1 = 0xFFFFFFFF;
  v2 = 100;

  wrong = ok = 0;

  if (branch(0,  v1, v2)) ++wrong; else ++ok;
  if (branch(1,  v1, v2)) ++ok; else ++wrong;
  if (branch(2,  v1, v2)) ++wrong; else ++ok;
  if (branch(3,  v1, v2)) ++ok; else ++wrong;
  if (branch(4,  v1, v2)) ++wrong; else ++ok;
  if (branch(5,  v1, v2)) ++ok; else ++wrong;
  if (branch(6,  v1, v2)) ++wrong; else ++ok;
  if (branch(7,  v1, v2)) ++ok; else ++wrong;
  if (branch(8,  v1, v2)) ++wrong; else ++ok;
  if (branch(9,  v1, v2)) ++ok; else ++wrong;
  if (branch(10, v1, v2)) ++wrong; else ++ok;
  if (branch(11, v1, v2)) ++ok; else ++wrong;
  if (branch(12, v1, v2)) ++wrong; else ++ok;
  if (branch(13, v1, v2)) ++ok; else ++wrong;
  if (branch(14, v1, v2)) ++wrong; else ++ok;
  if (branch(15, v1, v2)) ++ok; else ++wrong;

  if (wrong != 0 || ok != 16)
    printf("FAILED\n");
  else
    printf("OK\n");
}

int main()
{
  alr_cc_0();
  alr_cc_1();
  alr_cc_2();
  alr_cc_3();

  return 0;
}
