
/* To compile:
   gcc -Wall -g -o crc32 none/tests/arm64/crc32.c -march=armv8-a+crc
   -march=armv8-a+crc+crypto is also OK
*/

#include <stdio.h>
#include <malloc.h>  // memalign
#include <string.h>  // memset
#include <assert.h>

typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  signed long long int    Long;
typedef  unsigned long long int  ULong;

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)


#define TESTINST3(instruction, RMval, RNval, RD, RM, RN, carryin) \
{ \
   ULong out; \
   ULong nzcv_out; \
   ULong nzcv_in = (carryin ? (1<<29) : 0); \
   __asm__ __volatile__( \
      "msr nzcv,%4;" \
      "mov " #RM ",%2;" \
      "mov " #RN ",%3;" \
      instruction ";" \
      "mov %0," #RD ";" \
      "mrs %1,nzcv;" \
      : "=&r" (out), "=&r" (nzcv_out) \
      : "r" (RMval), "r" (RNval), "r" (nzcv_in) \
      : #RD, #RM, #RN, "cc", "memory" \
   ); \
   printf("%s :: rd %016llx rm %016llx, rn %016llx, " \
          "cin %d, nzcv %08llx %c%c%c%c\n",       \
      instruction, out, ((ULong)RMval), ((ULong)RNval), \
      carryin ? 1 : 0, \
      nzcv_out & 0xffff0000, \
      ((1<<31) & nzcv_out) ? 'N' : ' ', \
      ((1<<30) & nzcv_out) ? 'Z' : ' ', \
      ((1<<29) & nzcv_out) ? 'C' : ' ', \
      ((1<<28) & nzcv_out) ? 'V' : ' ' \
      ); \
}

int main ( void )
{
////////////////////////////////////////////////////////////////
printf("CRC32/CRC32C\n");

TESTINST3("crc32b w21,w20,w19", 0x4b154113f7d32514, 0xcce230caafbf9cc9, x21,x20,x19, 0);
TESTINST3("crc32b w21,w20,w19", 0x33d5d595721d4f13, 0xf4509311f443a7ce, x21,x20,x19, 0);
TESTINST3("crc32b w21,w20,w19", 0x4a3c6de6954cbc17, 0x111b21e39fbd7254, x21,x20,x19, 0);
TESTINST3("crc32b w21,w20,w19", 0xfbb5c64ed1b044c6, 0x33ca4c4fb3960326, x21,x20,x19, 0);
TESTINST3("crc32b w21,w20,w19", 0x2b7c5939d7c0f528, 0xb73870a5a6630162, x21,x20,x19, 0);
TESTINST3("crc32b w21,w20,w19", 0x02fe41918ac5cdba, 0x48e0815289728f05, x21,x20,x19, 0);
TESTINST3("crc32b w21,w20,w19", 0xb60a8f381f187bae, 0x008c208cc413ff72, x21,x20,x19, 0);

TESTINST3("crc32h w21,w20,w19", 0x4b154113f7d32514, 0xcce230caafbf9cc9, x21,x20,x19, 0);
TESTINST3("crc32h w21,w20,w19", 0x33d5d595721d4f13, 0xf4509311f443a7ce, x21,x20,x19, 0);
TESTINST3("crc32h w21,w20,w19", 0x4a3c6de6954cbc17, 0x111b21e39fbd7254, x21,x20,x19, 0);
TESTINST3("crc32h w21,w20,w19", 0xfbb5c64ed1b044c6, 0x33ca4c4fb3960326, x21,x20,x19, 0);
TESTINST3("crc32h w21,w20,w19", 0x2b7c5939d7c0f528, 0xb73870a5a6630162, x21,x20,x19, 0);
TESTINST3("crc32h w21,w20,w19", 0x02fe41918ac5cdba, 0x48e0815289728f05, x21,x20,x19, 0);
TESTINST3("crc32h w21,w20,w19", 0xb60a8f381f187bae, 0x008c208cc413ff72, x21,x20,x19, 0);

TESTINST3("crc32w w21,w20,w19", 0x4b154113f7d32514, 0xcce230caafbf9cc9, x21,x20,x19, 0);
TESTINST3("crc32w w21,w20,w19", 0x33d5d595721d4f13, 0xf4509311f443a7ce, x21,x20,x19, 0);
TESTINST3("crc32w w21,w20,w19", 0x4a3c6de6954cbc17, 0x111b21e39fbd7254, x21,x20,x19, 0);
TESTINST3("crc32w w21,w20,w19", 0xfbb5c64ed1b044c6, 0x33ca4c4fb3960326, x21,x20,x19, 0);
TESTINST3("crc32w w21,w20,w19", 0x2b7c5939d7c0f528, 0xb73870a5a6630162, x21,x20,x19, 0);
TESTINST3("crc32w w21,w20,w19", 0x02fe41918ac5cdba, 0x48e0815289728f05, x21,x20,x19, 0);
TESTINST3("crc32w w21,w20,w19", 0xb60a8f381f187bae, 0x008c208cc413ff72, x21,x20,x19, 0);

TESTINST3("crc32x w21,w20,x19", 0x4b154113f7d32514, 0xcce230caafbf9cc9, x21,x20,x19, 0);
TESTINST3("crc32x w21,w20,x19", 0x33d5d595721d4f13, 0xf4509311f443a7ce, x21,x20,x19, 0);
TESTINST3("crc32x w21,w20,x19", 0x4a3c6de6954cbc17, 0x111b21e39fbd7254, x21,x20,x19, 0);
TESTINST3("crc32x w21,w20,x19", 0xfbb5c64ed1b044c6, 0x33ca4c4fb3960326, x21,x20,x19, 0);
TESTINST3("crc32x w21,w20,x19", 0x2b7c5939d7c0f528, 0xb73870a5a6630162, x21,x20,x19, 0);
TESTINST3("crc32x w21,w20,x19", 0x02fe41918ac5cdba, 0x48e0815289728f05, x21,x20,x19, 0);
TESTINST3("crc32x w21,w20,x19", 0xb60a8f381f187bae, 0x008c208cc413ff72, x21,x20,x19, 0);

TESTINST3("crc32cb w21,w20,w19", 0x4b154113f7d32514, 0xcce230caafbf9cc9, x21,x20,x19, 0);
TESTINST3("crc32cb w21,w20,w19", 0x33d5d595721d4f13, 0xf4509311f443a7ce, x21,x20,x19, 0);
TESTINST3("crc32cb w21,w20,w19", 0x4a3c6de6954cbc17, 0x111b21e39fbd7254, x21,x20,x19, 0);
TESTINST3("crc32cb w21,w20,w19", 0xfbb5c64ed1b044c6, 0x33ca4c4fb3960326, x21,x20,x19, 0);
TESTINST3("crc32cb w21,w20,w19", 0x2b7c5939d7c0f528, 0xb73870a5a6630162, x21,x20,x19, 0);
TESTINST3("crc32cb w21,w20,w19", 0x02fe41918ac5cdba, 0x48e0815289728f05, x21,x20,x19, 0);
TESTINST3("crc32cb w21,w20,w19", 0xb60a8f381f187bae, 0x008c208cc413ff72, x21,x20,x19, 0);

TESTINST3("crc32ch w21,w20,w19", 0x4b154113f7d32514, 0xcce230caafbf9cc9, x21,x20,x19, 0);
TESTINST3("crc32ch w21,w20,w19", 0x33d5d595721d4f13, 0xf4509311f443a7ce, x21,x20,x19, 0);
TESTINST3("crc32ch w21,w20,w19", 0x4a3c6de6954cbc17, 0x111b21e39fbd7254, x21,x20,x19, 0);
TESTINST3("crc32ch w21,w20,w19", 0xfbb5c64ed1b044c6, 0x33ca4c4fb3960326, x21,x20,x19, 0);
TESTINST3("crc32ch w21,w20,w19", 0x2b7c5939d7c0f528, 0xb73870a5a6630162, x21,x20,x19, 0);
TESTINST3("crc32ch w21,w20,w19", 0x02fe41918ac5cdba, 0x48e0815289728f05, x21,x20,x19, 0);
TESTINST3("crc32ch w21,w20,w19", 0xb60a8f381f187bae, 0x008c208cc413ff72, x21,x20,x19, 0);

TESTINST3("crc32cw w21,w20,w19", 0x4b154113f7d32514, 0xcce230caafbf9cc9, x21,x20,x19, 0);
TESTINST3("crc32cw w21,w20,w19", 0x33d5d595721d4f13, 0xf4509311f443a7ce, x21,x20,x19, 0);
TESTINST3("crc32cw w21,w20,w19", 0x4a3c6de6954cbc17, 0x111b21e39fbd7254, x21,x20,x19, 0);
TESTINST3("crc32cw w21,w20,w19", 0xfbb5c64ed1b044c6, 0x33ca4c4fb3960326, x21,x20,x19, 0);
TESTINST3("crc32cw w21,w20,w19", 0x2b7c5939d7c0f528, 0xb73870a5a6630162, x21,x20,x19, 0);
TESTINST3("crc32cw w21,w20,w19", 0x02fe41918ac5cdba, 0x48e0815289728f05, x21,x20,x19, 0);
TESTINST3("crc32cw w21,w20,w19", 0xb60a8f381f187bae, 0x008c208cc413ff72, x21,x20,x19, 0);

TESTINST3("crc32cx w21,w20,x19", 0x4b154113f7d32514, 0xcce230caafbf9cc9, x21,x20,x19, 0);
TESTINST3("crc32cx w21,w20,x19", 0x33d5d595721d4f13, 0xf4509311f443a7ce, x21,x20,x19, 0);
TESTINST3("crc32cx w21,w20,x19", 0x4a3c6de6954cbc17, 0x111b21e39fbd7254, x21,x20,x19, 0);
TESTINST3("crc32cx w21,w20,x19", 0xfbb5c64ed1b044c6, 0x33ca4c4fb3960326, x21,x20,x19, 0);
TESTINST3("crc32cx w21,w20,x19", 0x2b7c5939d7c0f528, 0xb73870a5a6630162, x21,x20,x19, 0);
TESTINST3("crc32cx w21,w20,x19", 0x02fe41918ac5cdba, 0x48e0815289728f05, x21,x20,x19, 0);
TESTINST3("crc32cx w21,w20,x19", 0xb60a8f381f187bae, 0x008c208cc413ff72, x21,x20,x19, 0);

return 0;
}
