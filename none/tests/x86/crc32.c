
#include <stdlib.h>
#include <stdio.h>

typedef  signed int              Int;
typedef  unsigned int            UInt;
typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned long long int  ULong;


static inline UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}


static inline ULong randULong ( void )
{
   Int i;
   ULong r = 0;
   for (i = 0; i < 8; i++) {
      r = (r << 8) | (ULong)(0xFF & randUChar());
   }
   return r;
}

static inline UInt randUInt ( void )
{
   Int i;
   UInt r = 0;
   for (i = 0; i < 4; i++) {
      r = (r << 8) | (UInt)(0xFF & randUChar());
   }
   return r;
}

/////////////////////////////////////////////////////////////////

UInt do_s_crc32b ( UInt crcIn, UChar b )
{
   UInt i, crc = (b & 0xFF) ^ crcIn;
   for (i = 0; i < 8; i++)
      crc = (crc >> 1) ^ ((crc & 1) ? 0x82f63b78 : 0);
   return crc;
}

UInt do_s_crc32w ( UInt crcIn, UShort w )
{
   UInt i, crc = (w & 0xFFFF) ^ crcIn;
   for (i = 0; i < 16; i++)
      crc = (crc >> 1) ^ ((crc & 1) ? 0x82f63b78 : 0);
   return crc;
}

UInt do_s_crc32l ( UInt crcIn, UInt l )
{
   UInt i, crc = l ^ crcIn;
   for (i = 0; i < 32; i++)
      crc = (crc >> 1) ^ ((crc & 1) ? 0x82f63b78 : 0);
   return crc;
}

UInt do_h_crc32b ( UInt crcIn, UChar b )
{
   __asm__ __volatile__(
      "crc32b %%cl,%%esi\n\t"
      : "=S"(crcIn) : "0"(crcIn), "c"(b)
   );
   return crcIn;
}

UInt do_h_crc32w ( UInt crcIn, UShort w )
{
   __asm__ __volatile__(
      "crc32w %%cx,%%esi\n\t"
      : "=S"(crcIn) : "0"(crcIn), "c"(w)
   );
   return crcIn;
}

UInt do_h_crc32l ( UInt crcIn, UInt l )
{
   __asm__ __volatile__(
      "crc32l %%ecx,%%esi\n\t"
      : "=S"(crcIn) : "0"(crcIn), "c"(l)
   );
   return crcIn;
}

////////////////

UInt do_h_crc32b_mem ( UInt crcIn, UChar* a )
{
   __asm__ __volatile__(
      "crc32b (%2),%%esi\n\t"
      : "=S"(crcIn) : "0"(crcIn), "r"(a)
   );
   return crcIn;
}

UInt do_h_crc32w_mem ( UInt crcIn, UShort* a )
{
   __asm__ __volatile__(
      "crc32w (%2),%%esi\n\t"
      : "=S"(crcIn) : "0"(crcIn), "r"(a)
   );
   return crcIn;
}

UInt do_h_crc32l_mem ( UInt crcIn, UInt* a )
{
   __asm__ __volatile__(
      "crc32l (%2),%%esi\n\t"
      : "=S"(crcIn) : "0"(crcIn), "r"(a)
   );
   return crcIn;
}

void try_simple ( void )
{
   UInt c0 = 0xFFFFFFFF;
   UChar c = 0x42;

   UInt cs = do_s_crc32b(c0, c);
   UInt ch = do_h_crc32b(c0, c);
   printf("b  %08x %08x\n", cs, ch);

   UShort w = 0xed78;;
   cs = do_s_crc32w(c0, w);
   ch = do_h_crc32w(c0, w);
   printf("w  %08x %08x\n", cs, ch);

   UInt i = 0xCAFEBABE;
   cs = do_s_crc32l(c0, i);
   ch = do_h_crc32l(c0, i);
   printf("l  %08x %08x\n", cs, ch);
}

#define NMEM 1000
void try_mem ( void )
{
  UInt al, i;
  UChar* b = malloc(NMEM);
  for (i = 0; i < NMEM; i++)
     b[i] = (UChar)(i % 177);

  for (al = 0; al < 1; al++) {
     UInt crc = 0xFFFFFFFF;
     for (i = 0; i <= 1000-1-al; i += 1)
        crc = do_h_crc32b_mem( crc, &b[i+al] );
     printf("mem b misalign %d = %08x\n", al, crc);
  }

  for (al = 0; al < 2; al++) {
     UInt crc = 0xFFFFFFFF;
     for (i = 0; i <= 1000-2-al; i += 2)
       crc = do_h_crc32w_mem( crc, (UShort*)&b[i+al] );
     printf("mem w misalign %d = %08x\n", al, crc);
  }

  for (al = 0; al < 4; al++) {
     UInt crc = 0xFFFFFFFF;
     for (i = 0; i <= 1000-4-al; i += 4)
       crc = do_h_crc32l_mem( crc, (UInt*)&b[i+al] );
     printf("mem l misalign %d = %08x\n", al, crc);
  }

  free(b);
}

void try_misc ( void )
{
   UInt res = 0xAAAAAAAAU;
   __asm__ __volatile__(
      "movl $0x55555555, %%eax"  "\n\t"
      "movl $042, %%ebx"  "\n\t"
      "crc32b %%bl,%%eax"  "\n\t"
      "movl %%eax, %0"  "\n"
      : "=r"(res) : : "eax","ebx"
   );
   printf("try_misc 32bit-dst 0x%08x\n", res);
}

/////////////////////////////////////////////////////////////////

void test_CRC32_U8_x86 ( void )
{
   UInt block[4];
   Int i;
   UInt oszacp_mask = 0x8D5;
   for (i = 0; i < 10; i++) {
      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 0(%%eax), %%edx"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "crc32 %%dl,  %%ecx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "ecx", "edx"
      );
      printf("r crc32_u8  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);

      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "crc32b 0(%%eax), %%ecx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "ecx", "edx"
      );
      printf("m crc32_u8  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);
   }
}

void test_CRC32_U16_x86 ( void )
{
   UInt block[4];
   Int i;
   UInt oszacp_mask = 0x8D5;
   for (i = 0; i < 10; i++) {
      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 0(%%eax), %%edi"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "crc32 %%di,  %%ecx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "edi", "ecx", "edx"
      );
      printf("r crc32_u16  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);

      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "crc32w 0(%%eax), %%ecx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "ecx", "edx"
      );
      printf("m crc32_u16  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);
   }
}

void test_CRC32_U32_x86 ( void )
{
   UInt block[4];
   Int i;
   UInt oszacp_mask = 0x8D5;
   for (i = 0; i < 10; i++) {
      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 0(%%eax), %%edi"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "crc32 %%edi,  %%ecx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "edi", "ecx", "edx"
      );
      printf("r crc32_u32  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);

      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "crc32 0(%%eax), %%ecx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "ecx", "edx"
      );
      printf("m crc32_u32  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);
   }
}

/////////////////////////////////////////////////////////////////

int main ( int argc, char** argv )
{
   try_simple();
   try_mem();
   try_misc();
   test_CRC32_U8_x86();
   test_CRC32_U16_x86();
   test_CRC32_U32_x86();
   return 0;
}
