
#include <stdlib.h>
#include <stdio.h>

typedef  unsigned int            UInt;
typedef  unsigned long long int  ULong;
typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;


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

UInt do_s_crc32q ( UInt crcIn, ULong q )
{
   UInt crc = do_s_crc32l(crcIn, (UInt)q);
   return do_s_crc32l(crc, (UInt)(q >> 32));
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

UInt do_h_crc32q ( UInt crcIn, ULong q )
{
   __asm__ __volatile__(
      "crc32q %%rcx,%%rsi\n\t"
      : "=S"(crcIn) : "0"(crcIn), "c"(q)
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

UInt do_h_crc32q_mem ( UInt crcIn, ULong* a )
{
   __asm__ __volatile__(
      "crc32q (%2),%%rsi\n\t"
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

   ULong q = 0x0ddC0ffeeBadF00d;
   cs = do_s_crc32q(c0, q);
   ch = do_h_crc32q(c0, q);
   printf("q  %08x %08x\n", cs, ch);
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

  for (al = 0; al < 8; al++) {
     UInt crc = 0xFFFFFFFF;
     for (i = 0; i <= 1000-8-al; i += 8)
       crc = do_h_crc32q_mem( crc, (ULong*)&b[i+al] );
     printf("mem q misalign %d = %08x\n", al, crc);
  }

  free(b);
}

void try_misc ( void )
{
   ULong res = 0xAAAAAAAAAAAAAAAAULL;
   __asm__ __volatile__(
      "movabsq $0x5555555555555555, %%rax"  "\n\t"
      "movabsq $042, %%rbx"  "\n\t"
      "crc32b %%bl,%%rax"  "\n\t"
      "movq %%rax, %0"  "\n"
      : "=r"(res) : : "rax","rbx"
   );
   printf("try_misc 64bit-dst 0x%016llx\n", res);

   __asm__ __volatile__(
      "movabsq $0x5555555555555555, %%rax"  "\n\t"
      "movabsq $042, %%rbx"  "\n\t"
      "crc32b %%bl,%%eax"  "\n\t"
      "movq %%rax, %0"  "\n"
      : "=r"(res) : : "rax","rbx"
   );
   printf("try_misc 32bit-dst 0x%016llx\n", res);
}

/////////////////////////////////////////////////////////////////



int main ( int argc, char** argv )
{
   try_simple();
   try_mem();
   try_misc();
   return 0;
}
