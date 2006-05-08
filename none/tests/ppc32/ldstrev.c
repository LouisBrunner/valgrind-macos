
#include <stdio.h>

typedef  unsigned int    UInt;
typedef  unsigned short  UShort;

UInt read16le ( void* a )
{
  UInt res;
  __asm volatile(
    "   lhbrx   %0,0,%1         \n"     // Get half word and reverse the bytes
    : "=b" (res)  // %0 - Output operand
    : "b" (a)        // %1 - Input operand
    : "memory"       // Consider memory clobberred for aliasing
  );
  return res;
}

UInt read16be ( void* a )
{
  UInt res;
  __asm volatile(
    "   lhzx   %0,0,%1         \n"     // Get half word and reverse the bytes
    : "=b" (res)  // %0 - Output operand
    : "b" (a)        // %1 - Input operand
    : "memory"       // Consider memory clobberred for aliasing
  );
  return res;
}

UInt read32le ( void* a )
{
  UInt res;
  __asm volatile(
    "   lwbrx   %0,0,%1         \n"     // Get half word and reverse the bytes
    : "=b" (res)  // %0 - Output operand
    : "b" (a)        // %1 - Input operand
    : "memory"       // Consider memory clobberred for aliasing
  );
  return res;
}

UInt read32be ( void* a )
{
  UInt res;
  __asm volatile(
    "   lwzx   %0,0,%1         \n"     // Get half word and reverse the bytes
    : "=b" (res)  // %0 - Output operand
    : "b" (a)        // %1 - Input operand
    : "memory"       // Consider memory clobberred for aliasing
  );
  return res;
}

void write16be ( void* a, UInt data )
{
  __asm volatile(
    "  sthx %0,0,%1\n"
    :
    : "b" (data), "b" (a)
    : "memory" 
  );
}

void write16le ( void* a, UInt data )
{
  __asm volatile(
    "  sthbrx %0,0,%1\n"
    :
    : "b" (data), "b" (a)
    : "memory" 
  );
}

void write32be ( void* a, UInt data )
{
  __asm volatile(
    "  stwx %0,0,%1\n"
    :
    : "b" (data), "b" (a)
    : "memory" 
  );
}

void write32le ( void* a, UInt data )
{
  __asm volatile(
    "  stwbrx %0,0,%1\n"
    :
    : "b" (data), "b" (a)
    : "memory" 
  );
}

int main ( void )
{
   UInt foo = 0x12345678;  
   printf("ld be16 0x%08x\n", read16be( &foo ));
   printf("ld le16 0x%08x\n", read16le( &foo ));
   printf("ld be32 0x%08x\n", read32be( &foo ));
   printf("ld le32 0x%08x\n", read32le( &foo ));

   foo = 0x12345678; write16be( &foo, 0xABCD );
   printf("st be16 0x%08x\n", foo);

   foo = 0x12345678; write16le( &foo, 0xABCD );
   printf("st le16 0x%08x\n", foo);

   foo = 0x12345678; write32be( &foo, 0xABCD1425 );
   printf("st be32 0x%08x\n", foo);

   foo = 0x12345678; write32le( &foo, 0xABCD1425 );
   printf("st le32 0x%08x\n", foo);

   return 0;
}
