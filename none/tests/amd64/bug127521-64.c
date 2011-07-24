#include <stdlib.h>
#include <stdio.h>

typedef unsigned int UInt;
typedef unsigned long long int ULong;

void do_cmpxchg8b ( /*OUT*/
                    ULong* rdxOut,   ULong* raxOut, 
		    ULong* memHiOut, ULong* memLoOut,
                    ULong* zOut,
                    /*IN*/
                    ULong rdxIn,   ULong raxIn, 
                    ULong memHiIn, ULong memLoIn,
                    ULong rcxIn,   ULong rbxIn )
{
   UInt mem[2];
   ULong block[6];
   mem[0] = (UInt)memLoIn;
   mem[1] = (UInt)memHiIn;
   block[0] = rdxIn;
   block[1] = raxIn;
   block[2] = rcxIn;
   block[3] = rbxIn;
   block[4] = (ULong)&mem[0];
   block[5] = ~(0ULL);
   __asm__ __volatile__(
          "movq %0,%%r11\n"
        "\tmovq  0(%%r11),%%rdx\n"
        "\tmovq  8(%%r11),%%rax\n"
        "\tmovq 16(%%r11),%%rcx\n"
        "\tmovq 24(%%r11),%%rbx\n"
        "\tmovq 32(%%r11),%%r10\n"
        "\tlock cmpxchg8b (%%r10)\n"
        "\tmovabsq $0,%%r10\n"
        "\tsetz %%r10b\n"
        "\tmovq %%r10,40(%%r11)\n"
        "\tmovq %%rdx,0(%%r11)\n"
        "\tmovq %%rax,8(%%r11)\n"
	  : /*out*/
	  : /*in*/ "r"(&block[0])
	  : /*trash*/ "%r11", "%r10", "%rax", "%rbx", "%rcx", "%rdx", 
                      "cc", "memory" );
    *rdxOut = block[0];
    *raxOut = block[1];
    *memLoOut = (ULong)mem[0];
    *memHiOut = (ULong)mem[1];
    *zOut = block[5];
}

void try8b ( ULong d, ULong a, ULong mHi, ULong mLo, ULong c, ULong b )
{
   ULong dd, aa, mmHi, mmLo, zz;
   do_cmpxchg8b( &dd, &aa, &mmHi, &mmLo, &zz,
		 d,a,mHi,mLo,c,b);
   printf(" Q d:a=%llx:%llx mem=%llx:%llx c:b=%llx:%llx "
          "-> z=%lld d:a=%llx:%llx mem=%llx:%llx\n",
	  d,a, mHi,mLo, c,b, zz, dd,aa, mmHi,mmLo );
}

void do_cmpxchg16b ( /*OUT*/
                     ULong* rdxOut,   ULong* raxOut, 
		     ULong* memHiOut, ULong* memLoOut,
                     ULong* zOut,
                     /*IN*/
                     ULong rdxIn,   ULong raxIn, 
                     ULong memHiIn, ULong memLoIn,
                     ULong rcxIn,   ULong rbxIn )
{
   ULong mem[2] __attribute__((aligned(16)));
   ULong block[6];
   mem[0] = memLoIn;
   mem[1] = memHiIn;
   block[0] = rdxIn;
   block[1] = raxIn;
   block[2] = rcxIn;
   block[3] = rbxIn;
   block[4] = (ULong)&mem[0];
   block[5] = ~(0ULL);
   __asm__ __volatile__(
          "movq %0,%%r11\n"
        "\tmovq  0(%%r11),%%rdx\n"
        "\tmovq  8(%%r11),%%rax\n"
        "\tmovq 16(%%r11),%%rcx\n"
        "\tmovq 24(%%r11),%%rbx\n"
        "\tmovq 32(%%r11),%%r10\n"
        "\t.byte 0xf0, 0x49, 0x0f, 0xc7, 0x0a\n" /* lock cmpxchg16b (%%r10) */
        "\tmovabsq $0,%%r10\n"
        "\tsetz %%r10b\n"
        "\tmovq %%r10,40(%%r11)\n"
        "\tmovq %%rdx,0(%%r11)\n"
        "\tmovq %%rax,8(%%r11)\n"
	  : /*out*/
	  : /*in*/ "r"(&block[0])
	  : /*trash*/ "%r11", "%r10", "%rax", "%rbx", "%rcx", "%rdx", 
                      "cc", "memory" );
    *rdxOut = block[0];
    *raxOut = block[1];
    *memLoOut = mem[0];
    *memHiOut = mem[1];
    *zOut = block[5];
}

void try16b ( ULong d, ULong a, ULong mHi, ULong mLo, ULong c, ULong b )
{
   ULong dd, aa, mmHi, mmLo, zz;
   do_cmpxchg16b( &dd, &aa, &mmHi, &mmLo, &zz,
		  d,a,mHi,mLo,c,b);
   printf("QQ d:a=%llx:%llx mem=%llx:%llx c:b=%llx:%llx "
          "-> z=%lld d:a=%llx:%llx mem=%llx:%llx\n",
	  d,a, mHi,mLo, c,b, zz, dd,aa, mmHi,mmLo );
}

int main(void)
{
   ULong z = 0xDEADBEEF00000000ULL;

   try8b( 0,1, 5,4, 3,2 );
   try8b( 0,1, 0,1, 3,2 );

   try8b( 0,1, 0,4, 3,2 );
   try8b( 0,1, 0,0, 3,2 );

   try8b( 0,1, 5,0, 3,2 );
   try8b( 0,1, 1,1, 3,2 );

   try8b( 0+z,1+z, 5+z,4+z, 3+z,2+z );
   try8b( 0+z,1+z, 0+z,1+z, 3+z,2+z );

   try8b( 0+z,1+z, 0+z,4+z, 3+z,2+z );
   try8b( 0+z,1+z, 0+z,0+z, 3+z,2+z );

   try8b( 0+z,1+z, 5+z,0+z, 3+z,2+z );
   try8b( 0+z,1+z, 1+z,1+z, 3+z,2+z );

   try16b( 0,1, 5,4, 3,2 );
   try16b( 0,1, 0,1, 3,2 );

   try16b( 0,1, 0,4, 3,2 );
   try16b( 0,1, 0,0, 3,2 );

   try16b( 0,1, 5,0, 3,2 );
   try16b( 0,1, 1,1, 3,2 );

   try16b( 0+z,1+z, 5+z,4+z, 3+z,2+z );
   try16b( 0+z,1+z, 0+z,1+z, 3+z,2+z );

   try16b( 0+z,1+z, 0+z,4+z, 3+z,2+z );
   try16b( 0+z,1+z, 0+z,0+z, 3+z,2+z );

   try16b( 0+z,1+z, 5+z,0+z, 3+z,2+z );
   try16b( 0+z,1+z, 1+z,1+z, 3+z,2+z );

   return 0;
}

