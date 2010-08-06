
#include <stdio.h>

typedef  unsigned int            UInt;
typedef  unsigned long long int  ULong;

void cpuid ( UInt* eax, UInt* ebx, UInt* ecx, UInt* edx, 
             UInt index, UInt ecx_in )
{
   UInt a,b,c,d;
   asm volatile ("cpuid"
                 : "=a" (a), "=b" (b), "=c" (c), "=d" (d) \
                 : "0" (index), "2"(ecx_in) );
   *eax = a; *ebx = b; *ecx = c; *edx = d;
   printf("%08x %08x -> %08x %08x %08x %08x\n",
          index,ecx_in, a,b,c,d );
}

int main ( void )
{
  UInt eax, ebx, ecx, edx;
  UInt maxidx, maxextidx, i,ecx_in;

  printf("\n");
  cpuid(&eax,&ebx,&ecx,&edx, 0,0);
  maxidx = eax;
  for (i = 1; i <= maxidx +2; i++) {

    cpuid(&eax,&ebx,&ecx,&edx, i,0);

    if (i == 4) {
      printf("\n");
      for (ecx_in = 1; ecx_in < 10; ecx_in++) {
         cpuid(&eax,&ebx,&ecx,&edx, i,ecx_in);
      }
      printf("\n");
    }

    if (i == 0xb) {
      printf("\n");
      for (ecx_in = 1; ecx_in < 10; ecx_in++) {
         cpuid(&eax,&ebx,&ecx,&edx, i,ecx_in);
      }
      printf("\n");
    }

    if (i == 0xd) {
      printf("\n");
      for (ecx_in = 1; ecx_in < 5; ecx_in++) {
         cpuid(&eax,&ebx,&ecx,&edx, i,ecx_in);
      }
      printf("\n");
    }


  }

  printf("\n");

  cpuid(&eax,&ebx,&ecx,&edx, 0x80000000,0);
  maxextidx = eax;
  for (i = 0x80000001; i <= maxextidx +2; i++) {
     cpuid(&eax,&ebx,&ecx,&edx, i,0);
  }

  printf("invalid\n");
  cpuid(&eax,&ebx,&ecx,&edx, 1234,0);
  cpuid(&eax,&ebx,&ecx,&edx, 0x800004d3,0);


  return 0;
}
