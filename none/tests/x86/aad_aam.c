/* This tests the somewhat obscure 32-bit Intel aam and aad instructions */
/* by Vince Weaver (vince _at_ deater.net ) */

#include <stdio.h>

int parity(int v) {

    int i;
    int p = 1;

    for (i = 0; i < 8; i++)
      p ^= (1 & (v >> i));
    return p;
}

int main(int argc, char **argv) {

  printf("test begins\n");
  unsigned short i,out;
  unsigned int flags;
  int cf __attribute__((unused)),pf,af __attribute__((unused)),zf,sf;
  int of __attribute__((unused));
  
  /* test AAM */

  for(i=0;i<65535;i++) {
    // printf("%d, %d, %d\n",i,(i&0xff)/10,(i&0xff)%10);
    out=i;
    __asm__ __volatile__ ("mov %2 ,%%ax\n" 
			  "aam\n"
			  "pushf\n"
			  "mov %%ax, %0\n"
			  "pop %%eax\n"
			  "mov %%eax, %1\n"
			  :"=r"(out), "=r"(flags)  /* outputs */
			  :"r"(out)     /* input */
			  :"%eax"     /* clobbered */
    );
    cf=!!(flags&0x1);
    pf=!!(flags&0x4);
    af=!!(flags&0x10);
    zf=!!(flags&0x40);
    sf=!!(flags&0x80);
    of=!!(flags&0x800);

  //    printf("%d, %d, %d, ",i,(out>>8)&0xff,out&0xff);
  //  printf("%x CF=%d PF=%d AF=%d ZF=%d SF=%d OF=%d\n",
  //	   flags,cf,pf,af,zf,sf,of);

    if (zf && ((out&0xff)!=0)) {
      printf("Error with aam (zf)!\n");
    }
    if (pf != parity(out&0xff)) {
      printf("Error with aam (pf)!\n");
    }
    if (sf != !!(out&0x80)) {
      printf("Error with aam (sf)!\n");
    }


    if ( ((out>>8)&0xff) != ((i&0xff)/10)) {
      printf("Error with aam!\n");
    }
    if ( (out&0xff) != ((i&0xff)%10)) {
      printf("Error with aam!\n");
    }

  }

  /* test AAD */

  for(i=0;i<65535;i++) {
    //    printf("%x, %d\n",i, ((((i>>8)&0xff)*10)+(i&0xff))&0xff );
    out=i;
    __asm__ __volatile__ ("mov %2 ,%%ax\n" 
			  "aad\n"
			  "pushf\n"
			  "mov %%ax, %0\n"
			  "pop %%eax\n"
			  "mov %%eax, %1\n"
			  :"=r"(out), "=r"(flags)  /* outputs */
			  :"r"(out)     /* input */
			  :"%eax"     /* clobbered */
);

    cf=!!(flags&0x1);
    pf=!!(flags&0x4);
    af=!!(flags&0x10);
    zf=!!(flags&0x40);
    sf=!!(flags&0x80);
    of=!!(flags&0x800);

    //       printf("%x, %d ",i,out);
    //   printf("%x CF=%d PF=%d AF=%d ZF=%d SF=%d OF=%d\n",
    //	   flags,cf,pf,af,zf,sf,of);

    if (zf && ((out&0xff)!=0)) {
      printf("Error with aad (zf)!\n");
    }
    if (pf != parity(out&0xff)) {
      printf("Error with aad (pf)!\n");
    }
    if (sf != !!(out&0x80)) {
      printf("Error with aad (sf) %d %d!\n",sf,!!(out&0x80));
    }

    if ( out != ( ((((i>>8)&0xff)*10)+(i&0xff))&0xff) ) {
       printf("Error with aad!\n");
    }
  }

  printf("test completed\n");
  return 0;

}
