
/* Check basic stack overflow detection.

   It's difficult to get consistent behaviour across all platforms.
   For example, x86 w/ gcc-4.3.1 gives

     Expected: stack array "a" in frame 2 back from here
     Actual:   stack array "beforea" in frame 2 back from here

   whereas amd64 w/ gcc-4.3.1 gives

     Expected: stack array "a" in frame 2 back from here
     Actual:   unknown

   This happens because on x86 the arrays are placed on the
   stack without holes in between, but not so for amd64.  I don't
   know why.
*/


#include <stdio.h>

__attribute__((noinline)) void foo ( long* sa, int n )
{
  int i;
  for (i = 0; i < n; i++)
    sa[i] = 0;
}

__attribute__((noinline)) void bar ( long* sa, int n )
{
   foo(sa, n);
}

int main ( void )
{
  int i;
  long beforea[3];
  long a[7];
  long aftera[3];
  bar(a, 7+1);     /* generates error */
  bar(a, 7+0);     /* generates no error */
  for (i = 0; i < 7+1; i++) {
     a[i] = 0;
  }
 {char beforebuf[8];
  char buf[8];
  char afterbuf[8];
  sprintf(buf, "%d", 123456789);
  return 1 & ((a[4] + beforea[1] + aftera[1] + beforebuf[1] 
                    + buf[2] + afterbuf[3]) / 100000) ;
 }
}
