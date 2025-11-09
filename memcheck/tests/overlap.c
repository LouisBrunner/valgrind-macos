#include <string.h>
#include <stdio.h>

char b[50];

void reset_b(void)
{
   int i;

   for (i = 0; i < 50; i++)
      b[i] = '_';
   b[49] = '\0';
}

void reset_b2(void)
{
   reset_b();
   strcpy(b, "ABCDEFG");
}

int main(void)
{
   char x[100];
   char a[] = "abcdefghijklmnopqrstuvwxyz";
   int  i;

   /* testing memcpy/strcpy overlap */

   for (i = 0; i < 50; i++) {
      x[i] = i+1;    // don't put any zeroes in there
   }
   for (i = 50; i < 100; i++) {
      // because of the errors, the strcpy's will overrun, so put some
      // zeroes in the second half to stop them eventually
      x[i] = 0;  
               
   }

   memcpy(x+20, x, 20);    // ok
   memcpy(x+20, x, 21);    // overlap
   memcpy(x, x+20, 20);    // ok
   memcpy(x, x+20, 21);    // overlap

   strncpy(x+20, x, 20);    // ok
   strncpy(x+20, x, 21);    // overlap
   strncpy(x, x+20, 20);    // ok
   strncpy(x, x+20, 21);    // overlap
   
   x[39] = '\0';
   strcpy(x, x+20);    // ok

   x[39] = 39;
   x[40] = '\0';
   strcpy(x, x+20);    // overlap

   x[19] = '\0';
   strcpy(x+20, x);    // ok

/*
   x[19] = 19;
   x[20] = '\0';
   strcpy(x+20, x);    // overlap, but runs forever (or until it seg faults)
*/

   /* testing strcpy, strncpy() */

   reset_b();
   printf("`%s'\n", b);

   strcpy(b, a);
   printf("`%s'\n", b);
   
   reset_b();
   strncpy(b, a, 25);
   printf("`%s'\n", b);

   reset_b();
   strncpy(b, a, 26);
   printf("`%s'\n", b);

   reset_b();
   strncpy(b, a, 27);
   printf("`%s'\n", b);

   printf("\n");

   /* testing strncat() */

   reset_b2();
   printf("`%s'\n", b);
   
   reset_b2();
   strcat(b, a);
   printf("`%s'\n", b);
   
   reset_b2();
   strncat(b, a, 25);
   printf("`%s'\n", b);
   
   reset_b2();
   strncat(b, a, 26);
   printf("`%s'\n", b);
   
   reset_b2();
   strncat(b, a, 27);
   printf("`%s'\n", b);

   /* Nb: can't actually get strcat warning -- if any overlap occurs, it will
      always run forever, I think... */

   for ( i = 0; i < 2; i++) 
      strncat(a+20, a, 21);    // run twice to check 2nd error isn't shown
   strncat(a, a+20, 21);

   /* This is ok, but once gave a warning when strncpy() was wrong,
      and used 'n' for the length, even when the src was shorter than 'n' */
   {
      char dest[64];
      char src [16];
      strcpy( src, "short" );
      strncpy( dest, src, 20 );
   }

   return 0;
}
