
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int rl_insert ( int, int );

int main ( void )
{
   rl_insert(1, 'z');

   return 0;
}

int zzzstrlen ( char* str )
{
   if (str[1] == 0) return 2; else return 10;
}

int rl_insert ( int count, int c )
{
   char str[2];
   str[1] = 0;
   str[0] = c;
   printf("HERE strlen  is %d\n", zzzstrlen(str));
   return 0;
}
