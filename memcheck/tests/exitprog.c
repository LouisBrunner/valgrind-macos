

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define ZILLION 1000000

void foo ( void );
void bar ( void );

int main ( void )
{
   int i;
   char* a = malloc(ZILLION * sizeof(char));
   for (i = 0; i <= ZILLION; i++) {
      foo();
      a[i] = 0;
      bar();
   }
   a = (char*)177;
   _exit(1);
}

void foo ( void ) { }
void bar ( void ) { }
