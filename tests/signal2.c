
#include <stdio.h>
#include <signal.h>

void sig_hdlr ( int signo )
{
   printf ( "caught sig segv\n" );
   exit(1);
}

int main ( void )
{
   printf ( "installing sig handler\n" );
   signal(SIGSEGV, sig_hdlr);
   printf ( "doing bad thing\n" );
   * (int*) 0 = 0;
   printf ( "exited normally ?!\n" );
   return 0;
}
