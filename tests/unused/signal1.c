
#include <stdio.h>
#include <signal.h>

/* spurious comment only here to test cvs mail notifications. */

volatile int spin;

void sig_hdlr ( int signo )
{
   printf ( "caught signal\n" );
   spin = 0;
   printf ( "signal returns\n" );
}

int main ( void )
{
   spin = 1;
   printf ( "installing sig handler\n" );
   signal(SIGINT, sig_hdlr);
   printf ( "entering busy wait\n" );
   while (spin) { };
   printf ( "exited\n" );
   return 0;
}
