
#include <stdio.h>
#include <signal.h>

volatile int spin;

void sig_hdlr ( int signo )
{
   printf ( "caught signal\n" );
   spin = 0;
   printf ( "signal returns\n" );
}

void main ( void )
{
   spin = 1;
   printf ( "installing sig handler\n" );
   signal(SIGINT, sig_hdlr);
   printf ( "entering busy wait\n" );
   while (spin) { };
   printf ( "exited\n" );
}
