
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "tests/sys_mman.h"

void sig_hdlr ( int signo ) {
   printf ( "caught sig segv\n" ); exit(1);
}

int main ( void ) {
   char* badplace;
   printf ( "installing sig handler\n" );
   signal(SIGSEGV, sig_hdlr);
   printf ( "doing bad thing\n" );
   badplace = get_unmapped_page();
   *(int*)badplace = 0;
   printf ( "exited normally ?!\n" );
   return 0;
}

