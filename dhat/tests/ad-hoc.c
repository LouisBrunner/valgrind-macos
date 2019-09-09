#include "dhat/dhat.h"
#include <stdlib.h>
void g(void) {
   DHAT_AD_HOC_EVENT(30);
}

void f(void) {
   g();
   DHAT_AD_HOC_EVENT(20);
   g();
}

int main(void) {
   f();
   DHAT_AD_HOC_EVENT(10);
   f();

   // At one point malloc was broken with --mode=ad-hoc(!), and Valgrind was
   // printing messages like "VG_USERREQ__CLIENT_CALL1: func=0x0" when malloc
   // was called. So check that it's basically working...
   char* p = malloc(100);
   p = realloc(p, 200);
   free(p);

   return 0;
}

