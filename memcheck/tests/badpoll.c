#include <assert.h>
#include <stdlib.h>
#include <poll.h>

// At one point, poll()'s checking was not done accurately.  This test
// exposes this -- previously Memcheck only found one error, now if finds
// two.

int main(void)
{
   // Under-allocate by one byte so we get an addressability error.
   struct pollfd* ufds = malloc(2 * sizeof(struct pollfd) - 1);
   assert(ufds);

   ufds[0].fd = 0;
   ufds[0].events = 0;
   //ufds[1].fd = 0;    // leave undefined so we get another error.
   ufds[1].events = 0;

   // Previously, the bounds-error due to the under-allocation was detected,
   // but not the undefined value error due to ufds[1].fd not being defined.
   poll(ufds, 2, 200);

   return 0;
}
