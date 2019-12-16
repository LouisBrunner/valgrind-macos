#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#include <errno.h>

int main() {

   sigset_t oldset;

   const int TRASH = 123;

   // TRASH variable, that represents HOW parameter, should be ignored in this case
   // Taken from: http://man7.org/linux/man-pages/man2/sigprocmask.2.html :
   //    If set is NULL, then the signal mask is unchanged (i.e., HOW is
   //    ignored), but the current value of the signal mask is nevertheless
   //    returned in oldset (if it is not NULL).
   if (sigprocmask(TRASH, NULL, &oldset) == 0) {
       exit(EXIT_FAILURE);
   } else {
       exit(EXIT_SUCCESS);
   }

}
