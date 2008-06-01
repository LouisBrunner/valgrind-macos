
/* This test case was originally written by Nicholas Nethercote. */




/* For 'x', we get an uninitialised error for every addition to it.  For
   each one we get one origin identified, even though most of them involve
   more than one undefined value. */

/* For 'y', we get a single uninitialised value error, on the value handed
   to the exit() system call.  Fair enough.

   An important question is: which of the origins is reported in the
   error?  Well, considering that (1) m_execontext allocates ECUs
   (origin tags, basically) in increasing order, and (2) memcheck's
   instrumentation for dealing with two uninitialised sources simply
   involves 'max'-ing the otags, we expect the origin to be attributed
   to the last of the 8 mallocs, that is, to p_ui8.
*/

#include <stdlib.h>
#include <stdio.h>

static int x = 0;
static int y = 0;

int main(void)
{
   // Do them separately rather than all in one array so they all have
   // different origins.
   int* p_ui1 = malloc(sizeof(int));
   int* p_ui2 = malloc(sizeof(int));
   int* p_ui3 = malloc(sizeof(int));
   int* p_ui4 = malloc(sizeof(int));
   int* p_ui5 = malloc(sizeof(int));
   int* p_ui6 = malloc(sizeof(int));
   int* p_ui7 = malloc(sizeof(int));
   int* p_ui8 = malloc(sizeof(int));
   int  ui1 = *p_ui1;
   int  ui2 = *p_ui2;
   int  ui3 = *p_ui3;
   int  ui4 = *p_ui4;
   int  ui5 = *p_ui5;
   int  ui6 = *p_ui6;
   int  ui7 = *p_ui7;
   int  ui8 = *p_ui8;

#define P   printf("huh?")

   x += (ui1                                    == 0x12345678 ? P : 23);
   x += (ui1 +ui2                               == 0x12345678 ? P : 24);
   x += (ui1 +ui2 +ui3                          == 0x12345678 ? P : 25);
   x += (ui1 +ui2 +ui3 +ui4                     == 0x12345678 ? P : 26);
   x += (ui1 +ui2 +ui3 +ui4 +ui5                == 0x12345678 ? P : 27);
   x += (ui1 +ui2 +ui3 +ui4 +ui5 +ui6           == 0x12345678 ? P : 28);
   x += (ui1 +ui2 +ui3 +ui4 +ui5 +ui6 +ui7      == 0x12345678 ? P : 29);
   x += (ui1 +ui2 +ui3 +ui4 +ui5 +ui6 +ui7 +ui8 == 0x12345678 ? P : 30);

   y += (ui1                                   );
   y += (ui1 +ui2                              );
   y += (ui1 +ui2 +ui3                         );
   y += (ui1 +ui2 +ui3 +ui4                    );
   y += (ui1 +ui2 +ui3 +ui4 +ui5               );
   y += (ui1 +ui2 +ui3 +ui4 +ui5 +ui6          );
   y += (ui1 +ui2 +ui3 +ui4 +ui5 +ui6 +ui7     );
   y += (ui1 +ui2 +ui3 +ui4 +ui5 +ui6 +ui7 +ui8);

   return y & 1;
}
