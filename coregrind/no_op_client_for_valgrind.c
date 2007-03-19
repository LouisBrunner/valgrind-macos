
/* This program doesn't do anything.  So why is it here?  It's a
   helper for ptraced-based launchers (eg aix5).  They can't run 'no
   program' if the user types "valgrind --help", so they run this
   do-nothing program.  m_main notices that and turns the exe name
   back into NULL.  Then --help, --version etc work as they should. */

#include <stdio.h>
int main ( void )
{
  fprintf(stderr, 
     "This program (part of Valgrind) does nothing except print\n"
     "this text.  You should not see this text.  If you do, some\n"
     "part of valgrind's launch mechanism is not working correctly.\n");
  return 0;
}
