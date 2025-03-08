#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "memcheck/memcheck.h"

// this non-text symbol is required to trigger ada demangling
const char* __gnat_ada_main_program_name = "some string";

// the functions are transcribed from symbols in an
// Ada hello world using a package
void system__file_io__write_buf(char* m)
{
   if (m[4])
   {
      exit(-1);
   }
}

void ada__text_io__put_line(char *m)
{
   system__file_io__write_buf(m);
}

void bad_print__uninit_print(char *m)
{
   ada__text_io__put_line(m);
}

void _ada_main(char* m)
{
   bad_print__uninit_print(m);
}

int main(void)
{
   char* HW = strdup("Hello, world!\n");
   VALGRIND_MAKE_MEM_UNDEFINED(HW+4, 1);
   
   _ada_main(HW);
}
