
#if 0
/*
Subject:  valgrind glibc suppression
   Date:  Fri, 22 Mar 2002 23:54:44 -0500 (EST)
   From:  Alex Larsson <alexl@redhat.com>
     To:  jseward@acm.org


Hi.

I'm getting a lot of errors in __stpcpy(). I think this may be a bug in 
glibc. I didn't analyze the stpcpy asm in detail, so it might still be a 
valgrind bug, but it's probably a glibc bug.

Here is a test case:
*/
#endif

#include <string.h>
#include <stdlib.h>

int main()
{
  char *string;
  char buffer[10];
  
  string = malloc (1);
  string[0] = '\0';
  
  stpcpy (buffer, string);
}

#if 0
/*
Gives warnings like:
==10941== Use of uninitialised CPU condition code
==10941==    at 0x4034B9DA: __stpcpy (__stpcpy:36)
==10941==    by 0x402DF627: __libc_start_main (../sysdeps/generic/libc-start.c:129)
==10941==    by 0x80483D1: __libc_start_main@@GLIBC_2.0 (in /home/alex/other_src/valgrind-20020320/a.out)
==10941==    by <bogus frame pointer> ???

Here is the supression i use:
{
   __stpcpy(Value0)
   Value0
   fun:__stpcpy
   fun:*
}
*/
#endif
