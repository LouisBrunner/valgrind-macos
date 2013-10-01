#include <stdlib.h>
#include <stdio.h>
#include <valgrind.h>

void (*fnptr[256])(char*, char*);

#define BODY(f)                                 \
{                                               \
   fprintf(stderr, f);                          \
   calls++;                                     \
   (*fnptr[(int)*calls])(calls,seq);            \
}

void stacktrace(char*last, char* callsequence)
{
   fprintf(stderr, "\n");
   VALGRIND_PRINTF_BACKTRACE ("%s", callsequence);
}
__attribute__((noinline)) void f_a(char *calls, char*seq);
__attribute__((noinline)) void f_b(char *calls, char*seq);
__attribute__((noinline)) void f_c(char *calls, char*seq);
__attribute__((noinline)) void f_d(char *calls, char*seq);

__attribute__((noinline)) void f_a(char *calls, char*seq)
BODY("a")

__attribute__((noinline)) void f_b(char *calls, char*seq)
BODY("b")

__attribute__((noinline)) void f_c(char *calls, char*seq)
BODY("c");

__attribute__((noinline)) void f_d(char *calls, char*seq)
BODY("d");

void doit (int argc, char**argv)
{
   int i;
   for (i = 1; i < argc; i++) {
      char* calls = argv[i];
      char* seq = argv[i];
      calls--;
      BODY("test ")
   }
}

int main(int argc, char**argv)
{

   fnptr[0] = stacktrace;
   fnptr['a'] = f_a;
   fnptr['b'] = f_b;
   fnptr['c'] = f_c;
   fnptr['d'] = f_d;

   doit(argc, argv); // with default value of our argument.

   (void) VALGRIND_MONITOR_COMMAND("v.set merge-recursive-frames 3");
   doit(argc, argv);

   (void) VALGRIND_MONITOR_COMMAND("v.set merge-recursive-frames 2");
   doit(argc, argv);

   (void) VALGRIND_MONITOR_COMMAND("v.set merge-recursive-frames 1");
   doit(argc, argv);

   (void) VALGRIND_MONITOR_COMMAND("v.set merge-recursive-frames 0");
   doit(argc, argv);

   return 0;
}
