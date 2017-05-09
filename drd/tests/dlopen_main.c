#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#include "dlopen_lib.h"

int main(int argc, char **argv)
{
  const char *lib = argc > 1 ? argv[1] : "./libfoo.so";
  void *handle;
  void (*function)();
  const char *error;

  handle = dlopen(lib, RTLD_NOW);
  if (!handle) {
    fputs (dlerror(), stderr);
    exit(1);
  }

  function = dlsym(handle, "foo");
  error = dlerror();
  if (error)  {
    fputs(error, stderr);
    exit(1);
  }

  (*function)();
  dlclose(handle);
  return 0;
}
