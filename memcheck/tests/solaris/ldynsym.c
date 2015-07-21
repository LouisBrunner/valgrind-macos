/* Test that the .SUNW_ldynsym section is correctly read. */

#include <stdlib.h>

__attribute__((noinline))
static void function3(size_t size)
{
   malloc(size);
}

__attribute__((noinline))
static void function2(size_t size)
{
   function3(size);
}

__attribute__((noinline))
static void function(size_t size)
{
   function2(size);
}

int main(void)
{
   function(10);

   return 0;
}

