#include <assert.h>
#include <unistd.h>

int main(void)
{
   execve(NULL,        NULL, NULL);
   execve("../../tests/true", NULL, NULL);
   assert(0);  // shouldn't get here
}
