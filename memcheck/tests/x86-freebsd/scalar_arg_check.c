#include "../freebsd/scalar.h"

int main(void)
{
   /* sendfile uses 8 args */
   SY(SYS_sendfile, 101, 102, 103, 104, 105, 106, 107, 108);
}

