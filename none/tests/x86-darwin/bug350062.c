#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Refer https://bugs.kde.org/show_bug.cgi?id=350062

int main(int argc, char **argv)
{
    double x = 1.1;
    double i = floor(x);
    
    (void)i;

    fprintf(stderr, "PASS\n");
    return 0;
}
