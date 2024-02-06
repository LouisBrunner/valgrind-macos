#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[])
{
    size_t len = strlen(argv[1]);
    char*  buf = malloc(2 * len + 1);
    memcpy(buf, argv[1], len + 1);
    strncat(buf + len, buf, len);
    printf("%s\n", buf);
    free(buf);
    return 0;
}
