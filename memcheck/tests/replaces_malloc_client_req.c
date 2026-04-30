#include "../memcheck.h"
#include <stdio.h>

int main(void)
{
    printf("Tool replaces malloc? %d\n", VALGRIND_REPLACES_MALLOC);
    const size_t buflen = 100;
    char buf[buflen];
    size_t actual_len = VALGRIND_GET_TOOLNAME(buf, buflen);

    if (actual_len)
        printf("The name of the tool is %s actual length %zu\n", buf, actual_len);
    else
        printf("Not running under valgrind\n");

    size_t query_len = VALGRIND_GET_TOOLNAME(NULL, 42);

    printf("Queried tool len %zu\n", query_len);

    const size_t small_buflen = 5;
    char small_buf[small_buflen];
    actual_len = VALGRIND_GET_TOOLNAME(small_buf, small_buflen);
    if (actual_len) {
        if (actual_len > small_buflen) {
            printf("The name of the tool [truncated] is %s actual length %zu\n", small_buf, actual_len);
        } else {
            printf("The name of the tool [not truncated] is %s actual length %zu\n", small_buf, actual_len);
        }
    }
    else
        printf("Still not running under valgrind\n");

    // "bad" cases
    actual_len = VALGRIND_GET_TOOLNAME(buf, 0);
    actual_len = VALGRIND_GET_TOOLNAME(NULL, buflen);
}
