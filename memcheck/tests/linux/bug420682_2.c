/*
 * See https://bugs.kde.org/show_bug.cgi?id=420682
 *
 * Some scalar-style errors.
 */
#include <unistd.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <libaio.h>
#include "../../../memcheck/memcheck.h"

int main(void)
{
    long* px  = malloc(sizeof(long));
    long  x0  = px[0];
    syscall(__NR_io_pgetevents, x0-1, x0, x0, x0+1, x0+1, x0+1);

    struct timespec ts;
    struct aio_sigset {
        const sigset_t* sigmask;
        size_t sigsetsize;
    } as;
    struct io_event event;

    VALGRIND_MAKE_MEM_UNDEFINED(&event, sizeof(event));
    VALGRIND_MAKE_MEM_UNDEFINED(&ts, sizeof(ts));
    VALGRIND_MAKE_MEM_UNDEFINED(&as, sizeof(as));

    syscall(__NR_io_pgetevents, x0-1, x0, x0, &event, &ts, &as);

    free(px);
}
