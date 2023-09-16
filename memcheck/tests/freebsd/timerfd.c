/* Copied from the Linux manpage
 * with the printed times rounded to
 * seconds for reproducibility.
 * And some errors added.
 */

#include <err.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>
#include <math.h>
#include <errno.h>
#include <assert.h>
#include "../../memcheck.h"

static void
print_elapsed_time(void)
{
    int                     secs, nsecs;
    static int              first_call = 1;
    struct timespec         curr;
    static struct timespec  start;
    int                  round_secs;

    if (first_call) {
        first_call = 0;
        if (clock_gettime(CLOCK_MONOTONIC, &start) == -1)
            err(EXIT_FAILURE, "clock_gettime");
    }

    if (clock_gettime(CLOCK_MONOTONIC, &curr) == -1)
        err(EXIT_FAILURE, "clock_gettime");

    secs = curr.tv_sec - start.tv_sec;
    nsecs = curr.tv_nsec - start.tv_nsec;
    if (nsecs < 0) {
        secs--;
        nsecs += 1000000000;
    }
    round_secs = round((secs*1e9 + nsecs)/1e9);
    printf("%d: ", round_secs);
}

int
main(int argc, char *argv[])
{
    int                fd;
    ssize_t            s;
    uint64_t           exp, tot_exp, max_exp;
    struct timespec    now;
    struct itimerspec  new_value;

    if (argc != 2 && argc != 4) {
        fprintf(stderr, "%s init-secs [interval-secs max-exp]\n",
                argv[0]);
        exit(EXIT_FAILURE);
    }

    if (clock_gettime(CLOCK_REALTIME, &now) == -1)
        err(EXIT_FAILURE, "clock_gettime");

    /* Create a CLOCK_REALTIME absolute timer with initial
       expiration and interval as specified in command line. */

    new_value.it_value.tv_sec = now.tv_sec + atoi(argv[1]);
    new_value.it_value.tv_nsec = now.tv_nsec;
    if (argc == 2) {
        new_value.it_interval.tv_sec = 0;
        max_exp = 1;
    } else {
        new_value.it_interval.tv_sec = atoi(argv[2]);
        max_exp = atoi(argv[3]);
    }
    new_value.it_interval.tv_nsec = 0;

    fd = timerfd_create(CLOCK_REALTIME, 0);
    if (fd == -1)
        err(EXIT_FAILURE, "timerfd_create");

    if (timerfd_settime(fd, TFD_TIMER_ABSTIME, &new_value, NULL) == -1)
        err(EXIT_FAILURE, "timerfd_settime");

    print_elapsed_time();
    printf("timer started\n");

    for (tot_exp = 0; tot_exp < max_exp;) {
        s = read(fd, &exp, sizeof(uint64_t));
        if (s != sizeof(uint64_t))
            err(EXIT_FAILURE, "read");

        tot_exp += exp;
        print_elapsed_time();
        printf("read: %" PRIu64 "; total=%" PRIu64 "\n", exp, tot_exp);
    }

    close(fd);

    {
        int a = CLOCK_REALTIME;
        int b = TFD_CLOEXEC;
        int c = TFD_TIMER_ABSTIME;
        VALGRIND_MAKE_MEM_UNDEFINED(&a, sizeof(a));
        VALGRIND_MAKE_MEM_UNDEFINED(&b, sizeof(b));
        VALGRIND_MAKE_MEM_UNDEFINED(&c, sizeof(c));
        struct itimerspec * get_ts = malloc(sizeof(*get_ts) - 2);
        struct itimerspec * set_ts = malloc(sizeof(*set_ts) - 2);
        struct itimerspec ts;
        int retval;

        errno = 0;
        /* uninit clockid and flag but should work */
        int fd2 = timerfd_create(a, b);
        assert(fd2 != -1);
        assert(errno == 0); 
        /* bad flag should fail */
        timerfd_create(CLOCK_REALTIME, 1000000);
        /* bad clockid should fail */
        timerfd_create(1000000, TFD_CLOEXEC);

        /* memory too small for requested get */
        timerfd_gettime(fd2, get_ts);
        /* should succeed */
        timerfd_gettime(fd2, &ts);
        ts.it_interval.tv_nsec += 100000;

        /* uninit flag */
        timerfd_settime(fd2, c, &ts, NULL);
        errno = 0;
        ts.it_interval.tv_nsec += 100000;
        /* memory too small for requested old value */
        retval = timerfd_settime(fd2, TFD_TIMER_ABSTIME, &ts, set_ts);
        assert(retval == 0);
        assert(errno == 0);

        VALGRIND_MAKE_MEM_UNDEFINED(&fd2, sizeof(fd2));
        timerfd_gettime(fd2, &ts);
        ts.it_interval.tv_nsec += 100000;
        timerfd_settime(fd2, TFD_TIMER_ABSTIME, &ts, NULL);


        free(get_ts); 
        free(set_ts); 
    }

    exit(EXIT_SUCCESS);
}

