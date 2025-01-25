#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define CHECK_RES(res)                                                                                                 \
    if (res == -1)                                                                                                     \
    {                                                                                                                  \
        fprintf(stderr, "unexpected error at line %d: %s", __LINE__, strerror(errno));                                 \
        exit(EXIT_FAILURE);                                                                                            \
    }

static int tries = 0;

static void sigalrm_handler(int signum)
{
    (void)signum;
    // no need to do anything for the experiment
}

// using this handler in place of SIG_DFL to monitor when the "default" handler is called
static void sigalrm_default_handler(int signum)
{
    (void)signum;
    fprintf(stderr,
            "ERROR: unreachable code reached: sigalarm timer was disarmed, but still received SIGALRM (race condition "
            "tried %d times)\n",
            tries);
    exit(EXIT_FAILURE);
}

void sigalrm_timer_destroy(void)
{
    int res;

    // REMOVE TIMER
    const struct itimerval zero = {
        .it_interval =
            {
                .tv_sec = 0,
                .tv_usec = 0,
            },
        .it_value =
            {
                .tv_sec = 0,
                .tv_usec = 0,
            },
    };
    res = setitimer(ITIMER_REAL, &zero, NULL);
    CHECK_RES(res)

    // AND THEN REMOVE SIGNAL HANDLER
    struct sigaction sigact;
    sigact.sa_flags = 0;
    sigact.sa_handler = sigalrm_default_handler; // using this handler in place of SIG_DFL to monitor when the "default"
                                                 // handler is called
    res = sigemptyset(&sigact.sa_mask);
    CHECK_RES(res)
    res = sigaction(SIGALRM, &sigact, NULL);
    CHECK_RES(res)
}

void sigalrm_timer_setup(const struct timeval *period)
{
    int res;

    // SIGNAL
    struct sigaction sigact;
    sigact.sa_flags = 0;
    sigact.sa_handler = sigalrm_handler;
    res = sigemptyset(&sigact.sa_mask);
    CHECK_RES(res)
    res = sigaction(SIGALRM, &sigact, NULL);
    CHECK_RES(res)

    // TIMER
    const struct itimerval timer_val = {
        .it_interval = *period,
        .it_value = *period,
    };
    res = setitimer(ITIMER_REAL, &timer_val, NULL);
    CHECK_RES(res);
}

static void try_race_condition(void)
{
    ++tries;
    const struct timeval clk_period = {
        .tv_sec = 0,
        .tv_usec = 10,
    };
    sigalrm_timer_setup(&clk_period);
    sigalrm_timer_destroy();
}

int main(void)
{
    for (int i = 0; i < 10000; ++i)
    {
        try_race_condition();
    }
}
