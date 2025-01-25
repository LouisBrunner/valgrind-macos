#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

timer_t timerid;

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
    res = timer_delete(timerid);
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

void sigalrm_timer_setup(const struct timespec *period)
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

    // SIGEVENT
    struct sigevent    sev;
    sev.sigev_notify = SIGEV_SIGNAL;
    sev.sigev_signo = SIGALRM;
    sev.sigev_value.sival_ptr = &timerid;
    res = timer_create(CLOCK_REALTIME, &sev, &timerid);
    CHECK_RES(res);
    
    // TIMER
    const struct itimerspec timer_val = {
        .it_interval = *period,
        .it_value = *period,
    };
    res = timer_settime(timerid, 0, &timer_val, NULL);
    CHECK_RES(res);
}

static void try_race_condition(void)
{
    ++tries;
    const struct timespec clk_period = {
        .tv_sec = 0,
        .tv_nsec = 20000
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
