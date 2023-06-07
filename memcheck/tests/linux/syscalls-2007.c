/**
 * Test program for some Linux syscalls introduced during 2006 and 2007:
 * - epoll_pwait() was introduced in the 2.6.19 kernel, released in November
 *     2006.
 * - utimensat(), eventfd(), timerfd() and signalfd() were introduced in the
 *     2.6.22 kernel, released in July 2007.
 *
 * See also http://bugs.kde.org/show_bug.cgi?id=160907.
 */

#define _GNU_SOURCE

#include "../../config.h"
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#if defined(HAVE_SYS_EPOLL_H)
#include <sys/epoll.h>
#endif
#if defined(HAVE_SYS_EVENTFD_H)
#include <sys/eventfd.h>
#endif
#include <poll.h>
#if defined(HAVE_SYS_SIGNALFD_H)
#include <sys/signalfd.h>
#endif
#include <sys/stat.h>
#include <unistd.h>

int main (void)
{
#if defined(HAVE_SIGNALFD) && defined(HAVE_EVENTFD) \
    && defined(HAVE_EVENTFD_READ) && defined(HAVE_PPOLL)
  {
    sigset_t mask;
    int fd, fd2;
    eventfd_t ev;
    struct timespec ts = { .tv_sec = 1, .tv_nsec = 0 };
    struct pollfd pfd[2];

    sigemptyset (&mask);
    sigaddset (&mask, SIGUSR1);
    fd = signalfd (-1, &mask, 0);
    sigaddset (&mask, SIGUSR2);
    fd = signalfd (fd, &mask, 0);
    fd2 = eventfd (5, 0);
    eventfd_read (fd2, &ev);
    pfd[0].fd = fd;
    pfd[0].events = POLLIN|POLLOUT;
    pfd[1].fd = fd2;
    pfd[1].events = POLLIN|POLLOUT;
    ppoll (pfd, 2, &ts, &mask);
  }
#endif

#if defined(HAVE_UTIMENSAT)
  unlink("/tmp/valgrind-utimensat-test");
  close (creat ("/tmp/valgrind-utimensat-test", S_IRUSR | S_IWUSR));
  {
    struct timespec ts2[2] = { [0].tv_sec = 10000000, [1].tv_sec = 20000000 };
    utimensat (AT_FDCWD, "/tmp/valgrind-utimensat-test", ts2, 0);
  }
  unlink("/tmp/valgrind-utimensat-test");
#endif

#if defined(HAVE_EPOLL_CREATE) && defined(HAVE_EPOLL_PWAIT)
  {
    int fd3;
    struct epoll_event evs[10];
    sigset_t mask;

    sigemptyset (&mask);
    sigaddset (&mask, SIGUSR1);
    sigaddset (&mask, SIGUSR2);
    fd3 = epoll_create (10);
    epoll_pwait (fd3, evs, 10, 0, &mask);
  }
#endif

#if defined(HAVE_EPOLL_CREATE) && defined(HAVE_EPOLL_PWAIT)
  {
    int fd3;
    struct epoll_event evs[10];

    fd3 = epoll_create (10);
    /* epoll_pwait can take a NULL sigmask. */
    epoll_pwait (fd3, evs, 10, 1, NULL);
  }
#endif

  return 0;
}
