// this example taken from
// https://gist.github.com/sebcat/a61b2ed859cee16c560e

// FreeBSD has pdfork which uses file descriptors instead of PIDs for
// managing processes. This allows for race-less process supervision and
// for using the traditional multiplexing calls (e.g., select). There's no
// need wait()ing for SIGCHLD, &c.
//
// This example forks a child that sleeps for a certain time while the
// parent is waiting for the child to exit, or for a timeout to be reached,
// whichever comes first. The time values (in seconds) are provided from argv
//
// % ./pdfork
// usage: pdfork <child-sleep> <parent-wait>
// % ./pdfork 2 10
// child: exit
// parent: child exited
// % ./pdfork 10 2
// parent: kill


#include <sys/types.h>
#include <sys/select.h>
#include <sys/procdesc.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#define DURATION_MAX 100

static long time_or_die(const char *str, const char *name) {
    char *cptr;
    long res;

    res = strtol(str, &cptr, 10);
    if (errno == ERANGE || *cptr != '\0') {
         fprintf(stderr, "invalid %s parameter\n", name);
         exit(EXIT_FAILURE);
    } else if (res < 0 || res > DURATION_MAX) {
         fprintf(stderr, "%s out of permitted range\n", name);
         exit(EXIT_FAILURE);
    }

    return res;
}

int main(int argc, char *argv[]) {
    struct timeval to={0,0};
    long child_sleep, parent_wait;
    int fd, ret;
    fd_set fds;

    if (argc != 3) {
       fprintf(stderr, "usage: pdfork <child-sleep> <parent-wait>\n");
       return EXIT_FAILURE;
    }

    child_sleep = time_or_die(argv[1], "child-sleep");
    parent_wait = time_or_die(argv[2], "parent-wait");
    if ((ret = pdfork(&fd, 0)) == -1) {
        perror("pdfork");
        return EXIT_FAILURE;
    } else if (ret == 0) {
        sleep((unsigned int)child_sleep);
        fprintf(stderr, "child: exit\n");
        exit(0);
    } else {
        FD_ZERO(&fds);
        FD_SET(fd, &fds);
        to.tv_sec = parent_wait;
        if ((ret = select(fd+1, &fds, NULL, NULL, &to)) == -1) {
            perror("select");
            return EXIT_FAILURE;
        } else if (ret == 0) {
             fprintf(stderr, "parent: kill fd %d\n", fd);
             pid_t pid;
             pdgetpid(fd, &pid);
             pdkill(fd, 9);
        } else {
            fprintf(stderr, "parent: child exited\n");
        }
    }
        
    // and generate a couple of errors for valgrind
    int *badfd = malloc(sizeof(int));
    free(badfd);
    if (pdfork(badfd, 0) > 0)
    {
       fprintf(stderr, "parent after 1st bad pdfork\n");
       int anotherfd;
       int badflag;
       pid_t* pbadpid = malloc(sizeof(pid_t));
       free(pbadpid);
       pdgetpid(anotherfd, pbadpid);
       pdfork(&anotherfd, badflag);
    }

    return EXIT_SUCCESS;
}
 
